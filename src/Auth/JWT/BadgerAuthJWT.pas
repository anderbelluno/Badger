unit BadgerAuthJWT;

interface

uses
  SysUtils, Classes, Badger, BadgerTypes, BadgerHttpStatus, BadgerJWTClaims, BadgerJWTUtils, EncdDecd, superobject;

type
  TBadgerJWTAuth = class
  private
    FSecret: string;
    FStoragePath: string;
    FProtectedRoutes: TStringList;
    function MiddlewareProc(Request: THTTPRequest; out Response: THTTPResponse): Boolean;
  public
    constructor Create(const ASecret: string; const AStoragePath: string = '');
    destructor Destroy; override;

    function GenerateToken(const AUserID, ARole: string; AExpiresInHours: Integer = 24): string;
    function ValidateToken(const AToken: string): TBadgerJWTClaims;

    procedure RegisterMiddleware(Badger: TBadger; const ProtectedRoutes: array of string);
  end;

implementation

{ TBadgerJWTAuth }

constructor TBadgerJWTAuth.Create(const ASecret: string; const AStoragePath: string);
begin
  inherited Create;
  FSecret := ASecret;
  FStoragePath := AStoragePath;
  FProtectedRoutes := TStringList.Create;
end;

destructor TBadgerJWTAuth.Destroy;
begin
  FProtectedRoutes.Free;
  inherited;
end;

function TBadgerJWTAuth.GenerateToken(const AUserID, ARole: string; AExpiresInHours: Integer): string;
var
  LHeader, LPayload: string;
  LClaims: TBadgerJWTClaims;
begin
  LHeader := '{"alg":"HS256","typ":"JWT"}';
  LHeader := EncodeString(LHeader);

  LClaims := TBadgerJWTClaims.Create;
  try
    LClaims.UserID := AUserID;
    LClaims.Role := ARole;
    LClaims.Iss := DateTimeToUnix(Now);
    LClaims.Exp := DateTimeToUnix(Now + AExpiresInHours / 24);
    LPayload := LClaims.ToJSON.AsJSON;
    LPayload := EncodeString(LPayload);
    Result := Trim( LHeader + '.' + LPayload + '.' + CreateSignature(LHeader, LPayload, FSecret) );

    if FStoragePath <> '' then
      SaveToken(AUserID, Result, FStoragePath);
  finally
    LClaims.Free;
  end;
end;

function TBadgerJWTAuth.ValidateToken(const AToken: string): TBadgerJWTClaims;
var
  LParts: TStringList;
  LHeader, LPayload, LSignature, LExpectedSignature: string;
  LJSON: ISuperObject;
begin
  LParts := TStringList.Create;
  try
    ExtractStrings(['.'], [], PChar(AToken), LParts);
    if LParts.Count <> 3 then
      raise Exception.Create('Token inválido');
    LHeader := LParts[0];
    LPayload := LParts[1];
    LSignature := LParts[2];
    LExpectedSignature := CreateSignature(LHeader, LPayload, FSecret);
    if LSignature <> LExpectedSignature then
      raise Exception.Create('Assinatura inválida');
    LJSON := SO(DecodeString(LPayload));
    Result := TBadgerJWTClaims.FromJSON(LJSON);
    if (Result.Exp > 0) and (DateTimeToUnix(Now) > Result.Exp) then
    begin
      Result.Free;
      raise Exception.Create('Token expirado');
    end;
    if (FStoragePath <> '') and (LoadToken(Result.UserID, FStoragePath) <> AToken) then
    begin
      Result.Free;
      raise Exception.Create('Token não encontrado');
    end;
  finally
    LParts.Free;
  end;
end;

function TBadgerJWTAuth.MiddlewareProc(Request: THTTPRequest; out Response: THTTPResponse): Boolean;
var
  I: Integer;
  LToken: string;
  LClaims: TBadgerJWTClaims;
  LRouteMatch: Boolean;
begin
  LRouteMatch := False;
  for I := 0 to FProtectedRoutes.Count - 1 do
    if SameText(Request.URI, FProtectedRoutes[I]) then
    begin
      LRouteMatch := True;
      Break;
    end;

  if not LRouteMatch then
  begin
    Result := True;
    Exit;
  end;

  LToken := '';
  for I := 0 to Request.Headers.Count - 1 do
    if Pos('Authorization:', Request.Headers[I]) > 0 then
    begin
      LToken := Trim(Copy(Request.Headers[I], Pos(':', Request.Headers[I]) + 1, Length(Request.Headers[I])));
      if Pos('Bearer ', LToken) = 1 then
        LToken := Copy(LToken, 8, MaxInt);
      Break;
    end;

  if LToken = '' then
  begin
    Response.StatusCode := HTTP_UNAUTHORIZED;
    //Response.StatusText := 'Unauthorized';
    Response.Body := '{"error":"Token não fornecido"}';
    Response.ContentType := APPLICATION_JSON;
    Result := False;
    Exit;
  end;

  try
    LClaims := ValidateToken(LToken);
    try
      Request.UserID := LClaims.UserID;
      Request.UserRole := LClaims.Role;
      Result := True;
    finally
      LClaims.Free;
    end;
  except
    on E: Exception do
    begin
      Response.StatusCode := HTTP_UNAUTHORIZED;
      //Response.StatusText := 'Unauthorized';
      Response.Body := '{"error":"' + E.Message + '"}';
      Response.ContentType := APPLICATION_JSON;
      Result := False;
    end;
  end;
end;

procedure TBadgerJWTAuth.RegisterMiddleware(Badger: TBadger; const ProtectedRoutes: array of string);
var
  I: Integer;
begin
  FProtectedRoutes.Clear;
  for I := Low(ProtectedRoutes) to High(ProtectedRoutes) do
    FProtectedRoutes.Add(ProtectedRoutes[I]);
  Badger.AddMiddleware(MiddlewareProc);
end;

end.

