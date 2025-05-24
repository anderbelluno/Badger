unit BadgerAuthJWT;
{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, Badger, BadgerTypes, BadgerHttpStatus, BadgerJWTClaims,
  BadgerJWTUtils, superobject;

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
    function GenerateRefreshToken(const AUserID: string; AExpiresInDays: Integer = 7): string;
    function ValidateRefreshToken(const AToken: string): TBadgerJWTClaims;
    function RefreshToken(const ARefreshToken: string): string;

    procedure RegisterMiddleware(Badger: TBadger; const ProtectedRoutes: array of string);
  end;

implementation

const
  APPLICATION_JSON = 'application/json';

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
  LRefreshToken: string;
  LJSONArray: ISuperObject;
  LAccessTokenObj, LRefreshTokenObj: ISuperObject;
begin
  LHeader := '{"alg":"HS256","typ":"JWT"}';
  LHeader := CustomEncodeBase64(LHeader, True);

  LClaims := TBadgerJWTClaims.Create;
  try
    LClaims.UserID := AUserID;
    LClaims.Role := ARole;
    LClaims.Iss := DateTimeToUnix(Now);
    LClaims.Exp := DateTimeToUnix(Now + AExpiresInHours / 24);
    LPayload := LClaims.ToJSON.AsJSON;
    LPayload := CustomEncodeBase64(LPayload, True);

    Result := LHeader + '.' + LPayload + '.' + CreateSignature(LHeader, LPayload, FSecret);

    if FStoragePath <> '' then
      SaveToken(AUserID, Result, FStoragePath);

    LRefreshToken := GenerateRefreshToken(AUserID);
      {$IFNDEF FPC}
        {$IF CompilerVersion >= 20}  //Necessário pois foi usado versão diferente do SuperObject
            LJSONArray := SA();
        {$ELSE}
            LJSONArray := SA([]);
        {$IFEND}
      {$ELSE}
            LJSONArray := SA([]);
        {$ENDIF}
    LAccessTokenObj := SO();
    LAccessTokenObj.S['access_token'] := Result;

    LRefreshTokenObj := SO();
    LRefreshTokenObj.S['refresh_token'] := LRefreshToken;

    LJSONArray.AsArray.Add(LAccessTokenObj);
    LJSONArray.AsArray.Add(LRefreshTokenObj);

    Result := LJSONArray.AsJSON;

  finally
    LClaims.Free;
  end;
end;

function TBadgerJWTAuth.GenerateRefreshToken(const AUserID: string; AExpiresInDays: Integer): string;
var
  LHeader, LPayload: string;
  LClaims: TBadgerJWTClaims;
begin
  LHeader := '{"alg":"HS256","typ":"Refresh"}';
  LHeader := CustomEncodeBase64(LHeader, True);

  LClaims := TBadgerJWTClaims.Create;
  try
    LClaims.UserID := AUserID;
    LClaims.Iss := DateTimeToUnix(Now);
    LClaims.Exp := DateTimeToUnix(Now + AExpiresInDays);
    LPayload := LClaims.ToJSON.AsJSON;
    LPayload := CustomEncodeBase64(LPayload, True);

    Result := LHeader + '.' + LPayload + '.' + CreateSignature(LHeader, LPayload, FSecret);

    if FStoragePath <> '' then
      SaveRefreshToken(AUserID, Result, FStoragePath);
  finally
    LClaims.Free;
  end;
end;

function TBadgerJWTAuth.ValidateToken(const AToken: string): TBadgerJWTClaims;
var
  LParts: TStringList;
  LHeader, LPayload, LSignature, LExpectedSignature: string;
  LJSON: ISuperObject;
  FToken : String;
begin
  if (AToken = '') or (Pos('.', AToken) = 0) then
    raise Exception.Create('Token inválido: formato incorreto');

  LParts := TStringList.Create;
  try
    ExtractStrings(['.'], [], PChar(AToken), LParts);
    if LParts.Count <> 3 then
      raise Exception.Create('Token inválido: deve conter 3 partes');
    LHeader := LParts[0];
    LPayload := LParts[1];
    LSignature := LParts[2];

    LExpectedSignature := CreateSignature(LHeader, LPayload, FSecret);
    if LSignature <> LExpectedSignature then
      raise Exception.Create('Assinatura inválida');

    LJSON := SO(CustomDecodeBase64(LPayload));
    Result := TBadgerJWTClaims.FromJSON(LJSON);
    if (Result.Exp > 0) and (DateTimeToUnix(Now) > Result.Exp) then
    begin
      Result.Free;
      raise Exception.Create('Token expirado');
    end;
    FToken := Trim (LoadToken(Result.UserID, FStoragePath) );
    if (FStoragePath <> '') and (FToken <> AToken) then
    begin
      Result.Free;
      raise Exception.Create('Token não encontrado');
    end;
  finally
    LParts.Free;
  end;
end;

function TBadgerJWTAuth.ValidateRefreshToken(const AToken: string) : TBadgerJWTClaims;
var
  LParts: TStringList;
  LHeader, LPayload, LSignature, LExpectedSignature: string;
  LJSON: ISuperObject;
  FToken : String;
begin
  if (AToken = '') or (Pos('.', AToken) = 0) then
    raise Exception.Create('Refresh token inválido: formato incorreto');

  LParts := TStringList.Create;
  try
    ExtractStrings(['.'], [], PChar(AToken), LParts);
    if LParts.Count <> 3 then
      raise Exception.Create('Refresh token inválido: deve conter 3 partes');
    LHeader := LParts[0];
    LPayload := LParts[1];
    LSignature := LParts[2];

    LExpectedSignature := CreateSignature(LHeader, LPayload, FSecret);
    if LSignature <> LExpectedSignature then
      raise Exception.Create('Assinatura inválida');

    LJSON := SO(CustomDecodeBase64(LPayload));
    Result := TBadgerJWTClaims.FromJSON(LJSON);
    if (Result.Exp > 0) and (DateTimeToUnix(Now) > Result.Exp) then
    begin
      Result.Free;
      raise Exception.Create('Refresh token expirado');
    end;
    FToken := Trim ( LoadRefreshToken(Result.UserID, FStoragePath) );
    if (FStoragePath <> '') and ( FToken <> AToken) then
    begin
      Result.Free;
      raise Exception.Create('Refresh token não encontrado');
    end;
  finally
    LParts.Free;
  end;
end;

function TBadgerJWTAuth.RefreshToken(const ARefreshToken: string): string;
var
  LClaims: TBadgerJWTClaims;
  LResponse: ISuperObject;
  LNewAccessToken, LNewRefreshToken: string;
begin
  LClaims := ValidateRefreshToken(ARefreshToken);
  try
    LNewAccessToken := GenerateToken(LClaims.UserID, LClaims.Role, 24);
    Result := LNewAccessToken;
  finally
    LClaims.Free;
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
    if Pos('Authorization=', Request.Headers[I]) > 0 then
    begin
      LToken := Trim(Copy(Request.Headers[I], Pos('=', Request.Headers[I]) + 1, Length(Request.Headers[I])));
      if Pos('Bearer ', LToken) = 1 then
        LToken := Copy(LToken, 8, MaxInt);
      Break;
    end;

  if LToken = '' then
  begin
    Response.StatusCode := HTTP_UNAUTHORIZED;
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
