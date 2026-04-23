unit BadgerBasicAuth;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, Badger, BadgerTypes, BadgerHttpStatus, BadgerUtils, BadgerJWTUtils;

type
  TBasicAuth = class
  private
    FUsername: string;
    FPasswordHash: string;
    FPasswordSalt: string;
    FProtectedRoutes: TStringList;
    function ConstantTimeEquals(const A, B: string): Boolean;
    function HashPassword(const APassword: string): string;
    function BuildPasswordSalt: string;
    function GetPassword: string;
    procedure SetPassword(const AValue: string);
    function Check(var Request: THTTPRequest; var Response: THTTPResponse): Boolean;
  public
    constructor Create(const AUsername, APassword: string);
    destructor Destroy; override;
    procedure RegisterProtectedRoutes(Badger: TBadger; const ProtectedRoutes: array of string);
    property Username: string read FUsername write FUsername;
    property Password: string read GetPassword write SetPassword;
  end;

implementation

const
  APPLICATION_JSON = 'application/json';

{ TBasicAuth }

function NormalizeRoute(const ARoute: string): string;
begin
  Result := Trim(ARoute);
  while (Length(Result) > 1) and (Result[Length(Result)] = '/') do
    Delete(Result, Length(Result), 1);
end;

function IsProtectedRoute(const ARequestURI, AProtectedRoute: string): Boolean;
var
  RequestURI, ProtectedRoute: string;
begin
  RequestURI := NormalizeRoute(ARequestURI);
  ProtectedRoute := NormalizeRoute(AProtectedRoute);

  if ProtectedRoute = '' then
  begin
    Result := False;
    Exit;
  end;

  if ProtectedRoute = '/' then
  begin
    Result := True;
    Exit;
  end;

  if SameText(RequestURI, ProtectedRoute) then
  begin
    Result := True;
    Exit;
  end;

  Result :=
    (Length(RequestURI) > Length(ProtectedRoute)) and
    (CompareText(Copy(RequestURI, 1, Length(ProtectedRoute)), ProtectedRoute) = 0) and
    (RequestURI[Length(ProtectedRoute) + 1] = '/');
end;

constructor TBasicAuth.Create(const AUsername, APassword: string);
begin
  inherited Create;
  Randomize;
  FUsername := AUsername;
  FPasswordSalt := BuildPasswordSalt;
  SetPassword(APassword);
  FProtectedRoutes := TStringList.Create;
end;

destructor TBasicAuth.Destroy;
begin
  FProtectedRoutes.Free;
  inherited;
end;

function TBasicAuth.BuildPasswordSalt: string;
begin
  Result := IntToHex(DateTimeToUnix(Now), 8) + IntToHex(Random(MaxInt), 8);
end;

function TBasicAuth.HashPassword(const APassword: string): string;
begin
  Result := CreateSignature('BASIC_AUTH', CustomEncodeBase64(APassword, True), FPasswordSalt);
end;

function TBasicAuth.ConstantTimeEquals(const A, B: string): Boolean;
var
  I, ALen, BLen, MaxLen: Integer;
  Diff: Cardinal;
  CA, CB: Cardinal;
begin
  ALen := Length(A);
  BLen := Length(B);
  if ALen > BLen then
    MaxLen := ALen
  else
    MaxLen := BLen;

  Diff := Cardinal(ALen xor BLen);
  for I := 1 to MaxLen do
  begin
    if I <= ALen then
      CA := Ord(A[I])
    else
      CA := 0;

    if I <= BLen then
      CB := Ord(B[I])
    else
      CB := 0;

    Diff := Diff or (CA xor CB);
  end;
  Result := Diff = 0;
end;

function TBasicAuth.GetPassword: string;
begin
  // Intencionalmente não expõe a senha em claro após inicialização.
  Result := '';
end;

procedure TBasicAuth.SetPassword(const AValue: string);
begin
  FPasswordHash := HashPassword(AValue);
end;

function TBasicAuth.Check(var Request: THTTPRequest; var Response: THTTPResponse): Boolean;
var
  AuthHeader, DecodedAuth, vUsername, vPassword: string;
  I, ColonPos: Integer;
  LRouteMatch: Boolean;
begin
  LRouteMatch := False;
  for I := 0 to FProtectedRoutes.Count - 1 do
    if IsProtectedRoute(Request.URI, FProtectedRoutes[I]) then
    begin
      LRouteMatch := True;
      Break;
    end;

  if not LRouteMatch then
  begin
    Result := False;
    Exit;
  end;

  AuthHeader := Trim(Request.Headers.Values['Authorization']);

  if Pos('Basic ', AuthHeader) = 1 then
  begin
    AuthHeader := Copy(AuthHeader, 7, Length(AuthHeader));
    try
      DecodedAuth := CustomDecodeBase64(AuthHeader);
      ColonPos := Pos(':', DecodedAuth);
      if ColonPos > 0 then
      begin
        vUsername := Copy(DecodedAuth, 1, ColonPos - 1);
        vPassword := Copy(DecodedAuth, ColonPos + 1, Length(DecodedAuth));

        if ConstantTimeEquals(vUsername, FUsername) and
           ConstantTimeEquals(HashPassword(vPassword), FPasswordHash) then
        begin
          Request.UserID := vUsername;
          Result := False;
        end
        else
        begin
          Response.StatusCode := HTTP_UNAUTHORIZED;
          Response.Body := '{"error":"Invalid username or password"}';
          Response.ContentType := APPLICATION_JSON;
          Result := True;
        end;
      end
      else
      begin
        Response.StatusCode := HTTP_UNAUTHORIZED;
        Response.Body := '{"error":"Invalid Basic Auth format"}';
        Response.ContentType := APPLICATION_JSON;
        Result := True;
      end;
    except
      on E: Exception do
      begin
        Response.StatusCode := HTTP_INTERNAL_SERVER_ERROR;
        Response.Body := '{"error":"Error decoding Basic Auth: ' + E.Message + '"}';
        Response.ContentType := APPLICATION_JSON;
        Result := True;
      end;
    end;
  end
  else
  begin
    Response.StatusCode := HTTP_UNAUTHORIZED;
    Response.Body := '{"error":"Basic Authorization header missing or invalid"}';
    Response.ContentType := APPLICATION_JSON;
    Result := True;
  end;
end;

procedure TBasicAuth.RegisterProtectedRoutes(Badger: TBadger; const ProtectedRoutes: array of string);
var
  I: Integer;
begin
  FProtectedRoutes.Clear;
  for I := Low(ProtectedRoutes) to High(ProtectedRoutes) do
    FProtectedRoutes.Add(ProtectedRoutes[I]);
  Badger.AddMiddleware(Check);
end;

end.
