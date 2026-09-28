unit BadgerBasicAuth;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, Badger, BadgerTypes, BadgerHttpStatus, BadgerUtils, BadgerJWTUtils,
  BadgerRouteManager;

type
  TBasicAuth = class
  private
    FUsername: string;
    FPasswordHash: string;
    FPasswordSalt: string;
    FRealm: string;
    FProtectedRoutes: TStringList;
    function ConstantTimeEquals(const A, B: string): Boolean;
    function HashPassword(const APassword: string): string;
    function BuildPasswordSalt: string;
    function GetPassword: string;
    procedure SetPassword(const AValue: string);
    procedure Challenge(var Response: THTTPResponse; const ABody: string);
  public
    constructor Create(const AUsername, APassword: string);
    destructor Destroy; override;
    function Check(var Request: THTTPRequest; var Response: THTTPResponse): Boolean;
    procedure SetProtectedRoutes(const ProtectedRoutes: array of string);
    procedure RegisterProtectedRoutes(Badger: TBadger; const ProtectedRoutes: array of string);
    property Username: string read FUsername write FUsername;
    property Password: string read GetPassword write SetPassword;
    property Realm: string read FRealm write FRealm;
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

  { Mesmo prefixo por segmento de antes, agora com ':param' casando o segmento. }
  Result := BadgerPathUnderPattern(RequestURI, ProtectedRoute);
end;

constructor TBasicAuth.Create(const AUsername, APassword: string);
begin
  inherited Create;
  Randomize;
  FUsername := AUsername;
  FPasswordSalt := BuildPasswordSalt;
  SetPassword(APassword);
  FRealm := 'Badger';
  FProtectedRoutes := TStringList.Create;
end;

destructor TBasicAuth.Destroy;
begin
  FProtectedRoutes.Free;
  inherited;
end;

function TBasicAuth.BuildPasswordSalt: string;
var
  G: TGUID;
  I: Integer;
begin
  { GUID v4 vem do CSPRNG do SO (CoCreateGuid / getrandom), ao contrario de Random,
    que e LCG semeado pela hora e portanto adivinhavel. }
  Result := '';
  for I := 1 to 2 do
    if CreateGUID(G) = 0 then
      Result := Result + StringReplace(StringReplace(StringReplace(
        GUIDToString(G), '{', '', [rfReplaceAll]), '}', '', [rfReplaceAll]),
        '-', '', [rfReplaceAll]);
  if Result = '' then
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

procedure TBasicAuth.Challenge(var Response: THTTPResponse; const ABody: string);
var
  RealmValue: string;
begin
  RealmValue := Trim(FRealm);
  if RealmValue = '' then
    RealmValue := 'Badger';
  RealmValue := StringReplace(RealmValue, '"', '', [rfReplaceAll]);
  Response.StatusCode := HTTP_UNAUTHORIZED;
  Response.Body := ABody;
  Response.ContentType := APPLICATION_JSON;
  if Assigned(Response.HeadersCustom) then
    Response.HeadersCustom.Values['WWW-Authenticate'] :=
      'Basic realm="' + RealmValue + '"';
end;

function TBasicAuth.Check(var Request: THTTPRequest; var Response: THTTPResponse): Boolean;
var
  AuthHeader, DecodedAuth, vUsername, vPassword: string;
  I, ColonPos: Integer;
  LRouteMatch: Boolean;
  UserOk, PassOk: Boolean;
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

  if SameText(Copy(AuthHeader, 1, 6), 'Basic ') then
  begin
    AuthHeader := Copy(AuthHeader, 7, Length(AuthHeader));
    try
      DecodedAuth := CustomDecodeBase64(AuthHeader);
      ColonPos := Pos(':', DecodedAuth);
      if ColonPos > 0 then
      begin
        vUsername := Copy(DecodedAuth, 1, ColonPos - 1);
        vPassword := Copy(DecodedAuth, ColonPos + 1, Length(DecodedAuth));

        { Sem curto-circuito: com 'and' o hash so rodava para usuario certo, e a
          diferenca de tempo revelava quais usuarios existem. }
        UserOk := ConstantTimeEquals(vUsername, FUsername);
        PassOk := ConstantTimeEquals(HashPassword(vPassword), FPasswordHash);
        if UserOk and PassOk then
        begin
          Request.UserID := vUsername;
          Result := False;
        end
        else
        begin
          Challenge(Response, '{"error":"Invalid username or password"}');
          Result := True;
        end;
      end
      else
      begin
        Challenge(Response, '{"error":"Invalid Basic Auth format"}');
        Result := True;
      end;
    except
      { Credencial malformada e erro do cliente (401), nao 500; e E.Message ia cru
        para o corpo: vazava detalhe interno e, com aspas, gerava JSON invalido. }
      on E: Exception do
      begin
        Challenge(Response, '{"error":"Invalid Basic Auth format"}');
        Result := True;
      end;
    end;
  end
  else
  begin
    Challenge(Response, '{"error":"Basic Authorization header missing or invalid"}');
    Result := True;
  end;
end;

procedure TBasicAuth.SetProtectedRoutes(const ProtectedRoutes: array of string);
var
  I: Integer;
begin
  FProtectedRoutes.Clear;
  for I := Low(ProtectedRoutes) to High(ProtectedRoutes) do
    FProtectedRoutes.Add(ProtectedRoutes[I]);
end;

procedure TBasicAuth.RegisterProtectedRoutes(Badger: TBadger; const ProtectedRoutes: array of string);
begin
  SetProtectedRoutes(ProtectedRoutes);
  Badger.AddMiddleware(Check);
end;

end.
