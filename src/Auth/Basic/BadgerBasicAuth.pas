unit BadgerBasicAuth;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, Badger, BadgerTypes, BadgerHttpStatus, BadgerUtils;

type
  TBasicAuth = class
  private
    FUsername: string;
    FPassword: string;
    FProtectedRoutes: TStringList;
    function Check(var Request: THTTPRequest; var Response: THTTPResponse): Boolean;
  public
    constructor Create(const AUsername, APassword: string);
    destructor Destroy; override;
    procedure RegisterProtectedRoutes(Badger: TBadger; const ProtectedRoutes: array of string);
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
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
  FUsername := AUsername;
  FPassword := APassword;
  FProtectedRoutes := TStringList.Create;
end;

destructor TBasicAuth.Destroy;
begin
  FProtectedRoutes.Free;
  inherited;
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

        if (vUsername = FUsername) and (vPassword = FPassword) then
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
