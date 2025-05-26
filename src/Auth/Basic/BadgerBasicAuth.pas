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
    function Check(Request: THTTPRequest; out Response: THTTPResponse): Boolean;
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

function TBasicAuth.Check(Request: THTTPRequest; out Response: THTTPResponse): Boolean;
var
  AuthHeader, DecodedAuth, vUsername, vPassword: string;
  I, ColonPos: Integer;
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

  AuthHeader := '';
  for I := 0 to Request.Headers.Count - 1 do
  begin
    if Pos('Authorization=', Request.Headers[I]) > 0 then
    begin
      AuthHeader := Trim(Copy(Request.Headers[I], Pos('=', Request.Headers[I]) + 1, Length(Request.Headers[I])));
      Break;
    end;
  end;

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
          Result := True;
        end
        else
        begin
          Response.StatusCode := HTTP_UNAUTHORIZED;
          Response.Body := '{"error":"Invalid username or password"}';
          Response.ContentType := APPLICATION_JSON;
          Result := False;
        end;
      end
      else
      begin
        Response.StatusCode := HTTP_UNAUTHORIZED;
        Response.Body := '{"error":"Invalid Basic Auth format"}';
        Response.ContentType := APPLICATION_JSON;
        Result := False;
      end;
    except
      on E: Exception do
      begin
        Response.StatusCode := HTTP_INTERNAL_SERVER_ERROR;
        Response.Body := '{"error":"Error decoding Basic Auth: ' + E.Message + '"}';
        Response.ContentType := APPLICATION_JSON;
        Result := False;
      end;
    end;
  end
  else
  begin
    Response.StatusCode := HTTP_UNAUTHORIZED;
    Response.Body := '{"error":"Basic Authorization header missing or invalid"}';
    Response.ContentType := APPLICATION_JSON;
    Result := False;
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
