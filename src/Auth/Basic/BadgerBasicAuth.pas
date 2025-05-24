unit BadgerBasicAuth;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, SyUtils, Badger, BadgerTypes {$IFNDEF FPC} , EncdDecd {$ELSE}, base64 {$ENDIF};
type
  TBasicAuth = class
  private
    FUsername: string;
    FPassword: string;
  public
    constructor Create(const AUsername, APassword: string);
    function Check(Request: THTTPRequest; out Response: THTTPResponse): Boolean;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
  end;

implementation

{ TBasicAuth }

constructor TBasicAuth.Create(const AUsername, APassword: string);
begin
  inherited Create;
  FUsername := AUsername;
  FPassword := APassword;
end;

function TBasicAuth.Check(Request: THTTPRequest; out Response: THTTPResponse): Boolean;
var
  AuthHeader, DecodedAuth, vUsername, vPassword: string;
  I, ColonPos: Integer;
  header : String;
begin
  Result := False;
  AuthHeader := '';

  for I := 0 to Request.Headers.Count - 1 do
  begin
    header := Request.Headers[I] ;
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
      {$IFNDEF FPC}
         DecodedAuth := DecodeString(AuthHeader);
      {$ELSE}
         DecodedAuth := DecodeStringBase64(AuthHeader);
      {$ENDIF}
      ColonPos := Pos(':', DecodedAuth);
      if ColonPos > 0 then
      begin
        vUsername := Copy(DecodedAuth, 1, ColonPos - 1);
        vPassword := Copy(DecodedAuth, ColonPos + 1, Length(DecodedAuth));

        if (vUsername = FUsername) and (vPassword = FPassword) then
          Result := True
        else
        begin
          Response.StatusCode := 401;
          Response.Body := 'Invalid username or password';
        end;
      end
      else
      begin
        Response.StatusCode := 401;
        Response.Body := 'Invalid Basic Auth format';
      end;
    except
      Response.StatusCode := 500;
      Response.Body := 'Error decoding Basic Auth';
    end;
  end
  else
  begin
    Response.StatusCode := 401;
    Response.Body := 'Basic Authorization header missing or invalid';
  end;
end;

end.
