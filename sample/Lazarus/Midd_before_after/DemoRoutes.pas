unit DemoRoutes;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, BadgerTypes, BadgerHttpStatus;

type
  TDemoRoutes = class
  public
    class procedure Ping(Request: THTTPRequest; var Response: THTTPResponse);
  end;

implementation

class procedure TDemoRoutes.Ping(Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.ContentType := APPLICATION_JSON;
  Response.Body := '{"ok":true,"message":"pong"}';
end;

end.
