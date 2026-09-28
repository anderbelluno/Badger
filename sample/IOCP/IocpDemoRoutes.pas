unit IocpDemoRoutes;

{ IOCP demo: ping/echo/json/upload, Basic+JWT, CORS, WebSocket /chat. }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Badger, BadgerTypes, BadgerBasicAuth,
  BadgerAuthJWT, IocpDemoHttpRoutes;

type
  TIocpDemoRoutes = class
  public
    class procedure AfterStamp(var Request: THTTPRequest; var Response: THTTPResponse);
  end;

  TIocpDemoWs = class
  public
    Server: TBadger;
    procedure HandleMessage(ClientInfo: TClientSocketInfo; const URI, AMessage: string);
  end;

procedure RegisterIocpDemoRoutes(Server: TBadger);

implementation

var
  DemoBasic: TBasicAuth;
  DemoJWT: TBadgerJWTAuth;
  DemoWs: TIocpDemoWs;

class procedure TIocpDemoRoutes.AfterStamp(var Request: THTTPRequest; var Response: THTTPResponse);
begin
  if Assigned(Response.HeadersCustom) then
    Response.HeadersCustom.Values['X-Engine'] := 'IOCP';
end;

procedure TIocpDemoWs.HandleMessage(ClientInfo: TClientSocketInfo; const URI, AMessage: string);
begin
  if Assigned(Server) and Assigned(ClientInfo) then
    Server.SendToWebSocketRoute(URI, AMessage);
end;

procedure RegisterIocpDemoRoutes(Server: TBadger);
begin
  if DemoBasic = nil then
    DemoBasic := TBasicAuth.Create('username', 'password');
  if DemoJWT = nil then
    DemoJWT := TBadgerJWTAuth.Create('iocp-demo-secret', IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'jwt');
  IocpDemoSetAuthJWT(DemoJWT);
  if DemoWs = nil then
    DemoWs := TIocpDemoWs.Create;
  DemoWs.Server := Server;
  Server.OnWebSocketMessage := DemoWs.HandleMessage;

  Server.RouteManager
    .AddGet('/teste/ping', TIocpDemoHttpRoutes.Ping)
    .AddGet('/ping', TIocpDemoHttpRoutes.Ping)
    .AddPost('/echo', TIocpDemoHttpRoutes.Echo)
    .AddPost('/json', TIocpDemoHttpRoutes.Json)
    .AddGet('/download', TIocpDemoHttpRoutes.Download)
    .AddPost('/upload', TIocpDemoHttpRoutes.Upload)
    .AddPost('/login', TIocpDemoHttpRoutes.Login)
    .AddGet('/produtos/:id/:codigo', TIocpDemoHttpRoutes.Produtos)
    .AddGet('/produtos', TIocpDemoHttpRoutes.Produtos)
    .AddWebSocket('/chat');
  Server.AddAfterMiddleware(TIocpDemoRoutes.AfterStamp);
  DemoBasic.RegisterProtectedRoutes(Server, ['/json']);
  DemoJWT.RegisterProtectedRoutes(Server, ['/download']);
  Server.CorsEnabled := True;
  Server.CorsAllowedOrigins.Add('*');
end;

initialization
  DemoBasic := nil;
  DemoJWT := nil;
  DemoWs := nil;

finalization
  IocpDemoSetAuthJWT(nil);
  FreeAndNil(DemoWs);
  FreeAndNil(DemoJWT);
  FreeAndNil(DemoBasic);

end.
