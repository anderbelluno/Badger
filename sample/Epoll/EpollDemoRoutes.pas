unit EpollDemoRoutes;

{ Epoll proto on TBadger: same HTTP routes as IOCP plus WS /chat echo.
  Auth via RegisterProtectedRoutes (not TBadgerEpoll). }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Badger, BadgerTypes, BadgerBasicAuth, BadgerAuthJWT, IocpDemoHttpRoutes;

procedure RegisterEpollDemoRoutes(Server: TBadger);

implementation

type
  TEpollDemoWs = class
  public
    Server: TBadger;
    procedure HandleMessage(ClientInfo: TClientSocketInfo; const URI, AMessage: string);
    procedure AfterStamp(var Request: THTTPRequest; var Response: THTTPResponse);
  end;

var
  DemoBasic: TBasicAuth;
  DemoJWT: TBadgerJWTAuth;
  DemoWs: TEpollDemoWs;

procedure TEpollDemoWs.AfterStamp(var Request: THTTPRequest; var Response: THTTPResponse);
begin
  if not Assigned(Response.HeadersCustom) then
    Exit;
  if Assigned(Server) and Server.UseEpoll then
    Response.HeadersCustom.Values['X-Engine'] := 'epoll'
  else
    Response.HeadersCustom.Values['X-Engine'] := 'Synapse';
end;

procedure TEpollDemoWs.HandleMessage(ClientInfo: TClientSocketInfo; const URI, AMessage: string);
begin
  if Assigned(Server) and Assigned(ClientInfo) then
    Server.SendToWebSocketRoute(URI, AMessage);
end;

procedure RegisterEpollDemoRoutes(Server: TBadger);
begin
  if DemoBasic = nil then
    DemoBasic := TBasicAuth.Create('username', 'password');
  if DemoJWT = nil then
    DemoJWT := TBadgerJWTAuth.Create('iocp-demo-secret',
      IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'jwt');
  IocpDemoSetAuthJWT(DemoJWT);
  if DemoWs = nil then
    DemoWs := TEpollDemoWs.Create;
  DemoWs.Server := Server;
  Server.OnWebSocketMessage := DemoWs.HandleMessage;

  Server.RouteManager
    .AddGet('/teste/ping', TIocpDemoHttpRoutes.Ping)
    .AddGet('/ping', TIocpDemoHttpRoutes.Ping)
    .AddGet('/echo', TIocpDemoHttpRoutes.Echo)
    .AddPost('/echo', TIocpDemoHttpRoutes.Echo)
    .AddGet('/produtos/:id/:codigo', TIocpDemoHttpRoutes.Produtos)
    .AddGet('/produtos', TIocpDemoHttpRoutes.Produtos)
    .AddPost('/json', TIocpDemoHttpRoutes.Json)
    .AddGet('/download', TIocpDemoHttpRoutes.Download)
    .AddPost('/upload', TIocpDemoHttpRoutes.Upload)
    .AddPost('/login', TIocpDemoHttpRoutes.Login)
    .AddWebSocket('/chat');
  Server.AddAfterMiddleware(DemoWs.AfterStamp);
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
