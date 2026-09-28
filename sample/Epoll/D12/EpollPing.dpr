program EpollPing;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  BadgerEpollSys in '..\..\..\src\Epoll\BadgerEpollSys.pas',
  BadgerEpoll in '..\..\..\src\Epoll\BadgerEpoll.pas',
  BadgerHttpDispatch in '..\..\..\src\BadgerHttpDispatch.pas',
  BadgerHttpStatus in '..\..\..\src\BadgerHttpStatus.pas',
  BadgerHttpParser in '..\..\..\src\BadgerHttpParser.pas',
  blcksock in '..\..\..\ThirdParty\Synapse\blcksock.pas',
  BadgerTypes in '..\..\..\src\BadgerTypes.pas',
  BadgerRouteManager in '..\..\..\src\BadgerRouteManager.pas',
  BadgerLogger in '..\..\..\src\BadgerLogger.pas',
  BadgerHttpUtils in '..\..\..\src\BadgerHttpUtils.pas',
  BadgerUtils in '..\..\..\src\BadgerUtils.pas',
  BadgerUploadUtils in '..\..\..\src\BadgerUploadUtils.pas',
  BadgerMultipartDataReader in '..\..\..\src\BadgerMultipartDataReader.pas',
  BadgerMethods in '..\..\..\src\BadgerMethods.pas',
  BadgerWebSocket in '..\..\..\src\BadgerWebSocket.pas',
  BadgerRequestHandler in '..\..\..\src\BadgerRequestHandler.pas',
  Badger in '..\..\..\src\Badger.pas',
  BadgerJWTUtils in '..\..\..\src\Auth\JWT\BadgerJWTUtils.pas',
  BadgerJWTClaims in '..\..\..\src\Auth\JWT\BadgerJWTClaims.pas',
  superobject in '..\..\..\ThirdParty\SuperObject\D_Plus_Laz_Linux\superobject.pas',
  BadgerAuthJWT in '..\..\..\src\Auth\JWT\BadgerAuthJWT.pas',
  BadgerBasicAuth in '..\..\..\src\Auth\Basic\BadgerBasicAuth.pas',
  IocpDemoHttpRoutes in '..\..\IOCP\IocpDemoHttpRoutes.pas',
  EpollDemoRoutes in '..\EpollDemoRoutes.pas';

var
  Server: TBadger;
  I: Integer;
  ForceSynapse: Boolean;
  Engine: string;
begin
  ForceSynapse := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), '--synapse') then
      ForceSynapse := True;

  Server := TBadger.Create;
  try
    Server.Port := 8081;
    Server.Timeout := 5000;
    Server.ParallelProcessing := True;
    Server.MaxConcurrentConnections := 500;
    Server.EnableEventInfo := False;
    if ForceSynapse then
      Server.UseEpoll := False;
    RegisterEpollDemoRoutes(Server);
    try
      Server.Start;
    except
      on E: Exception do
      begin
        WriteLn('Start failed: ' + E.Message);
        Exit;
      end;
    end;
    if Server.UseEpoll then
      Engine := 'epoll'
    else
      Engine := 'Synapse';
    WriteLn('motor=' + Engine + ' UseEpoll=' + BoolToStr(Server.UseEpoll, True));
    WriteLn('Badger on http://127.0.0.1:' + IntToStr(Server.Port) + '/ping');
    WriteLn('wrk -t4 -c400 -d15s --latency http://127.0.0.1:' + IntToStr(Server.Port) + '/ping');
    WriteLn('Default is epoll. Pass --synapse to force UseEpoll := False.');
    WriteLn('GET /teste/ping  GET /echo  POST /echo  GET /produtos?id=1&codigo=x');
    WriteLn('POST /json  GET /download  POST /upload');
    WriteLn('POST /login {"username":"usuario","password":"senha123"}');
    WriteLn('POST /json  Basic username:password');
    WriteLn('GET /download  Authorization: Bearer <token>');
    WriteLn('WebSocket ws://127.0.0.1:' + IntToStr(Server.Port) + '/chat');
    WriteLn('HTTP/1.1 keep-alive; Timeout=' + IntToStr(Server.Timeout) + ' ms');
    WriteLn('Running. Ctrl+C to stop.');
    while Server.IsRunning do
      Sleep(250);
    Server.Stop;
  finally
    Server.Free;
  end;
end.
