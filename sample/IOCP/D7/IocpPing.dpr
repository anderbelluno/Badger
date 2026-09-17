program IocpPing;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  BadgerWinSock2 in '..\..\..\src\IOCP\BadgerWinSock2.pas',
  BadgerHttpStatus in '..\..\..\src\BadgerHttpStatus.pas',
  BadgerHttpParser in '..\..\..\src\BadgerHttpParser.pas',
  BadgerWebSocket in '..\..\..\src\BadgerWebSocket.pas',
  blcksock in '..\..\..\ThirdParty\Synapse\blcksock.pas',
  BadgerTypes in '..\..\..\src\BadgerTypes.pas',
  BadgerRouteManager in '..\..\..\src\BadgerRouteManager.pas',
  BadgerLogger in '..\..\..\src\BadgerLogger.pas',
  BadgerHttpUtils in '..\..\..\src\BadgerHttpUtils.pas',
  BadgerUtils in '..\..\..\src\BadgerUtils.pas',
  BadgerUploadUtils in '..\..\..\src\BadgerUploadUtils.pas',
  BadgerMultipartDataReader in '..\..\..\src\BadgerMultipartDataReader.pas',
  BadgerMethods in '..\..\..\src\BadgerMethods.pas',
  BadgerIOCP in '..\..\..\src\IOCP\BadgerIOCP.pas',
  BadgerRequestHandler in '..\..\..\src\BadgerRequestHandler.pas',
  Badger in '..\..\..\src\Badger.pas',
  BadgerJWTUtils in '..\..\..\src\Auth\JWT\BadgerJWTUtils.pas',
  BadgerJWTClaims in '..\..\..\src\Auth\JWT\BadgerJWTClaims.pas',
  superobject in '..\..\..\ThirdParty\SuperObject\D7\superobject.pas',
  BadgerAuthJWT in '..\..\..\src\Auth\JWT\BadgerAuthJWT.pas',
  BadgerBasicAuth in '..\..\..\src\Auth\Basic\BadgerBasicAuth.pas',
  IocpDemoRoutes in '..\IocpDemoRoutes.pas';

var
  Server: TBadger;
begin
  Server := TBadger.Create;
  try
    Server.Port := 8081;
    Server.ParallelProcessing := True;
    Server.MaxConcurrentConnections := 500;
    Server.EnableEventInfo := False;
    RegisterIocpDemoRoutes(Server);
    Server.Start;
    WriteLn('Badger IOCP on http://127.0.0.1:' + IntToStr(Server.Port) + '/teste/ping');
    WriteLn('WebSocket ws://127.0.0.1:' + IntToStr(Server.Port) + '/chat');
    WriteLn('POST /login {"username":"usuario","password":"senha123"}');
    WriteLn('POST /json  Basic username:password');
    WriteLn('GET /download  Authorization: Bearer <token>');
    WriteLn('Press Enter to stop');
    ReadLn;
    Server.Stop;
  finally
    Server.Free;
  end;
end.
