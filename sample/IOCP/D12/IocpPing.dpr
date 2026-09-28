program IocpPing;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
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
  BadgerHttpDispatch in '..\..\..\src\BadgerHttpDispatch.pas',
  BadgerIOCP in '..\..\..\src\IOCP\BadgerIOCP.pas',
  BadgerRequestHandler in '..\..\..\src\BadgerRequestHandler.pas',
  Badger in '..\..\..\src\Badger.pas',
  BadgerJWTUtils in '..\..\..\src\Auth\JWT\BadgerJWTUtils.pas',
  BadgerJWTClaims in '..\..\..\src\Auth\JWT\BadgerJWTClaims.pas',
  superobject in '..\..\..\ThirdParty\SuperObject\D_plus\superobject.pas',
  BadgerAuthJWT in '..\..\..\src\Auth\JWT\BadgerAuthJWT.pas',
  BadgerBasicAuth in '..\..\..\src\Auth\Basic\BadgerBasicAuth.pas',
  IocpDemoRoutes in '..\IocpDemoRoutes.pas';

var
  Server: TBadger;
begin
  { Leak dialog without stack = stock FastMM. For CallStack on D12:
    1) GetIt: install FastMM4 (or FastMM5)
    2) Put FastMM4 as the FIRST unit in this uses clause
    3) Enable FullDebugMode in FastMM4Options.inc
    4) Copy FastMM_FullDebugMode.dll next to the .exe
    Then the shutdown report includes allocation stacks. }
  ReportMemoryLeaksOnShutdown := True;
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
