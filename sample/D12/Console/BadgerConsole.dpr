program BadgerConsole;

{$APPTYPE CONSOLE}

{ Minimal Badger host — D12. Port 8080, GET /teste/ping only. }

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
  SampleRouteManager in '..\..\Common\SampleRouteManager.pas';

var
  Server: TBadger;
begin
  Logger.isActive := False;
  Logger.LogToConsole := False;
  Server := TBadger.Create;
  try
    Server.Port := 8080;
    Server.Timeout := 3000;
    Server.ParallelProcessing := True;
    Server.MaxConcurrentConnections := 500;
    Server.EnableEventInfo := False;
    Server.RouteManager.AddGet('/teste/ping', TSampleRouteManager.ping);
    try
      Server.Start;
    except
      on E: Exception do
      begin
        WriteLn('Start failed: ', E.Message);
        Exit;
      end;
    end;
    WriteLn('Badger Console on http://127.0.0.1:', Server.Port, '/teste/ping');
    WriteLn('Running. Enter to stop.');
    ReadLn;
    Server.Stop;
  finally
    Server.Free;
  end;
end.
