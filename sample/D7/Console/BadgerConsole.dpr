program BadgerConsole;

{$APPTYPE CONSOLE}

{ Minimal Badger host — Delphi 7. Port 8080, GET /teste/ping only.
  Configure library path: src, src\IOCP, src\Auth\*, ThirdParty\Synapse,
  ThirdParty\SuperObject\D7 (or the SO folder you use for D7), sample\Common. }

uses
  SysUtils,
  Badger,
  BadgerLogger,
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
        WriteLn('Start failed: ' + E.Message);
        Exit;
      end;
    end;
    WriteLn('Badger Console on http://127.0.0.1:' + IntToStr(Server.Port) + '/teste/ping');
    WriteLn('Running. Enter to stop.');
    ReadLn;
    Server.Stop;
  finally
    Server.Free;
  end;
end.
