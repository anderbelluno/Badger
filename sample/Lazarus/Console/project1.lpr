program Project1;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
  cmem,
  cthreads,
  {$ENDIF}
  SysUtils,
  Badger,
  BadgerLogger,
  SampleRouteManager;

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

    Server.RouteManager
      .AddGet('/teste/ping', TSampleRouteManager.ping);

    try
      Server.Start;
    except
      on E: Exception do
      begin
        WriteLn('Start failed: ', E.Message);
        Exit;
      end;
    end;

    WriteLn('Badger on http://127.0.0.1:', Server.Port, '/teste/ping');
    if Server.UseEpoll then
      WriteLn('motor=epoll UseEpoll=True')
    else
      WriteLn('motor=Synapse UseEpoll=False');
    WriteLn('Running. Ctrl+C or stop the debugger to exit.');
    while Server.IsRunning do
      Sleep(250);
    Server.Stop;
  finally
    Server.Free;
  end;
end.
