program EpollPing;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  SysUtils,
  Badger,
  EpollDemoRoutes;

var
  Server: TBadger;
begin
  Server := TBadger.Create;
  try
    Server.Port := 8081;
    Server.Timeout := 5000;
    Server.ParallelProcessing := True;
    Server.MaxConcurrentConnections := 500;
    Server.EnableEventInfo := False;
    RegisterEpollDemoRoutes(Server);
    try
      Server.Start;
    except
      on E: Exception do
      begin
        WriteLn('Start failed: ', E.Message);
        Exit;
      end;
    end;
    WriteLn('Badger epoll on http://127.0.0.1:', Server.Port, '/teste/ping');
    WriteLn('GET /echo  POST /echo  GET /produtos?id=1&codigo=x');
    WriteLn('POST /json  GET /download  POST /upload');
    WriteLn('POST /login {"username":"usuario","password":"senha123"}');
    WriteLn('POST /json  Basic username:password');
    WriteLn('GET /download  Authorization: Bearer <token>');
    WriteLn('WebSocket ws://127.0.0.1:', Server.Port, '/chat');
    WriteLn('HTTP/1.1 keep-alive; Timeout=', Server.Timeout, ' ms');
    WriteLn('Running. Stop the debugger or Ctrl+C.');
    while Server.IsRunning do
      Sleep(250);
    Server.Stop;
  finally
    Server.Free;
  end;
end.
