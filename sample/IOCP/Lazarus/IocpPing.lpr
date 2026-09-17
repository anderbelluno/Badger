program IocpPing;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

uses
  SysUtils,
  Badger, IocpDemoRoutes;

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
    WriteLn('Badger IOCP on http://127.0.0.1:', Server.Port, '/teste/ping');
    WriteLn('WebSocket ws://127.0.0.1:', Server.Port, '/chat');
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
