unit SampleWsDemo;

{ Registers WebSocket echo on TBadger (broadcast to same URI). }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Badger, BadgerTypes;

procedure RegisterSampleWsEcho(Server: TBadger; const AURI: string = '/chat');
procedure UnregisterSampleWsEcho;

implementation

type
  TSampleWsEcho = class
  public
    Server: TBadger;
    procedure HandleMessage(ClientInfo: TClientSocketInfo; const URI, AMessage: string);
  end;

var
  Echo: TSampleWsEcho;

procedure TSampleWsEcho.HandleMessage(ClientInfo: TClientSocketInfo;
  const URI, AMessage: string);
begin
  if Assigned(Server) and Assigned(ClientInfo) then
    Server.SendToWebSocketRoute(URI, AMessage);
end;

procedure RegisterSampleWsEcho(Server: TBadger; const AURI: string);
begin
  if Echo = nil then
    Echo := TSampleWsEcho.Create;
  Echo.Server := Server;
  Server.OnWebSocketMessage := Echo.HandleMessage;
  Server.RouteManager.AddWebSocket(AURI);
end;

procedure UnregisterSampleWsEcho;
begin
  if Assigned(Echo) then
    Echo.Server := nil;
end;

initialization
  Echo := nil;

finalization
  FreeAndNil(Echo);

end.
