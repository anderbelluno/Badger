unit SampleWsChatClient;

{ Local WS client for GUI samples. Connects to 127.0.0.1:/chat. }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, blcksock, synsock, BadgerWebSocket;

type
  TWsChatNotify = procedure(const AMsg: string) of object;

  TSampleWsChatClient = class(TThread)
  private
    FPort: Integer;
    FPath: string;
    FSock: TTCPBlockSocket;
    FSendLock: TCriticalSection;
    FOnLine: TWsChatNotify;
    FConnected: Boolean;
    procedure Notify(const AMsg: string);
  protected
    procedure Execute; override;
  public
    constructor Create(APort: Integer; const APath: string; AOnLine: TWsChatNotify);
    destructor Destroy; override;
    procedure SendText(const AMsg: string);
  end;

implementation

constructor TSampleWsChatClient.Create(APort: Integer; const APath: string;
  AOnLine: TWsChatNotify);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  Randomize;
  FPort := APort;
  FPath := APath;
  if FPath = '' then
    FPath := '/chat';
  FOnLine := AOnLine;
  FSendLock := TCriticalSection.Create;
  FSock := nil;
  FConnected := False;
end;

destructor TSampleWsChatClient.Destroy;
begin
  Terminate;
  FSendLock.Acquire;
  try
    if Assigned(FSock) then
      FSock.CloseSocket;
  finally
    FSendLock.Release;
  end;
  WaitFor;
  FSendLock.Free;
  inherited Destroy;
end;

procedure TSampleWsChatClient.Notify(const AMsg: string);
begin
  if Assigned(FOnLine) then
    FOnLine(AMsg);
end;

procedure TSampleWsChatClient.SendText(const AMsg: string);
var
  Frame: TBadgerWsBytes;
begin
  if (AMsg = '') or not FConnected then
    Exit;
  Frame := BadgerWsMaskedTextFrame(AMsg);
  if Frame = '' then
    Exit;
  FSendLock.Acquire;
  try
    if Assigned(FSock) and (FSock.Socket <> INVALID_SOCKET) then
      FSock.SendBuffer(Pointer(Frame), Length(Frame));
  finally
    FSendLock.Release;
  end;
end;

procedure TSampleWsChatClient.Execute;
var
  Sock: TTCPBlockSocket;
  Parser: TBadgerWsParser;
  Key, Line, Req: string;
  Buf: AnsiString;
  N, Got: Integer;
begin
  Sock := TTCPBlockSocket.Create;
  Parser := TBadgerWsParser.Create;
  try
    FSendLock.Acquire;
    try
      FSock := Sock;
    finally
      FSendLock.Release;
    end;
    Sock.ConnectionTimeout := 3000;
    Sock.Connect('127.0.0.1', IntToStr(FPort));
    if Sock.LastError <> 0 then
    begin
      Notify('WS connect: ' + Sock.LastErrorDesc);
      Exit;
    end;
    Key := BadgerWsRandomKey;
    Req :=
      'GET ' + FPath + ' HTTP/1.1' + #13#10 +
      'Host: 127.0.0.1:' + IntToStr(FPort) + #13#10 +
      'Upgrade: websocket' + #13#10 +
      'Connection: Upgrade' + #13#10 +
      'Sec-WebSocket-Key: ' + Key + #13#10 +
      'Sec-WebSocket-Version: 13' + #13#10 + #13#10;
    Sock.SendString(AnsiString(Req));
    Line := string(Sock.RecvString(3000));
    if Pos('101', Line) = 0 then
    begin
      Notify('WS handshake: ' + Line);
      Exit;
    end;
    repeat
      Line := string(Sock.RecvString(3000));
    until (Line = '') or (Sock.LastError <> 0);
    if Sock.LastError <> 0 then
    begin
      Notify('WS handshake: ' + Sock.LastErrorDesc);
      Exit;
    end;
    FConnected := True;
    Notify('conectado em ' + FPath);
    while not Terminated do
    begin
      if not Sock.CanRead(200) then
      begin
        if Sock.LastError <> 0 then
          Break;
        Continue;
      end;
      N := Sock.WaitingData;
      if N <= 0 then
        N := 1;
      if N > 4096 then
        N := 4096;
      SetLength(Buf, N);
      Got := Sock.RecvBufferEx(Pointer(Buf), N, 1000);
      if (Got <= 0) or (Sock.LastError <> 0) then
        Break;
      Parser.Feed(@Buf[1], Got);
      while Parser.TryParse do
      begin
        if Parser.Failed or (Parser.Opcode = WS_OP_CLOSE) then
        begin
          Terminate;
          Break;
        end;
        if (Parser.Opcode = WS_OP_TEXT) and (Parser.Payload <> '') then
          Notify(Parser.Text);
      end;
    end;
  finally
    FConnected := False;
    FSendLock.Acquire;
    try
      FSock := nil;
    finally
      FSendLock.Release;
    end;
    Parser.Free;
    Sock.Free;
  end;
end;

end.
