unit MainForm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SyncObjs, blcksock, Badger, EpollDemoRoutes, IocpWsChatClient;

type
  TFormMain = class(TForm)
    PanelTop: TPanel;
    btnStartStop: TButton;
    lblPort: TLabel;
    edtPort: TEdit;
    btnPing: TButton;
    btnOther: TButton;
    chkEpoll: TCheckBox;
    MemoLog: TMemo;
    tmrLog: TTimer;
    PanelWs: TPanel;
    lblWs: TLabel;
    MemoWs: TMemo;
    PanelWsSend: TPanel;
    edtWs: TEdit;
    btnWsSend: TButton;
    procedure btnStartStopClick(Sender: TObject);
    procedure btnPingClick(Sender: TObject);
    procedure btnOtherClick(Sender: TObject);
    procedure btnWsSendClick(Sender: TObject);
    procedure edtWsKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrLogTimer(Sender: TObject);
  private
    FServer: TBadger;
    FChat: TIocpWsChatClient;
    FLogLock: TCriticalSection;
    FWsQueue: TStringList;
    procedure Log(const AMsg: string);
    procedure ChatLine(const AMsg: string);
    procedure StopServer;
    procedure SetWsEnabled(AEnabled: Boolean);
    function BaseUrl: string;
    procedure HttpGet(const APath: string);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

procedure TFormMain.Log(const AMsg: string);
begin
  MemoLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + '  ' + AMsg);
end;

function TFormMain.BaseUrl: string;
begin
  Result := 'http://127.0.0.1:' + Trim(edtPort.Text);
end;

procedure TFormMain.ChatLine(const AMsg: string);
begin
  FLogLock.Acquire;
  try
    FWsQueue.Add(AMsg);
  finally
    FLogLock.Release;
  end;
end;

procedure TFormMain.SetWsEnabled(AEnabled: Boolean);
begin
  edtWs.Enabled := AEnabled;
  btnWsSend.Enabled := AEnabled;
end;

procedure TFormMain.tmrLogTimer(Sender: TObject);
var
  I: Integer;
  WsLines: TStringList;
begin
  if FWsQueue.Count = 0 then
    Exit;
  WsLines := TStringList.Create;
  try
    FLogLock.Acquire;
    try
      WsLines.Assign(FWsQueue);
      FWsQueue.Clear;
    finally
      FLogLock.Release;
    end;
    for I := 0 to WsLines.Count - 1 do
      MemoWs.Lines.Add(FormatDateTime('hh:nn:ss', Now) + '  ' + WsLines[I]);
  finally
    WsLines.Free;
  end;
end;

procedure TFormMain.StopServer;
begin
  FreeAndNil(FChat);
  if Assigned(FServer) then
  begin
    FServer.Stop;
    tmrLogTimer(nil);
    FreeAndNil(FServer);
    Log('Servidor parado');
  end;
  btnStartStop.Tag := 0;
  btnStartStop.Caption := 'Iniciar';
  edtPort.Enabled := True;
  btnPing.Enabled := False;
  btnOther.Enabled := False;
  SetWsEnabled(False);
end;

procedure TFormMain.btnStartStopClick(Sender: TObject);
begin
  if btnStartStop.Tag = 0 then
  begin
    FServer := TBadger.Create;
    try
      FServer.Port := StrToInt(Trim(edtPort.Text));
      FServer.ParallelProcessing := True;
      FServer.MaxConcurrentConnections := 500;
      FServer.EnableEventInfo := False;
      FServer.UseEpoll := (chkEpoll.State = cbChecked);
      RegisterEpollDemoRoutes(FServer);
      FServer.Start;
      FChat := TIocpWsChatClient.Create(FServer.Port, '/chat', ChatLine);
      FChat.Start;
    except
      FreeAndNil(FChat);
      FreeAndNil(FServer);
      raise;
    end;
    btnStartStop.Tag := 1;
    btnStartStop.Caption := 'Parar';
    edtPort.Enabled := False;
    btnPing.Enabled := True;
    btnOther.Enabled := True;
    SetWsEnabled(True);
    if FServer.UseEpoll then
      Log(Format('motor=epoll UseEpoll=True  %s/teste/ping', [BaseUrl]))
    else
      Log(Format('motor=Synapse UseEpoll=False  %s/teste/ping', [BaseUrl]));
    Log(Format('WS  ws://127.0.0.1:%s/chat', [Trim(edtPort.Text)]));
  end
  else
    StopServer;
end;

procedure TFormMain.HttpGet(const APath: string);
var
  Sock: TTCPBlockSocket;
  Status: Integer;
  Line, Body, Engine, Url: string;
begin
  Url := BaseUrl + APath;
  Sock := TTCPBlockSocket.Create;
  try
    Sock.ConnectionTimeout := 3000;
    Sock.Connect('127.0.0.1', Trim(edtPort.Text));
    if Sock.LastError <> 0 then
    begin
      Log(Url + '  ->  falhou');
      Exit;
    end;
    Sock.SendString(AnsiString(
      'GET ' + APath + ' HTTP/1.1' + #13#10 +
      'Host: 127.0.0.1' + #13#10 +
      'Connection: close' + #13#10 + #13#10));
    Line := string(Sock.RecvString(3000));
    Status := 0;
    if Length(Line) >= 12 then
      Status := StrToIntDef(Copy(Line, 10, 3), 0);
    Engine := '';
    repeat
      Line := string(Sock.RecvString(3000));
      if Pos('X-Engine:', Line) = 1 then
        Engine := Trim(Copy(Line, 10, MaxInt));
    until (Line = '') or (Sock.LastError <> 0);
    Body := string(Sock.RecvPacket(3000));
    if Engine <> '' then
      Log(Format('%s  ->  %d  X-Engine=%s  %s', [Url, Status, Engine, Body]))
    else
      Log(Format('%s  ->  %d  %s', [Url, Status, Body]));
  finally
    Sock.Free;
  end;
end;

procedure TFormMain.btnPingClick(Sender: TObject);
begin
  HttpGet('/teste/ping');
end;

procedure TFormMain.btnOtherClick(Sender: TObject);
begin
  HttpGet('/other');
end;

procedure TFormMain.btnWsSendClick(Sender: TObject);
var
  Msg: string;
begin
  Msg := Trim(edtWs.Text);
  if (Msg = '') or not Assigned(FChat) then
    Exit;
  FChat.SendText(Msg);
  edtWs.Clear;
end;

procedure TFormMain.edtWsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    btnWsSendClick(nil);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FServer := nil;
  FChat := nil;
  FLogLock := TCriticalSection.Create;
  FWsQueue := TStringList.Create;
  tmrLog.Enabled := True;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  StopServer;
  FWsQueue.Free;
  FLogLock.Free;
end;

end.
