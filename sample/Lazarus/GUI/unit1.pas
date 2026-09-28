unit Unit1;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF MSWINDOWS}Windows, {$ENDIF} Messages, Classes, SysUtils, SyncObjs,
    Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
    Badger, BadgerBasicAuth, BadgerAuthJWT, BadgerTypes, BadgerLogger,
    BadgerDBBridge, SampleRouteManager, SampleWsDemo, SampleWsChatClient,
    ConnPoolRoutes, SampleDbTemplate, ZConnection;

type

    { TForm1 }

    TForm1 = class(TForm)
        btnClearLog: TButton;
        btnSyna: TButton;
        btnTestDb: TButton;
        btnWsSend: TButton;
        chkDb: TCheckBox;
        edtDbHost: TEdit;
        edtDbName: TEdit;
        edtDbPass: TEdit;
        edtDbPort: TEdit;
        edtDbUser: TEdit;
        edtPoolN: TEdit;
        edtPorta: TEdit;
        edtTimeOut: TEdit;
        edtWs: TEdit;
        Label1: TLabel;
        Label2: TLabel;
        lblDb: TLabel;
        lblWs: TLabel;
        Memo1: TMemo;
        MemoWs: TMemo;
        Panel1: TPanel;
        Panel2: TPanel;
        PanelDb: TPanel;
        PanelWs: TPanel;
        PanelWsSend: TPanel;
        RadioGroup1: TRadioGroup;
        rdLog: TCheckBox;
        rdParallel: TCheckBox;
        rdIOCP: TCheckBox;
        tmrWs: TTimer;
        procedure btnClearLogClick(Sender: TObject);
        procedure btnSynaClick(Sender: TObject);
        procedure btnTestDbClick(Sender: TObject);
        procedure btnWsSendClick(Sender: TObject);
        procedure edtWsKeyPress(Sender: TObject; var Key: Char);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure tmrWsTimer(Sender: TObject);
    private
        ServerThread: TBadger;
        BasicAuth: TBasicAuth;
        JWTAuth: TBadgerJWTAuth;
        FDbTemplate: TZConnection;
        FDbBridge: TBadgerDBBridge;
        FChat: TSampleWsChatClient;
        FLogLock: TCriticalSection;
        FLogQueue: TStringList;
        FWsQueue: TStringList;
        procedure SyncFlushLog;
        procedure ChatLine(const AMsg: string);
        procedure ApplyDbFromUI;
        procedure SetWsEnabled(AEnabled: Boolean);
        procedure StopServer;
    public
        procedure HandleRequest(const RequestInfo: TRequestInfo);
        procedure HandleResponse(const ResponseInfo: TResponseInfo);
    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.SyncFlushLog;
var
  I: Integer;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    FLogLock.Acquire;
    try
      Lines.Assign(FLogQueue);
      FLogQueue.Clear;
    finally
      FLogLock.Release;
    end;
    for I := 0 to Lines.Count - 1 do
      Memo1.Lines.Add(Lines[I]);
    if Lines.Count > 0 then
      Memo1.SelStart := Length(Memo1.Text);
  finally
    Lines.Free;
  end;
end;

procedure TForm1.ChatLine(const AMsg: string);
begin
  FLogLock.Acquire;
  try
    FWsQueue.Add(AMsg);
  finally
    FLogLock.Release;
  end;
end;

procedure TForm1.SetWsEnabled(AEnabled: Boolean);
begin
  edtWs.Enabled := AEnabled;
  btnWsSend.Enabled := AEnabled;
end;

procedure TForm1.tmrWsTimer(Sender: TObject);
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

procedure TForm1.ApplyDbFromUI;
begin
  ApplySamplePgSettings(FDbTemplate,
    Trim(edtDbHost.Text),
    Trim(edtDbName.Text),
    Trim(edtDbUser.Text),
    edtDbPass.Text,
    'postgresql',
    StrToIntDef(edtDbPort.Text, 5432));
end;

procedure TForm1.StopServer;
begin
  FreeAndNil(FChat);
  UnregisterSampleWsEcho;
  if Assigned(ServerThread) then
  begin
    ServerThread.Stop;
    FreeAndNil(ServerThread);
  end;
  FreeAndNil(FDbBridge);
  tmrWsTimer(nil);
  SetWsEnabled(False);
end;

procedure TForm1.btnSynaClick(Sender: TObject);
begin
  Logger.isActive := False;
  Logger.LogToConsole := False;

  if btnSyna.Tag = 0 then
  begin
    try
      ServerThread := TBadger.Create;
      ServerThread.Port := StrToInt(edtPorta.Text);
      ServerThread.Timeout := StrToInt(edtTimeOut.Text);
      ServerThread.ParallelProcessing := rdParallel.Checked;
      ServerThread.MaxConcurrentConnections := 5000;
      ServerThread.EnableEventInfo := rdLog.Checked;
      {$IFDEF LINUX}
      ServerThread.UseEpoll := rdIOCP.Checked;
      {$ELSE}
      ServerThread.UseIOCP := rdIOCP.Checked;
      {$ENDIF}
      if ServerThread.EnableEventInfo then
      begin
        ServerThread.OnRequest := HandleRequest;
        ServerThread.OnResponse := HandleResponse;
      end;

      case RadioGroup1.ItemIndex of
        1: BasicAuth.RegisterProtectedRoutes(ServerThread, ['/rota1', '/teste/ping', '/download']);
        2: begin
              JWTAuth.RegisterProtectedRoutes(ServerThread, ['/rota1', '/teste/ping']);
              SampleRouteManager.FJWT := JWTAuth;
           end;
      end;

      ServerThread.RouteManager
        .AddPost('/upload', TSampleRouteManager.upLoad)
        .AddGet('/download', TSampleRouteManager.downLoad)
        .AddGet('/rota1', TSampleRouteManager.rota1)
        .AddGet('/teste/ping', TSampleRouteManager.ping)
        .AddPost('/AtuImage', TSampleRouteManager.AtuImage)
        .AddPost('/Login', TSampleRouteManager.Login)
        .AddGet('/RefreshToken', TSampleRouteManager.RefreshToken)
        .AddGet('/produtos/:id/:codigo', TSampleRouteManager.produtos)
        .AddGet('/produtos', TSampleRouteManager.produtos);

      if chkDb.Checked then
      begin
        ApplyDbFromUI;
        if FDbTemplate.Connected then
          FDbTemplate.Connected := False;
        FDbBridge := TBadgerDBBridge.Create(FDbTemplate, StrToIntDef(edtPoolN.Text, 8));
        FDbBridge.Register(ServerThread);
        ServerThread.RouteManager
          .AddGet('/db/ping', TConnPoolRoutes.DbPing)
          .AddGet('/db/work', TConnPoolRoutes.DbWork)
          .AddGet('/db/stats', TConnPoolRoutes.DbStats);
      end;

      RegisterSampleWsEcho(ServerThread);
      ServerThread.CorsEnabled := False;
      ServerThread.Start;

      FChat := TSampleWsChatClient.Create(ServerThread.Port, '/chat', ChatLine);
      FChat.Start;
    except
      StopServer;
      raise;
    end;

    edtPorta.Enabled := False;
    rdLog.Enabled := False;
    rdParallel.Enabled := False;
    rdIOCP.Enabled := False;
    chkDb.Enabled := False;
    btnTestDb.Enabled := False;
    btnSyna.Tag := 1;
    btnSyna.Caption := 'Parar Servidor';
    RadioGroup1.Enabled := False;
    edtTimeOut.Enabled := False;
    SetWsEnabled(True);
    Memo1.Lines.Add(Format('HTTP http://127.0.0.1:%s/teste/ping', [edtPorta.Text]));
    Memo1.Lines.Add(Format('WS   ws://127.0.0.1:%s/chat', [edtPorta.Text]));
    if chkDb.Checked then
      Memo1.Lines.Add(Format('DB   http://127.0.0.1:%s/db/ping  (pool=%s)',
        [edtPorta.Text, edtPoolN.Text]));
  end
  else
  begin
    StopServer;
    btnSyna.Tag := 0;
    btnSyna.Caption := 'Iniciar Servidor';
    edtPorta.Enabled := True;
    rdLog.Enabled := True;
    rdParallel.Enabled := True;
    rdIOCP.Enabled := True;
    chkDb.Enabled := True;
    btnTestDb.Enabled := True;
    RadioGroup1.Enabled := True;
    edtTimeOut.Enabled := True;
    Memo1.Lines.Add('Servidor parado');
  end;
end;

procedure TForm1.btnClearLogClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  MemoWs.Lines.Clear;
end;

procedure TForm1.btnTestDbClick(Sender: TObject);
var
  Msg: string;
begin
  ApplyDbFromUI;
  if TestSamplePgTemplate(FDbTemplate, Msg) then
    Memo1.Lines.Add(Msg)
  else
    Memo1.Lines.Add(Msg);
end;

procedure TForm1.btnWsSendClick(Sender: TObject);
var
  Msg: string;
begin
  Msg := Trim(edtWs.Text);
  if (Msg = '') or not Assigned(FChat) then
    Exit;
  FChat.SendText(Msg);
  edtWs.Clear;
end;

procedure TForm1.edtWsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    btnWsSendClick(nil);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ServerThread := nil;
  FDbBridge := nil;
  FChat := nil;
  FLogLock := TCriticalSection.Create;
  FLogQueue := TStringList.Create;
  FWsQueue := TStringList.Create;
  FDbTemplate := CreateSamplePgTemplate;
  BasicAuth := TBasicAuth.Create('username', 'password');
  JWTAuth := TBadgerJWTAuth.Create('secretekey',
    IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'jwt');
  edtDbHost.Text := FDbTemplate.HostName;
  edtDbPort.Text := IntToStr(FDbTemplate.Port);
  edtDbName.Text := FDbTemplate.Database;
  edtDbUser.Text := FDbTemplate.User;
  edtDbPass.Text := FDbTemplate.Password;
  edtPoolN.Text := '8';
  SetWsEnabled(False);
  tmrWs.Enabled := True;
  {$IFDEF LINUX}
  rdIOCP.Caption := 'epoll (Linux)';
  rdIOCP.Hint := 'Uncheck to force Synapse on Linux';
  {$ENDIF}
  Caption := 'Badger GUI';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  StopServer;
  FreeAndNil(BasicAuth);
  FreeAndNil(JWTAuth);
  FreeAndNil(FDbTemplate);
  FreeAndNil(FLogQueue);
  FreeAndNil(FWsQueue);
  FreeAndNil(FLogLock);
end;

procedure TForm1.HandleRequest(const RequestInfo: TRequestInfo);
begin
  if not rdLog.Checked then Exit;
  FLogLock.Acquire;
  try
    FLogQueue.Add('>> ' + RequestInfo.Method + ' ' + RequestInfo.URI
                + ' | IP: ' + RequestInfo.RemoteIP);
  finally
    FLogLock.Release;
  end;
  TThread.Queue(nil, SyncFlushLog);
end;

procedure TForm1.HandleResponse(const ResponseInfo: TResponseInfo);
begin
  if not rdLog.Checked then Exit;
  FLogLock.Acquire;
  try
    FLogQueue.Add('<< ' + IntToStr(ResponseInfo.StatusCode)
                + ' ' + ResponseInfo.StatusText
                + ' | ' + DateTimeToStr(ResponseInfo.Timestamp));
  finally
    FLogLock.Release;
  end;
  TThread.Queue(nil, SyncFlushLog);
end;

end.
