unit Unit1;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF MSWINDOWS}Windows, {$ENDIF} Messages, Classes, SysUtils, SyncObjs,
    Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
    Badger, BadgerBasicAuth, BadgerAuthJWT, BadgerTypes, BadgerLogger, SampleRouteManager;

type

    { TForm1 }

    TForm1 = class(TForm)
        btnClearLog: TButton;
        btnSyna: TButton;
        edtPorta: TEdit;
        edtTimeOut: TEdit;
        Label1: TLabel;
        Label2: TLabel;
        Memo1: TMemo;
        Panel1: TPanel;
        Panel2: TPanel;
        RadioGroup1: TRadioGroup;
        rdLog: TCheckBox;
        rdParallel: TCheckBox;
        procedure btnClearLogClick(Sender: TObject);
        procedure btnSynaClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private
        ServerThread: TBadger;
        BasicAuth: TBasicAuth;
        JWTAuth: TBadgerJWTAuth;
        FLogLock: TCriticalSection;
        FLogQueue: TStringList;
        procedure SyncFlushLog;
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

procedure TForm1.btnSynaClick(Sender: TObject);
begin
  Logger.isActive := true;
  Logger.LogFileName := 'logger.log';
  Logger.LogToConsole := False;

  if btnSyna.Tag = 0 then
  begin
    ServerThread := TBadger.Create;
    ServerThread.EnableEventInfo := rdLog.Checked;
    ServerThread.Port := StrToInt(edtPorta.Text);
    ServerThread.Timeout := StrToInt(edtTimeOut.Text);

    ServerThread.OnRequest  := HandleRequest;
    ServerThread.OnResponse := HandleResponse;

    case RadioGroup1.ItemIndex of
      1: BasicAuth.RegisterProtectedRoutes(ServerThread, ['/rota1', '/ping', '/download']);
      3: begin
            JWTAuth.RegisterProtectedRoutes(ServerThread, ['/rota1', '/ping']);
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

    ServerThread.ParallelProcessing:= rdParallel.Checked;
   { ServerThread.MaxConcurrentConnections:= 30000; }

    ServerThread.CorsEnabled := False;

    ServerThread.Start;
    edtPorta.Enabled := False;
    rdLog.Enabled := False;
    btnSyna.Tag := 1;
    btnSyna.Caption := 'Parar Servidor';
    RadioGroup1.Enabled := False;
    edtTimeOut.Enabled := False;
  end
  else
  begin
    ServerThread.Stop;
    FreeAndNil(ServerThread);
    btnSyna.Tag := 0;
    btnSyna.Caption := 'Iniciar Servidor';
    edtPorta.Enabled := True;
    rdLog.Enabled := True;
    RadioGroup1.Enabled := True;
    edtTimeOut.Enabled := True;
  end;
end;

procedure TForm1.btnClearLogClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ServerThread := nil;
  FLogLock := TCriticalSection.Create;
  FLogQueue := TStringList.Create;
  BasicAuth := TBasicAuth.Create('username', 'password');
  JWTAuth := TBadgerJWTAuth.Create('secretekey', 'c:\tokenss');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(ServerThread) then
  begin
    ServerThread.Stop;
    FreeAndNil(ServerThread);
  end;
  FreeAndNil(BasicAuth);
  FreeAndNil(JWTAuth);
  FreeAndNil(FLogQueue);
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
