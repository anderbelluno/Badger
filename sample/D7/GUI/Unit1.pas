unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Badger, BadgerBasicAuth, BadgerAuthJWT, BadgerTypes, SampleRouteManager,
  BadgerLogger, ExtCtrls, SyncObjs, StdCtrls;

type
  TForm1 = class(TForm)
    btnSyna: TButton;
    rdLog: TCheckBox;
    Memo1: TMemo;
    Label1: TLabel;
    edtPorta: TEdit;
    RadioGroup1: TRadioGroup;
    Panel2: TPanel;
    Panel1: TPanel;
    Label2: TLabel;
    edtTimeOut: TEdit;
    btnClearLog: TButton;
    rdParallel: TCheckBox;
    rdIOCP: TCheckBox;
    procedure btnSynaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
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

{$R *.dfm}

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
  Logger.isActive := False;
  Logger.LogToConsole := False;

  if btnSyna.Tag = 0 then
  begin
    ServerThread := TBadger.Create;
    ServerThread.Port := StrToInt(edtPorta.Text);
    ServerThread.Timeout := StrToInt(edtTimeOut.Text);
    ServerThread.ParallelProcessing := rdParallel.Checked;
    ServerThread.MaxConcurrentConnections := 500;
    ServerThread.EnableEventInfo := rdLog.Checked;
    ServerThread.UseIOCP := rdIOCP.Checked;
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

    ServerThread.CorsEnabled := False;
    ServerThread.Start;
    edtPorta.Enabled := False;
    rdLog.Enabled := False;
    rdParallel.Enabled := False;
    rdIOCP.Enabled := False;
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
    rdParallel.Enabled := True;
    rdIOCP.Enabled := True;
    RadioGroup1.Enabled := True;
    edtTimeOut.Enabled := True;
  end;
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
  TThread.Synchronize(nil, SyncFlushLog);
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
  TThread.Synchronize(nil, SyncFlushLog);
end;

procedure TForm1.btnClearLogClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

end.
