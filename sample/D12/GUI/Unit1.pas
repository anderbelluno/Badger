unit Unit1;

{

  Check the Project-Search Path for more info

}

interface

uses
  Badger,
  BadgerBasicAuth,
  BadgerAuthJWT,
  BadgerTypes,
  BadgerLogger,

  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.SyncObjs,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Layouts,
  FMX.Controls.Presentation, FMX.Edit, FMX.ListBox;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    btnSyna: TButton;
    rdLog: TCheckBox;
    Memo1: TMemo;
    Label1: TLabel;
    edtPorta: TEdit;
    ComboAuth: TComboBox;
    Layout2: TLayout;
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

uses
  SampleRouteManager;

{$R *.fmx}

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
      Memo1.GoToTextEnd;
  finally
    Lines.Free;
  end;
end;

procedure TForm1.btnClearLogClick(Sender: TObject);
begin
  TThread.Queue(nil,
    procedure
    begin
      Memo1.Lines.Clear;
    end);
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
    ServerThread.ParallelProcessing := rdParallel.IsChecked;
    ServerThread.MaxConcurrentConnections := 500;
    ServerThread.EnableEventInfo := rdLog.IsChecked;
    ServerThread.UseIOCP := rdIOCP.IsChecked;
    if ServerThread.EnableEventInfo then
    begin
      ServerThread.OnRequest := HandleRequest;
      ServerThread.OnResponse := HandleResponse;
    end;

    case ComboAuth.ItemIndex of
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
    edtPorta.Enabled   := False;
    rdLog.Enabled      := False;
    rdParallel.Enabled := False;
    rdIOCP.Enabled     := False;
    btnSyna.Tag        := 1;
    btnSyna.Text       := 'Parar Servidor';
    ComboAuth.Enabled  := False;
    edtTimeOut.Enabled := False;
  end
  else
  begin
    ServerThread.Stop;
    FreeAndNil(ServerThread);
    btnSyna.Tag        := 0;
    btnSyna.Text       := 'Iniciar Servidor';
    edtPorta.Enabled   := True;
    rdLog.Enabled      := True;
    rdParallel.Enabled := True;
    rdIOCP.Enabled     := True;
    ComboAuth.Enabled  := True;
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
  if not rdLog.IsChecked then Exit;
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
  if not rdLog.IsChecked then Exit;
  FLogLock.Acquire;
  try
    FLogQueue.Add('<< ' + ResponseInfo.StatusCode.ToString
                + ' ' + ResponseInfo.StatusText
                + ' | ' + DateTimeToStr(ResponseInfo.Timestamp));
  finally
    FLogLock.Release;
  end;
  TThread.Queue(nil, SyncFlushLog);
end;

end.
