unit Unit1;

{$mode delphi}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}Windows, {$ENDIF} Messages, Classes, SysUtils, SyncObjs,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Badger, BadgerBasicAuth, BadgerTypes, BadgerLogger, DemoRoutes, JsonEnvelopeAfter;

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
    JsonAfter: TJsonEnvelopeAfter;
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
    if ServerThread.EnableEventInfo then
    begin
      ServerThread.OnRequest := HandleRequest;
      ServerThread.OnResponse := HandleResponse;
    end;

    { Before + After: same RegisterProtectedRoutes pattern. }
    if RadioGroup1.ItemIndex = 1 then
    begin
      BasicAuth.RegisterProtectedRoutes(ServerThread, ['/ping']);
      JsonAfter.RegisterProtectedRoutes(ServerThread, ['/ping']);
    end;

    ServerThread.RouteManager.AddGet('/ping', TDemoRoutes.Ping);
    ServerThread.CorsEnabled := False;
    ServerThread.Start;
    edtPorta.Enabled := False;
    rdLog.Enabled := False;
    rdParallel.Enabled := False;
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
  Caption := 'Badger — Middleware Before/After';
  ServerThread := nil;
  FLogLock := TCriticalSection.Create;
  FLogQueue := TStringList.Create;
  BasicAuth := TBasicAuth.Create('username', 'password');
  JsonAfter := TJsonEnvelopeAfter.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(ServerThread) then
  begin
    ServerThread.Stop;
    FreeAndNil(ServerThread);
  end;
  FreeAndNil(JsonAfter);
  FreeAndNil(BasicAuth);
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
