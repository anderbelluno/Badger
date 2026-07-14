unit Unit1;

{$mode delphi}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}Windows, {$ENDIF} Messages, Classes, SysUtils, SyncObjs,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Badger, BadgerBasicAuth, BadgerTypes, BadgerLogger, DemoRoutes, superobject;

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
    FLogLock: TCriticalSection;
    FLogQueue: TStringList;
    procedure SyncFlushLog;
    { After-middleware: wraps the route body in a JSON envelope via SuperObject. }
    procedure AfterJsonEnvelope(var Request: THTTPRequest; var Response: THTTPResponse);
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

procedure TForm1.AfterJsonEnvelope(var Request: THTTPRequest;
  var Response: THTTPResponse);
var
  Root, Meta, Data: ISuperObject;
begin
  Root := SO();
  Data := nil;
  if Trim(Response.Body) <> '' then
  begin
    try
      Data := SO(Response.Body);
    except
      Data := nil;
    end;
  end;
  if Assigned(Data) then
    Root.O['data'] := Data
  else
    Root.S['data'] := Response.Body;

  Meta := SO();
  Meta.S['method'] := Request.Method;
  Meta.S['uri'] := Request.URI;
  Meta.I['status'] := Response.StatusCode;
  Root.O['meta'] := Meta;

  Response.ContentType := 'application/json';
  Response.Body := Root.AsJSON;
end;

procedure TForm1.btnSynaClick(Sender: TObject);
begin
  Logger.isActive := True;
  Logger.LogFileName := 'logger.log';
  Logger.LogToConsole := False;

  if btnSyna.Tag = 0 then
  begin
    ServerThread := TBadger.Create;
    ServerThread.EnableEventInfo := rdLog.Checked;
    ServerThread.Port := StrToInt(edtPorta.Text);
    ServerThread.Timeout := StrToInt(edtTimeOut.Text);

    ServerThread.OnRequest := HandleRequest;
    ServerThread.OnResponse := HandleResponse;

    { Before = TBasicAuth (same as GUI sample). After = JSON envelope (SuperObject). }
    if RadioGroup1.ItemIndex = 1 then
    begin
      BasicAuth.RegisterProtectedRoutes(ServerThread, ['/ping']);
      ServerThread.AddAfterMiddleware(AfterJsonEnvelope);
    end;

    ServerThread.RouteManager.AddGet('/ping', TDemoRoutes.Ping);

    ServerThread.ParallelProcessing := rdParallel.Checked;
    ServerThread.MaxConcurrentConnections := 50000;
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
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(ServerThread) then
  begin
    ServerThread.Stop;
    FreeAndNil(ServerThread);
  end;
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
