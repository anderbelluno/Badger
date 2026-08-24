unit Unit1;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SyncObjs, Badger, BadgerTypes, BadgerLogger, DemoMiddleware, DemoRoutes;

type
  { TForm1 }

  TForm1 = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    btnClear: TButton;
    chkRequireKey: TCheckBox;
    chkParallel: TCheckBox;
    edtPort: TEdit;
    edtApiKey: TEdit;
    lblPort: TLabel;
    lblApiKey: TLabel;
    lblHint: TLabel;
    memoLog: TMemo;
    pnlTop: TPanel;
    procedure btnClearClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FServer: TBadger;
    FMw: TMiddlewareDemo;
    FLogLock: TCriticalSection;
    FLogQueue: TStringList;
    procedure AppendLog(const AMsg: string);
    procedure SyncFlushLog;
    procedure StopServer;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.AppendLog(const AMsg: string);
begin
  FLogLock.Acquire;
  try
    FLogQueue.Add(FormatDateTime('hh:nn:ss.zzz', Now) + '  ' + AMsg);
  finally
    FLogLock.Release;
  end;
  TThread.Queue(nil, SyncFlushLog);
end;

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
      memoLog.Lines.Add(Lines[I]);
    if Lines.Count > 0 then
      memoLog.SelStart := Length(memoLog.Text);
  finally
    Lines.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'Badger — Middleware Before / After';
  edtPort.Text := '8090';
  edtApiKey.Text := 'demo-key';
  chkRequireKey.Checked := True;
  chkParallel.Checked := True;
  FLogLock := TCriticalSection.Create;
  FLogQueue := TStringList.Create;
  Logger.isActive := True;
  Logger.LogToConsole := True;
  AppendLog('Ready. Start server, then try the curl examples below.');
  AppendLog('GET /ping  /echo?msg=hi  /slow?ms=300  /secure');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  StopServer;
  FreeAndNil(FLogQueue);
  FreeAndNil(FLogLock);
end;

procedure TForm1.StopServer;
begin
  if Assigned(FServer) then
  begin
    try
      FServer.Stop;
    except
    end;
    FreeAndNil(FServer);
  end;
  FreeAndNil(FMw);
  btnStart.Enabled := True;
  btnStop.Enabled := False;
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  memoLog.Clear;
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  if Assigned(FServer) then
  begin
    AppendLog('Server already running');
    Exit;
  end;

  try
    FMw := TMiddlewareDemo.Create;
    FMw.OnLog := AppendLog;
    FMw.RequireApiKey := chkRequireKey.Checked;
    FMw.ApiKey := Trim(edtApiKey.Text);
    if FMw.ApiKey = '' then
      FMw.ApiKey := 'demo-key';

    FServer := TBadger.Create;
    FServer.Port := StrToIntDef(edtPort.Text, 8090);
    FServer.Timeout := 5000;
    FServer.NonBlockMode := True;
    FServer.ParallelProcessing := chkParallel.Checked;
    FServer.MaxConcurrentConnections := 100;
    FServer.EnableEventInfo := False;

    FMw.Register(FServer);

    FServer.RouteManager
      .AddGet('/ping', TDemoRoutes.Ping)
      .AddGet('/echo', TDemoRoutes.Echo)
      .AddGet('/secure', TDemoRoutes.Secure)
      .AddGet('/slow', TDemoRoutes.Slow);

    FServer.Start;
    btnStart.Enabled := False;
    btnStop.Enabled := True;

    AppendLog(Format('Listening on :%d | RequireApiKey=%s | key=%s | parallel=%s',
      [FServer.Port, BoolToStr(FMw.RequireApiKey, True), FMw.ApiKey,
       BoolToStr(chkParallel.Checked, True)]));
    AppendLog(Format('curl http://127.0.0.1:%d/ping', [FServer.Port]));
    AppendLog(Format('curl http://127.0.0.1:%d/secure', [FServer.Port]));
    AppendLog(Format('curl -H "X-Api-Key: %s" http://127.0.0.1:%d/secure',
      [FMw.ApiKey, FServer.Port]));
    AppendLog(Format('curl "http://127.0.0.1:%d/slow?ms=300"', [FServer.Port]));
  except
    on E: Exception do
    begin
      AppendLog('Start failed: ' + E.Message);
      StopServer;
    end;
  end;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  StopServer;
  AppendLog('Server stopped');
end;

end.
