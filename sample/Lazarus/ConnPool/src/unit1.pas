unit Unit1;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, Badger, BadgerTypes, BadgerLogger, BadgerDBPool, BadgerDBBridge,
  ConnPoolRoutes, ConnPoolWorkers, Unit2;

type
  { TForm1 }

  TForm1 = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    btnTestDb: TButton;
    btnStress: TButton;
    chkParallel: TCheckBox;
    edtHost: TEdit;
    edtPortDb: TEdit;
    edtDatabase: TEdit;
    edtUser: TEdit;
    edtPassword: TEdit;
    edtProtocol: TEdit;
    edtHttpPort: TEdit;
    lblHost: TLabel;
    lblPortDb: TLabel;
    lblDatabase: TLabel;
    lblUser: TLabel;
    lblPassword: TLabel;
    lblProtocol: TLabel;
    lblHttpPort: TLabel;
    lblPoolN: TLabel;
    lblThreads: TLabel;
    lblLoops: TLabel;
    lblHoldMs: TLabel;
    memoLog: TMemo;
    pnlTop: TPanel;
    sePoolN: TSpinEdit;
    seThreads: TSpinEdit;
    seLoops: TSpinEdit;
    seHoldMs: TSpinEdit;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnStressClick(Sender: TObject);
    procedure btnTestDbClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FServer: TBadger;
    FDbBridge: TBadgerDBBridge;
    FStress: TConnPoolStress;
    procedure Log(const AMsg: string);
    procedure ApplyDmFromUI;
    procedure RegisterRoutes;
    procedure StopServer;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Log(const AMsg: string);
begin
  memoLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + '  ' + AMsg);
end;

procedure TForm1.ApplyDmFromUI;
begin
  DataModule1.ApplySettings(
    Trim(edtHost.Text),
    Trim(edtDatabase.Text),
    Trim(edtUser.Text),
    edtPassword.Text,
    Trim(edtProtocol.Text),
    StrToIntDef(edtPortDb.Text, 5432));
end;

procedure TForm1.RegisterRoutes;
begin
  FServer.RouteManager
    .AddGet('/ping', TConnPoolRoutes.Ping)
    .AddGet('/db/ping', TConnPoolRoutes.DbPing)
    .AddGet('/db/work', TConnPoolRoutes.DbWork)
    .AddGet('/db/stats', TConnPoolRoutes.DbStats);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'Badger ConnPool — PostgreSQL multi-connection demo';
  DataModule1.ApplyDefaults;
  edtHost.Text := DataModule1.ZConnection1.HostName;
  edtPortDb.Text := IntToStr(DataModule1.ZConnection1.Port);
  edtDatabase.Text := DataModule1.ZConnection1.Database;
  edtUser.Text := DataModule1.ZConnection1.User;
  edtPassword.Text := DataModule1.ZConnection1.Password;
  edtProtocol.Text := DataModule1.ZConnection1.Protocol;
  edtHttpPort.Text := '8088';
  sePoolN.Value := 8;
  seThreads.Value := 20;
  seLoops.Value := 10;
  seHoldMs.Value := 30;
  chkParallel.Checked := True;
  Logger.isActive := True;
  Logger.LogToConsole := True;
  Log('Ready. Start Postgres (docker compose in ../db), then Test DB / Start server.');
  Log('HTTP: /ping  /db/ping  /db/work?ms=100  /db/stats');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  StopServer;
end;

procedure TForm1.StopServer;
begin
  if Assigned(FStress) then
  begin
    FStress.WaitDone(2000);
    FreeAndNil(FStress);
  end;
  if Assigned(FServer) then
  begin
    try
      FServer.Stop;
    except
    end;
    FreeAndNil(FServer);
  end;
  FreeAndNil(FDbBridge);
  btnStart.Enabled := True;
  btnStop.Enabled := False;
  btnStress.Enabled := False;
end;

procedure TForm1.btnTestDbClick(Sender: TObject);
var
  Msg: string;
begin
  ApplyDmFromUI;
  if DataModule1.TestTemplateConnection(Msg) then
    Log(Msg)
  else
    Log(Msg);
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  if Assigned(FServer) then
  begin
    Log('Server already running');
    Exit;
  end;

  ApplyDmFromUI;
  if DataModule1.ZConnection1.Connected then
    DataModule1.ZConnection1.Connected := False;

  try
    FDbBridge := TBadgerDBBridge.Create(DataModule1.ZConnection1, sePoolN.Value);
    FServer := TBadger.Create;
    FServer.Port := StrToIntDef(edtHttpPort.Text, 8088);
    FServer.Timeout := 5000;
    FServer.ParallelProcessing := chkParallel.Checked;
    FServer.MaxConcurrentConnections := 200;
    FServer.EnableEventInfo := False;
    FDbBridge.Register(FServer);
    RegisterRoutes;
    FServer.Start;
    btnStart.Enabled := False;
    btnStop.Enabled := True;
    btnStress.Enabled := True;
    Log(Format('Badger listening on :%d | pool=%d | parallel=%s',
      [FServer.Port, sePoolN.Value, BoolToStr(chkParallel.Checked, True)]));
    Log('Try: curl http://127.0.0.1:' + edtHttpPort.Text + '/db/work?ms=100');
  except
    on E: Exception do
    begin
      Log('Start failed: ' + E.Message);
      StopServer;
    end;
  end;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  StopServer;
  Log('Server stopped');
end;

procedure TForm1.btnStressClick(Sender: TObject);
var
  I: Integer;
begin
  if not Assigned(FDbBridge) or not Assigned(FDbBridge.Pool) then
  begin
    Log('Start the server first (pool is created with the DB bridge)');
    Exit;
  end;
  if Assigned(FStress) then
  begin
    Log('Stress already running / cleaning previous...');
    FStress.WaitDone(1000);
    FreeAndNil(FStress);
  end;

  FStress := TConnPoolStress.Create(FDbBridge.Pool, seThreads.Value, seLoops.Value, seHoldMs.Value);
  Log(Format('Stress start: threads=%d loops=%d hold=%dms',
    [seThreads.Value, seLoops.Value, seHoldMs.Value]));
  FStress.Start;
  FStress.WaitDone(180000);
  Log(Format('Stress done: ok=%d fail=%d', [FStress.Ok, FStress.Fail]));
  for I := 0 to FStress.Log.Count - 1 do
    Log(FStress.Log[I]);
  FreeAndNil(FStress);
end;

end.
