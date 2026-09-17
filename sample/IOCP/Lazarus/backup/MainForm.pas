unit MainForm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SyncObjs, Badger, BadgerWinSock2, IocpDemoRoutes;

type
  TFormMain = class(TForm)
    PanelTop: TPanel;
    btnStartStop: TButton;
    lblPort: TLabel;
    edtPort: TEdit;
    btnPing: TButton;
    btnOther: TButton;
    MemoLog: TMemo;
    tmrLog: TTimer;
    procedure btnStartStopClick(Sender: TObject);
    procedure btnPingClick(Sender: TObject);
    procedure btnOtherClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrLogTimer(Sender: TObject);
  private
    FServer: TBadger;
    FLogLock: TCriticalSection;
    FLogQueue: TStringList;
    procedure Log(const AMsg: string);
    procedure ServerLog(const AMsg: string);
    procedure StopServer;
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

procedure TFormMain.ServerLog(const AMsg: string);
begin
  FLogLock.Acquire;
  try
    FLogQueue.Add(AMsg);
  finally
    FLogLock.Release;
  end;
end;

procedure TFormMain.tmrLogTimer(Sender: TObject);
var
  I: Integer;
  Lines: TStringList;
begin
  if FLogQueue.Count = 0 then
    Exit;
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
      Log(Lines[I]);
  finally
    Lines.Free;
  end;
end;

procedure TFormMain.StopServer;
begin
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
end;

procedure TFormMain.btnStartStopClick(Sender: TObject);
begin
  if btnStartStop.Tag = 0 then
  begin
    FServer := TBadger.Create;
    try
      FServer.UseIOCP := True;
      FServer.ParallelProcessing := True;
      FServer.MaxConcurrentConnections := 500;
      FServer.Port := StrToInt(Trim(edtPort.Text));
      RegisterIocpDemoRoutes(FServer);
      FServer.Start;
    except
      FreeAndNil(FServer);
      raise;
    end;
    btnStartStop.Tag := 1;
    btnStartStop.Caption := 'Parar';
    edtPort.Enabled := False;
    btnPing.Enabled := True;
    btnOther.Enabled := True;
    Log(Format('IOCP em %s/teste/ping', [BaseUrl]));
  end
  else
    StopServer;
end;

procedure TFormMain.HttpGet(const APath: string);
var
  Status: Integer;
  Body: AnsiString;
  Url: string;
begin
  Url := BaseUrl + APath;
  if not BadgerHttpGetLocal(StrToInt(Trim(edtPort.Text)), AnsiString(APath), Status, Body) then
    Log(Url + '  ->  falhou')
  else
    Log(Format('%s  ->  %d  %s', [Url, Status, string(Body)]));
end;

procedure TFormMain.btnPingClick(Sender: TObject);
begin
  HttpGet('/teste/ping');
end;

procedure TFormMain.btnOtherClick(Sender: TObject);
begin
  HttpGet('/other');
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FServer := nil;
  FLogLock := TCriticalSection.Create;
  FLogQueue := TStringList.Create;
  tmrLog.Enabled := True;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  StopServer;
  FLogQueue.Free;
  FLogLock.Free;
end;

end.
