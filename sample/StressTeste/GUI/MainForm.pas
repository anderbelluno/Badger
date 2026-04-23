unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes, SysUtils, SyncObjs, StrUtils,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  blcksock;

type
  TStressConfig = record
    Host: string;
    Port: Integer;
    Path: string;
    ThreadCount: Integer;
    RequestsPerThread: Integer;
    TimeoutMs: Integer;
    KeepAlive: Boolean;
  end;

  TStressStats = record
    TotalRequests: Int64;
    SuccessCount: Int64;
    FailureCount: Int64;
    TotalLatencyMs: Int64;
    MinLatencyMs: Int64;
    MaxLatencyMs: Int64;
  end;

  TStressRunner = class;

  TStressWorker = class(TThread)
  private
    FOwner: TStressRunner;
    FWorkerId: Integer;
    FSocket: TTCPBlockSocket;
    FConnected: Boolean;
    procedure Disconnect;
    function EnsureConnected: Boolean;
    function ParseStatusCode(const StatusLine: string): Integer;
    function ReadResponse(out AStatusCode: Integer): Boolean;
    function SendRequest(out AStatusCode: Integer): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TStressRunner; AWorkerId: Integer);
    destructor Destroy; override;
  end;

  TStressRunner = class
  private
    FConfig: TStressConfig;
    FStats: TStressStats;
    FStatsLock: TCriticalSection;
    FWorkers: array of TStressWorker;
    FStopFlag: LongInt;
    FStartTick: QWord;
    FEndTick: QWord;
  public
    constructor Create(const AConfig: TStressConfig);
    destructor Destroy; override;
    procedure AddResult(const ALatencyMs: Int64; const ASuccess: Boolean);
    procedure Run;
    procedure RequestStop;
    function IsStopRequested: Boolean;
    function SnapshotStats: TStressStats;
    function GetElapsedMs: Int64;
    property Config: TStressConfig read FConfig;
  end;

  TStressRunThread = class(TThread)
  private
    FRunner: TStressRunner;
    FNotifyProc: TThreadMethod;
  protected
    procedure Execute; override;
  public
    constructor Create(ARunner: TStressRunner; ANotifyProc: TThreadMethod);
  end;

  { TfrmStressMain }

  TfrmStressMain = class(TForm)
    btnClearLog: TButton;
    btnStart: TButton;
    btnStop: TButton;
    chkKeepAlive: TCheckBox;
    edtHost: TEdit;
    edtPath: TEdit;
    edtPort: TEdit;
    edtRequests: TEdit;
    edtThreads: TEdit;
    edtTimeout: TEdit;
    lblHost: TLabel;
    lblPath: TLabel;
    lblPort: TLabel;
    lblRequests: TLabel;
    lblRunning: TLabel;
    lblSummary: TLabel;
    lblThreads: TLabel;
    lblTimeout: TLabel;
    MemoLog: TMemo;
    pnlConfig: TPanel;
    tmrUpdate: TTimer;
    procedure btnClearLogClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
  private
    FRunner: TStressRunner;
    FRunThread: TStressRunThread;
    procedure Log(const AText: string);
    procedure SetUiRunning(const ARunning: Boolean);
    function BuildConfig(out AConfig: TStressConfig): Boolean;
    procedure RunCompleted;
    procedure UpdateSummary;
  public
  end;

var
  frmStressMain: TfrmStressMain;

implementation

{$R *.lfm}

{ TStressWorker }

constructor TStressWorker.Create(AOwner: TStressRunner; AWorkerId: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
  FWorkerId := AWorkerId;
  FSocket := TTCPBlockSocket.Create;
  FConnected := False;
end;

destructor TStressWorker.Destroy;
begin
  Disconnect;
  FSocket.Free;
  inherited Destroy;
end;

procedure TStressWorker.Disconnect;
begin
  if FConnected then
    FSocket.CloseSocket;
  FConnected := False;
end;

function TStressWorker.EnsureConnected: Boolean;
begin
  Result := True;
  if FConnected then
    Exit;

  FSocket.Connect(FOwner.Config.Host, IntToStr(FOwner.Config.Port));
  FConnected := FSocket.LastError = 0;
  Result := FConnected;
end;

function TStressWorker.ParseStatusCode(const StatusLine: string): Integer;
var
  P1, P2: Integer;
  SCode: string;
begin
  Result := 0;
  P1 := Pos(' ', StatusLine);
  if P1 <= 0 then
    Exit;
  P2 := PosEx(' ', StatusLine, P1 + 1);
  if P2 <= 0 then
    P2 := Length(StatusLine) + 1;
  SCode := Trim(Copy(StatusLine, P1 + 1, P2 - P1 - 1));
  Result := StrToIntDef(SCode, 0);
end;

function TStressWorker.ReadResponse(out AStatusCode: Integer): Boolean;
var
  Line: string;
  HeaderLine: string;
  HeaderName: string;
  HeaderValue: string;
  P: Integer;
  ContentLength: Integer;
  IsChunked: Boolean;
  ChunkSize: Integer;
  ChunkLine: string;
begin
  Result := False;
  AStatusCode := 0;
  ContentLength := 0;
  IsChunked := False;

  Line := FSocket.RecvString(FOwner.Config.TimeoutMs);
  if FSocket.LastError <> 0 then
    Exit;
  AStatusCode := ParseStatusCode(Line);
  if AStatusCode = 0 then
    Exit;

  repeat
    HeaderLine := FSocket.RecvString(FOwner.Config.TimeoutMs);
    if FSocket.LastError <> 0 then
      Exit;
    if HeaderLine = '' then
      Break;

    P := Pos(':', HeaderLine);
    if P > 0 then
    begin
      HeaderName := Trim(Copy(HeaderLine, 1, P - 1));
      HeaderValue := Trim(Copy(HeaderLine, P + 1, MaxInt));
      if SameText(HeaderName, 'Content-Length') then
        ContentLength := StrToIntDef(HeaderValue, 0)
      else if SameText(HeaderName, 'Transfer-Encoding') and
              ContainsText(HeaderValue, 'chunked') then
        IsChunked := True;
    end;
  until False;

  if IsChunked then
  begin
    repeat
      ChunkLine := FSocket.RecvString(FOwner.Config.TimeoutMs);
      if FSocket.LastError <> 0 then
        Exit;
      P := Pos(';', ChunkLine);
      if P > 0 then
        ChunkLine := Copy(ChunkLine, 1, P - 1);
      ChunkSize := StrToIntDef('$' + Trim(ChunkLine), -1);
      if ChunkSize < 0 then
        Exit;
      if ChunkSize = 0 then
      begin
        // trailer headers (optional)
        repeat
          Line := FSocket.RecvString(FOwner.Config.TimeoutMs);
          if FSocket.LastError <> 0 then
            Exit;
        until Line = '';
        Break;
      end;
      if Length(FSocket.RecvBufferStr(ChunkSize, FOwner.Config.TimeoutMs)) <> ChunkSize then
        Exit;
      // CRLF after chunk data
      Line := FSocket.RecvString(FOwner.Config.TimeoutMs);
      if FSocket.LastError <> 0 then
        Exit;
    until False;
  end
  else if ContentLength > 0 then
  begin
    if Length(FSocket.RecvBufferStr(ContentLength, FOwner.Config.TimeoutMs)) <> ContentLength then
      Exit;
  end;

  Result := True;
end;

function TStressWorker.SendRequest(out AStatusCode: Integer): Boolean;
var
  RequestText: string;
begin
  Result := False;
  AStatusCode := 0;

  if not EnsureConnected then
    Exit;

  RequestText :=
    'GET ' + FOwner.Config.Path + ' HTTP/1.1' + #13#10 +
    'Host: ' + FOwner.Config.Host + ':' + IntToStr(FOwner.Config.Port) + #13#10 +
    'Connection: ' + IfThen(FOwner.Config.KeepAlive, 'keep-alive', 'close') + #13#10 +
    'User-Agent: BadgerStressGUI/1.0 (Worker ' + IntToStr(FWorkerId) + ')' + #13#10 +
    #13#10;

  FSocket.SendString(RequestText);
  if FSocket.LastError <> 0 then
  begin
    Disconnect;
    Exit;
  end;

  Result := ReadResponse(AStatusCode);
  if not Result or (not FOwner.Config.KeepAlive) then
    Disconnect;
end;

procedure TStressWorker.Execute;
var
  I: Integer;
  T0, T1: QWord;
  Latency: Int64;
  StatusCode: Integer;
  Success: Boolean;
begin
  for I := 1 to FOwner.Config.RequestsPerThread do
  begin
    if FOwner.IsStopRequested then
      Break;
    T0 := GetTickCount64;
    Success := SendRequest(StatusCode) and (StatusCode >= 200) and (StatusCode < 500);
    T1 := GetTickCount64;
    Latency := Int64(T1 - T0);
    FOwner.AddResult(Latency, Success);
  end;
end;

{ TStressRunner }

constructor TStressRunner.Create(const AConfig: TStressConfig);
begin
  inherited Create;
  FConfig := AConfig;
  FStatsLock := TCriticalSection.Create;
  FillChar(FStats, SizeOf(FStats), 0);
  FStats.MinLatencyMs := High(Int64);
  FStopFlag := 0;
  FStartTick := 0;
  FEndTick := 0;
end;

destructor TStressRunner.Destroy;
begin
  FStatsLock.Free;
  inherited Destroy;
end;

procedure TStressRunner.AddResult(const ALatencyMs: Int64; const ASuccess: Boolean);
begin
  FStatsLock.Acquire;
  try
    Inc(FStats.TotalRequests);
    if ASuccess then
      Inc(FStats.SuccessCount)
    else
      Inc(FStats.FailureCount);
    Inc(FStats.TotalLatencyMs, ALatencyMs);
    if ALatencyMs < FStats.MinLatencyMs then
      FStats.MinLatencyMs := ALatencyMs;
    if ALatencyMs > FStats.MaxLatencyMs then
      FStats.MaxLatencyMs := ALatencyMs;
  finally
    FStatsLock.Release;
  end;
end;

procedure TStressRunner.Run;
var
  I: Integer;
begin
  FStartTick := GetTickCount64;
  SetLength(FWorkers, FConfig.ThreadCount);

  for I := 0 to High(FWorkers) do
  begin
    FWorkers[I] := TStressWorker.Create(Self, I + 1);
    FWorkers[I].Start;
  end;

  for I := 0 to High(FWorkers) do
    FWorkers[I].WaitFor;

  FEndTick := GetTickCount64;
  for I := 0 to High(FWorkers) do
    FWorkers[I].Free;
end;

procedure TStressRunner.RequestStop;
begin
  InterlockedExchange(FStopFlag, 1);
end;

function TStressRunner.IsStopRequested: Boolean;
begin
  Result := InterlockedCompareExchange(FStopFlag, 0, 0) <> 0;
end;

function TStressRunner.SnapshotStats: TStressStats;
begin
  FStatsLock.Acquire;
  try
    Result := FStats;
  finally
    FStatsLock.Release;
  end;
end;

function TStressRunner.GetElapsedMs: Int64;
var
  EndTick: QWord;
begin
  if FStartTick = 0 then
    Exit(0);
  EndTick := FEndTick;
  if EndTick = 0 then
    EndTick := GetTickCount64;
  Result := Int64(EndTick - FStartTick);
end;

{ TStressRunThread }

constructor TStressRunThread.Create(ARunner: TStressRunner; ANotifyProc: TThreadMethod);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FRunner := ARunner;
  FNotifyProc := ANotifyProc;
end;

procedure TStressRunThread.Execute;
begin
  FRunner.Run;
  if Assigned(FNotifyProc) then
    TThread.Queue(nil, FNotifyProc);
end;

{ TfrmStressMain }

procedure TfrmStressMain.FormCreate(Sender: TObject);
begin
  SetUiRunning(False);
  lblRunning.Caption := 'Status: aguardando execução';
  lblSummary.Caption := 'Resumo: -';
end;

procedure TfrmStressMain.FormDestroy(Sender: TObject);
begin
  tmrUpdate.Enabled := False;
  if Assigned(FRunner) then
    FRunner.RequestStop;
  if Assigned(FRunThread) then
  begin
    FRunThread.WaitFor;
    FreeAndNil(FRunThread);
  end;
  FreeAndNil(FRunner);
end;

procedure TfrmStressMain.btnStartClick(Sender: TObject);
var
  Cfg: TStressConfig;
begin
  if Assigned(FRunThread) then
  begin
    Log('Um teste já está em execução.');
    Exit;
  end;

  if not BuildConfig(Cfg) then
    Exit;

  FRunner := TStressRunner.Create(Cfg);
  FRunThread := TStressRunThread.Create(FRunner, @RunCompleted);
  SetUiRunning(True);
  Log(Format('Iniciando stress: host=%s port=%d path=%s threads=%d requests/thread=%d timeout=%d keepalive=%s',
    [Cfg.Host, Cfg.Port, Cfg.Path, Cfg.ThreadCount, Cfg.RequestsPerThread, Cfg.TimeoutMs, BoolToStr(Cfg.KeepAlive, True)]));
  FRunThread.Start;
  tmrUpdate.Enabled := True;
end;

procedure TfrmStressMain.btnStopClick(Sender: TObject);
begin
  if Assigned(FRunner) then
  begin
    FRunner.RequestStop;
    Log('Solicitação de parada enviada.');
  end;
end;

procedure TfrmStressMain.btnClearLogClick(Sender: TObject);
begin
  MemoLog.Clear;
end;

procedure TfrmStressMain.tmrUpdateTimer(Sender: TObject);
begin
  UpdateSummary;
end;

procedure TfrmStressMain.Log(const AText: string);
begin
  MemoLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' - ' + AText);
  MemoLog.SelStart := Length(MemoLog.Text);
end;

procedure TfrmStressMain.SetUiRunning(const ARunning: Boolean);
begin
  btnStart.Enabled := not ARunning;
  btnStop.Enabled := ARunning;
  edtHost.Enabled := not ARunning;
  edtPort.Enabled := not ARunning;
  edtPath.Enabled := not ARunning;
  edtThreads.Enabled := not ARunning;
  edtRequests.Enabled := not ARunning;
  edtTimeout.Enabled := not ARunning;
  chkKeepAlive.Enabled := not ARunning;
  if ARunning then
    lblRunning.Caption := 'Status: executando'
  else
    lblRunning.Caption := 'Status: parado';
end;

function TfrmStressMain.BuildConfig(out AConfig: TStressConfig): Boolean;
begin
  Result := False;
  AConfig.Host := Trim(edtHost.Text);
  AConfig.Port := StrToIntDef(Trim(edtPort.Text), 0);
  AConfig.Path := Trim(edtPath.Text);
  AConfig.ThreadCount := StrToIntDef(Trim(edtThreads.Text), 0);
  AConfig.RequestsPerThread := StrToIntDef(Trim(edtRequests.Text), 0);
  AConfig.TimeoutMs := StrToIntDef(Trim(edtTimeout.Text), 0);
  AConfig.KeepAlive := chkKeepAlive.Checked;

  if AConfig.Host = '' then
  begin
    Log('Host inválido.');
    Exit;
  end;
  if (AConfig.Port <= 0) or (AConfig.Port > 65535) then
  begin
    Log('Porta inválida.');
    Exit;
  end;
  if (AConfig.Path = '') or (AConfig.Path[1] <> '/') then
  begin
    Log('Path inválido. Exemplo: /teste/ping');
    Exit;
  end;
  if AConfig.ThreadCount <= 0 then
  begin
    Log('Threads deve ser > 0.');
    Exit;
  end;
  if AConfig.RequestsPerThread <= 0 then
  begin
    Log('Requests/thread deve ser > 0.');
    Exit;
  end;
  if AConfig.TimeoutMs <= 0 then
  begin
    Log('Timeout deve ser > 0.');
    Exit;
  end;
  Result := True;
end;

procedure TfrmStressMain.RunCompleted;
begin
  tmrUpdate.Enabled := False;
  UpdateSummary;
  Log('Teste finalizado.');
  if Assigned(FRunThread) then
    FreeAndNil(FRunThread);
  if Assigned(FRunner) then
    FreeAndNil(FRunner);
  SetUiRunning(False);
end;

procedure TfrmStressMain.UpdateSummary;
var
  S: TStressStats;
  ElapsedMs: Int64;
  AvgMs: Double;
  Rps: Double;
begin
  if not Assigned(FRunner) then
  begin
    lblSummary.Caption := 'Resumo: -';
    Exit;
  end;

  S := FRunner.SnapshotStats;
  ElapsedMs := FRunner.GetElapsedMs;
  if S.TotalRequests > 0 then
    AvgMs := S.TotalLatencyMs / S.TotalRequests
  else
    AvgMs := 0;
  if ElapsedMs > 0 then
    Rps := (S.TotalRequests * 1000.0) / ElapsedMs
  else
    Rps := 0;

  if S.MinLatencyMs = High(Int64) then
    lblSummary.Caption := Format('Resumo: total=%d sucesso=%d falha=%d tempo=%dms rps=%s avg=%sms min/max=n/a',
      [S.TotalRequests, S.SuccessCount, S.FailureCount, ElapsedMs, FormatFloat('0.00', Rps), FormatFloat('0.00', AvgMs)])
  else
    lblSummary.Caption := Format('Resumo: total=%d sucesso=%d falha=%d tempo=%dms rps=%s avg=%sms min/max=%d/%dms',
      [S.TotalRequests, S.SuccessCount, S.FailureCount, ElapsedMs, FormatFloat('0.00', Rps), FormatFloat('0.00', AvgMs),
       S.MinLatencyMs, S.MaxLatencyMs]);
end;

end.
