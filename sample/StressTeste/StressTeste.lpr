program StressTeste;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, SyncObjs, StrUtils, blcksock;

type
  TStressConfig = record
    Host: string;
    Port: Integer;
    Path: string;
    ThreadCount: Integer;
    RequestsPerThread: Integer;
    TimeoutMs: Integer;
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
    function ParseStatusCode(const StatusLine: string): Integer;
    function SendRequest(out AStatusCode: Integer): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TStressRunner; AWorkerId: Integer);
  end;

  TStressRunner = class
  private
    FConfig: TStressConfig;
    FStats: TStressStats;
    FStatsLock: TCriticalSection;
    FWorkers: array of TStressWorker;
    FStartTick: QWord;
    FEndTick: QWord;
  public
    constructor Create(const AConfig: TStressConfig);
    destructor Destroy; override;
    procedure AddResult(const ALatencyMs: Int64; const ASuccess: Boolean);
    procedure Run;
    procedure PrintSummary;
    property Config: TStressConfig read FConfig;
  end;

function GetArgValue(const AKey, ADefault: string): string;
var
  I: Integer;
  Prefix: string;
begin
  Result := ADefault;
  Prefix := '--' + AKey + '=';
  for I := 1 to ParamCount do
  begin
    if StartsText(Prefix, ParamStr(I)) then
    begin
      Result := Copy(ParamStr(I), Length(Prefix) + 1, MaxInt);
      Exit;
    end;
  end;
end;

procedure PrintUsage;
begin
  Writeln('StressTeste - Cliente de stress para servidor Badger');
  Writeln('Uso:');
  Writeln('  StressTeste --host=127.0.0.1 --port=8080 --path=/teste/ping --threads=50 --requests=1000 --timeout=3000');
  Writeln('');
  Writeln('Parâmetros (com padrão):');
  Writeln('  --host=127.0.0.1');
  Writeln('  --port=8080');
  Writeln('  --path=/teste/ping');
  Writeln('  --threads=20');
  Writeln('  --requests=200');
  Writeln('  --timeout=3000');
end;

constructor TStressWorker.Create(AOwner: TStressRunner; AWorkerId: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
  FWorkerId := AWorkerId;
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

function TStressWorker.SendRequest(out AStatusCode: Integer): Boolean;
var
  Sock: TTCPBlockSocket;
  RequestText: string;
  Line: string;
begin
  Result := False;
  AStatusCode := 0;
  Sock := TTCPBlockSocket.Create;
  try
    Sock.Connect(FOwner.Config.Host, IntToStr(FOwner.Config.Port));
    if Sock.LastError <> 0 then
      Exit;

    RequestText :=
      'GET ' + FOwner.Config.Path + ' HTTP/1.1' + #13#10 +
      'Host: ' + FOwner.Config.Host + ':' + IntToStr(FOwner.Config.Port) + #13#10 +
      'Connection: close' + #13#10 +
      'User-Agent: BadgerStress/1.0 (Worker ' + IntToStr(FWorkerId) + ')' + #13#10 +
      #13#10;

    Sock.SendString(RequestText);
    if Sock.LastError <> 0 then
      Exit;

    Line := Sock.RecvString(FOwner.Config.TimeoutMs);
    if Sock.LastError <> 0 then
      Exit;

    AStatusCode := ParseStatusCode(Line);
    Result := AStatusCode > 0;
  finally
    Sock.Free;
  end;
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
    T0 := GetTickCount64;
    Success := SendRequest(StatusCode) and (StatusCode >= 200) and (StatusCode < 500);
    T1 := GetTickCount64;
    Latency := Int64(T1 - T0);
    FOwner.AddResult(Latency, Success);
  end;
end;

constructor TStressRunner.Create(const AConfig: TStressConfig);
begin
  inherited Create;
  FConfig := AConfig;
  FStatsLock := TCriticalSection.Create;
  FillChar(FStats, SizeOf(FStats), 0);
  FStats.MinLatencyMs := High(Int64);
end;

destructor TStressRunner.Destroy;
begin
  FStatsLock.Free;
  inherited;
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
  SetLength(FWorkers, FConfig.ThreadCount);

  FStartTick := GetTickCount64;
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

procedure TStressRunner.PrintSummary;
var
  TotalTimeMs: Int64;
  AvgMs: Double;
  Rps: Double;
begin
  TotalTimeMs := Int64(FEndTick - FStartTick);
  if FStats.TotalRequests > 0 then
    AvgMs := FStats.TotalLatencyMs / FStats.TotalRequests
  else
    AvgMs := 0;

  if TotalTimeMs > 0 then
    Rps := (FStats.TotalRequests * 1000.0) / TotalTimeMs
  else
    Rps := 0;

  Writeln('----------------------------------------');
  Writeln('Stress concluído');
  Writeln('Host: ', FConfig.Host, ':', FConfig.Port, '  Path: ', FConfig.Path);
  Writeln('Threads: ', FConfig.ThreadCount, '  Requests/thread: ', FConfig.RequestsPerThread);
  Writeln('Total requests: ', FStats.TotalRequests);
  Writeln('Sucesso: ', FStats.SuccessCount, '  Falha: ', FStats.FailureCount);
  Writeln('Tempo total (ms): ', TotalTimeMs);
  Writeln('Throughput (req/s): ', FormatFloat('0.00', Rps));
  Writeln('Latência média (ms): ', FormatFloat('0.00', AvgMs));
  if FStats.MinLatencyMs = High(Int64) then
    Writeln('Latência min/max (ms): n/a')
  else
    Writeln('Latência min/max (ms): ', FStats.MinLatencyMs, ' / ', FStats.MaxLatencyMs);
  Writeln('----------------------------------------');
end;

function BuildConfig: TStressConfig;
begin
  Result.Host := GetArgValue('host', '127.0.0.1');
  Result.Port := StrToIntDef(GetArgValue('port', '8080'), 8080);
  Result.Path := GetArgValue('path', '/teste/ping');
  Result.ThreadCount := StrToIntDef(GetArgValue('threads', '100'), 20);
  Result.RequestsPerThread := StrToIntDef(GetArgValue('requests', '200'), 200);
  Result.TimeoutMs := StrToIntDef(GetArgValue('timeout', '3000'), 3000);
end;

var
  Config: TStressConfig;
  Runner: TStressRunner;
begin
  if (ParamCount > 0) and ((ParamStr(1) = '-h') or (ParamStr(1) = '--help')) then
  begin
    PrintUsage;
    Halt(0);
  end;

  Config := BuildConfig;
  Runner := TStressRunner.Create(Config);
  try
    Runner.Run;
    Runner.PrintSummary;
  finally
    Runner.Free;
  end;
end.
