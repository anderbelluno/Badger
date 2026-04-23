unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes, SysUtils, SyncObjs, StrUtils,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Grids,
  blcksock;

type
  TAuthMode = (amNone, amBasic, amBearer, amJwtLogin);

  TStressConfig = record
    Host: string;
    Port: Integer;
    Path: string;
    ThreadCount: Integer;
    RequestsPerThread: Integer;
    TimeoutMs: Integer;
    KeepAlive: Boolean;
    AuthMode: TAuthMode;
    AuthUser: string;
    AuthPass: string;
    BearerToken: string;
    JwtLoginPath: string;
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
    FAuthHeader: string;
    procedure Disconnect;
    function EnsureConnected: Boolean;
    function EnsureAuthReady: Boolean;
    function BuildBasicAuthHeader: string;
    function FetchJwtToken(out AToken: string): Boolean;
    function ParseJsonStringField(const AJson, AField: string; out AValue: string): Boolean;
    function EscapeJson(const S: string): string;
    function Base64Encode(const S: string): string;
    function ParseStatusCode(const StatusLine: string): Integer;
    function ReadResponse(out AStatusCode: Integer; out ABody: string; ASocket: TTCPBlockSocket = nil): Boolean;
    function ReadFixedSize(ASocket: TTCPBlockSocket; ASize: Integer; out AData: string): Boolean;
    function TrimLineEnd(const S: string): string;
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
    cbAuthMode: TComboBox;
    chkKeepAlive: TCheckBox;
    edtAuthPass: TEdit;
    edtAuthUser: TEdit;
    edtBearerToken: TEdit;
    edtHost: TEdit;
    edtJwtLoginPath: TEdit;
    edtPath: TEdit;
    edtPort: TEdit;
    edtRequests: TEdit;
    edtThreads: TEdit;
    edtTimeout: TEdit;
    lblAuthMode: TLabel;
    lblAuthPass: TLabel;
    lblAuthUser: TLabel;
    lblBearerToken: TLabel;
    lblHost: TLabel;
    lblJwtLoginPath: TLabel;
    lblPath: TLabel;
    lblPort: TLabel;
    lblRequests: TLabel;
    lblRunning: TLabel;
    lblSummary: TLabel;
    lblThreads: TLabel;
    lblTimeout: TLabel;
    MemoLog: TMemo;
    grdStats: TStringGrid;
    pnlConfig: TPanel;
    tmrUpdate: TTimer;
    procedure btnClearLogClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure cbAuthModeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
  private
    FRunner: TStressRunner;
    FRunThread: TStressRunThread;
    FLastSampleRequests: Int64;
    FLastSampleTick: QWord;
    FRpsInstMin: Double;
    FRpsInstMax: Double;
    FRpsInstSum: Double;
    FRpsInstCount: Int64;
    FLastMetricsLogTick: QWord;
    procedure Log(const AText: string);
    procedure InitStatsGrid;
    procedure SetStatValue(const ARow: Integer; const AValue: string);
    procedure SetUiRunning(const ARunning: Boolean);
    procedure UpdateAuthControls;
    function AuthModeToText(const AMode: TAuthMode): string;
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
  FAuthHeader := '';
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

function TStressWorker.Base64Encode(const S: string): string;
const
  Base64Table = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  I: Integer;
  B0, B1, B2: Integer;
  Remaining: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do
  begin
    Remaining := Length(S) - I + 1;
    B0 := Ord(S[I]);
    if Remaining > 1 then
      B1 := Ord(S[I + 1])
    else
      B1 := 0;
    if Remaining > 2 then
      B2 := Ord(S[I + 2])
    else
      B2 := 0;

    Result := Result + Base64Table[(B0 shr 2) + 1];
    Result := Result + Base64Table[(((B0 and $03) shl 4) or (B1 shr 4)) + 1];
    if Remaining > 1 then
      Result := Result + Base64Table[(((B1 and $0F) shl 2) or (B2 shr 6)) + 1]
    else
      Result := Result + '=';
    if Remaining > 2 then
      Result := Result + Base64Table[(B2 and $3F) + 1]
    else
      Result := Result + '=';

    Inc(I, 3);
  end;
end;

function TStressWorker.BuildBasicAuthHeader: string;
begin
  Result := 'Authorization: Basic ' +
    Base64Encode(FOwner.Config.AuthUser + ':' + FOwner.Config.AuthPass);
end;

function TStressWorker.TrimLineEnd(const S: string): string;
begin
  Result := S;
  while (Length(Result) > 0) and
        ((Result[Length(Result)] = #13) or (Result[Length(Result)] = #10)) do
    Delete(Result, Length(Result), 1);
end;

function TStressWorker.ReadFixedSize(ASocket: TTCPBlockSocket; ASize: Integer; out AData: string): Boolean;
begin
  AData := '';
  if ASize <= 0 then
    Exit(True);
  AData := ASocket.RecvBufferStr(ASize, FOwner.Config.TimeoutMs);
  Result := (ASocket.LastError = 0) and (Length(AData) = ASize);
end;

function TStressWorker.ParseJsonStringField(const AJson, AField: string; out AValue: string): Boolean;
var
  KeyPos, ColonPos, I: Integer;
  KeyToken: string;
begin
  Result := False;
  AValue := '';
  KeyToken := '"' + AField + '"';
  KeyPos := Pos(KeyToken, AJson);
  if KeyPos <= 0 then
    Exit;

  ColonPos := PosEx(':', AJson, KeyPos + Length(KeyToken));
  if ColonPos <= 0 then
    Exit;

  I := ColonPos + 1;
  while (I <= Length(AJson)) and (AJson[I] <= ' ') do
    Inc(I);
  if (I > Length(AJson)) or (AJson[I] <> '"') then
    Exit;
  Inc(I);

  while I <= Length(AJson) do
  begin
    if (AJson[I] = '"') and ((I = 1) or (AJson[I - 1] <> '\')) then
    begin
      Result := True;
      Exit;
    end;
    AValue := AValue + AJson[I];
    Inc(I);
  end;
end;

function TStressWorker.EscapeJson(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    case S[I] of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #13: Result := Result + '\r';
      #10: Result := Result + '\n';
    else
      Result := Result + S[I];
    end;
  end;
end;

function TStressWorker.FetchJwtToken(out AToken: string): Boolean;
var
  AuthSock: TTCPBlockSocket;
  ReqBody: string;
  ReqText: string;
  StatusCode: Integer;
  RespBody: string;
begin
  Result := False;
  AToken := '';

  AuthSock := TTCPBlockSocket.Create;
  try
    AuthSock.Connect(FOwner.Config.Host, IntToStr(FOwner.Config.Port));
    if AuthSock.LastError <> 0 then
      Exit;

    ReqBody := '{"username":"' + EscapeJson(FOwner.Config.AuthUser) +
               '","password":"' + EscapeJson(FOwner.Config.AuthPass) + '"}';

    ReqText :=
      'POST ' + FOwner.Config.JwtLoginPath + ' HTTP/1.1' + #13#10 +
      'Host: ' + FOwner.Config.Host + ':' + IntToStr(FOwner.Config.Port) + #13#10 +
      'Connection: close' + #13#10 +
      'Content-Type: application/json' + #13#10 +
      'Content-Length: ' + IntToStr(Length(ReqBody)) + #13#10 +
      'User-Agent: BadgerStressGUI/1.0 (Auth Worker ' + IntToStr(FWorkerId) + ')' + #13#10 +
      #13#10 +
      ReqBody;

    AuthSock.SendString(ReqText);
    if AuthSock.LastError <> 0 then
      Exit;

    if not ReadResponse(StatusCode, RespBody, AuthSock) then
      Exit;
    if StatusCode <> 200 then
      Exit;

    if not ParseJsonStringField(RespBody, 'token', AToken) then
      if not ParseJsonStringField(RespBody, 'access_token', AToken) then
        Exit;

    Result := AToken <> '';
  finally
    AuthSock.CloseSocket;
    AuthSock.Free;
  end;
end;

function TStressWorker.EnsureAuthReady: Boolean;
var
  JwtToken: string;
begin
  case FOwner.Config.AuthMode of
    amNone:
      begin
        FAuthHeader := '';
        Exit(True);
      end;
    amBasic:
      begin
        if FAuthHeader = '' then
          FAuthHeader := BuildBasicAuthHeader;
        Exit(True);
      end;
    amBearer:
      begin
        if FAuthHeader = '' then
          FAuthHeader := 'Authorization: Bearer ' + FOwner.Config.BearerToken;
        Exit(True);
      end;
    amJwtLogin:
      begin
        if FAuthHeader = '' then
        begin
          if not FetchJwtToken(JwtToken) then
            Exit(False);
          FAuthHeader := 'Authorization: Bearer ' + JwtToken;
        end;
        Exit(True);
      end;
  end;
  Result := False;
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

function TStressWorker.ReadResponse(out AStatusCode: Integer; out ABody: string; ASocket: TTCPBlockSocket): Boolean;
var
  TargetSocket: TTCPBlockSocket;
  Line: string;
  HeaderLine: string;
  HeaderName: string;
  HeaderValue: string;
  P: Integer;
  ContentLength: Integer;
  IsChunked: Boolean;
  ChunkSize: Integer;
  ChunkLine: string;
  ChunkData: string;
  BodyData: string;
  ContentData: string;
begin
  Result := False;
  AStatusCode := 0;
  ABody := '';
  ContentLength := 0;
  IsChunked := False;
  BodyData := '';
  ContentData := '';
  ChunkData := '';

  if Assigned(ASocket) then
    TargetSocket := ASocket
  else
    TargetSocket := FSocket;

  Line := TrimLineEnd(TargetSocket.RecvString(FOwner.Config.TimeoutMs));
  if TargetSocket.LastError <> 0 then
    Exit;
  AStatusCode := ParseStatusCode(Line);
  if AStatusCode = 0 then
    Exit;

  repeat
    HeaderLine := TrimLineEnd(TargetSocket.RecvString(FOwner.Config.TimeoutMs));
    if TargetSocket.LastError <> 0 then
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
      ChunkLine := TargetSocket.RecvString(FOwner.Config.TimeoutMs);
      if TargetSocket.LastError <> 0 then
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
          Line := TrimLineEnd(TargetSocket.RecvString(FOwner.Config.TimeoutMs));
          if TargetSocket.LastError <> 0 then
            Exit;
        until Line = '';
        Break;
      end;
      if not ReadFixedSize(TargetSocket, ChunkSize, ChunkData) then
        Exit;
      BodyData := BodyData + ChunkData;
      // CRLF after chunk data
      Line := TrimLineEnd(TargetSocket.RecvString(FOwner.Config.TimeoutMs));
      if TargetSocket.LastError <> 0 then
        Exit;
    until False;
  end
  else if ContentLength > 0 then
  begin
    if not ReadFixedSize(TargetSocket, ContentLength, ContentData) then
      Exit;
    BodyData := ContentData;
  end;

  ABody := BodyData;
  Result := True;
end;

function TStressWorker.SendRequest(out AStatusCode: Integer): Boolean;
var
  RequestText: string;
  ResponseBody: string;
begin
  Result := False;
  AStatusCode := 0;

  if not EnsureAuthReady then
    Exit;

  if not EnsureConnected then
    Exit;

  RequestText :=
    'GET ' + FOwner.Config.Path + ' HTTP/1.1' + #13#10 +
    'Host: ' + FOwner.Config.Host + ':' + IntToStr(FOwner.Config.Port) + #13#10 +
    'Connection: ' + IfThen(FOwner.Config.KeepAlive, 'keep-alive', 'close') + #13#10 +
    'User-Agent: BadgerStressGUI/1.0 (Worker ' + IntToStr(FWorkerId) + ')' + #13#10 +
    IfThen(FAuthHeader <> '', FAuthHeader + #13#10, '') +
    #13#10;

  FSocket.SendString(RequestText);
  if FSocket.LastError <> 0 then
  begin
    Disconnect;
    Exit;
  end;

  Result := ReadResponse(AStatusCode, ResponseBody);
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
  cbAuthMode.Items.Clear;
  cbAuthMode.Items.Add('Nenhum');
  cbAuthMode.Items.Add('Basic');
  cbAuthMode.Items.Add('Bearer (token manual)');
  cbAuthMode.Items.Add('JWT (login automático)');
  cbAuthMode.ItemIndex := 0;
  edtAuthUser.Text := 'usuario';
  edtAuthPass.Text := 'senha123';
  edtJwtLoginPath.Text := '/Login';
  edtBearerToken.Text := '';

  FLastSampleRequests := 0;
  FLastSampleTick := 0;
  FRpsInstMin := 0;
  FRpsInstMax := 0;
  FRpsInstSum := 0;
  FRpsInstCount := 0;
  FLastMetricsLogTick := 0;
  InitStatsGrid;
  UpdateAuthControls;
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
  FLastSampleRequests := 0;
  FLastSampleTick := 0;
  FRpsInstMin := 0;
  FRpsInstMax := 0;
  FRpsInstSum := 0;
  FRpsInstCount := 0;
  FLastMetricsLogTick := 0;
  SetUiRunning(True);
  Log(Format('Iniciando stress: host=%s port=%d path=%s threads=%d requests/thread=%d timeout=%d keepalive=%s auth=%s',
    [Cfg.Host, Cfg.Port, Cfg.Path, Cfg.ThreadCount, Cfg.RequestsPerThread, Cfg.TimeoutMs, BoolToStr(Cfg.KeepAlive, True),
     AuthModeToText(Cfg.AuthMode)]));
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

procedure TfrmStressMain.cbAuthModeChange(Sender: TObject);
begin
  UpdateAuthControls;
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

procedure TfrmStressMain.InitStatsGrid;
begin
  grdStats.ColCount := 2;
  grdStats.RowCount := 13;
  grdStats.FixedCols := 0;
  grdStats.FixedRows := 1;
  grdStats.Options := grdStats.Options - [goEditing];
  grdStats.Cells[0, 0] := 'Métrica';
  grdStats.Cells[1, 0] := 'Valor';
  grdStats.Cells[0, 1] := 'Total requests';
  grdStats.Cells[0, 2] := 'Sucesso';
  grdStats.Cells[0, 3] := 'Falha';
  grdStats.Cells[0, 4] := 'Tempo (ms)';
  grdStats.Cells[0, 5] := 'RPS acumulado';
  grdStats.Cells[0, 6] := 'RPS instantâneo';
  grdStats.Cells[0, 7] := 'RPS mínimo';
  grdStats.Cells[0, 8] := 'RPS máximo';
  grdStats.Cells[0, 9] := 'RPS médio';
  grdStats.Cells[0, 10] := 'Lat. média (ms)';
  grdStats.Cells[0, 11] := 'Lat. min (ms)';
  grdStats.Cells[0, 12] := 'Lat. max (ms)';
end;

procedure TfrmStressMain.SetStatValue(const ARow: Integer; const AValue: string);
begin
  if (ARow >= 1) and (ARow < grdStats.RowCount) then
    grdStats.Cells[1, ARow] := AValue;
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
  cbAuthMode.Enabled := not ARunning;
  edtAuthUser.Enabled := not ARunning;
  edtAuthPass.Enabled := not ARunning;
  edtBearerToken.Enabled := not ARunning;
  edtJwtLoginPath.Enabled := not ARunning;
  if ARunning then
    lblRunning.Caption := 'Status: executando'
  else
    lblRunning.Caption := 'Status: parado';
end;

procedure TfrmStressMain.UpdateAuthControls;
var
  Mode: TAuthMode;
begin
  if cbAuthMode.ItemIndex < 0 then
    cbAuthMode.ItemIndex := 0;
  Mode := TAuthMode(cbAuthMode.ItemIndex);

  lblAuthUser.Visible := (Mode = amBasic) or (Mode = amJwtLogin);
  edtAuthUser.Visible := lblAuthUser.Visible;
  lblAuthPass.Visible := (Mode = amBasic) or (Mode = amJwtLogin);
  edtAuthPass.Visible := lblAuthPass.Visible;
  lblBearerToken.Visible := (Mode = amBearer);
  edtBearerToken.Visible := lblBearerToken.Visible;
  lblJwtLoginPath.Visible := (Mode = amJwtLogin);
  edtJwtLoginPath.Visible := lblJwtLoginPath.Visible;
end;

function TfrmStressMain.AuthModeToText(const AMode: TAuthMode): string;
begin
  case AMode of
    amNone: Result := 'none';
    amBasic: Result := 'basic';
    amBearer: Result := 'bearer';
    amJwtLogin: Result := 'jwt-login';
  else
    Result := 'unknown';
  end;
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
  if cbAuthMode.ItemIndex < 0 then
    cbAuthMode.ItemIndex := 0;
  AConfig.AuthMode := TAuthMode(cbAuthMode.ItemIndex);
  AConfig.AuthUser := Trim(edtAuthUser.Text);
  AConfig.AuthPass := Trim(edtAuthPass.Text);
  AConfig.BearerToken := Trim(edtBearerToken.Text);
  AConfig.JwtLoginPath := Trim(edtJwtLoginPath.Text);

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

  case AConfig.AuthMode of
    amBasic:
      begin
        if (AConfig.AuthUser = '') or (AConfig.AuthPass = '') then
        begin
          Log('Basic: informe usuário e senha.');
          Exit;
        end;
      end;
    amBearer:
      begin
        if AConfig.BearerToken = '' then
        begin
          Log('Bearer: informe o token.');
          Exit;
        end;
      end;
    amJwtLogin:
      begin
        if (AConfig.AuthUser = '') or (AConfig.AuthPass = '') then
        begin
          Log('JWT: informe usuário e senha para login.');
          Exit;
        end;
        if (AConfig.JwtLoginPath = '') or (AConfig.JwtLoginPath[1] <> '/') then
        begin
          Log('JWT: caminho de login inválido. Exemplo: /Login');
          Exit;
        end;
      end;
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
  AvgLatencyMs: Double;
  RpsAccum: Double;
  RpsInst: Double;
  RpsInstAvg: Double;
  DeltaReq: Int64;
  DeltaMs: Int64;
  NowTick: QWord;
  MinText: string;
  MaxText: string;
  MinLatencyText: string;
  MaxLatencyText: string;
begin
  if not Assigned(FRunner) then
  begin
    lblSummary.Caption := 'Resumo: -';
    Exit;
  end;

  S := FRunner.SnapshotStats;
  ElapsedMs := FRunner.GetElapsedMs;
  if S.TotalRequests > 0 then
    AvgLatencyMs := S.TotalLatencyMs / S.TotalRequests
  else
    AvgLatencyMs := 0;
  if ElapsedMs > 0 then
    RpsAccum := (S.TotalRequests * 1000.0) / ElapsedMs
  else
    RpsAccum := 0;

  NowTick := GetTickCount64;
  if FLastSampleTick = 0 then
  begin
    FLastSampleTick := NowTick;
    FLastSampleRequests := S.TotalRequests;
    RpsInst := 0;
  end
  else
  begin
    DeltaMs := Int64(NowTick - FLastSampleTick);
    DeltaReq := S.TotalRequests - FLastSampleRequests;
    if DeltaMs > 0 then
      RpsInst := (DeltaReq * 1000.0) / DeltaMs
    else
      RpsInst := 0;
    if RpsInst < 0 then
      RpsInst := 0;

    Inc(FRpsInstCount);
    FRpsInstSum := FRpsInstSum + RpsInst;
    if (FRpsInstCount = 1) or (RpsInst < FRpsInstMin) then
      FRpsInstMin := RpsInst;
    if (FRpsInstCount = 1) or (RpsInst > FRpsInstMax) then
      FRpsInstMax := RpsInst;

    FLastSampleTick := NowTick;
    FLastSampleRequests := S.TotalRequests;
  end;

  if FRpsInstCount > 0 then
    RpsInstAvg := FRpsInstSum / FRpsInstCount
  else
    RpsInstAvg := 0;

  if S.MinLatencyMs = High(Int64) then
  begin
    MinText := 'n/a';
    MaxText := 'n/a';
    MinLatencyText := 'n/a';
    MaxLatencyText := 'n/a';
  end
  else
  begin
    MinText := IntToStr(S.MinLatencyMs);
    MaxText := IntToStr(S.MaxLatencyMs);
    MinLatencyText := MinText;
    MaxLatencyText := MaxText;
  end;

  lblSummary.Caption := Format(
    'Resumo: total=%d sucesso=%d falha=%d tempo=%dms rps_acum=%s rps_inst=%s rps_min=%s rps_max=%s rps_med=%s lat_med=%sms lat_min/max=%s/%sms',
    [S.TotalRequests, S.SuccessCount, S.FailureCount, ElapsedMs,
     FormatFloat('0.00', RpsAccum), FormatFloat('0.00', RpsInst),
     FormatFloat('0.00', FRpsInstMin), FormatFloat('0.00', FRpsInstMax), FormatFloat('0.00', RpsInstAvg),
     FormatFloat('0.00', AvgLatencyMs), MinText, MaxText]);

  SetStatValue(1, IntToStr(S.TotalRequests));
  SetStatValue(2, IntToStr(S.SuccessCount));
  SetStatValue(3, IntToStr(S.FailureCount));
  SetStatValue(4, IntToStr(ElapsedMs));
  SetStatValue(5, FormatFloat('0.00', RpsAccum));
  SetStatValue(6, FormatFloat('0.00', RpsInst));
  SetStatValue(7, FormatFloat('0.00', FRpsInstMin));
  SetStatValue(8, FormatFloat('0.00', FRpsInstMax));
  SetStatValue(9, FormatFloat('0.00', RpsInstAvg));
  SetStatValue(10, FormatFloat('0.00', AvgLatencyMs));
  SetStatValue(11, MinLatencyText);
  SetStatValue(12, MaxLatencyText);

  if (FLastMetricsLogTick = 0) or ((NowTick - FLastMetricsLogTick) >= 2000) then
  begin
    Log(Format('snapshot total=%d ok=%d fail=%d rps=%s inst=%s lat=%sms',
      [S.TotalRequests, S.SuccessCount, S.FailureCount,
       FormatFloat('0.00', RpsAccum), FormatFloat('0.00', RpsInst), FormatFloat('0.00', AvgLatencyMs)]));
    FLastMetricsLogTick := NowTick;
  end;
end;

end.
