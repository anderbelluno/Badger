unit BadgerIOCP;

{ Windows IOCP HTTP I/O. Isolated from TBadger.Execute (Synapse).
  TBadger.Start adopts this engine when UseIOCP (default True on Windows). }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes, SyncObjs, BadgerWinSock2, BadgerHttpParser,
  BadgerTypes, BadgerRouteManager, BadgerLogger, BadgerWebSocket, BadgerHttpDispatch;

const
  IOCP_KEY_WORK = 2;
  IOCP_KEY_SHUTDOWN = 1;

type
  TIocpOp = (ioAccept, ioRecv, ioSend);

  PIocpCtx = ^TIocpCtx;

  PIocpOpHdr = ^TIocpOpHdr;
  TIocpOpHdr = record
    Overlapped: TOverlapped; { MUST remain the first field }
    Ctx: PIocpCtx;
    Kind: TIocpOp;
  end;

  TIocpCtx = record
    RecvOp: TIocpOpHdr;
    SendOp: TIocpOpHdr;
    Socket: TBadgerSocket;
    SendLen: Integer;
    SendPos: Integer;
    SendBuf: Pointer;
    SendAlloc: Integer;
    RecvWsa: TBadgerWsaBuf;
    SendWsa: TBadgerWsaBuf;
    Buf: array[0..8191] of AnsiChar;
    AcceptBuf: array[0..127] of Byte;
    SkipCompletion: Boolean;
    Parser: TBadgerHttpParser;
    Conn: TBadgerConn;
    LastActivity: DWORD;
    CloseAfterSend: Boolean;
    Counted: Boolean;
    IsWebSocket: Boolean;
    WsParser: TBadgerWsParser;
    WsClient: TClientSocketInfo;
    WsSendBusy: Boolean;
    WsRecvArmed: Boolean;
    WsQueue: TBadgerWsBytes;
    RouteParams: TStringList;
    RespHeaders: TStringList;
    { Contagem de referencias. 1 = registro em FLiveCtx; +1 por operacao postada.
      O contexto so e liberado quando chega a zero, entao o kernel nunca escreve em
      OVERLAPPED de memoria ja devolvida. }
    Refs: Integer;
    Closing: Integer;
    { True enquanto a rota executa. RecvOp.Kind continua ioRecv nesse intervalo, e
      sem este flag o watchdog tomava a conexao por ociosa e a fechava no meio da
      execucao de qualquer rota mais lenta que Timeout. }
    InDispatch: Boolean;
    { Prazo de headers (slowloris): 0 = aguardando o 1o byte do pedido, 1 = lendo
      headers desde ReqStart, 2 = corpo/rota. So a fase 1 tem prazo; o watchdog le
      estes dois campos e nunca o Parser (que BeginWs libera em outra thread). }
    HdrPhase: Byte;
    ReqStart: DWORD;
  end;

  TIocpKey = {$IFDEF FPC}PtrUInt{$ELSE}{$IFDEF WIN64}NativeUInt{$ELSE}DWORD{$ENDIF}{$ENDIF};

  EBadgerIOCP = class(Exception);

  TBadgerIOCPLog = procedure(const Msg: string) of object;

  TBadgerIOCP = class;

  TIocpWorker = class(TThread)
  private
    FOwner: TBadgerIOCP;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TBadgerIOCP);
  end;

  TBadgerIOCP = class
  private
    FIocp: THandle;
    FListen: TBadgerSocket;
    FPort: Integer;
    FWorkerCount: Integer;
    FPendingAccepts: Integer;
    FPendingAcceptCount: Integer;
    FRunning: Boolean;
    FStarted: Boolean;
    FAcceptEx: TAcceptExProc;
    FLock: TCriticalSection;
    FWorkers: array of TIocpWorker;
    FLiveCtx: TList;
    FAllocCount: Integer;
    FFreeCount: Integer;
    FOnLog: TBadgerIOCPLog;
    FRouteManager: TRouteManager;
    FMiddlewares: TList;
    FAfterMiddlewares: TList;
    FMiddlewareLock: TCriticalSection;
    FOnRequest: TOnRequest;
    FOnResponse: TOnResponse;
    FOnWebSocketMessage: TWebSocketMessageEvent;
    FOnWsAttach: TWsClientProc;
    FOnWsDetach: TWsClientProc;
    FEnableEventInfo: Boolean;
    FCorsEnabled: Boolean;
    FCorsAllowedOrigins: TStringList;
    FCorsAllowedMethods: TStringList;
    FCorsAllowedHeaders: TStringList;
    FCorsExposeHeaders: TStringList;
    FCorsAllowCredentials: Boolean;
    FCorsMaxAge: Integer;
    FTimeout: Integer;
    FMaxConcurrentConnections: Integer;
    FParallelProcessing: Boolean;
    FActiveConnections: Integer;
    FSerialLock: TCriticalSection;
    FWatchdog: TThread;
    FOwnsPipeline: Boolean;
    FTrustProxyHeaders: Boolean;
    FTrustedProxies: TStringList;
    FWsIdleTimeout: Integer;
    FHeaderTimeout: Integer;
    procedure Log(const Msg: string);
    function AllocCtx(Op: TIocpOp): PIocpCtx;
    procedure ReleaseCtx(Ctx: PIocpCtx);
    procedure FreeCtx(Ctx: PIocpCtx);
    procedure AddRef(Ctx: PIocpCtx);
    procedure ReleaseRef(Ctx: PIocpCtx);
    function TryAddRef(Ctx: PIocpCtx): Boolean;
    procedure CloseAllCtx;
    procedure EnableSkipCompletion(Ctx: PIocpCtx);
    function PostAccept: Boolean;
    procedure EnsurePendingAccepts;
    procedure DecPendingAccept;
    function CompleteAccept(Ctx: PIocpCtx; Success: Boolean): Boolean;
    procedure PostRecv(Ctx: PIocpCtx);
    procedure PostSend(Ctx: PIocpCtx);
    procedure PrepareAndSend(Ctx: PIocpCtx; const Resp: TBadgerWsBytes);
    procedure FillRemoteIP(Ctx: PIocpCtx);
    procedure HandleAccept(Ctx: PIocpCtx);
    procedure HandleRecv(Ctx: PIocpCtx; Bytes: DWORD);
    procedure HandleSend(Ctx: PIocpCtx; Bytes: DWORD);
    procedure HandleWsRecv(Ctx: PIocpCtx; Bytes: DWORD);
    function ProcessWsFrames(Ctx: PIocpCtx): Boolean;
    procedure BeginWs(Ctx: PIocpCtx; const URI, WSKey: string);
    procedure QueueWsFrame(Ctx: PIocpCtx; const Frame: TBadgerWsBytes);
    procedure FinishRequest(Ctx: PIocpCtx);
    procedure InitCorsDefaults;
    procedure Touch(Ctx: PIocpCtx);
    procedure RecycleKeepAlive(Ctx: PIocpCtx);
    procedure ScanIdleConnections;
    procedure WorkerLoop;
    procedure CloseListenSocket;
    function LiveCtxCount: Integer;
    procedure WaitLiveCtxIdle(TimeoutMs: DWORD);
    procedure DrainLiveCtx;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AdoptPipeline(ARouteManager: TRouteManager;
      AMiddlewares, AAfterMiddlewares: TList; AMiddlewareLock: TCriticalSection;
      ACorsOrigins, ACorsMethods, ACorsHeaders, ACorsExpose: TStringList);
    procedure Start;
    procedure Stop;
    procedure AddMiddleware(Middleware: TMiddlewareProc);
    procedure AddAfterMiddleware(Middleware: TAfterMiddlewareProc);
    property Port: Integer read FPort write FPort;
    property WorkerCount: Integer read FWorkerCount write FWorkerCount;
    property PendingAccepts: Integer read FPendingAccepts write FPendingAccepts;
    property Started: Boolean read FStarted;
    property OnLog: TBadgerIOCPLog read FOnLog write FOnLog;
    property RouteManager: TRouteManager read FRouteManager;
    property OnRequest: TOnRequest read FOnRequest write FOnRequest;
    property OnResponse: TOnResponse read FOnResponse write FOnResponse;
    property OnWebSocketMessage: TWebSocketMessageEvent read FOnWebSocketMessage write FOnWebSocketMessage;
    property OnWsAttach: TWsClientProc read FOnWsAttach write FOnWsAttach;
    property OnWsDetach: TWsClientProc read FOnWsDetach write FOnWsDetach;
    procedure SendWsText(ClientInfo: TClientSocketInfo; const AMessage: string);
    property EnableEventInfo: Boolean read FEnableEventInfo write FEnableEventInfo;
    property CorsEnabled: Boolean read FCorsEnabled write FCorsEnabled;
    property CorsAllowedOrigins: TStringList read FCorsAllowedOrigins;
    property CorsAllowedMethods: TStringList read FCorsAllowedMethods;
    property CorsAllowedHeaders: TStringList read FCorsAllowedHeaders;
    property CorsExposeHeaders: TStringList read FCorsExposeHeaders;
    property CorsAllowCredentials: Boolean read FCorsAllowCredentials write FCorsAllowCredentials;
    property CorsMaxAge: Integer read FCorsMaxAge write FCorsMaxAge;
    property TrustProxyHeaders: Boolean read FTrustProxyHeaders write FTrustProxyHeaders;
    property TrustedProxies: TStringList read FTrustedProxies write FTrustedProxies;
    property Timeout: Integer read FTimeout write FTimeout;
    { Ociosidade maxima de WebSocket, em ms. 0 = nao expira (default). Separado de
      Timeout porque navegadores nao enviam ping: com o timeout de HTTP (5 s) todo
      chat parado era derrubado. }
    property WsIdleTimeout: Integer read FWsIdleTimeout write FWsIdleTimeout;
    { Prazo em ms para os headers chegarem, contado do 1o byte do pedido. 0 desliga.
      Timeout sozinho nao basta: cada byte renova LastActivity e um cliente que
      pinga 1 byte a cada 4 s segurava a vaga para sempre (slowloris). Conexao
      keep-alive ociosa nao e afetada (conta so apos o 1o byte). }
    property HeaderTimeout: Integer read FHeaderTimeout write FHeaderTimeout;
    property MaxConcurrentConnections: Integer read FMaxConcurrentConnections write FMaxConcurrentConnections;
    property ParallelProcessing: Boolean read FParallelProcessing write FParallelProcessing;
    property ActiveConnections: Integer read FActiveConnections;
    function CtxStats: string;
  end;

implementation

const
  HTTP_100_CONTINUE: AnsiString = 'HTTP/1.1 100 Continue'#13#10#13#10;
  IOCP_SEND_CHUNK = 65536;
  { Teto da fila de envio WebSocket por conexao. }
  WS_MAX_QUEUE = 4 * 1024 * 1024;
  FILE_SKIP_COMPLETION_PORT_ON_SUCCESS = 1;

var
  GSetFileCompletionNotificationModes: function(FileHandle: THandle; Flags: Byte): BOOL; stdcall;
  { Vista+. Cancela I/O pendente emitido por QUALQUER thread (CancelIo so a da
    chamadora). Carregado dinamicamente: o Windows.pas do D7 nao declara. }
  GCancelIoEx: function(hFile: THandle; lpOverlapped: POverlapped): BOOL; stdcall;

const
  IOCP_ERROR_NOT_FOUND = 1168; { CancelIoEx: nada pendente (ausente no Windows.pas do D7) }

const
  { Profundidade maxima de tratamento inline de conclusoes sincronas. Com
    FILE_SKIP_COMPLETION_PORT_ON_SUCCESS, PostRecv -> HandleRecv -> ... -> PostRecv
    recursava sem limite enquanto houvesse dado no buffer do socket: um cliente com
    requisicoes em pipeline estourava a pilha do worker e derrubava o processo. }
  IOCP_MAX_INLINE_DEPTH = 4;

threadvar
  GIocpIsWorker: Boolean;
  GIocpDepth: Integer;

procedure LoadSkipCompletionApi;
begin
  if Assigned(GSetFileCompletionNotificationModes) then
    Exit;
  @GSetFileCompletionNotificationModes := GetProcAddress(GetModuleHandle('kernel32.dll'),
    'SetFileCompletionNotificationModes');
  @GCancelIoEx := GetProcAddress(GetModuleHandle('kernel32.dll'), 'CancelIoEx');
end;

function IocpOpName(Op: TIocpOp): string;
begin
  case Op of
    ioAccept: Result := 'accept';
    ioRecv: Result := 'recv';
    ioSend: Result := 'send';
  else
    Result := '?';
  end;
end;

{ TIocpWorker }

constructor TIocpWorker.Create(AOwner: TBadgerIOCP);
begin
  FOwner := AOwner;
  inherited Create(True);
  FreeOnTerminate := False;
end;

procedure TIocpWorker.Execute;
begin
  FOwner.WorkerLoop;
end;

type
  TIocpWatchdog = class(TThread)
  private
    FOwner: TBadgerIOCP;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TBadgerIOCP);
  end;

constructor TIocpWatchdog.Create(AOwner: TBadgerIOCP);
begin
  FOwner := AOwner;
  inherited Create(True);
  FreeOnTerminate := False;
end;

procedure TIocpWatchdog.Execute;
begin
  while not Terminated do
  begin
    Sleep(250);
    if Terminated then
      Break;
    FOwner.ScanIdleConnections;
    { Rearme periodico: os AcceptEx so eram repostos ao completar um accept ou
      fechar uma conexao contada. Se todos falhassem de uma vez (WSAENOBUFS, falta
      de handles) sem conexao ativa, o servidor parava de aceitar para sempre, em
      silencio. Com a cota cheia isto e so um lock e sai. }
    FOwner.EnsurePendingAccepts;
  end;
end;

procedure StartIocpThread(Th: TThread);
begin
  { Same as TBadger.Start: never Start/Resume inside the constructor.
    D12 AfterConstruction + Start-in-Create = EThread (ResumeThread twice).
    D7 has Resume, not Start. }
  {$IFDEF FPC}
  Th.Start;
  {$ELSE}
    {$IF CompilerVersion >= 21}
  Th.Start;
    {$ELSE}
  Th.Resume;
    {$IFEND}
  {$ENDIF}
end;

{ TBadgerIOCP }

constructor TBadgerIOCP.Create;
var
  Info: TSystemInfo;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FLiveCtx := TList.Create;
  FListen := INVALID_SOCKET;
  FIocp := 0;
  FPort := 8081;
  FPendingAccepts := 32;
  FPendingAcceptCount := 0;
  GetSystemInfo(Info);
  FWorkerCount := Integer(Info.dwNumberOfProcessors);
  if FWorkerCount < 2 then
    FWorkerCount := 2;
  if FWorkerCount > 16 then
    FWorkerCount := 16;
  FRouteManager := TRouteManager.Create;
  FMiddlewares := TList.Create;
  FAfterMiddlewares := TList.Create;
  FMiddlewareLock := TCriticalSection.Create;
  FEnableEventInfo := True;
  FTimeout := 5000;
  FHeaderTimeout := 30000;
  FMaxConcurrentConnections := 100;
  FParallelProcessing := True;
  FActiveConnections := 0;
  FSerialLock := TCriticalSection.Create;
  FWatchdog := nil;
  FOwnsPipeline := True;
  InitCorsDefaults;
end;

procedure TBadgerIOCP.AdoptPipeline(ARouteManager: TRouteManager;
  AMiddlewares, AAfterMiddlewares: TList; AMiddlewareLock: TCriticalSection;
  ACorsOrigins, ACorsMethods, ACorsHeaders, ACorsExpose: TStringList);
var
  I: Integer;
begin
  if FOwnsPipeline then
  begin
    if Assigned(FMiddlewares) then
    begin
      for I := 0 to FMiddlewares.Count - 1 do
        TObject(FMiddlewares[I]).Free;
      FreeAndNil(FMiddlewares);
    end;
    if Assigned(FAfterMiddlewares) then
    begin
      for I := 0 to FAfterMiddlewares.Count - 1 do
        TObject(FAfterMiddlewares[I]).Free;
      FreeAndNil(FAfterMiddlewares);
    end;
    FreeAndNil(FMiddlewareLock);
    FreeAndNil(FRouteManager);
    FreeAndNil(FCorsAllowedOrigins);
    FreeAndNil(FCorsAllowedMethods);
    FreeAndNil(FCorsAllowedHeaders);
    FreeAndNil(FCorsExposeHeaders);
  end;
  FRouteManager := ARouteManager;
  FMiddlewares := AMiddlewares;
  FAfterMiddlewares := AAfterMiddlewares;
  FMiddlewareLock := AMiddlewareLock;
  FCorsAllowedOrigins := ACorsOrigins;
  FCorsAllowedMethods := ACorsMethods;
  FCorsAllowedHeaders := ACorsHeaders;
  FCorsExposeHeaders := ACorsExpose;
  FOwnsPipeline := False;
end;

destructor TBadgerIOCP.Destroy;
var
  I: Integer;
begin
  Stop;
  if FOwnsPipeline then
  begin
    if Assigned(FMiddlewares) then
    begin
      for I := 0 to FMiddlewares.Count - 1 do
        TObject(FMiddlewares[I]).Free;
      FreeAndNil(FMiddlewares);
    end;
    if Assigned(FAfterMiddlewares) then
    begin
      for I := 0 to FAfterMiddlewares.Count - 1 do
        TObject(FAfterMiddlewares[I]).Free;
      FreeAndNil(FAfterMiddlewares);
    end;
    FreeAndNil(FMiddlewareLock);
    FreeAndNil(FRouteManager);
    FreeAndNil(FCorsAllowedOrigins);
    FreeAndNil(FCorsAllowedMethods);
    FreeAndNil(FCorsAllowedHeaders);
    FreeAndNil(FCorsExposeHeaders);
  end
  else
  begin
    FMiddlewares := nil;
    FAfterMiddlewares := nil;
    FMiddlewareLock := nil;
    FRouteManager := nil;
    FCorsAllowedOrigins := nil;
    FCorsAllowedMethods := nil;
    FCorsAllowedHeaders := nil;
    FCorsExposeHeaders := nil;
  end;
  FreeAndNil(FSerialLock);
  FLiveCtx.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TBadgerIOCP.InitCorsDefaults;
begin
  FCorsEnabled := False;
  FCorsAllowedOrigins := TStringList.Create;
  FCorsAllowedMethods := TStringList.Create;
  FCorsAllowedHeaders := TStringList.Create;
  FCorsExposeHeaders := TStringList.Create;
  FCorsAllowedOrigins.CaseSensitive := False;
  FCorsAllowedMethods.CaseSensitive := False;
  FCorsAllowedHeaders.CaseSensitive := False;
  FCorsExposeHeaders.CaseSensitive := False;
  FCorsAllowedMethods.Add('GET');
  FCorsAllowedMethods.Add('POST');
  FCorsAllowedMethods.Add('PUT');
  FCorsAllowedMethods.Add('DELETE');
  FCorsAllowedMethods.Add('PATCH');
  FCorsAllowedMethods.Add('OPTIONS');
  FCorsAllowedHeaders.Add('Content-Type');
  FCorsAllowedHeaders.Add('Authorization');
  FCorsAllowedHeaders.Add('X-Requested-With');
  FCorsAllowCredentials := False;
  FCorsMaxAge := 600;
end;

procedure TBadgerIOCP.AddMiddleware(Middleware: TMiddlewareProc);
begin
  FMiddlewareLock.Acquire;
  try
    FMiddlewares.Add(TMiddlewareWrapper.Create(Middleware));
  finally
    FMiddlewareLock.Release;
  end;
end;

procedure TBadgerIOCP.AddAfterMiddleware(Middleware: TAfterMiddlewareProc);
begin
  FMiddlewareLock.Acquire;
  try
    FAfterMiddlewares.Add(TAfterMiddlewareWrapper.Create(Middleware));
  finally
    FMiddlewareLock.Release;
  end;
end;

procedure TBadgerIOCP.Touch(Ctx: PIocpCtx);
begin
  if Ctx <> nil then
    Ctx^.LastActivity := GetTickCount;
end;

procedure TBadgerIOCP.DecPendingAccept;
begin
  FLock.Acquire;
  try
    if FPendingAcceptCount > 0 then
      Dec(FPendingAcceptCount);
  finally
    FLock.Release;
  end;
end;

function TBadgerIOCP.CompleteAccept(Ctx: PIocpCtx; Success: Boolean): Boolean;
begin
  Result := False;
  FLock.Acquire;
  try
    if FPendingAcceptCount > 0 then
      Dec(FPendingAcceptCount);
    if Success and FRunning and
       ((FMaxConcurrentConnections <= 0) or (FActiveConnections < FMaxConcurrentConnections)) then
    begin
      Inc(FActiveConnections);
      Ctx^.Counted := True;
      Result := True;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TBadgerIOCP.EnsurePendingAccepts;
begin
  while PostAccept do
    ;
end;

procedure TBadgerIOCP.RecycleKeepAlive(Ctx: PIocpCtx);
var
  Left: AnsiString;
begin
  if Ctx^.SendBuf <> nil then
  begin
    FreeMem(Ctx^.SendBuf);
    Ctx^.SendBuf := nil;
    Ctx^.SendAlloc := 0;
  end;
  Ctx^.SendLen := 0;
  Ctx^.SendPos := 0;
  Ctx^.CloseAfterSend := False;
  Touch(Ctx);
  if Assigned(Ctx^.Parser) then
  begin
    Left := Ctx^.Parser.Leftover;
    Ctx^.Parser.Reset;
    Ctx^.HdrPhase := 0;
    if Left <> '' then
    begin
      { Pipelining: o proximo pedido ja comecou a chegar. }
      Ctx^.HdrPhase := 1;
      Ctx^.ReqStart := GetTickCount;
      if Ctx^.Parser.Feed(@Left[1], Length(Left)) then
      begin
        Ctx^.HdrPhase := 2;
        FinishRequest(Ctx);
        Exit;
      end;
      if Ctx^.Parser.State <> hpsHeaders then
        Ctx^.HdrPhase := 2;
    end;
  end;
  PostRecv(Ctx);
end;

procedure TBadgerIOCP.ScanIdleConnections;
var
  I: Integer;
  NowTick: DWORD;
  Ctx: PIocpCtx;
  Idle: TList;
  Limit: Integer;
begin
  if ((FTimeout <= 0) and (FWsIdleTimeout <= 0) and (FHeaderTimeout <= 0)) or
     not FRunning then
    Exit;
  NowTick := GetTickCount;
  Idle := TList.Create;
  try
    FLock.Acquire;
    try
      for I := 0 to FLiveCtx.Count - 1 do
      begin
        Ctx := PIocpCtx(FLiveCtx[I]);
        { HTTP usa Timeout; WebSocket usa WsIdleTimeout (0 = nunca expira). }
        if Ctx^.IsWebSocket then
          Limit := FWsIdleTimeout
        else
          Limit := FTimeout;
        if (Ctx^.RecvOp.Kind = ioRecv) and Ctx^.Counted and (not Ctx^.InDispatch) and
           (((Limit > 0) and ((NowTick - Ctx^.LastActivity) >= DWORD(Limit))) or
            ((FHeaderTimeout > 0) and (not Ctx^.IsWebSocket) and (Ctx^.HdrPhase = 1) and
             ((NowTick - Ctx^.ReqStart) >= DWORD(FHeaderTimeout)))) then
        begin
          InterlockedIncrement(Ctx^.Refs);
          Idle.Add(Ctx);
        end;
      end;
    finally
      FLock.Release;
    end;
    for I := 0 to Idle.Count - 1 do
    begin
      Ctx := PIocpCtx(Idle[I]);
      { CancelIo cancelava apenas I/O emitido pela thread chamadora, e o watchdog nao
        emitiu estes WSARecv: era no-op e o timeout de idle nunca corria. ReleaseCtx
        faz shutdown + CancelIoEx, que cancela o I/O pendente de qualquer thread. }
      ReleaseCtx(Ctx);
      ReleaseRef(Ctx);
    end;
  finally
    Idle.Free;
  end;
end;

procedure TBadgerIOCP.Log(const Msg: string);
begin
  Logger.Info(Msg);
  if Assigned(FOnLog) then
    FOnLog(Msg);
end;

function TBadgerIOCP.CtxStats: string;
var
  I, NAccept, NRecv, NSend: Integer;
  Ctx: PIocpCtx;
begin
  NAccept := 0;
  NRecv := 0;
  NSend := 0;
  FLock.Acquire;
  try
    for I := 0 to FLiveCtx.Count - 1 do
    begin
      Ctx := PIocpCtx(FLiveCtx[I]);
      if Ctx^.RecvOp.Kind = ioAccept then
        Inc(NAccept)
      else if Ctx^.SendLen > Ctx^.SendPos then
        Inc(NSend)
      else
        Inc(NRecv);
    end;
    Result := Format('ctx sizeof=%d alloc=%d free=%d live=%d active=%d pendingAccept=%d (accept=%d recv=%d send=%d)',
      [SizeOf(TIocpCtx), FAllocCount, FFreeCount, FLiveCtx.Count, FActiveConnections,
       FPendingAcceptCount, NAccept, NRecv, NSend]);
  finally
    FLock.Release;
  end;
end;

function TBadgerIOCP.AllocCtx(Op: TIocpOp): PIocpCtx;
begin
  GetMem(Result, SizeOf(TIocpCtx));
  FillChar(Result^, SizeOf(TIocpCtx), 0);
  Result^.RecvOp.Ctx := Result;
  Result^.SendOp.Ctx := Result;
  Result^.RecvOp.Kind := Op;
  Result^.Socket := INVALID_SOCKET;
  Result^.Parser := TBadgerHttpParser.Create;
  Result^.Conn := TBadgerConn.Create;
  Result^.RouteParams := TStringList.Create;
  Result^.RespHeaders := TStringList.Create;
  Result^.Refs := 1;
  Result^.Closing := 0;
  FLock.Acquire;
  try
    FLiveCtx.Add(Result);
    Inc(FAllocCount);
  finally
    FLock.Release;
  end;
end;

procedure TBadgerIOCP.AddRef(Ctx: PIocpCtx);
begin
  InterlockedIncrement(Ctx^.Refs);
end;

procedure TBadgerIOCP.ReleaseRef(Ctx: PIocpCtx);
begin
  if Ctx = nil then
    Exit;
  if InterlockedDecrement(Ctx^.Refs) = 0 then
    FreeCtx(Ctx);
end;

{ Pega referencia sob FLock, so se o contexto ainda esta registrado. Usado por quem
  chega de fora do worker dono (SendWsText, watchdog): sem isso o ponteiro podia ser
  liberado entre a checagem e o uso. }
function TBadgerIOCP.TryAddRef(Ctx: PIocpCtx): Boolean;
begin
  Result := False;
  if Ctx = nil then
    Exit;
  FLock.Acquire;
  try
    if FLiveCtx.IndexOf(Ctx) >= 0 then
    begin
      InterlockedIncrement(Ctx^.Refs);
      Result := True;
    end;
  finally
    FLock.Release;
  end;
end;

{ Encerra a conexao: tira do registro, fecha o socket (o que cancela o I/O pendente
  e gera as completions de falha) e devolve a referencia do registro. A memoria so
  sai quando a ultima operacao pendente completar. Idempotente. }
procedure TBadgerIOCP.ReleaseCtx(Ctx: PIocpCtx);
var
  Idx: Integer;
  WasCounted: Boolean;
  Info: TClientSocketInfo;
begin
  if Ctx = nil then
    Exit;
  if InterlockedExchange(Ctx^.Closing, 1) <> 0 then
    Exit;

  WasCounted := Ctx^.Counted;
  Ctx^.Counted := False;
  FLock.Acquire;
  try
    if WasCounted and (FActiveConnections > 0) then
      Dec(FActiveConnections);
    Idx := FLiveCtx.IndexOf(Ctx);
    if Idx >= 0 then
      FLiveCtx.Delete(Idx);
    Inc(FFreeCount);
  finally
    FLock.Release;
  end;

  { WsClient NAO e zerado aqui: SendWsText/QueueWsFrame em outra thread (com ref
    no Ctx) leem Ctx^.WsClient tres vezes (checa, Acquire, Release) e zerar no meio
    dava AV em nil.IOLock, lock preso ou uso apos Free. O motor guarda uma ref do
    Info ate FreeCtx, quando ninguem mais segura o Ctx. }
  Info := Ctx^.WsClient;
  if Assigned(Info) then
  begin
    Info.Ctx := nil;
    if Assigned(FOnWsDetach) then
      FOnWsDetach(Info);
  end;

  { ReleaseCtx corre tambem fora do worker dono (watchdog, Stop, estouro da fila WS
    na thread da aplicacao). closesocket aqui liberava o valor do handle, que o
    Windows reusa na hora: um worker que ja tinha lido Ctx^.Socket podia postar
    WSASend/WSARecv no socket de OUTRA conexao. Agora: shutdown (novos posts falham
    com WSAESHUTDOWN) + CancelIoEx (aborta os pendentes); o handle so e fechado em
    FreeCtx, quando ninguem mais segura o Ctx. Sem CancelIoEx (XP) ou se ele falhar
    (LSP sem handle real), fecha como antes. }
  if (Ctx^.Socket <> 0) and (Ctx^.Socket <> INVALID_SOCKET) then
  begin
    shutdown(Ctx^.Socket, SD_BOTH);
    if not (Assigned(GCancelIoEx) and
            (GCancelIoEx(THandle(Ctx^.Socket), nil) or (GetLastError = IOCP_ERROR_NOT_FOUND))) then
    begin
      closesocket(Ctx^.Socket);
      Ctx^.Socket := INVALID_SOCKET;
    end;
  end;

  ReleaseRef(Ctx);
  if WasCounted then
    EnsurePendingAccepts;
end;

{ Teardown real. Corre apenas quando Refs chega a zero. }
procedure TBadgerIOCP.FreeCtx(Ctx: PIocpCtx);
begin
  if Ctx = nil then
    Exit;
  if Assigned(Ctx^.WsParser) then
  begin
    Ctx^.WsParser.Free;
    Ctx^.WsParser := nil;
  end;
  Ctx^.WsQueue := '';
  if Assigned(Ctx^.WsClient) then
  begin
    Ctx^.WsClient.Release; { ref do motor, tomada em BeginWs }
    Ctx^.WsClient := nil;
  end;
  if Assigned(Ctx^.Parser) then
  begin
    Ctx^.Parser.Free;
    Ctx^.Parser := nil;
  end;
  if Assigned(Ctx^.RouteParams) then
  begin
    Ctx^.RouteParams.Free;
    Ctx^.RouteParams := nil;
  end;
  if Assigned(Ctx^.RespHeaders) then
  begin
    Ctx^.RespHeaders.Free;
    Ctx^.RespHeaders := nil;
  end;
  if Assigned(Ctx^.Conn) then
  begin
    Ctx^.Conn.Free;
    Ctx^.Conn := nil;
  end;
  if Ctx^.SendBuf <> nil then
  begin
    FreeMem(Ctx^.SendBuf);
    Ctx^.SendBuf := nil;
    Ctx^.SendAlloc := 0;
  end;
  if (Ctx^.Socket <> 0) and (Ctx^.Socket <> INVALID_SOCKET) then
    closesocket(Ctx^.Socket);
  Ctx^.Socket := INVALID_SOCKET;
  FreeMem(Ctx);
end;

{ Fecha todas as conexoes registradas, com referencia garantida. }
procedure TBadgerIOCP.CloseAllCtx;
var
  Tmp: TList;
  I: Integer;
  Ctx: PIocpCtx;
begin
  Tmp := TList.Create;
  try
    FLock.Acquire;
    try
      for I := 0 to FLiveCtx.Count - 1 do
      begin
        Ctx := PIocpCtx(FLiveCtx[I]);
        InterlockedIncrement(Ctx^.Refs);
        Tmp.Add(Ctx);
      end;
    finally
      FLock.Release;
    end;
    for I := 0 to Tmp.Count - 1 do
    begin
      ReleaseCtx(PIocpCtx(Tmp[I]));
      ReleaseRef(PIocpCtx(Tmp[I]));
    end;
  finally
    Tmp.Free;
  end;
end;

procedure TBadgerIOCP.EnableSkipCompletion(Ctx: PIocpCtx);
begin
  Ctx^.SkipCompletion := False;
  LoadSkipCompletionApi;
  if Assigned(GSetFileCompletionNotificationModes) then
    Ctx^.SkipCompletion := GSetFileCompletionNotificationModes(THandle(Ctx^.Socket),
      FILE_SKIP_COMPLETION_PORT_ON_SUCCESS);
end;

procedure TBadgerIOCP.CloseListenSocket;
var
  S: TBadgerSocket;
begin
  FLock.Acquire;
  try
    S := FListen;
    FListen := INVALID_SOCKET;
  finally
    FLock.Release;
  end;
  if (S <> 0) and (S <> INVALID_SOCKET) then
    closesocket(S);
end;

function TBadgerIOCP.LiveCtxCount: Integer;
begin
  FLock.Acquire;
  try
    Result := FLiveCtx.Count;
  finally
    FLock.Release;
  end;
end;

procedure TBadgerIOCP.WaitLiveCtxIdle(TimeoutMs: DWORD);
var
  StartTick: DWORD;
begin
  StartTick := GetTickCount;
  while LiveCtxCount > 0 do
  begin
    if (GetTickCount - StartTick) >= TimeoutMs then
      Break;
    Sleep(10);
  end;
end;

procedure TBadgerIOCP.DrainLiveCtx;
begin
  { Com refcount o fechamento e o free ficaram separados: CloseAllCtx tira do
    registro e devolve a referencia, e a memoria sai quando a ultima operacao
    pendente completar. Nao ha mais FreeMem a forca aqui. }
  CloseAllCtx;
  WaitLiveCtxIdle(1000);
end;

function TBadgerIOCP.PostAccept: Boolean;
var
  Ctx: PIocpCtx;
  Bytes: DWORD;
  Listen: TBadgerSocket;
  AcceptEx: TAcceptExProc;
  One: Integer;
begin
  Result := False;
  Listen := INVALID_SOCKET;
  AcceptEx := nil;
  FLock.Acquire;
  try
    if not FRunning then
      Exit;
    if FPendingAcceptCount >= FPendingAccepts then
      Exit;
    if (FMaxConcurrentConnections > 0) and
       (FActiveConnections + FPendingAcceptCount >= FMaxConcurrentConnections) then
      Exit;
    Listen := FListen;
    AcceptEx := FAcceptEx;
    Inc(FPendingAcceptCount);
  finally
    FLock.Release;
  end;

  if (Listen = INVALID_SOCKET) or not Assigned(AcceptEx) then
  begin
    DecPendingAccept;
    Exit;
  end;

  Ctx := AllocCtx(ioAccept);
  Ctx^.Socket := BadgerCreateOverlappedSocket;
  if Ctx^.Socket = INVALID_SOCKET then
  begin
    DecPendingAccept;
    ReleaseCtx(Ctx);
    Exit;
  end;

  One := 1;
  setsockopt(Ctx^.Socket, IPPROTO_TCP, TCP_NODELAY, @One, SizeOf(One));

  Bytes := 0;
  AddRef(Ctx);
  if not AcceptEx(Listen, Ctx^.Socket, @Ctx^.AcceptBuf[0], 0,
    SizeOf(TBadgerSockAddrIn) + 16, SizeOf(TBadgerSockAddrIn) + 16,
    Bytes, POverlapped(@Ctx^.RecvOp)) then
  begin
    if WSAGetLastError <> WSA_IO_PENDING then
    begin
      DecPendingAccept;
      ReleaseCtx(Ctx);
      ReleaseRef(Ctx);
      Exit;
    end;
  end;
  Result := True;
end;

procedure TBadgerIOCP.PostRecv(Ctx: PIocpCtx);
var
  Flags, Recvd: DWORD;
  N: Integer;
begin
  Ctx^.RecvOp.Kind := ioRecv;
  Touch(Ctx);
  FillChar(Ctx^.RecvOp.Overlapped, SizeOf(TOverlapped), 0);
  Ctx^.RecvWsa.len := SizeOf(Ctx^.Buf);
  Ctx^.RecvWsa.buf := @Ctx^.Buf[0];
  Flags := 0;
  Recvd := 0;
  { Referencia pela operacao: o kernel vai escrever neste OVERLAPPED. }
  AddRef(Ctx);
  N := WSARecv(Ctx^.Socket, @Ctx^.RecvWsa, 1, Recvd, Flags, POverlapped(@Ctx^.RecvOp), nil);
  if N = 0 then
  begin
    if Ctx^.SkipCompletion then
    begin
      { Completou na hora e nao havera notificacao. Inline so em worker e com pilha
        rasa; senao a conclusao vai para a porta e o WorkerLoop a trata (e devolve a
        referencia) pelo caminho normal. }
      if GIocpIsWorker and (GIocpDepth < IOCP_MAX_INLINE_DEPTH) then
      begin
        Inc(GIocpDepth);
        try
          HandleRecv(Ctx, Recvd);
        finally
          Dec(GIocpDepth);
          ReleaseRef(Ctx);
        end;
      end
      else if not PostQueuedCompletionStatus(FIocp, Recvd, IOCP_KEY_WORK,
        POverlapped(@Ctx^.RecvOp)) then
      begin
        try
          HandleRecv(Ctx, Recvd);
        finally
          ReleaseRef(Ctx);
        end;
      end;
    end;
  end
  else if (N = SOCKET_ERROR) and (WSAGetLastError <> WSA_IO_PENDING) then
  begin
    ReleaseCtx(Ctx);
    ReleaseRef(Ctx);
  end;
end;

procedure TBadgerIOCP.PostSend(Ctx: PIocpCtx);
var
  Sent: DWORD;
  Remain: Integer;
  N: Integer;
begin
  Remain := Ctx^.SendLen - Ctx^.SendPos;
  if (Remain <= 0) or (Ctx^.SendBuf = nil) then
  begin
    ReleaseCtx(Ctx);
    Exit;
  end;
  if Remain > IOCP_SEND_CHUNK then
    Remain := IOCP_SEND_CHUNK;
  Ctx^.SendOp.Kind := ioSend;
  FillChar(Ctx^.SendOp.Overlapped, SizeOf(TOverlapped), 0);
  Ctx^.SendWsa.len := Cardinal(Remain);
  Ctx^.SendWsa.buf := PAnsiChar(Ctx^.SendBuf) + Ctx^.SendPos;
  Sent := 0;
  AddRef(Ctx);
  N := WSASend(Ctx^.Socket, @Ctx^.SendWsa, 1, Sent, 0, POverlapped(@Ctx^.SendOp), nil);
  if N = 0 then
  begin
    if Ctx^.SkipCompletion then
    begin
      { Mesma regra de PostRecv. Tambem tira HandleSend das threads da aplicacao
        (SendWsText): fora de worker a conclusao sempre vai para a porta. }
      if GIocpIsWorker and (GIocpDepth < IOCP_MAX_INLINE_DEPTH) then
      begin
        Inc(GIocpDepth);
        try
          HandleSend(Ctx, Sent);
        finally
          Dec(GIocpDepth);
          ReleaseRef(Ctx);
        end;
      end
      else if not PostQueuedCompletionStatus(FIocp, Sent, IOCP_KEY_WORK,
        POverlapped(@Ctx^.SendOp)) then
      begin
        try
          HandleSend(Ctx, Sent);
        finally
          ReleaseRef(Ctx);
        end;
      end;
    end;
  end
  else if (N = SOCKET_ERROR) and (WSAGetLastError <> WSA_IO_PENDING) then
  begin
    ReleaseCtx(Ctx);
    ReleaseRef(Ctx);
  end;
end;

procedure TBadgerIOCP.PrepareAndSend(Ctx: PIocpCtx; const Resp: TBadgerWsBytes);
begin
  if Ctx^.SendBuf <> nil then
  begin
    FreeMem(Ctx^.SendBuf);
    Ctx^.SendBuf := nil;
    Ctx^.SendAlloc := 0;
  end;
  Ctx^.SendLen := Length(Resp);
  Ctx^.SendPos := 0;
  if Ctx^.SendLen <= 0 then
  begin
    ReleaseCtx(Ctx);
    Exit;
  end;
  GetMem(Ctx^.SendBuf, Ctx^.SendLen);
  Ctx^.SendAlloc := Ctx^.SendLen;
  Move(Pointer(Resp)^, Ctx^.SendBuf^, Ctx^.SendLen);
  PostSend(Ctx);
end;

function IPv4ToStr(Addr: Cardinal): string;
begin
  Result := IntToStr(Addr and $FF) + '.' +
    IntToStr((Addr shr 8) and $FF) + '.' +
    IntToStr((Addr shr 16) and $FF) + '.' +
    IntToStr((Addr shr 24) and $FF);
end;

procedure TBadgerIOCP.FillRemoteIP(Ctx: PIocpCtx);
var
  Addr: TBadgerSockAddrIn;
  Len: Integer;
begin
  if not Assigned(Ctx^.Conn) then
    Exit;
  Ctx^.Conn.RemoteIP := '';
  Len := SizeOf(Addr);
  FillChar(Addr, SizeOf(Addr), 0);
  if getpeername(Ctx^.Socket, @Addr, Len) = SOCKET_ERROR then
    Exit;
  Ctx^.Conn.RemoteIP := IPv4ToStr(Addr.sin_addr);
end;

procedure TBadgerIOCP.HandleAccept(Ctx: PIocpCtx);
var
  Listen: TBadgerSocket;
  Ok: Boolean;
begin
  FLock.Acquire;
  try
    Listen := FListen;
  finally
    FLock.Release;
  end;
  Ok := FRunning and (Listen <> INVALID_SOCKET);
  if Ok then
    Ok := setsockopt(Ctx^.Socket, SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT, @Listen, SizeOf(Listen)) <> SOCKET_ERROR;
  if Ok then
    Ok := CreateIoCompletionPort(THandle(Ctx^.Socket), FIocp, IOCP_KEY_WORK, 0) <> 0;
  if Ok then
  begin
    EnableSkipCompletion(Ctx);
    FillRemoteIP(Ctx);
  end;
  if not CompleteAccept(Ctx, Ok) then
  begin
    ReleaseCtx(Ctx);
    EnsurePendingAccepts;
    Exit;
  end;
  Touch(Ctx);
  PostRecv(Ctx);
  EnsurePendingAccepts;
end;

procedure TBadgerIOCP.FinishRequest(Ctx: PIocpCtx);
var
  Pipe: TBadgerDispatchPipeline;
  D: TBadgerDispatchResult;
  Serial: Boolean;
begin
  if not Assigned(Ctx^.Parser) then
  begin
    ReleaseCtx(Ctx);
    Exit;
  end;
  BadgerAssignDispatchPipeline(Pipe, FRouteManager, FMiddlewares, FAfterMiddlewares,
    FCorsEnabled, FCorsAllowedOrigins, FCorsAllowedMethods, FCorsAllowedHeaders,
    FCorsExposeHeaders, FCorsAllowCredentials, FCorsMaxAge, FEnableEventInfo,
    FOnRequest, FOnResponse);
  Pipe.TrustProxyHeaders := FTrustProxyHeaders;
  Pipe.TrustedProxies := FTrustedProxies;
  D.Wire := '';
  D.CloseConn := True;
  D.WsUpgrade := False;
  D.WsURI := '';
  D.WsKey := '';
  Serial := not FParallelProcessing;
  if Serial then
    FSerialLock.Acquire;
  Ctx^.InDispatch := True;
  try
    D := BadgerDispatchHttp(Ctx^.Parser, Ctx^.Conn, Ctx^.RouteParams, Ctx^.RespHeaders, Pipe);
    Ctx^.CloseAfterSend := D.CloseConn;
    if D.WsUpgrade then
      BeginWs(Ctx, D.WsURI, D.WsKey);
  finally
    if Serial then
      FSerialLock.Release;
    Ctx^.InDispatch := False;
    Touch(Ctx);
  end;
  if not D.WsUpgrade then
    PrepareAndSend(Ctx, D.Wire);
end;


procedure TBadgerIOCP.HandleRecv(Ctx: PIocpCtx; Bytes: DWORD);
begin
  Touch(Ctx);
  if Bytes = 0 then
  begin
    ReleaseCtx(Ctx);
    Exit;
  end;
  if Ctx^.IsWebSocket then
  begin
    HandleWsRecv(Ctx, Bytes);
    Exit;
  end;
  if not Assigned(Ctx^.Parser) then
  begin
    ReleaseCtx(Ctx);
    Exit;
  end;
  if Ctx^.HdrPhase = 0 then
  begin
    Ctx^.HdrPhase := 1;
    Ctx^.ReqStart := GetTickCount;
  end;
  if Ctx^.Parser.Feed(@Ctx^.Buf[0], Integer(Bytes)) then
  begin
    Ctx^.HdrPhase := 2;
    FinishRequest(Ctx);
  end
  else
  begin
    if Ctx^.Parser.State <> hpsHeaders then
      Ctx^.HdrPhase := 2;
    { 'Expect: 100-continue': sem o interim o cliente so manda o corpo depois de
      estourar o proprio timeout. Envio direto (25 bytes num socket pronto) evita
      uma segunda operacao sobreposta so para isto. }
    if Ctx^.Parser.ExpectContinue and not Ctx^.Parser.ContinueSent then
    begin
      Ctx^.Parser.ContinueSent := True;
      BadgerSendAll(Ctx^.Socket, HTTP_100_CONTINUE);
    end;
    PostRecv(Ctx);
  end;
end;

procedure TBadgerIOCP.HandleSend(Ctx: PIocpCtx; Bytes: DWORD);
var
  Next: TBadgerWsBytes;
begin
  Touch(Ctx);
  if Bytes = 0 then
  begin
    ReleaseCtx(Ctx);
    Exit;
  end;
  Inc(Ctx^.SendPos, Integer(Bytes));
  if Ctx^.SendPos < Ctx^.SendLen then
  begin
    PostSend(Ctx);
    Exit;
  end;
  if Ctx^.IsWebSocket then
  begin
    Next := '';
    if Assigned(Ctx^.WsClient) then
    begin
      Ctx^.WsClient.IOLock.Acquire;
      try
        Ctx^.WsSendBusy := False;
        Next := Ctx^.WsQueue;
        Ctx^.WsQueue := '';
        if Next <> '' then
          Ctx^.WsSendBusy := True;
      finally
        Ctx^.WsClient.IOLock.Release;
      end;
    end;
    if Next <> '' then
      PrepareAndSend(Ctx, Next)
    else if not Ctx^.WsRecvArmed then
    begin
      Ctx^.WsRecvArmed := True;
      if not ProcessWsFrames(Ctx) then
        ReleaseCtx(Ctx)
      else
        PostRecv(Ctx);
    end;
  end
  else if Ctx^.CloseAfterSend or not FRunning then
    ReleaseCtx(Ctx)
  else
    RecycleKeepAlive(Ctx);
end;

procedure TBadgerIOCP.BeginWs(Ctx: PIocpCtx; const URI, WSKey: string);
var
  Info: TClientSocketInfo;
  Left: AnsiString;
begin
  Ctx^.IsWebSocket := True;
  Ctx^.CloseAfterSend := False;
  Ctx^.WsRecvArmed := False;
  Ctx^.WsSendBusy := True;
  Ctx^.WsParser := TBadgerWsParser.Create;
  if Assigned(Ctx^.Parser) then
  begin
    Left := Ctx^.Parser.Leftover;
    if Left <> '' then
      Ctx^.WsParser.Feed(@Left[1], Length(Left));
    Ctx^.Parser.Free;
    Ctx^.Parser := nil;
  end;
  Info := TClientSocketInfo.Create; { ref do motor: solta em FreeCtx }
  Info.Socket := nil;
  Info.Ctx := Ctx;
  Info.URI := URI;
  Info.InUse := True;
  Ctx^.WsClient := Info;
  { Ref entregue ao par attach/detach: quem assina OnWsDetach a solta (TBadger faz
    Info.Release). Sem detach, so a ref do motor existe e FreeCtx libera o Info. }
  if Assigned(FOnWsDetach) then
    Info.AddRef;
  if Assigned(FOnWsAttach) then
    FOnWsAttach(Info);
  Logger.Info('WebSocket handshake established for ' + URI);
  PrepareAndSend(Ctx, BadgerWsHandshakeMessage(WSKey));
end;

function TBadgerIOCP.ProcessWsFrames(Ctx: PIocpCtx): Boolean;
begin
  Result := Assigned(Ctx^.WsParser);
  if not Result then
    Exit;
  while Ctx^.WsParser.TryParse do
  begin
    if Ctx^.WsParser.Failed then
    begin
      { Fecha com status em vez de derrubar o TCP calado. }
      QueueWsFrame(Ctx, BadgerWsCloseFrame(Ctx^.WsParser.CloseCode));
      Result := False;
      Exit;
    end;
    { Fragmento intermediario: bytes consumidos, mensagem ainda incompleta. }
    if not Ctx^.WsParser.Complete then
      Continue;
    case Ctx^.WsParser.Opcode of
      WS_OP_CLOSE:
        begin
          QueueWsFrame(Ctx, BadgerWsCloseFrame(WS_CLOSE_NORMAL));
          Result := False;
          Exit;
        end;
      WS_OP_TEXT:
        if Assigned(FOnWebSocketMessage) and Assigned(Ctx^.WsClient) then
          FOnWebSocketMessage(Ctx^.WsClient, Ctx^.WsClient.URI,
            Ctx^.WsParser.Text);
      WS_OP_BINARY:
        { Sem evento binario na API publica: recusa explicita em vez de descarte. }
        begin
          QueueWsFrame(Ctx, BadgerWsCloseFrame(WS_CLOSE_UNSUPPORTED));
          Result := False;
          Exit;
        end;
      WS_OP_PING:
        QueueWsFrame(Ctx, BadgerWsPongFrame(Ctx^.WsParser.Payload));
    end;
  end;
end;

procedure TBadgerIOCP.HandleWsRecv(Ctx: PIocpCtx; Bytes: DWORD);
begin
  if not Assigned(Ctx^.WsParser) then
  begin
    ReleaseCtx(Ctx);
    Exit;
  end;
  Ctx^.WsParser.Feed(@Ctx^.Buf[0], Integer(Bytes));
  if not ProcessWsFrames(Ctx) then
    ReleaseCtx(Ctx)
  else
    PostRecv(Ctx);
end;

procedure TBadgerIOCP.QueueWsFrame(Ctx: PIocpCtx; const Frame: TBadgerWsBytes);
var
  StartNow, Overflow: Boolean;
begin
  if (Frame = '') or not Assigned(Ctx^.WsClient) or (Ctx^.Closing <> 0) then
    Exit;
  StartNow := False;
  Overflow := False;
  Ctx^.WsClient.IOLock.Acquire;
  try
    if Ctx^.WsSendBusy then
    begin
      { Sem teto, um cliente lento com broadcaster rapido acumula memoria sem
        limite (e a concatenacao repetida e O(n2)). Estourar = derrubar a conexao:
        so esvaziar a fila perdia mensagens em silencio e a conexao seguia. }
      if Length(Ctx^.WsQueue) + Length(Frame) > WS_MAX_QUEUE then
      begin
        Ctx^.WsQueue := '';
        Overflow := True;
      end
      else
        Ctx^.WsQueue := Ctx^.WsQueue + Frame;
    end
    else
    begin
      Ctx^.WsSendBusy := True;
      StartNow := True;
    end;
  finally
    Ctx^.WsClient.IOLock.Release;
  end;
  if Overflow then
  begin
    Logger.Warning('WebSocket send queue overflow; closing ' + Ctx^.WsClient.URI);
    ReleaseCtx(Ctx);
  end
  else if StartNow then
    PrepareAndSend(Ctx, Frame);
end;

procedure TBadgerIOCP.SendWsText(ClientInfo: TClientSocketInfo; const AMessage: string);
var
  Ctx: PIocpCtx;
begin
  if not Assigned(ClientInfo) or (ClientInfo.Ctx = nil) then
    Exit;
  Ctx := PIocpCtx(ClientInfo.Ctx);
  { Chamado de thread da aplicacao (broadcast, echo). Antes era CtxIsLive seguido de
    uso do ponteiro: o contexto podia ser liberado no intervalo. A referencia mantem
    a memoria viva por toda a operacao. }
  if not TryAddRef(Ctx) then
    Exit;
  try
    if ClientInfo.Ctx <> Ctx then
      Exit;
    QueueWsFrame(Ctx, BadgerWsTextFrame(AMessage));
  finally
    ReleaseRef(Ctx);
  end;
end;

procedure TBadgerIOCP.WorkerLoop;
var
  Bytes: DWORD;
  Key: TIocpKey;
  Ov: POverlapped;
  Hdr: PIocpOpHdr;
  Ctx: PIocpCtx;
  Ok: BOOL;
  Kind: TIocpOp;
begin
  GIocpIsWorker := True;
  GIocpDepth := 0;
  while True do
  begin
    Bytes := 0;
    Key := 0;
    Ov := nil;
    Ok := GetQueuedCompletionStatus(FIocp, Bytes, Key, Ov, INFINITE);
    if Key = IOCP_KEY_SHUTDOWN then
      Break;
    if Ov = nil then
    begin
      if not FRunning then
        Break;
      Continue;
    end;
    Hdr := PIocpOpHdr(Ov);
    Ctx := Hdr^.Ctx;
    Kind := Hdr^.Kind;
    { Sem CtxIsLive: era check-then-act sobre memoria que outra thread podia ter
      liberado (e GetMem podia reciclar o endereco para outra conexao). A referencia
      tomada ao postar garante que este ponteiro e valido agora. }
    try
      try
        if not Ok then
        begin
          if Kind = ioAccept then
          begin
            CompleteAccept(Ctx, False);
            ReleaseCtx(Ctx);
            EnsurePendingAccepts;
          end
          else
            ReleaseCtx(Ctx);
          Continue;
        end;
        case Kind of
          ioAccept:
            HandleAccept(Ctx);
          ioRecv:
            HandleRecv(Ctx, Bytes);
          ioSend:
            HandleSend(Ctx, Bytes);
        end;
      except
        on E: Exception do
        begin
          Logger.Error('IOCP worker (' + IocpOpName(Kind) + '): ' + E.Message);
          if (Kind = ioAccept) and not Ctx^.Counted then
            CompleteAccept(Ctx, False);
          ReleaseCtx(Ctx);
        end;
      end;
    finally
      { Devolve a referencia da operacao que acabou de completar. }
      ReleaseRef(Ctx);
    end;
  end;
end;

procedure TBadgerIOCP.Start;
var
  Addr: TBadgerSockAddrIn;
  One: Integer;
  I: Integer;
begin
  if FStarted then
    Exit;

  if not BadgerWSAAddRef then
    raise EBadgerIOCP.Create('WSAStartup failed');

  try
    FListen := BadgerCreateOverlappedSocket;
    if FListen = INVALID_SOCKET then
      raise EBadgerIOCP.CreateFmt('WSASocket failed (%d)', [WSAGetLastError]);

    One := 1;
    { SO_REUSEADDR no Windows nao e o do POSIX: permite que outro processo faca bind
      no mesmo porto e sequestre conexoes. SO_EXCLUSIVEADDRUSE e o equivalente seguro. }
    setsockopt(FListen, SOL_SOCKET, SO_EXCLUSIVEADDRUSE, @One, SizeOf(One));

    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_addr := 0;
    Addr.sin_port := htons(Word(FPort));
    if bind(FListen, @Addr, SizeOf(Addr)) = SOCKET_ERROR then
      raise EBadgerIOCP.CreateFmt('bind failed on port %d (%d)', [FPort, WSAGetLastError]);
    if listen(FListen, SOMAXCONN) = SOCKET_ERROR then
      raise EBadgerIOCP.CreateFmt('listen failed (%d)', [WSAGetLastError]);

    if not BadgerLoadAcceptEx(FListen, FAcceptEx) then
      raise EBadgerIOCP.CreateFmt('AcceptEx not available (%d)', [WSAGetLastError]);

    FIocp := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
    if (FIocp = 0) or (FIocp = INVALID_HANDLE_VALUE) then
      raise EBadgerIOCP.CreateFmt('CreateIoCompletionPort failed (%d)', [GetLastError]);

    if CreateIoCompletionPort(THandle(FListen), FIocp, IOCP_KEY_WORK, 0) = 0 then
      raise EBadgerIOCP.CreateFmt('associate listen socket failed (%d)', [GetLastError]);

    if FWorkerCount < 1 then
      FWorkerCount := 1;
    if FPendingAccepts < 1 then
      FPendingAccepts := 1;

    FRunning := True;
    SetLength(FWorkers, FWorkerCount);
    for I := 0 to FWorkerCount - 1 do
    begin
      FWorkers[I] := TIocpWorker.Create(Self);
      StartIocpThread(FWorkers[I]);
    end;

    FWatchdog := TIocpWatchdog.Create(Self);
    StartIocpThread(FWatchdog);

    EnsurePendingAccepts;

    FStarted := True;
    Log(Format('start port=%d workers=%d pendingAccepts=%d %s',
      [FPort, FWorkerCount, FPendingAccepts, CtxStats]));
  except
    CloseListenSocket;
    if (FIocp <> 0) and (FIocp <> INVALID_HANDLE_VALUE) then
    begin
      CloseHandle(FIocp);
      FIocp := 0;
    end;
    BadgerWSARelease;
    raise;
  end;
end;

procedure TBadgerIOCP.Stop;
var
  I: Integer;
begin
  if not FStarted then
    Exit;

  FRunning := False;
  if Assigned(FWatchdog) then
  begin
    FWatchdog.Terminate;
    FWatchdog.WaitFor;
    FWatchdog.Free;
    FWatchdog := nil;
  end;
  CloseListenSocket;
  Log('listen closed: ' + CtxStats);

  { Ordem importa. Antes os workers eram encerrados primeiro e so depois os sockets
    eram fechados e a memoria liberada: as completions geradas pelo fechamento caiam
    numa porta que ninguem mais drenava, e o OVERLAPPED (primeiro campo do contexto)
    era devolvido ao heap com o kernel ainda apontando para ele. Agora fechamos as
    conexoes com os workers vivos, deixamos as completions serem consumidas, e so
    entao sinalizamos o shutdown. }
  CloseAllCtx;
  WaitLiveCtxIdle(5000);
  Log('after client drain: ' + CtxStats);

  for I := 0 to Length(FWorkers) - 1 do
    PostQueuedCompletionStatus(FIocp, 0, IOCP_KEY_SHUTDOWN, nil);

  for I := 0 to Length(FWorkers) - 1 do
  begin
    if Assigned(FWorkers[I]) then
    begin
      FWorkers[I].WaitFor;
      FWorkers[I].Free;
      FWorkers[I] := nil;
    end;
  end;
  SetLength(FWorkers, 0);

  Log('workers stopped: ' + CtxStats);
  { Rede de seguranca: o que sobrou apos o dreno acima. }
  DrainLiveCtx;
  Log('after drain: ' + CtxStats);

  if (FIocp <> 0) and (FIocp <> INVALID_HANDLE_VALUE) then
  begin
    CloseHandle(FIocp);
    FIocp := 0;
  end;

  FAcceptEx := nil;
  FStarted := False;
  BadgerWSARelease;
end;

end.
