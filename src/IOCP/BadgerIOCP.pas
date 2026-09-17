unit BadgerIOCP;

{ Windows IOCP HTTP I/O. Isolated from TBadger.Execute (Synapse).
  TBadger.Start adopts this engine when UseIOCP (default True on Windows). }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Windows, SysUtils, Classes, SyncObjs, BadgerWinSock2, BadgerHttpParser, BadgerHttpStatus,
  BadgerTypes, BadgerRouteManager, BadgerLogger, BadgerWebSocket;

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
    procedure Log(const Msg: string);
    function AllocCtx(Op: TIocpOp): PIocpCtx;
    procedure ReleaseCtx(Ctx: PIocpCtx);
    function CtxIsLive(Ctx: PIocpCtx): Boolean;
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
    procedure RunAfterMiddlewares(var Req: THTTPRequest; var Resp: THTTPResponse);
    procedure FireHttpEvents(const Req: THTTPRequest; const Resp: THTTPResponse; const ResponseHeader: string);
    function HandleCorsPreflight(const Req: THTTPRequest; var Resp: THTTPResponse): Boolean;
    procedure ApplyCorsHeaders(const Req: THTTPRequest; var Resp: THTTPResponse);
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
    property Timeout: Integer read FTimeout write FTimeout;
    property MaxConcurrentConnections: Integer read FMaxConcurrentConnections write FMaxConcurrentConnections;
    property ParallelProcessing: Boolean read FParallelProcessing write FParallelProcessing;
    property ActiveConnections: Integer read FActiveConnections;
    function CtxStats: string;
  end;

implementation

const
  IOCP_SEND_CHUNK = 65536;
  FILE_SKIP_COMPLETION_PORT_ON_SUCCESS = 1;

var
  GSetFileCompletionNotificationModes: function(FileHandle: THandle; Flags: Byte): BOOL; stdcall;

procedure LoadSkipCompletionApi;
begin
  if Assigned(GSetFileCompletionNotificationModes) then
    Exit;
  @GSetFileCompletionNotificationModes := GetProcAddress(GetModuleHandle('kernel32.dll'),
    'SetFileCompletionNotificationModes');
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

procedure AttachParserBody(Parser: TBadgerHttpParser; var Req: THTTPRequest);
var
  CT: string;
  Raw: AnsiString;
begin
  if not Assigned(Parser) then
    Exit;
  Raw := Parser.Body;
  if Length(Raw) = 0 then
    Exit;
  CT := LowerCase(Req.Headers.Values['Content-Type']);
  if (Pos('application/json', CT) > 0) or (Pos('text/', CT) > 0) then
    Req.Body := Utf8BytesToString(Raw)
  else
  begin
    Req.BodyStream := TMemoryStream.Create;
    Req.BodyStream.WriteBuffer(Raw[1], Length(Raw));
    Req.BodyStream.Position := 0;
  end;
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
    if Left <> '' then
    begin
      if Ctx^.Parser.Feed(@Left[1], Length(Left)) then
      begin
        FinishRequest(Ctx);
        Exit;
      end;
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
begin
  if (FTimeout <= 0) or not FRunning then
    Exit;
  NowTick := GetTickCount;
  Idle := TList.Create;
  try
    FLock.Acquire;
    try
      for I := 0 to FLiveCtx.Count - 1 do
      begin
        Ctx := PIocpCtx(FLiveCtx[I]);
        if (Ctx^.RecvOp.Kind = ioRecv) and Ctx^.Counted and (not Ctx^.IsWebSocket) and
           ((NowTick - Ctx^.LastActivity) >= DWORD(FTimeout)) then
          Idle.Add(Ctx);
      end;
    finally
      FLock.Release;
    end;
    for I := 0 to Idle.Count - 1 do
    begin
      Ctx := PIocpCtx(Idle[I]);
      if CtxIsLive(Ctx) and (Ctx^.Socket <> 0) and (Ctx^.Socket <> INVALID_SOCKET) then
        CancelIo(THandle(Ctx^.Socket));
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
  FLock.Acquire;
  try
    FLiveCtx.Add(Result);
    Inc(FAllocCount);
  finally
    FLock.Release;
  end;
end;

procedure TBadgerIOCP.ReleaseCtx(Ctx: PIocpCtx);
var
  Idx: Integer;
  WasCounted: Boolean;
  Info: TClientSocketInfo;
begin
  if Ctx = nil then
    Exit;
  WasCounted := Ctx^.Counted;
  if WasCounted then
  begin
    Ctx^.Counted := False;
    FLock.Acquire;
    try
      if FActiveConnections > 0 then
        Dec(FActiveConnections);
    finally
      FLock.Release;
    end;
  end;
  FLock.Acquire;
  try
    Idx := FLiveCtx.IndexOf(Ctx);
    if Idx >= 0 then
      FLiveCtx.Delete(Idx);
    Inc(FFreeCount);
  finally
    FLock.Release;
  end;
  Info := Ctx^.WsClient;
  Ctx^.WsClient := nil;
  if Assigned(Info) then
  begin
    Info.Ctx := nil;
    if Assigned(FOnWsDetach) then
      FOnWsDetach(Info);
  end;
  if Assigned(Ctx^.WsParser) then
  begin
    Ctx^.WsParser.Free;
    Ctx^.WsParser := nil;
  end;
  Ctx^.WsQueue := '';
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
  if WasCounted then
    EnsurePendingAccepts;
end;

function TBadgerIOCP.CtxIsLive(Ctx: PIocpCtx): Boolean;
begin
  if Ctx = nil then
  begin
    Result := False;
    Exit;
  end;
  FLock.Acquire;
  try
    Result := FLiveCtx.IndexOf(Ctx) >= 0;
  finally
    FLock.Release;
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
        Tmp.Add(FLiveCtx[I]);
    finally
      FLock.Release;
    end;
    for I := 0 to Tmp.Count - 1 do
    begin
      Ctx := PIocpCtx(Tmp[I]);
      if (Ctx^.Socket <> 0) and (Ctx^.Socket <> INVALID_SOCKET) then
      begin
        CancelIo(THandle(Ctx^.Socket));
        closesocket(Ctx^.Socket);
        Ctx^.Socket := INVALID_SOCKET;
      end;
    end;
    if Tmp.Count > 0 then
      Sleep(20);
    for I := 0 to Tmp.Count - 1 do
    begin
      Ctx := PIocpCtx(Tmp[I]);
      Logger.Debug('drain leftover ' + IocpOpName(Ctx^.RecvOp.Kind) + ' ptr=' + Format('%p', [Pointer(Ctx)]));
      ReleaseCtx(Ctx);
    end;
  finally
    Tmp.Free;
  end;
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
  if not AcceptEx(Listen, Ctx^.Socket, @Ctx^.AcceptBuf[0], 0,
    SizeOf(TBadgerSockAddrIn) + 16, SizeOf(TBadgerSockAddrIn) + 16,
    Bytes, POverlapped(@Ctx^.RecvOp)) then
  begin
    if WSAGetLastError <> WSA_IO_PENDING then
    begin
      DecPendingAccept;
      ReleaseCtx(Ctx);
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
  N := WSARecv(Ctx^.Socket, @Ctx^.RecvWsa, 1, Recvd, Flags, POverlapped(@Ctx^.RecvOp), nil);
  if N = 0 then
  begin
    if Ctx^.SkipCompletion then
      HandleRecv(Ctx, Recvd);
  end
  else if (N = SOCKET_ERROR) and (WSAGetLastError <> WSA_IO_PENDING) then
    ReleaseCtx(Ctx);
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
  N := WSASend(Ctx^.Socket, @Ctx^.SendWsa, 1, Sent, 0, POverlapped(@Ctx^.SendOp), nil);
  if N = 0 then
  begin
    if Ctx^.SkipCompletion then
      HandleSend(Ctx, Sent);
  end
  else if (N = SOCKET_ERROR) and (WSAGetLastError <> WSA_IO_PENDING) then
    ReleaseCtx(Ctx);
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

procedure TBadgerIOCP.RunAfterMiddlewares(var Req: THTTPRequest; var Resp: THTTPResponse);
var
  I: Integer;
  AfterWrapper: TAfterMiddlewareWrapper;
begin
  if not Assigned(FAfterMiddlewares) then
    Exit;
  for I := FAfterMiddlewares.Count - 1 downto 0 do
  begin
    AfterWrapper := TAfterMiddlewareWrapper(FAfterMiddlewares[I]);
    if not Assigned(AfterWrapper) or not Assigned(AfterWrapper.Middleware) then
      Continue;
    try
      AfterWrapper.Middleware(Req, Resp);
    except
      on E: Exception do
        Logger.Error('After-middleware exception: ' + E.Message);
    end;
  end;
end;

procedure SplitCommaTokens(const AValue: string; ADest: TStringList);
var
  I: Integer;
  Token: string;
begin
  ADest.Clear;
  Token := '';
  for I := 1 to Length(AValue) do
  begin
    if AValue[I] = ',' then
    begin
      Token := Trim(Token);
      if Token <> '' then
        ADest.Add(Token);
      Token := '';
    end
    else
      Token := Token + AValue[I];
  end;
  Token := Trim(Token);
  if Token <> '' then
    ADest.Add(Token);
end;

function TBadgerIOCP.HandleCorsPreflight(const Req: THTTPRequest; var Resp: THTTPResponse): Boolean;
var
  Origin, ACRM, ACRH, AllowOrigin, MethodsStr, HeadersStr, HdrPart: string;
  AllowWildcardOrigin, OriginAllowed: Boolean;
  I: Integer;
  HdrParts: TStringList;
begin
  Result := False;
  if not FCorsEnabled then
    Exit;
  if Req.Method <> 'OPTIONS' then
    Exit;
  Origin := Trim(Req.Headers.Values['Origin']);
  ACRM := Trim(Req.Headers.Values['Access-Control-Request-Method']);
  ACRH := Req.Headers.Values['Access-Control-Request-Headers'];
  if (Origin = '') or (ACRM = '') then
    Exit;

  Result := True;
  AllowWildcardOrigin := FCorsAllowedOrigins.IndexOf('*') >= 0;
  if FCorsAllowCredentials then
    OriginAllowed := FCorsAllowedOrigins.IndexOf(Origin) >= 0
  else
    OriginAllowed := AllowWildcardOrigin or (FCorsAllowedOrigins.IndexOf(Origin) >= 0);

  if not OriginAllowed then
  begin
    Resp.StatusCode := HTTP_FORBIDDEN;
    Resp.Body := 'CORS origin not allowed';
    Resp.ContentType := TEXT_PLAIN;
    Exit;
  end;

  if FCorsAllowedMethods.IndexOf(UpperCase(ACRM)) < 0 then
  begin
    Resp.StatusCode := HTTP_METHOD_NOT_ALLOWED;
    Resp.Body := '';
    Resp.ContentType := '';
    Resp.HeadersCustom.Values['Allow'] := FCorsAllowedMethods.CommaText;
    Exit;
  end;

  HeadersStr := '';
  if ACRH <> '' then
  begin
    HdrParts := TStringList.Create;
    try
      SplitCommaTokens(ACRH, HdrParts);
      for I := 0 to HdrParts.Count - 1 do
      begin
        HdrPart := Trim(HdrParts[I]);
        if HdrPart = '' then
          Continue;
        if FCorsAllowedHeaders.IndexOf(HdrPart) < 0 then
        begin
          Resp.StatusCode := HTTP_BAD_REQUEST;
          Resp.Body := '';
          Resp.ContentType := '';
          Exit;
        end;
        if HeadersStr = '' then
          HeadersStr := HdrPart
        else
          HeadersStr := HeadersStr + ',' + HdrPart;
      end;
    finally
      HdrParts.Free;
    end;
  end
  else
    HeadersStr := FCorsAllowedHeaders.CommaText;

  if FCorsAllowCredentials then
    AllowOrigin := Origin
  else if AllowWildcardOrigin then
    AllowOrigin := '*'
  else
    AllowOrigin := Origin;

  MethodsStr := FCorsAllowedMethods.CommaText;
  Resp.StatusCode := HTTP_NO_CONTENT;
  Resp.Body := '';
  Resp.ContentType := '';
  Resp.HeadersCustom.Values['Access-Control-Allow-Origin'] := AllowOrigin;
  Resp.HeadersCustom.Values['Access-Control-Allow-Methods'] := MethodsStr;
  Resp.HeadersCustom.Values['Access-Control-Allow-Headers'] := HeadersStr;
  if FCorsAllowCredentials then
    Resp.HeadersCustom.Values['Access-Control-Allow-Credentials'] := 'true';
  if FCorsMaxAge > 0 then
    Resp.HeadersCustom.Values['Access-Control-Max-Age'] := IntToStr(FCorsMaxAge);
  if AllowOrigin <> '*' then
    Resp.HeadersCustom.Values['Vary'] := 'Origin, Access-Control-Request-Method, Access-Control-Request-Headers';
end;

procedure TBadgerIOCP.ApplyCorsHeaders(const Req: THTTPRequest; var Resp: THTTPResponse);
var
  Origin, AllowOrigin, ExposeStr: string;
  AllowWildcardOrigin, OriginAllowed: Boolean;
begin
  if not FCorsEnabled then
    Exit;
  Origin := Trim(Req.Headers.Values['Origin']);
  if Origin = '' then
    Exit;
  AllowWildcardOrigin := FCorsAllowedOrigins.IndexOf('*') >= 0;
  if FCorsAllowCredentials then
    OriginAllowed := FCorsAllowedOrigins.IndexOf(Origin) >= 0
  else
    OriginAllowed := AllowWildcardOrigin or (FCorsAllowedOrigins.IndexOf(Origin) >= 0);
  if not OriginAllowed then
    Exit;
  if FCorsAllowCredentials then
    AllowOrigin := Origin
  else if AllowWildcardOrigin then
    AllowOrigin := '*'
  else
    AllowOrigin := Origin;
  Resp.HeadersCustom.Values['Access-Control-Allow-Origin'] := AllowOrigin;
  if FCorsAllowCredentials then
    Resp.HeadersCustom.Values['Access-Control-Allow-Credentials'] := 'true';
  ExposeStr := FCorsExposeHeaders.CommaText;
  if ExposeStr <> '' then
    Resp.HeadersCustom.Values['Access-Control-Expose-Headers'] := ExposeStr;
  if AllowOrigin <> '*' then
    Resp.HeadersCustom.Values['Vary'] := 'Origin';
end;

procedure TBadgerIOCP.FireHttpEvents(const Req: THTTPRequest; const Resp: THTTPResponse;
  const ResponseHeader: string);
var
  RequestInfo: TRequestInfo;
  ResponseInfo: TResponseInfo;
begin
  if not FEnableEventInfo then
    Exit;
  if Assigned(FOnRequest) then
  begin
    FillChar(RequestInfo, SizeOf(RequestInfo), 0);
    RequestInfo.Headers := TStringList.Create;
    RequestInfo.QueryParams := TStringList.Create;
    try
      RequestInfo.RemoteIP := Req.FRemoteIP;
      RequestInfo.Method := Req.Method;
      RequestInfo.URI := Req.URI;
      RequestInfo.RequestLine := Req.RequestLine;
      if Assigned(Req.Headers) then
        RequestInfo.Headers.Assign(Req.Headers);
      RequestInfo.Body := Req.Body;
      if Assigned(Req.QueryParams) then
        RequestInfo.QueryParams.Assign(Req.QueryParams);
      RequestInfo.Timestamp := Now;
      FOnRequest(RequestInfo);
    finally
      RequestInfo.Headers.Free;
      RequestInfo.QueryParams.Free;
    end;
  end;
  if Assigned(FOnResponse) then
  begin
    FillChar(ResponseInfo, SizeOf(ResponseInfo), 0);
    ResponseInfo.Headers := TStringList.Create;
    try
      ResponseInfo.StatusCode := Resp.StatusCode;
      ResponseInfo.StatusText := THTTPStatus.GetStatusText(Resp.StatusCode);
      ResponseInfo.Body := Resp.Body;
      ResponseInfo.ContentType := Resp.ContentType;
      ResponseInfo.Headers.Text := ResponseHeader;
      ResponseInfo.Timestamp := Now;
      FOnResponse(ResponseInfo);
    finally
      ResponseInfo.Headers.Free;
    end;
  end;
end;

procedure TBadgerIOCP.FinishRequest(Ctx: PIocpCtx);
var
  Req: THTTPRequest;
  Resp: THTTPResponse;
  RouteEntry: TRouteEntry;
  MiddlewareWrapper: TMiddlewareWrapper;
  Handled: Boolean;
  SkipRoute: Boolean;
  I: Integer;
  Wire: AnsiString;
  LForwardedFor: string;
  Header: string;
  CloseConn: Boolean;
  Serial: Boolean;
  WsUpgraded: Boolean;
  WSKey: string;
begin
  Wire := '';
  CloseConn := True;
  WsUpgraded := False;
  if not Assigned(Ctx^.Parser) then
  begin
    ReleaseCtx(Ctx);
    Exit;
  end;

  FillChar(Req, SizeOf(Req), 0);
  FillChar(Resp, SizeOf(Resp), 0);
  Ctx^.RouteParams.Clear;
  Ctx^.RespHeaders.Clear;
  Req.Headers := Ctx^.Parser.Headers;
  Req.QueryParams := Ctx^.Parser.QueryParams;
  Req.RouteParams := Ctx^.RouteParams;
  Resp.HeadersCustom := Ctx^.RespHeaders;
  Handled := False;
  SkipRoute := False;
  Serial := not FParallelProcessing;
  if Serial then
    FSerialLock.Acquire;
  try
    try
      Req.Socket := nil;
      Req.Method := Ctx^.Parser.Method;
      Req.URI := Ctx^.Parser.URI;
      Req.RequestLine := Ctx^.Parser.RequestLine;
      AttachParserBody(Ctx^.Parser, Req);
      if Assigned(Ctx^.Conn) then
        Req.FRemoteIP := Ctx^.Conn.RemoteIP;

      LForwardedFor := Trim(Ctx^.Parser.RealIP);
      if LForwardedFor <> '' then
        Req.FRemoteIP := LForwardedFor
      else
      begin
        LForwardedFor := Ctx^.Parser.ForwardedFor;
        if LForwardedFor <> '' then
        begin
          I := Pos(',', LForwardedFor);
          if I > 0 then
            Req.FRemoteIP := Trim(Copy(LForwardedFor, 1, I - 1))
          else
            Req.FRemoteIP := Trim(LForwardedFor);
        end;
      end;

      if Ctx^.Parser.State = hpsError then
      begin
        if Ctx^.Parser.Error = 'body too large' then
        begin
          Resp.StatusCode := HTTP_PAYLOAD_TOO_LARGE;
          Resp.Body := '{"error":"Request body too large"}';
          Resp.ContentType := APPLICATION_JSON;
        end
        else
        begin
          Resp.StatusCode := HTTP_BAD_REQUEST;
          Resp.Body := Ctx^.Parser.Error;
          Resp.ContentType := TEXT_PLAIN;
        end;
        Handled := True;
        SkipRoute := True;
      end;

      if (not SkipRoute) and Ctx^.Parser.IsWebSocketUpgrade then
      begin
        WSKey := Trim(Req.Headers.Values['Sec-WebSocket-Key']);
        if WSKey = '' then
        begin
          Resp.StatusCode := HTTP_BAD_REQUEST;
          Resp.Body := '{"error":"Missing Sec-WebSocket-Key"}';
          Resp.ContentType := APPLICATION_JSON;
          Handled := True;
          SkipRoute := True;
        end
        else if Req.Headers.Values['Sec-WebSocket-Version'] <> '13' then
        begin
          Resp.StatusCode := HTTP_BAD_REQUEST;
          Resp.Body := '{"error":"Unsupported WebSocket version"}';
          Resp.ContentType := APPLICATION_JSON;
          Resp.HeadersCustom.Values['Sec-WebSocket-Version'] := '13';
          Handled := True;
          SkipRoute := True;
        end
        else
        begin
          SkipRoute := True;
          Handled := True;
          WsUpgraded := True;
          BeginWs(Ctx, Req.URI, WSKey);
        end;
      end;

      if (not SkipRoute) and HandleCorsPreflight(Req, Resp) then
        SkipRoute := True
      else if not SkipRoute then
      begin
        try
          for I := 0 to FMiddlewares.Count - 1 do
          begin
            MiddlewareWrapper := TMiddlewareWrapper(FMiddlewares[I]);
            try
              if MiddlewareWrapper.Middleware(Req, Resp) then
              begin
                Handled := True;
                Break;
              end;
            except
              on E: Exception do
              begin
                Resp.StatusCode := HTTP_INTERNAL_SERVER_ERROR;
                Resp.Body := '{"error":"Middleware exception: ' + E.Message + '"}';
                Resp.ContentType := APPLICATION_JSON;
                Handled := True;
                Break;
              end;
            end;
          end;

          if not Handled then
          begin
            if FRouteManager.MatchRoute(Req.Method, Ctx^.Parser.URILower, RouteEntry, Req.RouteParams) then
            begin
              if Assigned(RouteEntry) and Assigned(TMethod(RouteEntry.Callback).Code) then
              begin
                try
                  RouteEntry.Callback(Req, Resp);
                except
                  on E: Exception do
                  begin
                    Resp.StatusCode := HTTP_INTERNAL_SERVER_ERROR;
                    Resp.Body := '{"error":"Route exception: ' + E.Message + '"}';
                    Resp.ContentType := APPLICATION_JSON;
                  end;
                end;
              end
              else
              begin
                Resp.StatusCode := HTTP_INTERNAL_SERVER_ERROR;
                Resp.Body := '{"error":"Route handler not assigned"}';
                Resp.ContentType := APPLICATION_JSON;
              end;
            end
            else
            begin
              Resp.StatusCode := HTTP_NOT_FOUND;
              Resp.Body := 'Not Found';
              Resp.ContentType := TEXT_PLAIN;
            end;
          end;

          if Ctx^.Parser.Origin <> '' then
            ApplyCorsHeaders(Req, Resp);
        finally
          RunAfterMiddlewares(Req, Resp);
        end;
      end;
    except
      on E: Exception do
      begin
        Logger.Error('Dispatch exception: ' + E.Message);
        Resp.StatusCode := HTTP_INTERNAL_SERVER_ERROR;
        Resp.Body := 'Internal Server Error';
        Resp.ContentType := TEXT_PLAIN;
      end;
    end;

    if not WsUpgraded then
    begin
      CloseConn := Ctx^.Parser.WantsClose;
      if Ctx^.Parser.State = hpsError then
        CloseConn := True;
      Ctx^.CloseAfterSend := CloseConn;
      Wire := BadgerAssembleHTTPMessage(Resp.StatusCode, Resp.Body, Resp.Stream,
        Resp.ContentType, CloseConn, Resp.HeadersCustom);
      if FEnableEventInfo and (Assigned(FOnRequest) or Assigned(FOnResponse)) then
      begin
        Header := string(Copy(Wire, 1, Pos(AnsiString(#13#10#13#10), Wire) + 3));
        FireHttpEvents(Req, Resp, Header);
      end;
    end;
  finally
    if Assigned(Resp.Stream) then
      Resp.Stream.Free;
    if Assigned(Req.BodyStream) then
      Req.BodyStream.Free;
    if Serial then
      FSerialLock.Release;
  end;
  if not WsUpgraded then
    PrepareAndSend(Ctx, Wire);
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
  if Ctx^.Parser.Feed(@Ctx^.Buf[0], Integer(Bytes)) then
    FinishRequest(Ctx)
  else
    PostRecv(Ctx);
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
  Info := TClientSocketInfo.Create;
  Info.Socket := nil;
  Info.Ctx := Ctx;
  Info.URI := URI;
  Info.InUse := True;
  Ctx^.WsClient := Info;
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
      Result := False;
      Exit;
    end;
    case Ctx^.WsParser.Opcode of
      WS_OP_CLOSE:
        begin
          Result := False;
          Exit;
        end;
      WS_OP_TEXT:
        if (Ctx^.WsParser.Payload <> '') and Assigned(FOnWebSocketMessage) and
           Assigned(Ctx^.WsClient) then
          FOnWebSocketMessage(Ctx^.WsClient, Ctx^.WsClient.URI,
            Ctx^.WsParser.Text);
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
  StartNow: Boolean;
begin
  if (Frame = '') or not Assigned(Ctx^.WsClient) then
    Exit;
  StartNow := False;
  Ctx^.WsClient.IOLock.Acquire;
  try
    if Ctx^.WsSendBusy then
      Ctx^.WsQueue := Ctx^.WsQueue + Frame
    else
    begin
      Ctx^.WsSendBusy := True;
      StartNow := True;
    end;
  finally
    Ctx^.WsClient.IOLock.Release;
  end;
  if StartNow then
    PrepareAndSend(Ctx, Frame);
end;

procedure TBadgerIOCP.SendWsText(ClientInfo: TClientSocketInfo; const AMessage: string);
var
  Ctx: PIocpCtx;
begin
  if not Assigned(ClientInfo) or (ClientInfo.Ctx = nil) then
    Exit;
  Ctx := PIocpCtx(ClientInfo.Ctx);
  if not CtxIsLive(Ctx) then
    Exit;
  QueueWsFrame(Ctx, BadgerWsTextFrame(AMessage));
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
    if not CtxIsLive(Ctx) then
      Continue;
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
      if (Kind = ioAccept) and Assigned(Ctx) and not Ctx^.Counted then
        CompleteAccept(Ctx, False);
      ReleaseCtx(Ctx);
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
    setsockopt(FListen, SOL_SOCKET, SO_REUSEADDR, @One, SizeOf(One));

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
  Log('listen closed, waiting AcceptEx drain: ' + CtxStats);
  WaitLiveCtxIdle(2000);
  Log('after AcceptEx wait: ' + CtxStats);

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
