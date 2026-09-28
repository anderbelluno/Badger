unit BadgerEpoll;

{ Linux epoll HTTP I/O. One listen + epfd per worker (SO_REUSEPORT), Horse-style.
  Dispatch is BadgerHttpDispatch. TBadger.Start adopts this when UseEpoll. }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

{$IFDEF LINUX}

uses
  SysUtils, Classes, SyncObjs, BadgerEpollSys, BadgerHttpParser, BadgerHttpDispatch,
  BadgerTypes, BadgerRouteManager, BadgerLogger, BadgerWebSocket;

type
  TBadgerEpoll = class;
  TBadgerEpollWorker = class;

  TEpollCtxKind = (ekListen, ekPipe, ekConn);

  TEpollCtx = class
    Kind: TEpollCtxKind;
    Fd: Integer;
    Parser: TBadgerHttpParser;
    Conn: TBadgerConn;
    RouteParams: TStringList;
    RespHeaders: TStringList;
    SendBuf: Pointer;
    SendLen: Integer;
    SendPos: Integer;
    CloseAfterSend: Boolean;
    LastActivity: LongWord;
    RecvBuf: array[0..8191] of AnsiChar;
    IsWebSocket: Boolean;
    WsParser: TBadgerWsParser;
    WsClient: TClientSocketInfo;
    WsSendBusy: Boolean;
    WsRecvArmed: Boolean;
    WsQueue: TBadgerWsBytes;
    Worker: TBadgerEpollWorker;
    constructor Create;
    destructor Destroy; override;
  end;

  TBadgerEpollWorker = class(TThread)
  private
    FOwner: TBadgerEpoll;
    FListenFd: Integer;
    FEpfd: Integer;
    FPipeR: Integer;
    FPipeW: Integer;
    FListenCtx: TEpollCtx;
    FPipeCtx: TEpollCtx;
    FConns: TList;
    FConnsLock: TCriticalSection;
    procedure CloseCtx(Ctx: TEpollCtx);
    procedure AcceptLoop;
    procedure HandleRead(Ctx: TEpollCtx);
    procedure HandleWrite(Ctx: TEpollCtx);
    procedure FinishHttp(Ctx: TEpollCtx);
    procedure Arm(Ctx: TEpollCtx; Events: Cardinal);
    procedure RecycleKeepAlive(Ctx: TEpollCtx);
    procedure ScanIdle;
    procedure DrainConns;
    procedure BeginWs(Ctx: TEpollCtx; const URI, WSKey: string);
    function ProcessWsFrames(Ctx: TEpollCtx): Boolean;
    procedure HandleWsRead(Ctx: TEpollCtx);
    procedure QueueWsFrame(Ctx: TEpollCtx; const Frame: TBadgerWsBytes);
    procedure PrepareSend(Ctx: TEpollCtx; Data: Pointer; Len: Integer);
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TBadgerEpoll; AListenFd: Integer);
    destructor Destroy; override;
    procedure SignalStop;
  end;

  TBadgerEpoll = class
  private
    FPort: Integer;
    FTimeout: Integer;
    FMaxConcurrentConnections: Integer;
    FParallelProcessing: Boolean;
    FWorkerCount: Integer;
    FRunning: Boolean;
    FRouteManager: TRouteManager;
    FMiddlewares: TList;
    FAfterMiddlewares: TList;
    FOwnsPipeline: Boolean;
    FTrustProxyHeaders: Boolean;
    FTrustedProxies: TStringList;
    FMiddlewareLock: TCriticalSection;
    FCorsEnabled: Boolean;
    FCorsAllowedOrigins: TStringList;
    FCorsAllowedMethods: TStringList;
    FCorsAllowedHeaders: TStringList;
    FCorsExposeHeaders: TStringList;
    FCorsAllowCredentials: Boolean;
    FCorsMaxAge: Integer;
    FEnableEventInfo: Boolean;
    FOnRequest: TOnRequest;
    FOnResponse: TOnResponse;
    FOnWebSocketMessage: TWebSocketMessageEvent;
    FOnWsAttach: TWsClientProc;
    FOnWsDetach: TWsClientProc;
    FWsClients: TList;
    FWsLock: TCriticalSection;
    FSerialLock: TCriticalSection;
    FActiveLock: TCriticalSection;
    FActiveConnections: Integer;
    FWorkers: array of TBadgerEpollWorker;
    procedure InitCorsDefaults;
    procedure AttachWs(Info: TClientSocketInfo);
    procedure DetachWs(Ctx: TEpollCtx);
    procedure InitPipeline;
    procedure FreePipeline;
    function AllocListen: Integer;
    function WorkerCountDefault: Integer;
    function CanAccept: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncActive;
    procedure DecActive;
    procedure Start;
    procedure Stop;
    procedure AdoptPipeline(ARouteManager: TRouteManager;
      AMiddlewares, AAfterMiddlewares: TList; AMiddlewareLock: TCriticalSection;
      ACorsOrigins, ACorsMethods, ACorsHeaders, ACorsExpose: TStringList);
    procedure AddMiddleware(Middleware: TMiddlewareProc);
    procedure AddAfterMiddleware(Middleware: TAfterMiddlewareProc);
    procedure SendWsText(ClientInfo: TClientSocketInfo; const AMessage: string);
    procedure BroadcastWebSocketText(const AMessage: string);
    procedure SendToWebSocketRoute(const AURI, AMessage: string);
    property Port: Integer read FPort write FPort;
    property TrustProxyHeaders: Boolean read FTrustProxyHeaders write FTrustProxyHeaders;
    property TrustedProxies: TStringList read FTrustedProxies write FTrustedProxies;
    property Timeout: Integer read FTimeout write FTimeout;
    property MaxConcurrentConnections: Integer read FMaxConcurrentConnections
      write FMaxConcurrentConnections;
    property ParallelProcessing: Boolean read FParallelProcessing write FParallelProcessing;
    property WorkerCount: Integer read FWorkerCount write FWorkerCount;
    property RouteManager: TRouteManager read FRouteManager;
    property EnableEventInfo: Boolean read FEnableEventInfo write FEnableEventInfo;
    property OnRequest: TOnRequest read FOnRequest write FOnRequest;
    property OnResponse: TOnResponse read FOnResponse write FOnResponse;
    property OnWebSocketMessage: TWebSocketMessageEvent read FOnWebSocketMessage write FOnWebSocketMessage;
    property OnWsAttach: TWsClientProc read FOnWsAttach write FOnWsAttach;
    property OnWsDetach: TWsClientProc read FOnWsDetach write FOnWsDetach;
    property CorsEnabled: Boolean read FCorsEnabled write FCorsEnabled;
    property CorsAllowedOrigins: TStringList read FCorsAllowedOrigins;
    property CorsAllowedMethods: TStringList read FCorsAllowedMethods;
    property CorsAllowedHeaders: TStringList read FCorsAllowedHeaders;
    property CorsExposeHeaders: TStringList read FCorsExposeHeaders;
    property CorsAllowCredentials: Boolean read FCorsAllowCredentials write FCorsAllowCredentials;
    property CorsMaxAge: Integer read FCorsMaxAge write FCorsMaxAge;
    property Running: Boolean read FRunning;
  end;

{$ENDIF}

implementation

{$IFDEF LINUX}

const
  CLIENT_IN = EPOLLIN or EPOLLET or EPOLLONESHOT;
  CLIENT_OUT = EPOLLOUT or EPOLLET or EPOLLONESHOT;
  LISTEN_IN = EPOLLIN or EPOLLET;
  EPOLL_SEND_CHUNK = 65536;
  { Teto da fila de envio WebSocket por conexao. }
  EPOLL_WS_MAX_QUEUE = 4 * 1024 * 1024;

constructor TEpollCtx.Create;
begin
  inherited Create;
  Fd := BADGER_SYS_INVALID;
end;

destructor TEpollCtx.Destroy;
begin
  if SendBuf <> nil then
  begin
    FreeMem(SendBuf);
    SendBuf := nil;
  end;
  FreeAndNil(WsParser);
  FreeAndNil(Parser);
  FreeAndNil(Conn);
  FreeAndNil(RouteParams);
  FreeAndNil(RespHeaders);
  inherited Destroy;
end;

constructor TBadgerEpollWorker.Create(AOwner: TBadgerEpoll; AListenFd: Integer);
begin
  FOwner := AOwner;
  FListenFd := AListenFd;
  FEpfd := BADGER_SYS_INVALID;
  FPipeR := BADGER_SYS_INVALID;
  FPipeW := BADGER_SYS_INVALID;
  FConns := TList.Create;
  FConnsLock := TCriticalSection.Create;
  inherited Create(True);
  FreeOnTerminate := False;
end;

destructor TBadgerEpollWorker.Destroy;
begin
  DrainConns;
  FreeAndNil(FListenCtx);
  FreeAndNil(FPipeCtx);
  BadgerSysClose(FPipeR);
  BadgerSysClose(FPipeW);
  BadgerSysClose(FEpfd);
  FConns.Free;
  FConnsLock.Free;
  inherited Destroy;
end;

procedure TBadgerEpollWorker.SignalStop;
var
  B: Byte;
begin
  Terminate;
  B := 1;
  if FPipeW >= 0 then
    BadgerSysWrite(FPipeW, @B, 1);
end;

procedure TBadgerEpollWorker.DrainConns;
var
  I: Integer;
  Ctx: TEpollCtx;
begin
  FConnsLock.Acquire;
  try
    for I := 0 to FConns.Count - 1 do
    begin
      Ctx := TEpollCtx(FConns[I]);
      if Ctx.IsWebSocket then
        FOwner.DetachWs(Ctx);
      if Ctx.Fd >= 0 then
      begin
        BadgerSysEpollDel(FEpfd, Ctx.Fd);
        BadgerSysClose(Ctx.Fd);
        Ctx.Fd := BADGER_SYS_INVALID;
      end;
      Ctx.Free;
      FOwner.DecActive;
    end;
    FConns.Clear;
  finally
    FConnsLock.Release;
  end;
end;

procedure TBadgerEpollWorker.CloseCtx(Ctx: TEpollCtx);
var
  I: Integer;
  Live: Boolean;
begin
  if Ctx = nil then
    Exit;
  Live := True;
  if Ctx.Kind = ekConn then
  begin
    FConnsLock.Acquire;
    try
      I := FConns.IndexOf(Ctx);
      if I < 0 then
        Live := False
      else
        FConns.Delete(I);
    finally
      FConnsLock.Release;
    end;
    if not Live then
      Exit;
    FOwner.DecActive;
    if Ctx.IsWebSocket then
      FOwner.DetachWs(Ctx);
  end;
  if Ctx.Fd >= 0 then
  begin
    BadgerSysEpollDel(FEpfd, Ctx.Fd);
    BadgerSysClose(Ctx.Fd);
    Ctx.Fd := BADGER_SYS_INVALID;
  end;
  Ctx.Free;
end;

procedure TBadgerEpollWorker.Arm(Ctx: TEpollCtx; Events: Cardinal);
begin
  BadgerSysEpollCtl(FEpfd, EPOLL_CTL_MOD, Ctx.Fd, Events, Ctx);
end;

procedure TBadgerEpollWorker.PrepareSend(Ctx: TEpollCtx; Data: Pointer; Len: Integer);
begin
  if Ctx.SendBuf <> nil then
  begin
    FreeMem(Ctx.SendBuf);
    Ctx.SendBuf := nil;
  end;
  Ctx.SendLen := Len;
  Ctx.SendPos := 0;
  if Len <= 0 then
  begin
    if Ctx.IsWebSocket then
      Arm(Ctx, CLIENT_IN)
    else
      CloseCtx(Ctx);
    Exit;
  end;
  GetMem(Ctx.SendBuf, Len);
  Move(Data^, Ctx.SendBuf^, Len);
  HandleWrite(Ctx);
end;

procedure TBadgerEpollWorker.QueueWsFrame(Ctx: TEpollCtx; const Frame: TBadgerWsBytes);
var
  StartNow: Boolean;
begin
  if (Frame = '') or not Assigned(Ctx.WsClient) then
    Exit;
  StartNow := False;
  Ctx.WsClient.IOLock.Acquire;
  try
    if Ctx.WsSendBusy then
    begin
      { Sem teto, cliente lento com broadcaster rapido acumula memoria sem limite. }
      if Length(Ctx.WsQueue) + Length(Frame) > EPOLL_WS_MAX_QUEUE then
        Ctx.WsQueue := ''
      else
        Ctx.WsQueue := Ctx.WsQueue + Frame;
    end
    else
    begin
      Ctx.WsSendBusy := True;
      StartNow := True;
    end;
  finally
    Ctx.WsClient.IOLock.Release;
  end;
  if StartNow then
    PrepareSend(Ctx, Pointer(Frame), Length(Frame));
end;

procedure TBadgerEpollWorker.BeginWs(Ctx: TEpollCtx; const URI, WSKey: string);
var
  Info: TClientSocketInfo;
  Left: AnsiString;
  Wire: AnsiString;
begin
  Ctx.IsWebSocket := True;
  Ctx.CloseAfterSend := False;
  Ctx.WsRecvArmed := False;
  Ctx.WsSendBusy := True;
  Ctx.WsParser := TBadgerWsParser.Create;
  if Assigned(Ctx.Parser) then
  begin
    Left := Ctx.Parser.Leftover;
    if Left <> '' then
      Ctx.WsParser.Feed(@Left[1], Length(Left));
    FreeAndNil(Ctx.Parser);
  end;
  Info := TClientSocketInfo.Create;
  Info.Socket := nil;
  Info.Ctx := Ctx;
  Info.URI := URI;
  Info.InUse := True;
  Ctx.WsClient := Info;
  FOwner.AttachWs(Info);
  Logger.Info('WebSocket handshake established for ' + URI);
  Wire := BadgerWsHandshakeMessage(WSKey);
  PrepareSend(Ctx, Pointer(Wire), Length(Wire));
end;

function TBadgerEpollWorker.ProcessWsFrames(Ctx: TEpollCtx): Boolean;
begin
  Result := Assigned(Ctx.WsParser);
  if not Result then
    Exit;
  while Ctx.WsParser.TryParse do
  begin
    if Ctx.WsParser.Failed then
    begin
      { Fecha com status em vez de derrubar o TCP calado. }
      QueueWsFrame(Ctx, BadgerWsCloseFrame(Ctx.WsParser.CloseCode));
      Result := False;
      Exit;
    end;
    { Fragmento intermediario: bytes consumidos, mensagem ainda incompleta. }
    if not Ctx.WsParser.Complete then
      Continue;
    case Ctx.WsParser.Opcode of
      WS_OP_CLOSE:
        begin
          QueueWsFrame(Ctx, BadgerWsCloseFrame(WS_CLOSE_NORMAL));
          Result := False;
          Exit;
        end;
      WS_OP_TEXT:
        if Assigned(FOwner.FOnWebSocketMessage) and Assigned(Ctx.WsClient) then
          FOwner.FOnWebSocketMessage(Ctx.WsClient, Ctx.WsClient.URI, Ctx.WsParser.Text);
      WS_OP_BINARY:
        begin
          { Sem evento binario na API publica: recusa explicita em vez de descarte. }
          QueueWsFrame(Ctx, BadgerWsCloseFrame(WS_CLOSE_UNSUPPORTED));
          Result := False;
          Exit;
        end;
      WS_OP_PING:
        QueueWsFrame(Ctx, BadgerWsPongFrame(Ctx.WsParser.Payload));
    end;
  end;
end;

procedure TBadgerEpollWorker.HandleWsRead(Ctx: TEpollCtx);
var
  N: Integer;
begin
  Ctx.LastActivity := BadgerSysTick;
  while True do
  begin
    N := BadgerSysRecv(Ctx.Fd, @Ctx.RecvBuf[0], SizeOf(Ctx.RecvBuf));
    if N = 0 then
    begin
      CloseCtx(Ctx);
      Exit;
    end;
    if N < 0 then
    begin
      if BadgerSysWouldBlock then
      begin
        if (Ctx.SendLen > 0) and (Ctx.SendPos < Ctx.SendLen) then
          Exit;
        Arm(Ctx, CLIENT_IN);
        Exit;
      end;
      CloseCtx(Ctx);
      Exit;
    end;
    if not Assigned(Ctx.WsParser) then
    begin
      CloseCtx(Ctx);
      Exit;
    end;
    Ctx.WsParser.Feed(@Ctx.RecvBuf[0], N);
    if not ProcessWsFrames(Ctx) then
    begin
      CloseCtx(Ctx);
      Exit;
    end;
  end;
end;

procedure TBadgerEpollWorker.RecycleKeepAlive(Ctx: TEpollCtx);
var
  Left: AnsiString;
begin
  if Ctx.SendBuf <> nil then
  begin
    FreeMem(Ctx.SendBuf);
    Ctx.SendBuf := nil;
  end;
  Ctx.SendLen := 0;
  Ctx.SendPos := 0;
  Ctx.CloseAfterSend := False;
  Ctx.LastActivity := BadgerSysTick;
  if Assigned(Ctx.Parser) then
  begin
    Left := Ctx.Parser.Leftover;
    Ctx.Parser.Reset;
    if Left <> '' then
    begin
      if Ctx.Parser.Feed(@Left[1], Length(Left)) then
      begin
        FinishHttp(Ctx);
        Exit;
      end;
    end;
  end;
  Arm(Ctx, CLIENT_IN);
end;

procedure TBadgerEpollWorker.FinishHttp(Ctx: TEpollCtx);
var
  Pipe: TBadgerDispatchPipeline;
  D: TBadgerDispatchResult;
  Serial: Boolean;
begin
  BadgerAssignDispatchPipeline(Pipe, FOwner.RouteManager, FOwner.FMiddlewares,
    FOwner.FAfterMiddlewares, FOwner.FCorsEnabled, FOwner.FCorsAllowedOrigins,
    FOwner.FCorsAllowedMethods, FOwner.FCorsAllowedHeaders, FOwner.FCorsExposeHeaders,
    FOwner.FCorsAllowCredentials, FOwner.FCorsMaxAge, FOwner.FEnableEventInfo,
    FOwner.FOnRequest, FOwner.FOnResponse);
  Pipe.TrustProxyHeaders := FOwner.FTrustProxyHeaders;
  Pipe.TrustedProxies := FOwner.FTrustedProxies;
  D.Wire := '';
  D.CloseConn := True;
  D.WsUpgrade := False;
  D.WsURI := '';
  D.WsKey := '';
  Serial := not FOwner.FParallelProcessing;
  if Serial then
    FOwner.FSerialLock.Acquire;
  try
    D := BadgerDispatchHttp(Ctx.Parser, Ctx.Conn, Ctx.RouteParams, Ctx.RespHeaders, Pipe);
  finally
    if Serial then
      FOwner.FSerialLock.Release;
  end;
  if D.WsUpgrade then
  begin
    BeginWs(Ctx, D.WsURI, D.WsKey);
    Exit;
  end;
  Ctx.CloseAfterSend := D.CloseConn;
  PrepareSend(Ctx, Pointer(D.Wire), Length(D.Wire));
end;

procedure TBadgerEpollWorker.HandleWrite(Ctx: TEpollCtx);
var
  N: Integer;
  P: PAnsiChar;
  Next: TBadgerWsBytes;
begin
  Ctx.LastActivity := BadgerSysTick;
  while Ctx.SendPos < Ctx.SendLen do
  begin
    P := PAnsiChar(Ctx.SendBuf) + Ctx.SendPos;
    N := Ctx.SendLen - Ctx.SendPos;
    if N > EPOLL_SEND_CHUNK then
      N := EPOLL_SEND_CHUNK;
    N := BadgerSysSend(Ctx.Fd, P, N);
    if N > 0 then
    begin
      Inc(Ctx.SendPos, N);
      Continue;
    end;
    if (N < 0) and BadgerSysWouldBlock then
    begin
      Arm(Ctx, CLIENT_OUT);
      Exit;
    end;
    CloseCtx(Ctx);
    Exit;
  end;
  if Ctx.IsWebSocket then
  begin
    Next := '';
    if Assigned(Ctx.WsClient) then
    begin
      Ctx.WsClient.IOLock.Acquire;
      try
        Ctx.WsSendBusy := False;
        Next := Ctx.WsQueue;
        Ctx.WsQueue := '';
        if Next <> '' then
          Ctx.WsSendBusy := True;
      finally
        Ctx.WsClient.IOLock.Release;
      end;
    end;
    if Next <> '' then
      PrepareSend(Ctx, Pointer(Next), Length(Next))
    else if not Ctx.WsRecvArmed then
    begin
      Ctx.WsRecvArmed := True;
      if not ProcessWsFrames(Ctx) then
        CloseCtx(Ctx)
      else if (Ctx.SendLen = 0) or (Ctx.SendPos >= Ctx.SendLen) then
        Arm(Ctx, CLIENT_IN);
    end
    else
      Arm(Ctx, CLIENT_IN);
    Exit;
  end;
  if Ctx.CloseAfterSend then
    CloseCtx(Ctx)
  else
    RecycleKeepAlive(Ctx);
end;

procedure TBadgerEpollWorker.HandleRead(Ctx: TEpollCtx);
var
  N: Integer;
begin
  if Ctx.IsWebSocket then
  begin
    HandleWsRead(Ctx);
    Exit;
  end;
  Ctx.LastActivity := BadgerSysTick;
  while True do
  begin
    N := BadgerSysRecv(Ctx.Fd, @Ctx.RecvBuf[0], SizeOf(Ctx.RecvBuf));
    if N = 0 then
    begin
      CloseCtx(Ctx);
      Exit;
    end;
    if N < 0 then
    begin
      if BadgerSysWouldBlock then
      begin
        Arm(Ctx, CLIENT_IN);
        Exit;
      end;
      CloseCtx(Ctx);
      Exit;
    end;
    if not Assigned(Ctx.Parser) then
    begin
      CloseCtx(Ctx);
      Exit;
    end;
    if Ctx.Parser.Feed(@Ctx.RecvBuf[0], N) then
    begin
      FinishHttp(Ctx);
      Exit;
    end;
  end;
end;

procedure TBadgerEpollWorker.AcceptLoop;
var
  Fd: Integer;
  IP: string;
  Ctx: TEpollCtx;
begin
  while FOwner.FRunning and not Terminated do
  begin
    Fd := BadgerSysAccept(FListenFd, IP);
    if Fd < 0 then
    begin
      if BadgerSysWouldBlock then
        Break;
      Break;
    end;
    if not FOwner.CanAccept then
    begin
      BadgerSysClose(Fd);
      Continue;
    end;
    BadgerSysSetNonBlock(Fd);
    BadgerSysSetNoDelay(Fd);
    Ctx := TEpollCtx.Create;
    Ctx.Kind := ekConn;
    Ctx.Fd := Fd;
    Ctx.Worker := Self;
    Ctx.Parser := TBadgerHttpParser.Create;
    Ctx.Conn := TBadgerConn.Create;
    Ctx.Conn.RemoteIP := IP;
    Ctx.RouteParams := TStringList.Create;
    Ctx.RespHeaders := TStringList.Create;
    Ctx.LastActivity := BadgerSysTick;
    FConnsLock.Acquire;
    try
      FConns.Add(Ctx);
    finally
      FConnsLock.Release;
    end;
    FOwner.IncActive;
    if BadgerSysEpollCtl(FEpfd, EPOLL_CTL_ADD, Fd, CLIENT_IN, Ctx) < 0 then
      CloseCtx(Ctx)
    else
      { ET: request bytes may already be queued before ADD — no edge would fire. }
      HandleRead(Ctx);
  end;
end;

procedure TBadgerEpollWorker.ScanIdle;
var
  I: Integer;
  Ctx: TEpollCtx;
  NowTick: LongWord;
  Limit: Integer;
  Dead: TList;
begin
  Limit := FOwner.FTimeout;
  if Limit <= 0 then
    Exit;
  NowTick := BadgerSysTick;
  Dead := TList.Create;
  try
    FConnsLock.Acquire;
    try
      for I := 0 to FConns.Count - 1 do
      begin
        Ctx := TEpollCtx(FConns[I]);
        if Ctx.IsWebSocket then
          Continue;
        if (Ctx.SendLen > 0) and (Ctx.SendPos < Ctx.SendLen) then
          Continue;
        if (NowTick - Ctx.LastActivity) >= LongWord(Limit) then
          Dead.Add(Ctx);
      end;
    finally
      FConnsLock.Release;
    end;
    for I := 0 to Dead.Count - 1 do
      CloseCtx(TEpollCtx(Dead[I]));
  finally
    Dead.Free;
  end;
end;

procedure TBadgerEpollWorker.Execute;
var
  Events: array[0..255] of TBadgerEpollEvent;
  N, I: Integer;
  Ctx: TEpollCtx;
  LastScan: LongWord;
  Dummy: array[0..7] of Byte;
begin
  FEpfd := BadgerSysEpollCreate;
  if FEpfd < 0 then
    Exit;
  if not BadgerSysPipe(FPipeR, FPipeW) then
    Exit;

  FPipeCtx := TEpollCtx.Create;
  FPipeCtx.Kind := ekPipe;
  FPipeCtx.Fd := FPipeR;
  BadgerSysEpollCtl(FEpfd, EPOLL_CTL_ADD, FPipeR, EPOLLIN, FPipeCtx);

  FListenCtx := TEpollCtx.Create;
  FListenCtx.Kind := ekListen;
  FListenCtx.Fd := FListenFd;
  BadgerSysEpollCtl(FEpfd, EPOLL_CTL_ADD, FListenFd, LISTEN_IN, FListenCtx);

  LastScan := BadgerSysTick;
  while not Terminated and FOwner.FRunning do
  begin
    N := BadgerSysEpollWait(FEpfd, @Events[0], Length(Events), 250);
    if N < 0 then
    begin
      if BadgerSysInterrupted then
        Continue;
      Break;
    end;
    for I := 0 to N - 1 do
    begin
      Ctx := TEpollCtx(Events[I].data.ptr);
      if Ctx = nil then
        Continue;
      case Ctx.Kind of
        ekPipe:
          begin
            BadgerSysRecv(FPipeR, @Dummy[0], SizeOf(Dummy));
            Exit;
          end;
        ekListen:
          AcceptLoop;
        ekConn:
          if (Events[I].events and (EPOLLERR or EPOLLHUP)) <> 0 then
            CloseCtx(Ctx)
          else if (Events[I].events and EPOLLOUT) <> 0 then
            HandleWrite(Ctx)
          else
            HandleRead(Ctx);
      end;
    end;
    if Integer(BadgerSysTick - LastScan) >= 250 then
    begin
      ScanIdle;
      LastScan := BadgerSysTick;
    end;
  end;
end;

{ TBadgerEpoll }

function TBadgerEpoll.WorkerCountDefault: Integer;
begin
{$IFDEF FPC}
  Result := GetCPUCount;
{$ELSE}
  Result := TThread.ProcessorCount;
{$ENDIF}
  if Result < 2 then
    Result := 2;
  if Result > 16 then
    Result := 16;
end;

procedure TBadgerEpoll.InitCorsDefaults;
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

procedure TBadgerEpoll.InitPipeline;
begin
  FRouteManager := TRouteManager.Create;
  FMiddlewares := TList.Create;
  FAfterMiddlewares := TList.Create;
  FMiddlewareLock := TCriticalSection.Create;
  FOwnsPipeline := True;
  InitCorsDefaults;
end;

procedure TBadgerEpoll.FreePipeline;
var
  I: Integer;
begin
  if not FOwnsPipeline then
  begin
    FRouteManager := nil;
    FMiddlewares := nil;
    FAfterMiddlewares := nil;
    FMiddlewareLock := nil;
    FCorsAllowedOrigins := nil;
    FCorsAllowedMethods := nil;
    FCorsAllowedHeaders := nil;
    FCorsExposeHeaders := nil;
    Exit;
  end;
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

procedure TBadgerEpoll.AdoptPipeline(ARouteManager: TRouteManager;
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

procedure TBadgerEpoll.AddMiddleware(Middleware: TMiddlewareProc);
begin
  FMiddlewareLock.Acquire;
  try
    FMiddlewares.Add(TMiddlewareWrapper.Create(Middleware));
  finally
    FMiddlewareLock.Release;
  end;
end;

procedure TBadgerEpoll.AddAfterMiddleware(Middleware: TAfterMiddlewareProc);
begin
  FMiddlewareLock.Acquire;
  try
    FAfterMiddlewares.Add(TAfterMiddlewareWrapper.Create(Middleware));
  finally
    FMiddlewareLock.Release;
  end;
end;

procedure TBadgerEpoll.AttachWs(Info: TClientSocketInfo);
begin
  if not Assigned(Info) then
    Exit;
  if Assigned(FOnWsAttach) then
  begin
    FOnWsAttach(Info);
    Exit;
  end;
  FWsLock.Acquire;
  try
    FWsClients.Add(Info);
  finally
    FWsLock.Release;
  end;
end;

procedure TBadgerEpoll.DetachWs(Ctx: TEpollCtx);
var
  Info: TClientSocketInfo;
  I: Integer;
begin
  if not Assigned(Ctx) then
    Exit;
  Info := Ctx.WsClient;
  Ctx.WsClient := nil;
  if not Assigned(Info) then
    Exit;
  Info.Ctx := nil;
  if Assigned(FOnWsDetach) then
  begin
    FOnWsDetach(Info);
    Exit;
  end;
  FWsLock.Acquire;
  try
    I := FWsClients.IndexOf(Info);
    if I >= 0 then
      FWsClients.Delete(I);
  finally
    FWsLock.Release;
  end;
  Info.Release;
end;

procedure TBadgerEpoll.SendWsText(ClientInfo: TClientSocketInfo; const AMessage: string);
var
  Ctx: TEpollCtx;
begin
  if not Assigned(ClientInfo) or (ClientInfo.Ctx = nil) then
    Exit;
  Ctx := TEpollCtx(ClientInfo.Ctx);
  if not Assigned(Ctx.Worker) then
    Exit;
  Ctx.Worker.QueueWsFrame(Ctx, BadgerWsTextFrame(AMessage));
end;

procedure TBadgerEpoll.BroadcastWebSocketText(const AMessage: string);
begin
  SendToWebSocketRoute('*', AMessage);
end;

procedure TBadgerEpoll.SendToWebSocketRoute(const AURI, AMessage: string);
var
  I: Integer;
  Snapshot: TList;
  Info: TClientSocketInfo;
begin
  Snapshot := TList.Create;
  try
    FWsLock.Acquire;
    try
      for I := 0 to FWsClients.Count - 1 do
      begin
        Info := TClientSocketInfo(FWsClients[I]);
        if Assigned(Info) and (Info.Ctx <> nil) and
           (SameText(Info.URI, AURI) or (AURI = '*')) then
        begin
          { Referencia garantida: o snapshot e usado fora do lock. }
          Info.AddRef;
          Snapshot.Add(Info);
        end;
      end;
    finally
      FWsLock.Release;
    end;
    for I := 0 to Snapshot.Count - 1 do
    begin
      try
        SendWsText(TClientSocketInfo(Snapshot[I]), AMessage);
      except
        on E: Exception do
          Logger.Error('SendToWebSocketRoute: ' + E.Message);
      end;
      TClientSocketInfo(Snapshot[I]).Release;
    end;
  finally
    Snapshot.Free;
  end;
end;

constructor TBadgerEpoll.Create;
begin
  inherited Create;
  FPort := 8081;
  FTimeout := 5000;
  FMaxConcurrentConnections := 500;
  FParallelProcessing := True;
  FEnableEventInfo := False;
  FWorkerCount := WorkerCountDefault;
  FSerialLock := TCriticalSection.Create;
  FActiveLock := TCriticalSection.Create;
  FWsLock := TCriticalSection.Create;
  FWsClients := TList.Create;
  InitPipeline;
end;

destructor TBadgerEpoll.Destroy;
begin
  Stop;
  FreePipeline;
  FreeAndNil(FWsClients);
  FreeAndNil(FWsLock);
  FreeAndNil(FSerialLock);
  FreeAndNil(FActiveLock);
  inherited Destroy;
end;

procedure TBadgerEpoll.IncActive;
begin
  FActiveLock.Acquire;
  try
    Inc(FActiveConnections);
  finally
    FActiveLock.Release;
  end;
end;

procedure TBadgerEpoll.DecActive;
begin
  FActiveLock.Acquire;
  try
    if FActiveConnections > 0 then
      Dec(FActiveConnections);
  finally
    FActiveLock.Release;
  end;
end;

function TBadgerEpoll.CanAccept: Boolean;
begin
  if FMaxConcurrentConnections <= 0 then
  begin
    Result := True;
    Exit;
  end;
  FActiveLock.Acquire;
  try
    Result := FActiveConnections < FMaxConcurrentConnections;
  finally
    FActiveLock.Release;
  end;
end;

function TBadgerEpoll.AllocListen: Integer;
begin
  Result := BadgerSysSocket;
  if Result < 0 then
    Exit;
  if not BadgerSysListen(Result, FPort) then
  begin
    BadgerSysClose(Result);
    Result := BADGER_SYS_INVALID;
  end;
end;

procedure TBadgerEpoll.Start;
var
  I: Integer;
  Fd: Integer;
begin
  if FRunning then
    Exit;
  if FWorkerCount < 1 then
    FWorkerCount := WorkerCountDefault;
  SetLength(FWorkers, FWorkerCount);
  FActiveConnections := 0;
  FRunning := True;
  for I := 0 to FWorkerCount - 1 do
  begin
    Fd := AllocListen;
    if Fd < 0 then
    begin
      FRunning := False;
      Stop;
      raise Exception.Create('epoll bind/listen failed on port ' + IntToStr(FPort));
    end;
    FWorkers[I] := TBadgerEpollWorker.Create(Self, Fd);
    FWorkers[I].Start;
  end;
  Logger.Info(Format('BadgerEpoll started port=%d workers=%d', [FPort, FWorkerCount]));
end;

procedure TBadgerEpoll.Stop;
var
  I: Integer;
  W: TBadgerEpollWorker;
begin
  if not FRunning and (Length(FWorkers) = 0) then
    Exit;
  FRunning := False;
  for I := 0 to High(FWorkers) do
    if Assigned(FWorkers[I]) then
      FWorkers[I].SignalStop;
  for I := 0 to High(FWorkers) do
  begin
    W := FWorkers[I];
    if W = nil then
      Continue;
    W.WaitFor;
    BadgerSysClose(W.FListenFd);
    W.FListenFd := BADGER_SYS_INVALID;
    W.Free;
    FWorkers[I] := nil;
  end;
  SetLength(FWorkers, 0);
end;

{$ENDIF}

end.
