unit Badger;

{$I BadgerDefines.inc}

interface

uses
  {$IFDEF BADGER_WINDOWS}Windows, {$ENDIF}
  {$IFDEF FPC}
    {$IFDEF UNIX}BaseUnix, Unix, {$ENDIF}
  {$ENDIF}
  {$IF (DEFINED(LINUX) OR DEFINED(POSIX)) AND NOT DEFINED(FPC)}
  Posix.StrOpts,
  {$IFEND}
  blcksock, synsock, SyncObjs, Classes, SysUtils, BadgerRouteManager, BadgerMethods, BadgerTypes, BadgerLogger, BadgerUtils;

type
  // On Delphi Linux, Synapse may use the macOS ioctl value for FIONREAD ($4004667F
  // instead of Linux's $541B), causing WaitingData to always return 0 and
  // RecvPacket to set WSAECONNRESET instead of reading available data.
  // Override WaitingData here to use the correct Linux constant directly.
  {$IF (DEFINED(LINUX) OR DEFINED(POSIX)) AND NOT DEFINED(FPC)}
  TBadgerClientSocket = class(TTCPBlockSocket)
  public
    function WaitingData: Integer; override;
  end;
  {$ELSE}
  TBadgerClientSocket = TTCPBlockSocket;
  {$IFEND}



  TBadger = class(TThread)
  private
    FServerSocket: TTCPBlockSocket;
    FRouteManager: TRouteManager;
    FMethods: TBadgerMethods;
    FMiddlewares: TList;
    FAfterMiddlewares: TList;
    FMiddlewareLock: TCriticalSection;
    FPort: Integer;
    FNonBlockMode: Boolean;
    FTimeout: Integer;
    FOnRequest: TOnRequest;
    FOnResponse: TOnResponse;
    FOnWebSocketMessage: TWebSocketMessageEvent;
    FParallelProcessing: Boolean;
    FMaxConcurrentConnections: Integer;
    FActiveConnections: Integer;
    FSocketLock: TCriticalSection;
    FClientSocketsLock: TCriticalSection;
    FShutdownEvent: TEvent;
    FIsShuttingDown: Boolean;
    FClientSockets: TList;
    FIsRunning: Boolean;
    FEnableEventInfo: Boolean;
    FCorsEnabled: Boolean;
    FCorsAllowedOrigins: TStringList;
    FCorsAllowedMethods: TStringList;
    FCorsAllowedHeaders: TStringList;
    FCorsExposeHeaders: TStringList;
    FCorsAllowCredentials: Boolean;
    FCorsMaxAge: Integer;
    FUseIOCP: Boolean;
    {$IFDEF BADGER_WINDOWS}
    FIocp: TObject;
    procedure StartIocpEngine;
    {$ENDIF}
  protected
    procedure Execute; override;
    function CanAcceptNewConnection: Boolean;
    procedure IncActiveConnections;
    procedure SafeCloseSocket;
    procedure CloseClientSocketsForShutdown;
    procedure AddClientSocket(Socket: TTCPBlockSocket);
    procedure RemoveClientSocket(Socket: TTCPBlockSocket);
    procedure CleanupClientSockets;
    procedure DeliverWebSocketText(Info: TClientSocketInfo; const AMessage: string);
    {$IFDEF BADGER_WINDOWS}
    procedure IocpWsAttach(Info: TClientSocketInfo);
    procedure IocpWsDetach(Info: TClientSocketInfo);
    procedure IocpWsMessage(ClientInfo: TClientSocketInfo; const URI, AMessage: string);
    {$ENDIF}
    {$IF DEFINED(UNIX) OR DEFINED(LINUX) OR DEFINED(POSIX)}
    function WaitForThreadTermination(TimeoutMs: Integer): Boolean;
    {$IFEND}
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMiddleware(Middleware: TMiddlewareProc);
    procedure AddAfterMiddleware(Middleware: TAfterMiddlewareProc);
    procedure Start;
    procedure Stop;
    procedure DecActiveConnections;
    procedure NotifyClientSocketClosed(Socket: TTCPBlockSocket);
    procedure SendWebSocketTextFrame(Socket: TTCPBlockSocket; const AMessage: string); overload;
    procedure SendWebSocketTextFrame(ClientInfo: TClientSocketInfo; const AMessage: string); overload;
    procedure BroadcastWebSocketText(const AMessage: string);
    procedure SendToWebSocketRoute(const AURI, AMessage: string);
    procedure SetClientSocketURI(Socket: TTCPBlockSocket; const AURI: string);
    function GetClientSocketInfo(Socket: TTCPBlockSocket): TClientSocketInfo;
    property NonBlockMode: Boolean read FNonBlockMode write FNonBlockMode;
    property Port: Integer read FPort write FPort;
    property RouteManager: TRouteManager read FRouteManager;
    property Timeout: Integer read FTimeout write FTimeout default 5000;
    { Windows/IOCP default True (dispatch on workers). False serializes routes.
      Classic: True = one handler thread per connection. }
    property ParallelProcessing: Boolean read FParallelProcessing write FParallelProcessing;
    property MaxConcurrentConnections: Integer read FMaxConcurrentConnections write FMaxConcurrentConnections default 100;
    property OnRequest: TOnRequest read FOnRequest write FOnRequest;
    property OnResponse: TOnResponse read FOnResponse write FOnResponse;
    property OnWebSocketMessage: TWebSocketMessageEvent read FOnWebSocketMessage write FOnWebSocketMessage;
    property IsRunning: Boolean read FIsRunning;
    property EnableEventInfo: Boolean read FEnableEventInfo write FEnableEventInfo;
    property CorsEnabled: Boolean read FCorsEnabled write FCorsEnabled;
    property CorsAllowedOrigins: TStringList read FCorsAllowedOrigins;
    property CorsAllowedMethods: TStringList read FCorsAllowedMethods;
    property CorsAllowedHeaders: TStringList read FCorsAllowedHeaders;
    property CorsExposeHeaders: TStringList read FCorsExposeHeaders;
    property CorsAllowCredentials: Boolean read FCorsAllowCredentials write FCorsAllowCredentials;
    property CorsMaxAge: Integer read FCorsMaxAge write FCorsMaxAge;
    { Windows: default True (IOCP). False forces Synapse+select.
      Ignored on other OS (Synapse until epoll). }
    property UseIOCP: Boolean read FUseIOCP write FUseIOCP;
  end;

implementation

uses
  BadgerRequestHandler, BadgerWebSocket{$IFDEF BADGER_WINDOWS}, BadgerIOCP{$ENDIF};

{$IF (DEFINED(LINUX) OR DEFINED(POSIX)) AND NOT DEFINED(FPC)}
{ TBadgerClientSocket }

function TBadgerClientSocket.WaitingData: Integer;
const
  LINUX_FIONREAD = $541B;
var
  x: Integer;
begin
  x := 0;
  if Posix.StrOpts.ioctl(FSocket, LINUX_FIONREAD, @x) = 0 then
    Result := x
  else
    Result := 0;
  if Result > 65536 then
    Result := 65536;
end;
{$IFEND}

{ TBadger }

constructor TBadger.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FServerSocket := TTCPBlockSocket.Create;
  FRouteManager := TRouteManager.Create;
  FMethods := TBadgerMethods.Create;
  FMiddlewares := TList.Create;
  FAfterMiddlewares := TList.Create;
  FMiddlewareLock := TCriticalSection.Create;
  FClientSockets := TList.Create;
  FSocketLock := TCriticalSection.Create;
  FClientSocketsLock := TCriticalSection.Create;
  FShutdownEvent := TEvent.Create(nil, True, False, '');
  FPort := 8080;
  FNonBlockMode := True;
  FTimeout := 5000;
  FMaxConcurrentConnections := 100;
  FActiveConnections := 0;
  FIsShuttingDown := False;
  FEnableEventInfo := True;
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
  FCorsExposeHeaders.Clear;
  FCorsAllowCredentials := False;
  FCorsMaxAge := 600;
  {$IFDEF BADGER_WINDOWS}
  FUseIOCP := True;
  FParallelProcessing := True;
  FIocp := nil;
  {$ELSE}
  FUseIOCP := False;
  FParallelProcessing := False;
  {$ENDIF}

  Logger.Info('TBadger created');
end;

destructor TBadger.Destroy;
var
  I: Integer;
  TimeoutCounter: Integer;
const
  MaxWaitTime = 15000;
begin

  if not FIsShuttingDown then
  begin
    try
      Stop;
    except
      on E: Exception do
        Logger.Error(Format('Error in Stop during Destroy: %s', [E.Message]));
    end;
  end;

  if FParallelProcessing then
  begin
   // Logger.Info(Format('Waiting for active connections: %d', [FActiveConnections]));
    TimeoutCounter := 0;
    while (FActiveConnections > 0) and (TimeoutCounter < MaxWaitTime) do
    begin
      Sleep(100);
      Inc(TimeoutCounter, 100);
    end;
    if FActiveConnections > 0 then
   //   Logger.Info(Format('Warning: %d active connections remaining', [FActiveConnections]));
  end;

  try
    CleanupClientSockets;
  except
    on E: Exception do
      Logger.Error(Format('Error in CleanupClientSockets: %s', [E.Message]));
  end;

  try
    if Assigned(FServerSocket) then
    begin
      if FServerSocket.Socket <> INVALID_SOCKET then
        FServerSocket.CloseSocket;
      FreeAndNil(FServerSocket);
    end;
  except
    on E: Exception do
      Logger.Error(Format('Error freeing FServerSocket: %s', [E.Message]));
  end;

  {$IFDEF BADGER_WINDOWS}
  try
    if Assigned(FIocp) then
      FreeAndNil(FIocp);
  except
    on E: Exception do
      Logger.Error(Format('Error freeing IOCP engine: %s', [E.Message]));
  end;
  {$ENDIF}

  try
    if Assigned(FRouteManager) then FreeAndNil(FRouteManager);
  except
    on E: Exception do
      Logger.Error(Format('Error freeing FRouteManager: %s', [E.Message]));
  end;

  try
    if Assigned(FMethods) then FreeAndNil(FMethods);
  except
    on E: Exception do
      Logger.Error(Format('Error freeing FMethods: %s', [E.Message]));
  end;

  try
    if Assigned(FMiddlewares) then
    begin
      for I := 0 to FMiddlewares.Count - 1 do
        if Assigned(FMiddlewares[I]) then
          TObject(FMiddlewares[I]).Free;
      FreeAndNil(FMiddlewares);
    end;
  except
    on E: Exception do
      Logger.Error(Format('Error freeing FMiddlewares: %s', [E.Message]));
  end;

  try
    if Assigned(FAfterMiddlewares) then
    begin
      for I := 0 to FAfterMiddlewares.Count - 1 do
        if Assigned(FAfterMiddlewares[I]) then
          TObject(FAfterMiddlewares[I]).Free;
      FreeAndNil(FAfterMiddlewares);
    end;
  except
    on E: Exception do
      Logger.Error(Format('Error freeing FAfterMiddlewares: %s', [E.Message]));
  end;

  try
    if Assigned(FMiddlewareLock) then FreeAndNil(FMiddlewareLock);
  except
    on E: Exception do
      Logger.Error(Format('Error freeing FMiddlewareLock: %s', [E.Message]));
  end;

  try
    if Assigned(FClientSockets) then FreeAndNil(FClientSockets);
  except
    on E: Exception do
      Logger.Error(Format('Error freeing FClientSockets: %s', [E.Message]));
  end;

  try
    if Assigned(FShutdownEvent) then FreeAndNil(FShutdownEvent);
  except
    on E: Exception do
      Logger.Error(Format('Error freeing FShutdownEvent: %s', [E.Message]));
  end;

  try
    if Assigned(FSocketLock) then FreeAndNil(FSocketLock);
  except
    on E: Exception do
      Logger.Error(Format('Error freeing FSocketLock: %s', [E.Message]));
  end;

  try
    if Assigned(FClientSocketsLock) then FreeAndNil(FClientSocketsLock);
  except
    on E: Exception do
      Logger.Error(Format('Error freeing FClientSocketsLock: %s', [E.Message]));
  end;

  try
    if Assigned(FCorsAllowedOrigins) then FreeAndNil(FCorsAllowedOrigins);
    if Assigned(FCorsAllowedMethods) then FreeAndNil(FCorsAllowedMethods);
    if Assigned(FCorsAllowedHeaders) then FreeAndNil(FCorsAllowedHeaders);
    if Assigned(FCorsExposeHeaders) then FreeAndNil(FCorsExposeHeaders);
  except
    on E: Exception do
      Logger.Error(Format('Error freeing CORS lists: %s', [E.Message]));
  end;

  inherited;
end;

procedure TBadger.SafeCloseSocket;
begin
  if not Assigned(FSocketLock) or not Assigned(FServerSocket) then Exit;

  FSocketLock.Acquire;
  try
    if FServerSocket.Socket <> INVALID_SOCKET then
    try
      FServerSocket.CloseSocket;
     // Logger.info('Server socket closed safely');
    except
      on E: Exception do
        Logger.Error(Format('Error in SafeCloseSocket: %s', [E.Message]));
    end;
  finally
    FSocketLock.Release;
  end;
end;

procedure TBadger.AddClientSocket(Socket: TTCPBlockSocket);
var
  SocketInfo: TClientSocketInfo;
begin
  if not Assigned(FClientSockets) or not Assigned(FClientSocketsLock) or not Assigned(Socket) then Exit;

  FClientSocketsLock.Acquire;
  try
    SocketInfo := TClientSocketInfo.Create;
    SocketInfo.Socket := Socket;
    SocketInfo.InUse := True;
    SocketInfo.URI := '';
    FClientSockets.Add(SocketInfo);
  //  Logger.info(Format('Added client socket. Total: %d', [FClientSockets.Count]));
  finally
    FClientSocketsLock.Release;
  end;
end;

procedure TBadger.RemoveClientSocket(Socket: TTCPBlockSocket);
var
  I: Integer;
  SocketInfo: TClientSocketInfo;
begin
  if not Assigned(FClientSockets) or not Assigned(FClientSocketsLock) or not Assigned(Socket) then Exit;

  FClientSocketsLock.Acquire;
  try
    for I := 0 to FClientSockets.Count - 1 do
    begin
      SocketInfo := TClientSocketInfo(FClientSockets[I]);
      if Assigned(SocketInfo) and (SocketInfo.Socket = Socket) then
      begin
        FClientSockets.Delete(I);
        SocketInfo.Free;
    //    Logger.info(Format('Removed client socket. Total: %d', [FClientSockets.Count]));
        Break;
      end;
    end;
  finally
    FClientSocketsLock.Release;
  end;
end;

procedure TBadger.CleanupClientSockets;
var
  I: Integer;
  SocketInfo: TClientSocketInfo;
begin
  if not Assigned(FClientSockets) or not Assigned(FClientSocketsLock) then Exit;

//  Logger.info(Format('Cleaning up %d client sockets', [FClientSockets.Count]));

  FClientSocketsLock.Acquire;
  try
    for I := FClientSockets.Count - 1 downto 0 do
    begin
      SocketInfo := TClientSocketInfo(FClientSockets[I]);
      if Assigned(SocketInfo) then
      begin
        if Assigned(SocketInfo.Socket) then
        begin
          try
            if not SocketInfo.InUse and (SocketInfo.Socket.Socket <> INVALID_SOCKET) then
              SocketInfo.Socket.CloseSocket;
          except
            on E: Exception do
              Logger.Error(Format('Error closing client socket %d: %s', [I, E.Message]));
          end;
        end;
        try
          SocketInfo.Free;
        except
          on E: Exception do
            Logger.Error(Format('Error freeing client socket info %d: %s', [I, E.Message]));
        end;
      end;
    end;
    FClientSockets.Clear;
  finally
    FClientSocketsLock.Release;
  end;

//  Logger.info('Client sockets cleanup completed');
end;

procedure TBadger.CloseClientSocketsForShutdown;
var
  I: Integer;
  SocketInfo: TClientSocketInfo;
begin
  if not Assigned(FClientSockets) or not Assigned(FClientSocketsLock) then Exit;

  FClientSocketsLock.Acquire;
  try
    for I := FClientSockets.Count - 1 downto 0 do
    begin
      SocketInfo := TClientSocketInfo(FClientSockets[I]);
      if Assigned(SocketInfo) and Assigned(SocketInfo.Socket) then
      begin
        try
          if SocketInfo.Socket.Socket <> INVALID_SOCKET then
            SocketInfo.Socket.CloseSocket;
        except
          on E: Exception do
            Logger.Error(Format('Error closing client socket during shutdown %d: %s', [I, E.Message]));
        end;
      end;
    end;
  finally
    FClientSocketsLock.Release;
  end;
end;

{$IF DEFINED(UNIX) OR DEFINED(LINUX) OR DEFINED(POSIX)}
function TBadger.WaitForThreadTermination(TimeoutMs: Integer): Boolean;
begin
  WaitFor;
  Result := Finished;
end;
{$IFEND}

function TBadger.CanAcceptNewConnection: Boolean;
begin
  Result := (not FIsShuttingDown) and (FActiveConnections < FMaxConcurrentConnections);
end;

procedure TBadger.IncActiveConnections;
begin
{$IFDEF Delphi2009Plus}
  TInterlocked.Increment(FActiveConnections)
{$ELSE}
  InterlockedIncrement(FActiveConnections);
{$ENDIF}

//  Logger.info(Format('Incremented active connections: %d', [FActiveConnections]));
end;

procedure TBadger.DecActiveConnections;
var
  NewValue: Integer;
begin
  {$IFDEF Delphi2009Plus}
    NewValue := TInterlocked.Decrement(FActiveConnections);
  {$ELSE}
    NewValue := InterlockedDecrement(FActiveConnections);
  {$ENDIF}

  if NewValue < 0 then
  begin
    {$IFDEF Delphi2009Plus}
      TInterlocked.Exchange(FActiveConnections, 0);
    {$ELSE}
      InterlockedExchange(FActiveConnections, 0);
    {$ENDIF}
  end;
end;

procedure TBadger.NotifyClientSocketClosed(Socket: TTCPBlockSocket);
begin
  RemoveClientSocket(Socket);
  DecActiveConnections;
end;

procedure TBadger.SendWebSocketTextFrame(Socket: TTCPBlockSocket; const AMessage: string);
var
  Frame: AnsiString;
begin
  if not Assigned(Socket) or (Socket.Socket = INVALID_SOCKET) then Exit;
  Frame := BadgerWsTextFrame(AMessage);
  if Frame = '' then Exit;
  Socket.SendBuffer(Pointer(Frame), Length(Frame));
end;

procedure TBadger.SendWebSocketTextFrame(ClientInfo: TClientSocketInfo; const AMessage: string);
begin
  DeliverWebSocketText(ClientInfo, AMessage);
end;

procedure TBadger.DeliverWebSocketText(Info: TClientSocketInfo; const AMessage: string);
begin
  if not Assigned(Info) then Exit;
  if Assigned(Info.Socket) then
  begin
    if Info.Socket.Socket = INVALID_SOCKET then Exit;
    Info.IOLock.Acquire;
    try
      SendWebSocketTextFrame(Info.Socket, AMessage);
    finally
      Info.IOLock.Release;
    end;
  end
{$IFDEF BADGER_WINDOWS}
  else if Assigned(FIocp) then
    TBadgerIOCP(FIocp).SendWsText(Info, AMessage);
{$ENDIF}
end;

procedure TBadger.BroadcastWebSocketText(const AMessage: string);
var
  I: Integer;
  SocketInfo: TClientSocketInfo;
begin
  if not Assigned(FClientSockets) or not Assigned(FClientSocketsLock) then Exit;

  FClientSocketsLock.Acquire;
  try
    for I := 0 to FClientSockets.Count - 1 do
    begin
      SocketInfo := TClientSocketInfo(FClientSockets[I]);
      try
        DeliverWebSocketText(SocketInfo, AMessage);
      except
      end;
    end;
  finally
    FClientSocketsLock.Release;
  end;
end;

procedure TBadger.SetClientSocketURI(Socket: TTCPBlockSocket; const AURI: string);
var
  I: Integer;
  SocketInfo: TClientSocketInfo;
begin
  if not Assigned(FClientSockets) or not Assigned(FClientSocketsLock) then Exit;

  FClientSocketsLock.Acquire;
  try
    for I := 0 to FClientSockets.Count - 1 do
    begin
      SocketInfo := TClientSocketInfo(FClientSockets[I]);
      if Assigned(SocketInfo) and (SocketInfo.Socket = Socket) then
      begin
        SocketInfo.URI := AURI;
        Break;
      end;
    end;
  finally
    FClientSocketsLock.Release;
  end;
end;

function TBadger.GetClientSocketInfo(Socket: TTCPBlockSocket): TClientSocketInfo;
var
  I: Integer;
begin
  Result := nil;
  if not Assigned(FClientSockets) or not Assigned(FClientSocketsLock) then Exit;

  FClientSocketsLock.Acquire;
  try
    for I := 0 to FClientSockets.Count - 1 do
      if TClientSocketInfo(FClientSockets[I]).Socket = Socket then
      begin
        Result := TClientSocketInfo(FClientSockets[I]);
        Break;
      end;
  finally
    FClientSocketsLock.Release;
  end;
end;

procedure TBadger.SendToWebSocketRoute(const AURI, AMessage: string);
var
  I: Integer;
  SocketInfo: TClientSocketInfo;
begin
  if not Assigned(FClientSockets) or not Assigned(FClientSocketsLock) then Exit;

  FClientSocketsLock.Acquire;
  try
    for I := 0 to FClientSockets.Count - 1 do
    begin
      SocketInfo := TClientSocketInfo(FClientSockets[I]);
      if Assigned(SocketInfo) and
         (SameText(SocketInfo.URI, AURI) or (AURI = '*')) then
      begin
        try
          DeliverWebSocketText(SocketInfo, AMessage);
        except
        end;
      end;
    end;
  finally
    FClientSocketsLock.Release;
  end;
end;

procedure TBadger.AddMiddleware(Middleware: TMiddlewareProc);
begin
  if not Assigned(FMiddlewareLock) then
    Exit;

  FMiddlewareLock.Acquire;
  try
    if not FIsShuttingDown and Assigned(FMiddlewares) then
      FMiddlewares.Add(TMiddlewareWrapper.Create(Middleware));
  finally
    FMiddlewareLock.Release;
  end;
end;

procedure TBadger.AddAfterMiddleware(Middleware: TAfterMiddlewareProc);
begin
  if not Assigned(FMiddlewareLock) then
    Exit;

  FMiddlewareLock.Acquire;
  try
    if not FIsShuttingDown and Assigned(FAfterMiddlewares) then
      FAfterMiddlewares.Add(TAfterMiddlewareWrapper.Create(Middleware));
  finally
    FMiddlewareLock.Release;
  end;
end;

{$IFDEF BADGER_WINDOWS}
procedure TBadger.IocpWsAttach(Info: TClientSocketInfo);
begin
  if not Assigned(FClientSockets) or not Assigned(FClientSocketsLock) or not Assigned(Info) then
    Exit;
  FClientSocketsLock.Acquire;
  try
    FClientSockets.Add(Info);
  finally
    FClientSocketsLock.Release;
  end;
end;

procedure TBadger.IocpWsDetach(Info: TClientSocketInfo);
var
  I: Integer;
begin
  if not Assigned(Info) then
    Exit;
  if Assigned(FClientSockets) and Assigned(FClientSocketsLock) then
  begin
    FClientSocketsLock.Acquire;
    try
      I := FClientSockets.IndexOf(Info);
      if I >= 0 then
        FClientSockets.Delete(I);
    finally
      FClientSocketsLock.Release;
    end;
  end;
  Info.Free;
end;

procedure TBadger.IocpWsMessage(ClientInfo: TClientSocketInfo; const URI, AMessage: string);
begin
  if Assigned(FOnWebSocketMessage) then
    FOnWebSocketMessage(ClientInfo, URI, AMessage);
end;

procedure TBadger.StartIocpEngine;
var
  Eng: TBadgerIOCP;
begin
  if not Assigned(FIocp) then
    FIocp := TBadgerIOCP.Create;
  Eng := TBadgerIOCP(FIocp);
  Eng.AdoptPipeline(FRouteManager, FMiddlewares, FAfterMiddlewares, FMiddlewareLock,
    FCorsAllowedOrigins, FCorsAllowedMethods, FCorsAllowedHeaders, FCorsExposeHeaders);
  Eng.Port := FPort;
  Eng.Timeout := FTimeout;
  Eng.MaxConcurrentConnections := FMaxConcurrentConnections;
  Eng.ParallelProcessing := FParallelProcessing;
  Eng.OnRequest := FOnRequest;
  Eng.OnResponse := FOnResponse;
  Eng.OnWebSocketMessage := IocpWsMessage;
  Eng.OnWsAttach := IocpWsAttach;
  Eng.OnWsDetach := IocpWsDetach;
  Eng.EnableEventInfo := FEnableEventInfo;
  Eng.CorsEnabled := FCorsEnabled;
  Eng.CorsAllowCredentials := FCorsAllowCredentials;
  Eng.CorsMaxAge := FCorsMaxAge;
  Eng.Start;
end;
{$ENDIF}

procedure TBadger.Start;
begin
  if FIsShuttingDown then
  begin
//    Logger.Info('Cannot start: server is shutting down');
    Exit;
  end;

  if not Terminated and not Suspended then
  begin
 //   Logger.Info('Server already running, skipping Start');
    Exit;
  end;

  if not Assigned(FSocketLock) or not Assigned(FServerSocket) then
  begin
    Exit;
  end;

  { Windows: IOCP when UseIOCP. Other OS: Synapse (epoll next on Linux). }
  {$IFDEF BADGER_WINDOWS}
  if FUseIOCP then
  begin
    StartIocpEngine;
    FShutdownEvent.ResetEvent;
    {$IF DEFINED(FPC) OR DEFINED(DelphiXEPlus)}
    inherited Start;
    {$ELSE}
    Resume;
    {$IFEND}
    Exit;
  end;
  {$ENDIF}

  FSocketLock.Acquire;
  try

    if FServerSocket.Socket <> INVALID_SOCKET then
    begin
      FServerSocket.CloseSocket;
//     Logger.Info('Previous server socket closed');
    end;

 //  Logger.Info('TBadger.Start: Configuring socket');
    try
      FServerSocket.EnableReuse(True);
      FServerSocket.CreateSocket;
      FServerSocket.setLinger(True, 10000);
//      Logger.Info(Format('TBadger.Start: Binding to port %d', [FPort]));
      FServerSocket.Bind('0.0.0.0', IntToStr(FPort));
      if FServerSocket.LastError = 0 then
      begin
//        Logger.Info('TBadger.Start: Starting listen');
        FServerSocket.Listen;
//        Logger.Info(Format('Server started on port %d', [FPort]));
      end
      else
      begin
 //       Logger.Info(Format('Failed to bind port %d: %s', [FPort, FServerSocket.LastErrorDesc]));
        FServerSocket.CloseSocket;
        Exit;
      end;
    except
      on E: Exception do
      begin
 //       Logger.Info(Format('Error configuring FServerSocket: %s', [E.Message]));
        FServerSocket.CloseSocket;
        Exit;
      end;
    end;
  finally
    FSocketLock.Release;
//    Logger.Info('TBadger.Start: Releasing socket lock');
  end;

  FShutdownEvent.ResetEvent;

  {$IF DEFINED(FPC) OR DEFINED(DelphiXEPlus)}
  inherited Start;
  {$ELSE}
  Resume;
  {$IFEND}
end;

procedure TBadger.Stop;
var
  TimeoutCounter: Integer;
  {$IFDEF BADGER_WINDOWS}
  WaitResult: DWORD;
  {$ENDIF}
const
  MaxWaitTime = 15000;
begin
//  Logger.Info('TBadger.Stop: Starting shutdown sequence');

  Logger.Debug('DBG: Stop called. Terminated= ' + BoolToStr(Terminated, True) + ' Suspended= ' + BoolToStr(Suspended, True));
  
  FIsShuttingDown := True;

  {$IFDEF BADGER_WINDOWS}
  if Assigned(FIocp) then
  begin
    TBadgerIOCP(FIocp).Stop;
    FIsRunning := False;
  end;
  {$ENDIF}

  if Terminated or Suspended then
  begin
    if not Terminated then
    begin
       SafeCloseSocket;
       Terminate;
       CleanupClientSockets;
    end;
    Exit;
  end;

  if Assigned(FShutdownEvent) then
    FShutdownEvent.SetEvent;

  SafeCloseSocket;
  CloseClientSocketsForShutdown;

  if FParallelProcessing then
  begin
 //   Logger.Info(Format('Waiting for active connections to close. Current count: %d', [FActiveConnections]));
    TimeoutCounter := 0;
    while (FActiveConnections > 0) and (TimeoutCounter < MaxWaitTime) do
    begin
      Sleep(100);
      Inc(TimeoutCounter, 100);
    end;
    if FActiveConnections > 0 then
 //     Logger.Info(Format('Warning: Timeout waiting for %d active connections to close', [FActiveConnections]));
  end;

  Terminate;

  try
    {$IFDEF BADGER_WINDOWS}
    // Windows: usa WaitForSingleObject
    TimeoutCounter := 0;
    WaitResult := WaitForSingleObject(Handle, 100);
    while (WaitResult = WAIT_TIMEOUT) and (TimeoutCounter < MaxWaitTime) do
    begin
      Sleep(100);
      Inc(TimeoutCounter, 100);
      WaitResult := WaitForSingleObject(Handle, 100);
    end;

    if WaitResult <> WAIT_OBJECT_0 then
    begin
  //    Logger.Warning('Thread termination timeout');
    end;
    {$ENDIF}

    {$IF DEFINED(UNIX) OR DEFINED(LINUX) OR DEFINED(POSIX)}
    WaitFor;
    {$IFEND}
  except
    on E: Exception do
    begin
      Logger.Error(Format('Error waiting for thread: %s', [E.Message]));
    end;
  end;

  CleanupClientSockets;
end;

procedure TBadger.Execute;
var
  ClientSocket: TTCPBlockSocket;
  ResponseInfo: TResponseInfo;
  Accepted: Boolean;
begin
  FIsRunning := True;
  try
    {$IFDEF BADGER_WINDOWS}
    if FUseIOCP then
    begin
      while not Terminated and not FIsShuttingDown do
      begin
        if Assigned(FShutdownEvent) then
          FShutdownEvent.WaitFor(200)
        else
          Sleep(200);
      end;
      Exit;
    end;
    {$ENDIF}

    while not Terminated and not FIsShuttingDown do
    begin
      try
        if not Assigned(FSocketLock) or not Assigned(FServerSocket) then Break;

        if FParallelProcessing and not CanAcceptNewConnection then
        begin
          // n�o segura lock global enquanto aguarda capacidade
          Sleep(10);
          Continue;
        end;

        if not Assigned(FServerSocket) or (FServerSocket.Socket = INVALID_SOCKET) then
          Continue;

        if not FServerSocket.CanRead(100) then
          Continue;

        ClientSocket := TBadgerClientSocket.Create;
        try
          Accepted := False;
          FSocketLock.Acquire;
          try
            if Terminated or FIsShuttingDown then
              Continue;

            if not Assigned(FServerSocket) or (FServerSocket.Socket = INVALID_SOCKET) then
              Continue;

            ClientSocket.Socket := FServerSocket.Accept;
            Accepted := (ClientSocket.LastError = 0);
          finally
            FSocketLock.Release;
          end;

          if Accepted then
          begin
            AddClientSocket(ClientSocket);

            if FParallelProcessing then
            begin
              IncActiveConnections;
              THTTPRequestHandler.CreateParallel(ClientSocket, FRouteManager, FMethods, FMiddlewares, FAfterMiddlewares,
                                                FMiddlewareLock, FTimeout, FOnRequest, FOnResponse, Self, FEnableEventInfo);
              ClientSocket := nil;
            end
            else
            begin
              THTTPRequestHandler.Create(ClientSocket, FRouteManager, FMethods, FMiddlewares, FAfterMiddlewares,
                                         FMiddlewareLock, FTimeout, FOnRequest, FOnResponse, Self, FEnableEventInfo);
              RemoveClientSocket(ClientSocket);
              ClientSocket := nil;
            end;
          end
          else
          begin
            if Assigned(FOnResponse) then
            begin
              FillChar(ResponseInfo, SizeOf(ResponseInfo), 0);
              ResponseInfo.Headers := TStringList.Create;
              try
                ResponseInfo.StatusCode := 500;
                ResponseInfo.StatusText := 'Internal Server Error';
                ResponseInfo.Body := 'Error accepting connection: ' + ClientSocket.LastErrorDesc;
                FOnResponse(ResponseInfo);
              finally
                ResponseInfo.Headers.Free;
              end;
            end;
          end;
        finally
          if Assigned(ClientSocket) then
          begin
            RemoveClientSocket(ClientSocket);
            try
              ClientSocket.CloseSocket;
              FreeAndNil(ClientSocket);
            except
              on E: Exception do
                ;
            end;
          end;
        end;

        if Terminated or FIsShuttingDown then Break;
        //Sleep(10);
      except
        on E: Exception do
        begin
          // OutputDebugString(PChar(Format('TBadger.Execute: Unexpected exception: %s', [E.Message])));
          Break;
        end;
      end;
    end;

    // OutputDebugString(PChar('TBadger.Execute: Server thread terminated'));
    SafeCloseSocket;
  finally
    FIsRunning := False;
  end;
end;

end.
