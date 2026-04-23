unit Badger;

{$I BadgerDefines.inc}

interface

uses
  {$IFDEF BADGER_WINDOWS}Windows, {$ENDIF}
  {$IFDEF FPC}
    {$IFDEF UNIX}BaseUnix, Unix, {$ENDIF}
  {$ENDIF}
  blcksock, synsock, SyncObjs, Classes, SysUtils, BadgerRouteManager, BadgerMethods, BadgerTypes, BadgerLogger, BadgerUtils;

type
  TClientSocketInfo = class
    Socket: TTCPBlockSocket;
    InUse: Boolean;
  end;

  TBadger = class(TThread)
  private
    FServerSocket: TTCPBlockSocket;
    FRouteManager: TRouteManager;
    FMethods: TBadgerMethods;
    FMiddlewares: TList;
    FMiddlewareLock: TCriticalSection;
    FPort: Integer;
    FNonBlockMode: Boolean;
    FTimeout: Integer;
    FOnRequest: TOnRequest;
    FOnResponse: TOnResponse;
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
  protected
    procedure Execute; override;
    function CanAcceptNewConnection: Boolean;
    procedure IncActiveConnections;
    procedure SafeCloseSocket;
    procedure CloseClientSocketsForShutdown;
    procedure AddClientSocket(Socket: TTCPBlockSocket);
    procedure RemoveClientSocket(Socket: TTCPBlockSocket);
    procedure CleanupClientSockets;
    {$IFDEF UNIX}
    function WaitForThreadTermination(TimeoutMs: Integer): Boolean;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMiddleware(Middleware: TMiddlewareProc);
    procedure Start;
    procedure Stop;
    procedure DecActiveConnections;
    procedure NotifyClientSocketClosed(Socket: TTCPBlockSocket);
    property NonBlockMode: Boolean read FNonBlockMode write FNonBlockMode;
    property Port: Integer read FPort write FPort;
    property RouteManager: TRouteManager read FRouteManager;
    property Timeout: Integer read FTimeout write FTimeout default 5000;
    property ParallelProcessing: Boolean read FParallelProcessing write FParallelProcessing default False;
    property MaxConcurrentConnections: Integer read FMaxConcurrentConnections write FMaxConcurrentConnections default 100;
    property OnRequest: TOnRequest read FOnRequest write FOnRequest;
    property OnResponse: TOnResponse read FOnResponse write FOnResponse;
    property IsRunning: Boolean read FIsRunning;
    property EnableEventInfo: Boolean read FEnableEventInfo write FEnableEventInfo;
    property CorsEnabled: Boolean read FCorsEnabled write FCorsEnabled;
    property CorsAllowedOrigins: TStringList read FCorsAllowedOrigins;
    property CorsAllowedMethods: TStringList read FCorsAllowedMethods;
    property CorsAllowedHeaders: TStringList read FCorsAllowedHeaders;
    property CorsExposeHeaders: TStringList read FCorsExposeHeaders;
    property CorsAllowCredentials: Boolean read FCorsAllowCredentials write FCorsAllowCredentials;
    property CorsMaxAge: Integer read FCorsMaxAge write FCorsMaxAge;
  end;

implementation

uses
  BadgerRequestHandler;

{ TBadger }

constructor TBadger.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FServerSocket := TTCPBlockSocket.Create;
  FRouteManager := TRouteManager.Create;
  FMethods := TBadgerMethods.Create;
  FMiddlewares := TList.Create;
  FMiddlewareLock := TCriticalSection.Create;
  FClientSockets := TList.Create;
  FSocketLock := TCriticalSection.Create;
  FClientSocketsLock := TCriticalSection.Create;
  FShutdownEvent := TEvent.Create(nil, True, False, '');
  FPort := 8080;
  FNonBlockMode := True;
  FTimeout := 5000;
  FParallelProcessing := False;
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
      if Assigned(SocketInfo) and Assigned(SocketInfo.Socket) then
      begin
        try
          if not SocketInfo.InUse and (SocketInfo.Socket.Socket <> INVALID_SOCKET) then
          begin
            SocketInfo.Socket.CloseSocket;
  //          Logger.info(Format('Closed client socket %d', [I]));
          end;
        except
          on E: Exception do
            Logger.Error(Format('Error closing client socket %d: %s', [I, E.Message]));
        end;
        try
          SocketInfo.Free;
 //         Logger.info(Format('Freed client socket info %d', [I]));
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

{$IFDEF UNIX}
function TBadger.WaitForThreadTermination(TimeoutMs: Integer): Boolean;
begin
  WaitFor;
  Result := Finished;
end;
{$ENDIF}

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
 //   Logger.Info('Cannot start: resources not available');
    Exit;
  end;

//  Logger.Info('TBadger.Start: Acquiring socket lock');
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
      FServerSocket.NonBlockMode := FNonBlockMode;
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

  {$IFDEF FPC}
    inherited Start;
  {$ELSE}
    Resume;
  {$ENDIF}
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

    {$IFDEF UNIX}
    WaitFor;
    {$ENDIF}
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
  // OutputDebugString(PChar('TBadger.Execute: Server thread started'));
  FIsRunning := True;
  try
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

        ClientSocket := TTCPBlockSocket.Create;
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
              THTTPRequestHandler.CreateParallel(ClientSocket, FRouteManager, FMethods, FMiddlewares, FMiddlewareLock,
                                                FTimeout, FOnRequest, FOnResponse, Self, FEnableEventInfo);
              ClientSocket := nil;
            end
            else
            begin
              THTTPRequestHandler.Create(ClientSocket, FRouteManager, FMethods, FMiddlewares, FMiddlewareLock,
                                         FTimeout, FOnRequest, FOnResponse, Self, FEnableEventInfo);
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
