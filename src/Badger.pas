unit Badger;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  blcksock, synsock, SyncObjs, Classes, SysUtils, BadgerRouteManager, BadgerMethods, BadgerTypes, Windows;

type
  TBadger = class(TThread)
  private
    FServerSocket: TTCPBlockSocket;
    FRouteManager: TRouteManager;
    FMethods: TBadgerMethods;
    FMiddlewares: TList;
    FPort: Integer;
    FNonBlockMode: Boolean;
    FTimeout: Integer;
    FOnRequest: TOnRequest;
    FOnResponse: TOnResponse;
    FParallelProcessing: Boolean;
    FMaxConcurrentConnections: Integer;
    FActiveConnections: Integer;
    FSocketLock: TCriticalSection;
    FShutdownEvent: TEvent;  // Novo: evento para shutdown controlado
    FIsShuttingDown: Boolean; // Novo: flag de shutdown
    FClientSockets: TList;   // Novo: lista para rastrear sockets cliente
  protected
    procedure Execute; override;
    function CanAcceptNewConnection: Boolean;
    procedure IncActiveConnections;
    procedure SafeCloseSocket;  // Novo: método para fechar socket com segurança
    procedure AddClientSocket(Socket: TTCPBlockSocket);  // Novo: adicionar socket à lista
    procedure RemoveClientSocket(Socket: TTCPBlockSocket);  // Novo: remover socket da lista
    procedure CleanupClientSockets;  // Novo: limpar todos os sockets cliente
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMiddleware(Middleware: TMiddlewareProc);
    procedure Start;
    procedure Stop;
    procedure DecActiveConnections;
    procedure NotifyClientSocketClosed(Socket: TTCPBlockSocket);  // Novo: notificar socket fechado

    property NonBlockMode: Boolean read FNonBlockMode write FNonBlockMode;
    property Port: Integer read FPort write FPort;
    property RouteManager: TRouteManager read FRouteManager;
    property Timeout: Integer read FTimeout write FTimeout default 5000;
    property ParallelProcessing: Boolean read FParallelProcessing write FParallelProcessing default False;
    property MaxConcurrentConnections: Integer read FMaxConcurrentConnections write FMaxConcurrentConnections default 100;
    property OnRequest: TOnRequest read FOnRequest write FOnRequest;
    property OnResponse: TOnResponse read FOnResponse write FOnResponse;
  end;

implementation

uses
  BadgerRequestHandler;

{ TBadger }

constructor TBadger.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;  // MUDANÇA: Não liberar automaticamente
  FServerSocket := TTCPBlockSocket.Create;
  FRouteManager := TRouteManager.Create;
  FMethods := TBadgerMethods.Create;
  FMiddlewares := TList.Create;
  FClientSockets := TList.Create;  // Novo: lista de sockets cliente
  FSocketLock := TCriticalSection.Create;
  FShutdownEvent := TEvent.Create(nil, True, False, '');  // Novo
  FPort := 8080;
  FNonBlockMode := True;
  FTimeout := 5000;
  FParallelProcessing := False;
  FMaxConcurrentConnections := 100;
  FActiveConnections := 0;
  FIsShuttingDown := False;  // Novo
  OutputDebugString(PChar('TBadger created'));
end;

destructor TBadger.Destroy;
var
  I: Integer;
  TimeoutCounter: Integer;
const
  MaxWaitTime = 5000;
begin
  OutputDebugString(PChar('TBadger.Destroy started'));

  // Garantir que Stop seja chamado
  if not FIsShuttingDown then
    Stop;

  // Limpar todos os sockets cliente primeiro
  CleanupClientSockets;

  // Aguardar conexões ativas se necessário
  if FParallelProcessing then
  begin
    OutputDebugString(PChar(Format('Destroying TBadger, waiting for active connections: %d', [FActiveConnections])));
    TimeoutCounter := 0;
    while (FActiveConnections > 0) and (TimeoutCounter < MaxWaitTime) do
    begin
      Sleep(100);
      Inc(TimeoutCounter, 100);
    end;
    if FActiveConnections > 0 then
      OutputDebugString(PChar(Format('Warning: Destroying TBadger with %d active connections remaining', [FActiveConnections])));
  end;

  // Liberar recursos com segurança
  try
    if Assigned(FServerSocket) then
    begin
      SafeCloseSocket;
      FreeAndNil(FServerSocket);
      OutputDebugString(PChar('FServerSocket freed'));
    end;
  except
    on E: Exception do
      OutputDebugString(PChar(Format('Error freeing FServerSocket: %s', [E.Message])));
  end;

  // Liberar outros recursos
  try
    FreeAndNil(FRouteManager);
    FreeAndNil(FMethods);

    if Assigned(FMiddlewares) then
    begin
      for I := 0 to FMiddlewares.Count - 1 do
        if Assigned(FMiddlewares[I]) then
          TObject(FMiddlewares[I]).Free;
      FreeAndNil(FMiddlewares);
    end;

    FreeAndNil(FClientSockets);  // Novo: liberar lista de sockets cliente
    FreeAndNil(FShutdownEvent);
    FreeAndNil(FSocketLock);
  except
    on E: Exception do
      OutputDebugString(PChar(Format('Error in destructor cleanup: %s', [E.Message])));
  end;

  OutputDebugString(PChar('TBadger destroyed'));
  inherited;
end;

procedure TBadger.SafeCloseSocket;
begin
  if not Assigned(FSocketLock) then Exit;

  FSocketLock.Acquire;
  try
    if Assigned(FServerSocket) then
    begin
      try
        if FServerSocket.Socket <> INVALID_SOCKET then
        begin
          FServerSocket.CloseSocket;
          OutputDebugString(PChar('Server socket closed safely'));
        end;
      except
        on E: Exception do
          OutputDebugString(PChar(Format('Error in SafeCloseSocket: %s', [E.Message])));
      end;
    end;
  finally
    FSocketLock.Release;
  end;
end;

procedure TBadger.AddClientSocket(Socket: TTCPBlockSocket);
begin
  if not Assigned(FClientSockets) or not Assigned(Socket) then Exit;

  FSocketLock.Acquire;
  try
    if FClientSockets.IndexOf(Socket) = -1 then
    begin
      FClientSockets.Add(Socket);
      OutputDebugString(PChar(Format('Added client socket. Total: %d', [FClientSockets.Count])));
    end;
  finally
    FSocketLock.Release;
  end;
end;

procedure TBadger.RemoveClientSocket(Socket: TTCPBlockSocket);
begin
  if not Assigned(FClientSockets) or not Assigned(Socket) then Exit;

  FSocketLock.Acquire;
  try
    FClientSockets.Remove(Socket);
    OutputDebugString(PChar(Format('Removed client socket. Total: %d', [FClientSockets.Count])));
  finally
    FSocketLock.Release;
  end;
end;

procedure TBadger.CleanupClientSockets;
var
  I: Integer;
  Socket: TTCPBlockSocket;
begin
  if not Assigned(FClientSockets) or not Assigned(FSocketLock) then Exit;

  OutputDebugString(PChar(Format('Cleaning up %d client sockets', [FClientSockets.Count])));

  FSocketLock.Acquire;
  try
    for I := FClientSockets.Count - 1 downto 0 do
    begin
      Socket := TTCPBlockSocket(FClientSockets[I]);
      if Assigned(Socket) then
      begin
        try
          if Socket.Socket <> INVALID_SOCKET then
            Socket.CloseSocket;
          Socket.Free;
          OutputDebugString(PChar(Format('Freed client socket %d', [I])));
        except
          on E: Exception do
            OutputDebugString(PChar(Format('Error freeing client socket %d: %s', [I, E.Message])));
        end;
      end;
    end;
    FClientSockets.Clear;
  finally
    FSocketLock.Release;
  end;

  OutputDebugString(PChar('Client sockets cleanup completed'));
end;

function TBadger.CanAcceptNewConnection: Boolean;
begin
  Result := (not FIsShuttingDown) and (FActiveConnections < FMaxConcurrentConnections);
end;

procedure TBadger.IncActiveConnections;
begin
  InterlockedIncrement(FActiveConnections);
  OutputDebugString(PChar(Format('Incremented active connections: %d', [FActiveConnections])));
end;

procedure TBadger.DecActiveConnections;
begin
  if not FIsShuttingDown then
  begin
    InterlockedDecrement(FActiveConnections);
    OutputDebugString(PChar(Format('Decremented active connections: %d', [FActiveConnections])));
  end
  else
    OutputDebugString(PChar('Warning: Server is shutting down, skipping DecActiveConnections'));
end;

procedure TBadger.NotifyClientSocketClosed(Socket: TTCPBlockSocket);
begin
  // Remover socket da lista quando o RequestHandler terminar
  RemoveClientSocket(Socket);
  DecActiveConnections;
end;

procedure TBadger.AddMiddleware(Middleware: TMiddlewareProc);
begin
  if not FIsShuttingDown and Assigned(FMiddlewares) then
    FMiddlewares.Add(TMiddlewareWrapper.Create(Middleware));
end;

procedure TBadger.Start;
begin
  if FIsShuttingDown then
  begin
    OutputDebugString(PChar('Cannot start: server is shutting down'));
    Exit;
  end;

  if not Terminated and not Suspended then
  begin
    OutputDebugString(PChar('Server already running, skipping Start'));
    Exit;
  end;

  if not Assigned(FSocketLock) or not Assigned(FServerSocket) then
  begin
    OutputDebugString(PChar('Cannot start: resources not available'));
    Exit;
  end;

  OutputDebugString(PChar('TBadger.Start: Acquiring socket lock'));
  FSocketLock.Acquire;
  try
    OutputDebugString(PChar('TBadger.Start: Closing existing socket'));
    FServerSocket.CloseSocket;
    OutputDebugString(PChar('TBadger.Start: Creating new socket'));
    FServerSocket.CreateSocket;
    FServerSocket.setLinger(True, 10000);
    FServerSocket.NonBlockMode := FNonBlockMode;
    OutputDebugString(PChar(Format('TBadger.Start: Binding to port %d', [FPort])));
    FServerSocket.Bind('0.0.0.0', IntToStr(FPort));
    if FServerSocket.LastError = 0 then
    begin
      OutputDebugString(PChar('TBadger.Start: Starting listen'));
      FServerSocket.Listen;
      OutputDebugString(PChar(Format('Server started on port %d', [FPort])));
    end
    else
    begin
      OutputDebugString(PChar(Format('Failed to bind port %d: %s', [FPort, FServerSocket.LastErrorDesc])));
      FServerSocket.CloseSocket;
      Exit;
    end;
  finally
    OutputDebugString(PChar('TBadger.Start: Releasing socket lock'));
    FSocketLock.Release;
  end;

  FShutdownEvent.ResetEvent;
  Resume;
end;

procedure TBadger.Stop;
var
  TimeoutCounter: Integer;
const
  MaxWaitTime = 10000;
begin
  OutputDebugString(PChar('TBadger.Stop: Starting shutdown sequence'));

  // Marcar como em processo de shutdown
  FIsShuttingDown := True;

  if Terminated or Suspended then
  begin
    OutputDebugString(PChar('Server already stopped or suspended'));
    Exit;
  end;

  // Sinalizar evento de shutdown
  if Assigned(FShutdownEvent) then
    FShutdownEvent.SetEvent;

  OutputDebugString(PChar('TBadger.Stop: Terminating thread'));
  Terminate;

  // Fechar socket primeiro para interromper Accept
  SafeCloseSocket;

  if FParallelProcessing then
  begin
    OutputDebugString(PChar(Format('Waiting for active connections to close. Current count: %d', [FActiveConnections])));
    TimeoutCounter := 0;
    while (FActiveConnections > 0) and (TimeoutCounter < MaxWaitTime) do
    begin
      Sleep(100);
      Inc(TimeoutCounter, 100);
    end;
    if FActiveConnections > 0 then
      OutputDebugString(PChar(Format('Warning: Timeout waiting for %d active connections to close', [FActiveConnections])));
  end;

  // Aguardar término da thread com timeout
  OutputDebugString(PChar('TBadger.Stop: Waiting for thread termination'));
  try
    // No Delphi, usar WaitFor sem timeout ou implementar timeout manual
    TimeoutCounter := 0;
    while not Finished and (TimeoutCounter < 5000) do
    begin
      Sleep(100);
      Inc(TimeoutCounter, 100);
    end;

    if not Finished then
    begin
      OutputDebugString(PChar('Warning: Thread termination timeout, forcing termination'));
      // Em último caso, forçar terminação (não recomendado, mas necessário)
    end
    else
    begin
      WaitFor; // Aguardar término final se a thread já finalizou
    end;
  except
    on E: Exception do
      OutputDebugString(PChar(Format('Error waiting for thread: %s', [E.Message])));
  end;

  OutputDebugString(PChar('TBadger.Stop: Server stopped successfully'));
end;

procedure TBadger.Execute;
var
  ClientSocket: TTCPBlockSocket;
  ResponseInfo: TResponseInfo;
begin
  OutputDebugString(PChar('TBadger.Execute: Server thread started'));

  while not Terminated and not FIsShuttingDown do
  begin
    try
      // Verificar se deve continuar executando
      if not Assigned(FSocketLock) or not Assigned(FServerSocket) then Break;

      FSocketLock.Acquire;
      try
        if Terminated or FIsShuttingDown then Break;

        if Assigned(FServerSocket) and (FServerSocket.Socket <> INVALID_SOCKET) and
           FServerSocket.CanRead(100) then  // Timeout menor para resposta mais rápida
        begin
          if FParallelProcessing and not CanAcceptNewConnection then
          begin
            OutputDebugString(PChar(Format('TBadger.Execute: Max concurrent connections reached: %d', [FActiveConnections])));
            Sleep(10);
            Continue;
          end;

          OutputDebugString(PChar('TBadger.Execute: Creating client socket'));
          ClientSocket := TTCPBlockSocket.Create;
          try
            OutputDebugString(PChar('TBadger.Execute: Accepting connection'));
            ClientSocket.Socket := FServerSocket.Accept;
            if ClientSocket.LastError = 0 then
            begin
              OutputDebugString(PChar(Format('TBadger.Execute: New connection accepted. Active connections: %d', [FActiveConnections + 1])));

              // Adicionar socket à lista de controle
              AddClientSocket(ClientSocket);

              if FParallelProcessing then
              begin
                IncActiveConnections;
                THTTPRequestHandler.CreateParallel(ClientSocket, FRouteManager, FMethods, FMiddlewares,
                                                  FTimeout, FOnRequest, FOnResponse, Self);
              end
              else
              begin
                THTTPRequestHandler.Create(ClientSocket, FRouteManager, FMethods, FMiddlewares,
                                           FTimeout, FOnRequest, FOnResponse);
                // Para processamento não paralelo, remover da lista após uso
                RemoveClientSocket(ClientSocket);
              end;
              ClientSocket := nil; // Evitar liberação dupla - agora controlado pela lista
            end
            else
            begin
              OutputDebugString(PChar(Format('TBadger.Execute: Error accepting connection: %s', [ClientSocket.LastErrorDesc])));
              if Assigned(FOnResponse) then
              begin
                ResponseInfo.StatusCode := 500;
                ResponseInfo.StatusText := 'Internal Server Error';
                ResponseInfo.Body := 'Error accepting connection: ' + ClientSocket.LastErrorDesc;
                FOnResponse(ResponseInfo);
              end;
            end;
          except
            on E: Exception do
            begin
              OutputDebugString(PChar(Format('TBadger.Execute: Exception in accept: %s', [E.Message])));
              if Assigned(FOnResponse) then
              begin
                ResponseInfo.StatusCode := 500;
                ResponseInfo.StatusText := 'Internal Server Error';
                ResponseInfo.Body := 'Exception in accept: ' + E.Message;
                FOnResponse(ResponseInfo);
              end;
            end;
          end;

          if Assigned(ClientSocket) then
          begin
            try
              // Remover da lista antes de liberar
              RemoveClientSocket(ClientSocket);
              ClientSocket.Free;
              OutputDebugString(PChar('TBadger.Execute: ClientSocket freed due to error'));
            except
              on E: Exception do
                OutputDebugString(PChar(Format('TBadger.Execute: Error freeing ClientSocket: %s', [E.Message])));
            end;
          end;
        end;
      finally
        FSocketLock.Release;
      end;

      if Terminated or FIsShuttingDown then Break;
      Sleep(10); // Evitar consumo excessivo de CPU

    except
      on E: Exception do
      begin
        OutputDebugString(PChar(Format('TBadger.Execute: Unexpected exception: %s', [E.Message])));
        Break; // Sair do loop em caso de erro crítico
      end;
    end;
  end;

  OutputDebugString(PChar('TBadger.Execute: Server thread terminated'));

  // Fechar socket final se necessário
  SafeCloseSocket;
end;

end.
