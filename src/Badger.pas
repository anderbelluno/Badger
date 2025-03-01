unit Badger;

interface

uses
  blcksock, httpsend, synsock, SyncObjs, Classes, sysutils, BadgerRouteManager, SyUtils, BadgerMethods;

type
  TLastRequest  = procedure(Value: String) of object;
  TLastResponse = procedure(Value: String) of object;

  // Definição de tipos para middlewares
  THTTPRequest = record
    Socket: TTCPBlockSocket;
    URI, Method, RequestLine: string;
    Headers: TStringList;
  end;
  THTTPResponse = record
    StatusCode: Integer;
    Body: string;
  end;
  TMiddlewareProc = function(Request: THTTPRequest; out Response: THTTPResponse): Boolean of object;

  // Classe para encapsular o middleware (necessário para TList no Delphi 7)
  TMiddlewareWrapper = class
  public
    Middleware: TMiddlewareProc;
    constructor Create(AMiddleware: TMiddlewareProc);
  end;

  TClientThread = class(TThread)
  private
    VLastRequest: TLastRequest;
    VLastResponse: TLastResponse;
    FClientSocket: TTCPBlockSocket;
    FRouteManager: TRouteManager;
    FURI: string;
    FMethod: string;
    FRequestLine: string;
    FResponseLine: string;
    FCriticalSection: TCriticalSection;
    FMethods: TBadgerMethods;
    FMiddlewares: TList; // TList em vez de TList<TMiddlewareProc>
  protected
    function ParseRequestHeader(ClientSocket: TTCPBlockSocket): TStringList;
  public
    constructor Create(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager; ACriticalSection: TCriticalSection;
                       AMethods: TBadgerMethods; AMiddlewares: TList; ALastRequest: TLastRequest = nil; ALastResponse: TLastResponse = nil);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TBadger = class(TThread)
  private
    VLastRequest: TLastRequest;
    VLastResponse: TLastResponse;
    FServerSocket: TTCPBlockSocket;
    FRouteManager: TRouteManager;
    FCriticalSection: TCriticalSection;
    FMethods: TBadgerMethods;
    FMiddlewares: TList; // TList para middlewares
    FPort: Integer;
    FNonBlockMode: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMiddleware(Middleware: TMiddlewareProc);

    property Port: Integer read FPort write FPort;
    property OnLastRequest: TLastRequest read VLastRequest write VLastRequest;
    property OnLastResponse: TLastResponse read VLastResponse write VLastResponse;
    property NonBlockMode: Boolean read FNonBlockMode write FNonBlockMode;
  end;

implementation

{ TMiddlewareWrapper }

constructor TMiddlewareWrapper.Create(AMiddleware: TMiddlewareProc);
begin
  inherited Create;
  Middleware := AMiddleware;
end;

{ TBadger }

constructor TBadger.Create;
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FCriticalSection := TCriticalSection.Create;
  FServerSocket := TTCPBlockSocket.Create;
  FRouteManager := TRouteManager.Create;
  FMethods := TBadgerMethods.Create;
  FMiddlewares := TList.Create;
end;

destructor TBadger.Destroy;
var
  I: Integer;
begin
  FServerSocket.Free;
  FCriticalSection.Free;
  FRouteManager.Free;
  FMethods.Free;
  for I := 0 to FMiddlewares.Count - 1 do
    TObject(FMiddlewares[I]).Free; // Liberar os wrappers
  FMiddlewares.Free;
  inherited;
end;

procedure TBadger.AddMiddleware(Middleware: TMiddlewareProc);
begin
  FMiddlewares.Add(TMiddlewareWrapper.Create(Middleware));
end;

procedure TBadger.Execute;
var
  ClientSocket: TTCPBlockSocket;
begin
  FServerSocket.CreateSocket;
  FServerSocket.NonBlockMode := FNonBlockMode;
  FServerSocket.Bind('0.0.0.0', IntToStr(FPort));
  FServerSocket.Listen;

  while not Terminated do
  begin
    if FServerSocket.CanRead(1000) then
    begin
      ClientSocket := TTCPBlockSocket.Create;
      try
        ClientSocket.Socket := FServerSocket.Accept;
        if ClientSocket.LastError = 0 then
          TClientThread.Create(ClientSocket, FRouteManager, FCriticalSection, FMethods, FMiddlewares, VLastRequest, VLastResponse)
        else
        begin
          if Assigned(VLastResponse) then
            VLastResponse('Error accepting connection: ' + ClientSocket.LastErrorDesc);
          ClientSocket.Free;
        end;
      except
        ClientSocket.Free;
        if Assigned(VLastResponse) then
          VLastResponse('Exception in accept: ' + Exception(ExceptObject).Message);
      end;
    end;
  end;
  FServerSocket.CloseSocket;
end;

{ TClientThread }

constructor TClientThread.Create(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager;
  ACriticalSection: TCriticalSection; AMethods: TBadgerMethods; AMiddlewares: TList;
  ALastRequest: TLastRequest = nil; ALastResponse: TLastResponse = nil);
var
  I: Integer;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FClientSocket := AClientSocket;
  FCriticalSection := ACriticalSection;
  VLastRequest := ALastRequest;
  VLastResponse := ALastResponse;
  FRouteManager := ARouteManager;
  FMethods := AMethods;
  FMiddlewares := TList.Create;
  // Copiar os middlewares do servidor
  for I := 0 to AMiddlewares.Count - 1 do
    FMiddlewares.Add(TMiddlewareWrapper.Create(TMiddlewareWrapper(AMiddlewares[I]).Middleware));
  Resume;
end;

destructor TClientThread.Destroy;
var
  I: Integer;
begin
  for I := 0 to FMiddlewares.Count - 1 do
    TObject(FMiddlewares[I]).Free; // Liberar os wrappers
  FMiddlewares.Free;
  inherited;
end;

function TClientThread.ParseRequestHeader(ClientSocket: TTCPBlockSocket): TStringList;
var
  HeaderLine: string;
begin
  Result := TStringList.Create;
  try
    repeat
      HeaderLine := ClientSocket.RecvString(5000);
      if HeaderLine <> '' then
        Result.Add(HeaderLine);
    until HeaderLine = '';
  except
    Result.Free;
    raise;
  end;
end;

procedure TClientThread.Execute;
  procedure Exec(Index: Integer);
  var
    Callback: TRoutingCallback;
    MethodPointer: TMethod;
  begin
    MethodPointer.Data := Self;
    MethodPointer.Code := Pointer(FRouteManager.FRoutes.Objects[Index]);
    Callback := TRoutingCallback(MethodPointer);
    Callback(FClientSocket, FURI, FMethod, FRequestLine);
  end;

var
  Index, I: Integer;
  Req: THTTPRequest;
  Resp: THTTPResponse;
  MiddlewareWrapper: TMiddlewareWrapper;
  Headers: TStringList;
  ResponseString: string; // Variável auxiliar para construir a resposta
begin
try
  try
    if FClientSocket.LastError = 0 then
    begin
      FRequestLine := FClientSocket.RecvString(5000);
      if FMethods.ExtractMethodAndURI(FRequestLine, FMethod, FURI) then
      begin
        FCriticalSection.Enter;
        try
          if Assigned(VLastRequest) then
            VLastRequest(FRequestLine);

          Headers := ParseRequestHeader(FClientSocket);
          try
            Req.Socket := FClientSocket;
            Req.URI := FURI;
            Req.Method := FMethod;
            Req.RequestLine := FRequestLine;
            Req.Headers := Headers;

            // Executar middlewares
            for I := 0 to FMiddlewares.Count - 1 do
            begin
              MiddlewareWrapper := TMiddlewareWrapper(FMiddlewares[I]);
              if not MiddlewareWrapper.Middleware(Req, Resp) then
              begin
                // Construir a resposta manualmente
                ResponseString := Format('HTTP/1.1 %d', [Resp.StatusCode]) + ' ' + Resp.Body + CRLF + 'Content-Type: text/plain' + CRLF + CRLF + Resp.Body;
                FClientSocket.SendString(ResponseString);
                FResponseLine := Format('HTTP/1.1 %d', [Resp.StatusCode]);
                if Assigned(VLastResponse) then
                  VLastResponse(FResponseLine);
                Exit;
              end;
            end;

            // Executar a rota
            Index := FRouteManager.FRoutes.IndexOf(FURI);
            if Index <> -1 then
            begin
              Exec(Index);
              FResponseLine := 'HTTP/1.1 200 OK';
            end
            else
            begin
              FClientSocket.SendString('HTTP/1.1 404 Not Found' + CRLF + 'Content-Type: text/plain' + CRLF + CRLF);
              FResponseLine := 'HTTP/1.1 404 Not Found';
            end;

            if Assigned(VLastResponse) then
              VLastResponse(FResponseLine);
          finally
            Headers.Free;
          end;
        finally
          FCriticalSection.Leave;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      FClientSocket.SendString('HTTP/1.1 500 Internal Server Error' + CRLF + 'Content-Type: text/plain' + CRLF + CRLF + E.Message);
      FResponseLine := 'HTTP/1.1 500 Internal Server Error';
      if Assigned(VLastResponse) then
        VLastResponse(FResponseLine);
    end;
  end;
finally
  FClientSocket.Free;
end;
end;

end.
