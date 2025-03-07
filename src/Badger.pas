unit Badger;

interface

uses
  blcksock, httpsend, synsock, SyncObjs, Classes, sysutils, StrUtils, BadgerRouteManager,
  SyUtils, BadgerMethods, BadgerHttpStatus, BadgerTypes, Math;

type
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
    FMiddlewares: TList;
  protected
    function ParseRequestHeader(ClientSocket: TTCPBlockSocket): TStringList;
    function BuildHTTPResponse(StatusCode: Integer; Body: string; Stream: TStream; ContentType: string): string;
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
    FMiddlewares: TList;
    FPort: Integer;
    FNonBlockMode: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMiddleware(Middleware: TMiddlewareProc);
    procedure Start;
    procedure Stop;

    property Port: Integer read FPort write FPort;
    property OnLastRequest: TLastRequest read VLastRequest write VLastRequest;
    property OnLastResponse: TLastResponse read VLastResponse write VLastResponse;
    property NonBlockMode: Boolean read FNonBlockMode write FNonBlockMode;
  end;

implementation

{ TBadger }

constructor TBadger.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FCriticalSection := TCriticalSection.Create;
  FServerSocket := TTCPBlockSocket.Create;
  FRouteManager := TRouteManager.Create;
  FMethods := TBadgerMethods.Create;
  FMiddlewares := TList.Create;
  FPort := 8080;
  FNonBlockMode := True;
end;

destructor TBadger.Destroy;
var
  I: Integer;
begin
  Stop;
  FServerSocket.Free;
  FCriticalSection.Free;
  FRouteManager.Free;
  FMethods.Free;
  for I := 0 to FMiddlewares.Count - 1 do
    TObject(FMiddlewares[I]).Free;
  FMiddlewares.Free;
  inherited;
end;

procedure TBadger.AddMiddleware(Middleware: TMiddlewareProc);
begin
  FMiddlewares.Add(TMiddlewareWrapper.Create(Middleware));
end;

procedure TBadger.Start;
begin
  if not Terminated and not Suspended then
    Exit;
  FServerSocket.CloseSocket;
  FServerSocket.CreateSocket;
  FServerSocket.NonBlockMode := FNonBlockMode;
  FServerSocket.Bind('0.0.0.0', IntToStr(FPort));
  FServerSocket.Listen;
  Resume;
end;

procedure TBadger.Stop;
begin
  if Terminated or Suspended then
    Exit;
  Terminate;
  FServerSocket.CloseSocket;
  Sleep(100);
end;

procedure TBadger.Execute;
var
  ClientSocket: TTCPBlockSocket;
  ClientThread: TClientThread;
begin
  while not Terminated do
  begin
    if FServerSocket.CanRead(1000) then
    begin
      ClientSocket := TTCPBlockSocket.Create;
      try
        ClientSocket.Socket := FServerSocket.Accept;
        if ClientSocket.LastError = 0 then
        begin
          ClientThread := TClientThread.Create(ClientSocket, FRouteManager, FCriticalSection, FMethods, FMiddlewares, VLastRequest, VLastResponse);
        end
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
  for I := 0 to AMiddlewares.Count - 1 do
    FMiddlewares.Add(TMiddlewareWrapper.Create(TMiddlewareWrapper(AMiddlewares[I]).Middleware));
  Resume;
end;

destructor TClientThread.Destroy;
var
  I: Integer;
begin
  if Assigned(FClientSocket) then
  begin
    FClientSocket.CloseSocket;
    FClientSocket.Free;
  end;
  for I := 0 to FMiddlewares.Count - 1 do
    TObject(FMiddlewares[I]).Free;
  FMiddlewares.Free;
  inherited;
end;

function TClientThread.ParseRequestHeader(ClientSocket: TTCPBlockSocket): TStringList;
{var
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
end;}
var
  HeaderLine: string;
  SeparatorPos: Integer;
  Key, Value: string;
begin
  Result := TStringList.Create;
  try
    repeat
      HeaderLine := ClientSocket.RecvString(5000);
      if HeaderLine <> '' then
      begin
        SeparatorPos := Pos(':', HeaderLine);
        if SeparatorPos > 0 then
        begin
          Key := Trim(Copy(HeaderLine, 1, SeparatorPos - 1));
          Value := Trim(Copy(HeaderLine, SeparatorPos + 1, Length(HeaderLine)));
          Result.Add(Key + '=' + Value); // Adiciona no formato "Key=Value"
        end
        else
        begin
          // Caso não haja separador, adiciona como está (pode ser uma linha inválida)
          Result.Add(HeaderLine + '='); // Valor vazio
        end;
      end;
    until HeaderLine = '';
  except
    Result.Free;
    raise;
  end;
end;

function TClientThread.BuildHTTPResponse(StatusCode: Integer; Body: string; Stream: TStream; ContentType: string): string;
begin
  if Assigned(Stream) then
    Result := Format('HTTP/1.1 %d %s', [StatusCode, THTTPStatus.GetStatusText(StatusCode)]) + CRLF +
              'Content-Type: ' + ContentType + CRLF +
              'Content-Length: ' + IntToStr(Stream.Size) + CRLF + CRLF
  else
    Result := Format('HTTP/1.1 %d %s', [StatusCode, THTTPStatus.GetStatusText(StatusCode)]) + CRLF +
              'Content-Type: ' + IfThen(ContentType = '', 'text/plain', ContentType) + CRLF + CRLF + Body;
end;

procedure TClientThread.Execute;
  procedure Exec(Index: Integer; const Request: THTTPRequest; out Response: THTTPResponse);
  var
    Callback: TRoutingCallback;
    MethodPointer: TMethod;
  begin
    MethodPointer.Data := Self;
    MethodPointer.Code := Pointer(FRouteManager.FRoutes.Objects[Index]);
    Callback := TRoutingCallback(MethodPointer);
    Callback(Request, Response);
  end;

var
  Index, I, ContentLength, BytesReceived, TotalBytes: Integer;
  Req: THTTPRequest;
  Resp: THTTPResponse;
  MiddlewareWrapper: TMiddlewareWrapper;
  Headers: TStringList;
  Body: string;
  Buffer: array[0..8191] of Byte;
  TempBytes: TBytes;
  BytesRead: Integer;
  QueryParams: TStringList;
begin
  QueryParams := TStringList.Create;
  try
    Req.QueryParams := TStringList.Create;
    try
      if FClientSocket.LastError = 0 then
      begin
        FRequestLine := FClientSocket.RecvString(5000);
        if FMethods.ExtractMethodAndURI(FRequestLine, FMethod, FURI, QueryParams) then
        begin
          FCriticalSection.Enter;
          try
            if Assigned(VLastRequest) then
              VLastRequest(FRequestLine);

            Headers := ParseRequestHeader(FClientSocket);
            try
              ContentLength := StrToIntDef(Headers.Values['Content-Length'], 0);
              if ContentLength > 0 then
              begin
                SetLength(TempBytes, ContentLength);
                TotalBytes := 0;
                while TotalBytes < ContentLength do
                begin
                  BytesReceived := FClientSocket.RecvBufferEx(@Buffer, Min(ContentLength - TotalBytes, SizeOf(Buffer)), 5000);
                  if BytesReceived <= 0 then
                  begin
                    if FClientSocket.LastError <> 0 then
                      raise Exception.Create('Erro ao ler o corpo: ' + FClientSocket.LastErrorDesc);
                    Break;
                  end;
                  Move(Buffer, TempBytes[TotalBytes], BytesReceived);
                  Inc(TotalBytes, BytesReceived);
                end;
                if TotalBytes > 0 then
                  Body := TEncoding.UTF8.GetString(TempBytes, 0, TotalBytes)
                else
                  Body := '';
              end
              else
                Body := '';

              Req.Socket := FClientSocket;
              Req.URI := FURI;
              Req.Method := FMethod;
              Req.RequestLine := FRequestLine;
              Req.Headers := Headers;
              Req.Body := Body;
              Req.QueryParams.Assign(QueryParams);

              for I := 0 to FMiddlewares.Count - 1 do
              begin
                MiddlewareWrapper := TMiddlewareWrapper(FMiddlewares[I]);
                if not MiddlewareWrapper.Middleware(Req, Resp) then
                begin
                  FClientSocket.SendString(BuildHTTPResponse(Resp.StatusCode, Resp.Body, Resp.Stream, Resp.ContentType));
                  if Assigned(Resp.Stream) then
                  begin
                    Resp.Stream.Position := 0;
                    repeat
                      BytesRead := Resp.Stream.Read(Buffer, SizeOf(Buffer));
                      if BytesRead > 0 then
                        FClientSocket.SendBuffer(@Buffer, BytesRead);
                    until BytesRead = 0;
                    FreeAndNil(Resp.Stream);
                  end;
                  FResponseLine := Format('HTTP/1.1 %d %s', [Resp.StatusCode, Resp.Body]);
                  if Assigned(VLastResponse) then
                    VLastResponse(FResponseLine);
                  Exit;
                end;
              end;

              Index := FRouteManager.FRoutes.IndexOf(FURI);
              if Index <> -1 then
              begin
                Exec(Index, Req, Resp);
                FClientSocket.SendString(BuildHTTPResponse(Resp.StatusCode, Resp.Body, Resp.Stream, Resp.ContentType));
                if Assigned(Resp.Stream) then
                begin
                  Resp.Stream.Position := 0;
                  repeat
                    BytesRead := Resp.Stream.Read(Buffer, SizeOf(Buffer));
                    if BytesRead > 0 then
                      FClientSocket.SendBuffer(@Buffer, BytesRead);
                  until BytesRead = 0;
                  FreeAndNil(Resp.Stream);
                end;
                FResponseLine := Format('HTTP/1.1 %d %s', [Resp.StatusCode, Resp.Body]);
              end
              else
              begin
                FClientSocket.SendString(BuildHTTPResponse(HTTP_NOT_FOUND, 'Not Found', nil, 'text/plain'));
                FResponseLine := Format('HTTP/1.1 %d %s', [HTTP_NOT_FOUND, Resp.Body]);
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
        FClientSocket.SendString(BuildHTTPResponse(HTTP_INTERNAL_SERVER_ERROR, 'Internal Server Error: ' + E.Message, nil, 'text/plain'));
        FResponseLine := Format('HTTP/1.1 %d', [HTTP_INTERNAL_SERVER_ERROR]);
        if Assigned(VLastResponse) then
          VLastResponse(FResponseLine);
      end;
    end;
  finally
    FreeAndNil(QueryParams);
    FreeAndNil(Req.QueryParams);
    if Assigned(FClientSocket) then
    begin
      FClientSocket.CloseSocket;
      FreeAndNil(FClientSocket);
    end;
  end;
end;

end.
