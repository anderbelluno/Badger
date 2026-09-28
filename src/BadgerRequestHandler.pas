unit BadgerRequestHandler;

{$I BadgerDefines.inc}

interface

uses
  blcksock, httpsend, synsock, SyncObjs, synachar, synautil, Math, Classes, SysUtils, StrUtils,
  BadgerRouteManager, BadgerMethods, BadgerHttpStatus, BadgerHttpParser, BadgerWebSocket, BadgerTypes, Badger, BadgerLogger;

type
  THTTPRequestHandler = class(TThread)
  private
    FOnRequest: TOnRequest;
    FOnResponse: TOnResponse;
    FClientSocket: TTCPBlockSocket;
    FRouteManager: TRouteManager;
    FURI: string;
    FMethod: string;
    FRequestLine: string;
    FMethods: TBadgerMethods;
    FMiddlewares: TList;
    FAfterMiddlewares: TList;
    FOwnMiddlewareObjects: Boolean;
    FMiddlewareLock: TCriticalSection;
    FTimeout: Integer;
    FParentServer: TBadger;
    FIsParallel: Boolean;
    FEnableEventInfo: Boolean;
    FSocketNotified: Boolean;
    { Prazo de headers do pedido corrente (ver RecvLineLimited). }
    FReqStart: LongWord;
    FHeaderTimeout: Integer;
    function HeaderExpired: Boolean;
    procedure RunAfterMiddlewares(var Req: THTTPRequest; var Resp: THTTPResponse);
  protected
    procedure ParseRequestHeader(ClientSocket: TTCPBlockSocket; aHeaders: TStringList);
    procedure ProcessWebSocketHandshakeAndLoop(ClientSocket: TTCPBlockSocket; const URI, WSKey: string);
    function BuildHTTPResponse(StatusCode: Integer; Body: string; Stream: TStream; ContentType: string; CloseConnection: Boolean; HeaderCustom: TStringList): string;
  public
    constructor Create(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager;
                      AMethods: TBadgerMethods; AMiddlewares, AAfterMiddlewares: TList; AMiddlewareLock: TCriticalSection; ATimeout: Integer;
                      AOnRequest: TOnRequest; AOnResponse: TOnResponse; AParentServer: TBadger; AEnableEventInfo: Boolean);
    constructor CreateParallel(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager;
                              AMethods: TBadgerMethods; AMiddlewares, AAfterMiddlewares: TList; AMiddlewareLock: TCriticalSection; ATimeout: Integer;
                              AOnRequest: TOnRequest; AOnResponse: TOnResponse; AParentServer: TBadger; AEnableEventInfo: Boolean);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

type
  EHeaderTooLarge = class(Exception);
  EHeaderTimeout = class(Exception);

const
  MaxLineSize = 16384; // request line, header e linha de chunk

{ RecvString com teto. MaxLineLength do Synapse nao serve: compara o buffer
  inteiro (linha + corpo/pipeline ja recebidos) antes de procurar o CRLF, e
  rejeitaria POSTs legitimos. Aqui so cresce o buffer enquanto nao ha CRLF; achado
  o CRLF, o proprio RecvString extrai a linha (mesma semantica de antes). Sem isto
  uma linha sem CRLF crescia em memoria ate o timeout. }
function RecvLineLimited(Sock: TTCPBlockSocket; Timeout, MaxLen: Integer;
  out TooLong: Boolean; var ReqStart: LongWord; DeadlineMs: Integer): string; overload;
var
  Buf: AnsiString;

  { Prazo de headers (slowloris): conta do 1o byte do pedido (ReqStart = 0 ate
    la), entao keep-alive ocioso nao e afetado. Checado a cada pacote E ao fim da
    linha: linhas inteiras pingadas a cada poucos segundos nunca entram no laco. }
  function Expired: Boolean;
  begin
    Result := False;
    if (DeadlineMs <= 0) or (Sock.LineBuffer = '') then
      Exit;
    if ReqStart = 0 then
      ReqStart := GetTick
    else
      Result := TickDelta(ReqStart, GetTick) >= LongWord(DeadlineMs);
  end;

begin
  Result := '';
  TooLong := False;
  while Pos(AnsiString(#13#10), Sock.LineBuffer) = 0 do
  begin
    if (Length(Sock.LineBuffer) > MaxLen) or Expired then
    begin
      TooLong := True;
      Exit;
    end;
    Buf := Sock.LineBuffer;
    Sock.LineBuffer := '';
    Buf := Buf + Sock.RecvPacket(Timeout);
    Sock.LineBuffer := Buf;
    if Sock.LastError <> 0 then
      Exit;
  end;
  if Expired then
  begin
    TooLong := True;
    Exit;
  end;
  Result := Sock.RecvString(Timeout);
end;

function RecvLineLimited(Sock: TTCPBlockSocket; Timeout, MaxLen: Integer;
  out TooLong: Boolean): string; overload;
var
  NoStart: LongWord;
begin
  NoStart := 0;
  Result := RecvLineLimited(Sock, Timeout, MaxLen, TooLong, NoStart, 0);
end;

{ THTTPRequestHandler }

function THTTPRequestHandler.HeaderExpired: Boolean;
begin
  Result := (FHeaderTimeout > 0) and (FReqStart <> 0) and
    (TickDelta(FReqStart, GetTick) >= LongWord(FHeaderTimeout));
end;

constructor THTTPRequestHandler.Create(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager;
  AMethods: TBadgerMethods; AMiddlewares, AAfterMiddlewares: TList; AMiddlewareLock: TCriticalSection; ATimeout: Integer;
  AOnRequest: TOnRequest; AOnResponse: TOnResponse; AParentServer: TBadger; AEnableEventInfo: Boolean);
var
  I: Integer;
begin
  FreeOnTerminate := True;
  FClientSocket := AClientSocket;
  FOnRequest := AOnRequest;
  FOnResponse := AOnResponse;
  FRouteManager := ARouteManager;
  FMethods := AMethods;
  FMiddlewareLock := AMiddlewareLock;
  FMiddlewares := TList.Create;
  FAfterMiddlewares := TList.Create;
  FOwnMiddlewareObjects := False;
  FTimeout := ATimeout;
  FParentServer := AParentServer;
  FIsParallel := False;
  FEnableEventInfo := AEnableEventInfo;
  FSocketNotified := False;

  if Assigned(FMiddlewareLock) then
    FMiddlewareLock.Acquire;
  try
    if Assigned(AMiddlewares) then
      for I := 0 to AMiddlewares.Count - 1 do
        FMiddlewares.Add(AMiddlewares[I]);
    if Assigned(AAfterMiddlewares) then
      for I := 0 to AAfterMiddlewares.Count - 1 do
        FAfterMiddlewares.Add(AAfterMiddlewares[I]);
  finally
    if Assigned(FMiddlewareLock) then
      FMiddlewareLock.Release;
  end;
  {$IF DEFINED(DelphiXEPlus) AND NOT DEFINED(FPC)}
  inherited Create(False);  // AfterConstruction auto-starts; no explicit Start needed
  {$ELSE}
  inherited Create(True);
  {$IFDEF FPC}
  Start;
  {$ELSE}
  Resume;
  {$ENDIF}
  {$IFEND}
end;

constructor THTTPRequestHandler.CreateParallel(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager;
  AMethods: TBadgerMethods; AMiddlewares, AAfterMiddlewares: TList; AMiddlewareLock: TCriticalSection; ATimeout: Integer;
  AOnRequest: TOnRequest; AOnResponse: TOnResponse; AParentServer: TBadger; AEnableEventInfo: Boolean);
var
  I: Integer;
begin
  FreeOnTerminate := True;
  FClientSocket := AClientSocket;
  FOnRequest := AOnRequest;
  FOnResponse := AOnResponse;
  FRouteManager := ARouteManager;
  FMethods := AMethods;
  FMiddlewareLock := AMiddlewareLock;
  FMiddlewares := TList.Create;
  FAfterMiddlewares := TList.Create;
  FOwnMiddlewareObjects := False;
  FTimeout := ATimeout;
  FParentServer := AParentServer;
  FIsParallel := True;
  FEnableEventInfo := AEnableEventInfo;
  FSocketNotified := False;

  if Assigned(FMiddlewareLock) then
    FMiddlewareLock.Acquire;
  try
    if Assigned(AMiddlewares) then
      for I := 0 to AMiddlewares.Count - 1 do
        FMiddlewares.Add(AMiddlewares[I]);
    if Assigned(AAfterMiddlewares) then
      for I := 0 to AAfterMiddlewares.Count - 1 do
        FAfterMiddlewares.Add(AAfterMiddlewares[I]);
  finally
    if Assigned(FMiddlewareLock) then
      FMiddlewareLock.Release;
  end;
  {$IF DEFINED(DelphiXEPlus) AND NOT DEFINED(FPC)}
  inherited Create(False);  // AfterConstruction auto-starts; no explicit Start needed
  {$ELSE}
  inherited Create(True);
  {$IFDEF FPC}
  Start;
  {$ELSE}
  Resume;
  {$ENDIF}
  {$IFEND}
end;

destructor THTTPRequestHandler.Destroy;
var
  I: Integer;
begin
  if FIsParallel and Assigned(FParentServer) and (not FSocketNotified) then
  begin
    try
      if Assigned(FClientSocket) then
        FParentServer.NotifyClientSocketClosed(FClientSocket)
      else
        FParentServer.DecActiveConnections;
    except
      on E: Exception do
        Logger.Error(Format('Error in NotifyClientSocketClosed/DecActiveConnections: %s', [E.Message]));
    end;
  end;

  if Assigned(FClientSocket) then
  begin
    try
      FClientSocket.CloseSocket;
    except
      on E: Exception do
        Logger.Error(Format('Error closing client socket: %s', [E.Message]));
    end;
    try
      FClientSocket.Free;
      FClientSocket := nil;
    except
      on E: Exception do
        Logger.Error(Format('Error freeing client socket: %s', [E.Message]));
    end;
  end;

  if FOwnMiddlewareObjects then
  begin
    for I := 0 to FMiddlewares.Count - 1 do
      TObject(FMiddlewares[I]).Free;
    for I := 0 to FAfterMiddlewares.Count - 1 do
      TObject(FAfterMiddlewares[I]).Free;
  end;
  FMiddlewares.Free;
  FAfterMiddlewares.Free;
  inherited;
end;

procedure THTTPRequestHandler.RunAfterMiddlewares(var Req: THTTPRequest; var Resp: THTTPResponse);
var
  I: Integer;
  AfterWrapper: TAfterMiddlewareWrapper;
begin
  if not Assigned(FAfterMiddlewares) then
    Exit;

  { LIFO — outermost after runs last (Horse onion). }
  for I := FAfterMiddlewares.Count - 1 downto 0 do
  begin
    AfterWrapper := TAfterMiddlewareWrapper(FAfterMiddlewares[I]);
    if not Assigned(AfterWrapper) or not Assigned(AfterWrapper.Middleware) then
      Continue;
    try
      AfterWrapper.Middleware(Req, Resp);
    except
      on E: Exception do
        Logger.Error(Format('After-middleware exception: %s', [E.Message]));
    end;
  end;
end;

procedure THTTPRequestHandler.ParseRequestHeader(ClientSocket: TTCPBlockSocket; aHeaders: TStringList);
const
  MaxHeaderLineSize = 16384; // 16KB por linha
  MaxHeaderTotalSize = 65536; // 64KB acumulado
var
  HeaderLine: string;
  SeparatorPos: Integer;
  Key, Value: string;
  TotalHeaderSize: Integer;
  TooLong: Boolean;
begin
  TotalHeaderSize := 0;
  repeat
    HeaderLine := RecvLineLimited(ClientSocket, FTimeout, MaxHeaderLineSize, TooLong,
      FReqStart, FHeaderTimeout);
    if TooLong and HeaderExpired then
      raise EHeaderTimeout.Create('Request headers timeout');
    if TooLong then
      raise EHeaderTooLarge.Create('Header line too large');
    if ClientSocket.LastError <> 0 then
      raise Exception.Create('Failed to read header line');

    // Synapse can keep trailing CR on RecvString; normalize to avoid
    // waiting FTimeout for the real blank line terminator.
    while (Length(HeaderLine) > 0) and
          ((HeaderLine[Length(HeaderLine)] = #13) or (HeaderLine[Length(HeaderLine)] = #10)) do
      Delete(HeaderLine, Length(HeaderLine), 1);

    Inc(TotalHeaderSize, Length(HeaderLine));
    if Length(HeaderLine) > MaxHeaderLineSize then
      raise EHeaderTooLarge.Create('Header line too large');
    if TotalHeaderSize > MaxHeaderTotalSize then
      raise EHeaderTooLarge.Create('Request headers too large');

    if HeaderLine <> '' then
    begin
      SeparatorPos := Pos(':', HeaderLine);
      if SeparatorPos > 0 then
      begin
        Key := Trim(Copy(HeaderLine, 1, SeparatorPos - 1));
        Value := Trim(Copy(HeaderLine, SeparatorPos + 1, Length(HeaderLine)));
        { Mesma regra do parser IOCP (BadgerIsHeaderName): nome com '=' virava
          outro header na TStringList. Cai no 400 'Invalid request headers'. }
        if not BadgerIsHeaderName(Key) then
          raise Exception.Create('Invalid header name');
        aHeaders.Add(Key + '=' + Value);
      end
      else
        aHeaders.Add(HeaderLine + '=');
    end;
  until HeaderLine = '';
end;

procedure THTTPRequestHandler.ProcessWebSocketHandshakeAndLoop(ClientSocket: TTCPBlockSocket; const URI, WSKey: string);
var
  B1, B2: Byte;
  FrameOpcode: Byte;
  IsMasked: Boolean;
  PayloadLen: Int64;
  MaskKey: array[0..3] of Byte;
  I: Integer;
  DecodedStr: AnsiString;
  WsInfo: TClientSocketInfo;
begin
  ClientSocket.SendString(string(BadgerWsHandshakeMessage(WSKey)));

  Logger.Info('WebSocket handshake established for ' + URI);

  repeat
    if not ClientSocket.CanRead(200) then
    begin
      if ClientSocket.LastError <> 0 then Break;
      Continue;
    end;

    B1 := ClientSocket.RecvByte(1000);
    if ClientSocket.LastError <> 0 then Break;

    FrameOpcode := B1 and $0F;

    if FrameOpcode = WS_OP_CLOSE then Break;

    B2 := ClientSocket.RecvByte(1000);
    if ClientSocket.LastError <> 0 then Break;
    IsMasked   := (B2 and $80) = $80;
    PayloadLen := B2 and $7F;

    if PayloadLen = 126 then
    begin
      PayloadLen := (Int64(ClientSocket.RecvByte(1000)) shl 8) or ClientSocket.RecvByte(1000);
      if ClientSocket.LastError <> 0 then Break;
    end
    else if PayloadLen = 127 then
    begin
      PayloadLen := 0;
      for I := 1 to 8 do
        PayloadLen := (PayloadLen shl 8) or Int64(ClientSocket.RecvByte(1000));
      if ClientSocket.LastError <> 0 then Break;
      if PayloadLen > WS_MAX_PAYLOAD then Break;
    end;

    if IsMasked then
    begin
      for I := 0 to 3 do
        MaskKey[I] := ClientSocket.RecvByte(1000);
      if ClientSocket.LastError <> 0 then Break;
    end;

    if PayloadLen > 0 then
    begin
      SetLength(DecodedStr, Integer(PayloadLen));
      ClientSocket.RecvBufferEx(Pointer(DecodedStr), Integer(PayloadLen), 1000);
      if ClientSocket.LastError <> 0 then Break;

      if IsMasked then
        for I := 0 to Integer(PayloadLen) - 1 do
          DecodedStr[I + 1] := AnsiChar(Ord(DecodedStr[I + 1]) xor MaskKey[I mod 4]);

      if (FrameOpcode = WS_OP_TEXT) and
         Assigned(FParentServer) and Assigned(FParentServer.OnWebSocketMessage) then
      begin
        { GetClientSocketInfo devolve com referencia tomada; soltar apos o callback,
          senao o objeto vaza (ou e liberado no meio do uso, como antes). }
        WsInfo := FParentServer.GetClientSocketInfo(ClientSocket);
        try
          FParentServer.OnWebSocketMessage(WsInfo, URI,
            BadgerWsUtf8ToString(Pointer(DecodedStr), Length(DecodedStr)));
        finally
          if Assigned(WsInfo) then
            WsInfo.Release;
        end;
      end;
    end;

  until Terminated;

  Logger.Info('WebSocket connection closed for ' + URI);
end;

function THTTPRequestHandler.BuildHTTPResponse(StatusCode: Integer;
  Body: string; Stream: TStream; ContentType: string;
  CloseConnection: Boolean; HeaderCustom: TStringList): string;
begin
  { '' = o builder gera o Date em GMT (RFC 9110 5.6.7). Rfc822DateTime(Now) do
    Synapse saia em hora local ('-0300'), divergindo do motor IOCP. }
  Result := BadgerBuildHTTPResponse(StatusCode, Body, Stream, ContentType,
    CloseConnection, HeaderCustom, '');
end;

procedure THTTPRequestHandler.Execute;
  procedure Exec(ARoute: {$IFDEF Delphi2009Plus}TRoutingCallback{$ELSE}TObject{$ENDIF}; const ARequest: THTTPRequest; var Response: THTTPResponse);
  {$IFDEF Delphi2009Plus}
  begin
    ARoute(ARequest, Response);
  end;
  {$ELSE}
  var
    Callback: TRoutingCallback;
    MethodPointer: TMethod;
  begin
    MethodPointer.Data := Self;
    MethodPointer.Code := Pointer(ARoute);
    Callback := TRoutingCallback(MethodPointer);
    Callback(ARequest, Response);
  end;
  {$ENDIF}

  function ReadChunkedBodyToStream(AStream: TMemoryStream; out ATotalBytes: Integer): Boolean;
  const
    ChunkReadBufferSize = 1048576;
    ChunkMaxRequestBodySize = 52428800;
  var
    ChunkLine, ChunkSizeHex: string;
    ChunkSize, NeedRead, Got, P: Integer;
    LineTooLong: Boolean;
{$IFDEF Delphi2009Plus}
    LocalBytes: TBytes;
{$ELSE}
    LocalBytes: array of Byte;
{$ENDIF}
  begin
    Result := False;
    ATotalBytes := 0;

    while True do
    begin
      ChunkLine := Trim(RecvLineLimited(FClientSocket, FTimeout, MaxLineSize, LineTooLong));
      if LineTooLong or (FClientSocket.LastError <> 0) then
        Exit;

      if ChunkLine = '' then
        Continue;

      P := Pos(';', ChunkLine); // ignora chunk extensions
      if P > 0 then
        ChunkSizeHex := Trim(Copy(ChunkLine, 1, P - 1))
      else
        ChunkSizeHex := ChunkLine;

      try
        ChunkSize := StrToInt('$' + ChunkSizeHex);
      except
        Exit;
      end;

      if ChunkSize < 0 then
        Exit;

      if ChunkSize = 0 then
      begin
        // L� trailers at� linha em branco final
        repeat
          ChunkLine := RecvLineLimited(FClientSocket, FTimeout, MaxLineSize, LineTooLong);
          if LineTooLong or (FClientSocket.LastError <> 0) then
            Exit;
        until ChunkLine = '';
        Result := True;
        Exit;
      end;

      { Subtracao: 'ATotalBytes + ChunkSize' estourava Integer com '7FFFFFFF' e
        passava no teto, levando a ler ate 2 GB. }
      if ChunkSize > ChunkMaxRequestBodySize - ATotalBytes then
        Exit;

      NeedRead := ChunkSize;
      while NeedRead > 0 do
      begin
        SetLength(LocalBytes, Min(NeedRead, ChunkReadBufferSize));
        Got := FClientSocket.RecvBufferEx(@LocalBytes[0], Length(LocalBytes), FTimeout);
        if Got <= 0 then
          Exit;
        AStream.WriteBuffer(LocalBytes[0], Got);
        Inc(ATotalBytes, Got);
        Dec(NeedRead, Got);
      end;

      // consome CRLF ao final do chunk
      ChunkLine := RecvLineLimited(FClientSocket, FTimeout, MaxLineSize, LineTooLong);
      if LineTooLong or (FClientSocket.LastError <> 0) or (ChunkLine <> '') then
        Exit;
    end;
  end;

  { Content-Length estrito (RFC 7230 3.3.2): so digitos; repetido so com valores
    iguais. StrToIntDef aceitava '+5', '$10', valor acima de MaxInt (virava 0) e o
    primeiro de dois valores divergentes: cada leniencia desalinha o corpo com um
    proxy na frente (request smuggling). }
  function ReadContentLength(AHeaders: TStringList; out AHas: Boolean;
    out ALen: Integer): Boolean;
  var
    K, J: Integer;
    V: string;
    N: Int64;
  begin
    Result := False;
    AHas := False;
    ALen := 0;
    for K := 0 to AHeaders.Count - 1 do
    begin
      if not SameText(AHeaders.Names[K], 'Content-Length') then
        Continue;
      V := Trim(Copy(AHeaders[K], Length(AHeaders.Names[K]) + 2, MaxInt));
      if (V = '') or (Length(V) > 10) then
        Exit;
      for J := 1 to Length(V) do
        if (V[J] < '0') or (V[J] > '9') then
          Exit;
      N := StrToInt64(V);
      if N > MaxInt then
        Exit;
      if AHas and (Integer(N) <> ALen) then
        Exit;
      AHas := True;
      ALen := Integer(N);
    end;
    Result := True;
  end;

  { Junta todas as linhas Transfer-Encoding: Values[] so via a primeira, e
    'TE: chunked' + 'TE: identity' escondia o token final. }
  function JoinTransferEncoding(AHeaders: TStringList): string;
  var
    K: Integer;
  begin
    Result := '';
    for K := 0 to AHeaders.Count - 1 do
      if SameText(AHeaders.Names[K], 'Transfer-Encoding') then
      begin
        if Result <> '' then
          Result := Result + ',';
        Result := Result + Copy(AHeaders[K], Length(AHeaders.Names[K]) + 2, MaxInt);
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

const
  MaxBufferSize = 1048576;       // 1MB buffer de leitura
  MaxRequestBodySize = 52428800; // 50MB limite de request body
var
  I, ContentLength, TotalBytes: Integer;
  Req: THTTPRequest;
  Resp: THTTPResponse;
  RouteEntry: TRouteEntry;
  RouteParams: TStringList;
  MiddlewareWrapper: TMiddlewareWrapper;
  Headers: TStringList;
  BodyStream: TMemoryStream;
  CloseConnection: Boolean;
  LRouteStr: string;
{$IFDEF Delphi2009Plus}
  TempBytes: TBytes;
  ResponseBodyBytes: TBytes;
  UTF8Body: RawByteString;
{$ELSE}
  TempBytes: array of Byte;
  ResponseBodyBytes: array of Byte;
  UTF8Body: string;
{$ENDIF}
  BufferSize: Integer;
  BytesRead: Integer;
  QueryParams: TStringList;
  ResponseHeader: string;
  ContentType: string;
  RequestInfo: TRequestInfo;
  ResponseInfo: TResponseInfo;
  Handled: Boolean;
  TransferEncoding: string;
  IsChunked: Boolean;
  HasContentLength: Boolean;
  LineTooLong: Boolean;
  Origin, ACRM, ACRH, AllowOrigin, MethodsStr, HeadersStr, ExposeStr: string;
  LForwardedFor: string;
  HdrParts: TStringList;
  HdrPart: string;
  OriginAllowed: Boolean;
  AllowWildcardOrigin: Boolean;
  SkipRequestProcessing: Boolean;
  DoWsUpgrade: Boolean;
  WsUpgradeKey: string;
begin
  FillChar(Req, SizeOf(Req), 0);
  FillChar(Resp, SizeOf(Resp), 0);
  QueryParams := nil;
  Req.QueryParams := nil;
  Req.Headers := nil;
  Req.Body := '';
  Req.BodyStream := nil;
  Req.RouteParams := nil;
  Resp.Stream := nil;
  Resp.HeadersCustom := nil; // da branch1
  RouteParams := nil;
  Headers := nil;
  BodyStream := nil;
  Handled := False;
  try
    QueryParams := TStringList.Create;
    Req.QueryParams := TStringList.Create;
    Req.Headers := TStringList.Create;
    Req.RouteParams := TStringList.Create;
    Resp.HeadersCustom := TStringList.Create;
    RouteParams := TStringList.Create;

    try
      repeat
        // Reset per-request state to avoid keep-alive cross-request leakage
        QueryParams.Clear;
        RouteParams.Clear;
        Req.QueryParams.Clear;
        Req.Headers.Clear;
        Req.RouteParams.Clear;
        Req.Body := '';
        if Assigned(Req.BodyStream) then
          FreeAndNil(Req.BodyStream);
        Req.Method := '';
        Req.URI := '';
        Req.RequestLine := '';
        Req.UserID := '';
        Req.UserRole := '';
        Req.DbPool := nil;
        Req.DbConn := nil;
        Resp.StatusCode := 0;
        Resp.Body := '';
        Resp.ContentType := '';
        if Assigned(Resp.Stream) then
          FreeAndNil(Resp.Stream);
        Resp.HeadersCustom.Clear;
        Headers := nil;
        BodyStream := nil;
        Handled := False;
        SkipRequestProcessing := False;
        DoWsUpgrade := False;
        WsUpgradeKey := '';
        Origin := '';
        ACRM := '';
        ACRH := '';

        if FClientSocket.LastError <> 0 then Break;
        FReqStart := 0;
        if Assigned(FParentServer) then
          FHeaderTimeout := FParentServer.HeaderTimeout
        else
          FHeaderTimeout := 0;
        FRequestLine := RecvLineLimited(FClientSocket, FTimeout, MaxLineSize, LineTooLong,
          FReqStart, FHeaderTimeout);
        if LineTooLong and HeaderExpired then
        begin
          FClientSocket.SendString('HTTP/1.1 408 Request Timeout'#13#10 +
            'Content-Length: 0'#13#10'Connection: close'#13#10#13#10);
          Break;
        end;
        if LineTooLong then
        begin
          FClientSocket.SendString('HTTP/1.1 414 URI Too Long'#13#10 +
            'Content-Length: 0'#13#10'Connection: close'#13#10#13#10);
          Break;
        end;
        while (Length(FRequestLine) > 0) and
              ((FRequestLine[Length(FRequestLine)] = #13) or (FRequestLine[Length(FRequestLine)] = #10)) do
          Delete(FRequestLine, Length(FRequestLine), 1);
        if FRequestLine = '' then Break;

        if not FMethods.ExtractMethodAndURI(FRequestLine, FMethod, FURI, QueryParams) then
        begin
          Resp.StatusCode := HTTP_BAD_REQUEST;
          Resp.Body := 'Bad Request';
          Resp.ContentType := TEXT_PLAIN;
          Handled := True;
          SkipRequestProcessing := True;
        end;

        if not SkipRequestProcessing then
        begin
          Req.Method := FMethod;
          Req.URI := FURI;
          Req.RequestLine := FRequestLine;
          Req.QueryParams.Assign(QueryParams);
          Req.Socket := FClientSocket;
          Req.FRemoteIP := FClientSocket.GetRemoteSinIP;
          LRouteStr := UpperCase(Req.Method) + ' ' + LowerCase(Req.URI);
        end;

        Headers := TStringList.Create();
        try
          try
            ParseRequestHeader(FClientSocket, Headers);
            Req.Headers.Assign(Headers);

            // Proxy reverso (nginx): sobrescreve o IP do socket (127.0.0.1) pelo IP real do cliente.
            // X-Real-IP tem precedência; X-Forwarded-For pode ser lista "clientIP, proxy1, proxy2".
            if (not SkipRequestProcessing) and Assigned(FParentServer) and
               FParentServer.TrustProxyHeaders and
               ((FParentServer.TrustedProxies.Count = 0) or
                (FParentServer.TrustedProxies.IndexOf(Req.FRemoteIP) >= 0)) then
            begin
              LForwardedFor := Trim(Headers.Values['X-Real-IP']);
              if LForwardedFor <> '' then
                Req.FRemoteIP := LForwardedFor
              else
              begin
                LForwardedFor := Headers.Values['X-Forwarded-For'];
                if LForwardedFor <> '' then
                begin
                  I := Pos(',', LForwardedFor);
                  if I > 0 then
                    Req.FRemoteIP := Trim(Copy(LForwardedFor, 1, I - 1))
                  else
                    Req.FRemoteIP := Trim(LForwardedFor);
                end;
              end;
            end;
          except
            on E: EHeaderTimeout do
            begin
              Resp.StatusCode := HTTP_REQUEST_TIMEOUT;
              Resp.Body := '{"error":"Request headers timeout"}';
              Resp.ContentType := APPLICATION_JSON;
              Handled := True;
              SkipRequestProcessing := True;
            end;
            on E: EHeaderTooLarge do
            begin
              Resp.StatusCode := HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE;
              Resp.Body := '{"error":"Request headers too large"}';
              Resp.ContentType := APPLICATION_JSON;
              Handled := True;
              SkipRequestProcessing := True;
            end;
            on E: Exception do
            begin
              Resp.StatusCode := HTTP_BAD_REQUEST;
              Resp.Body := '{"error":"Invalid request headers"}';
              Resp.ContentType := APPLICATION_JSON;
              Handled := True;
              SkipRequestProcessing := True;
            end;
          end;

          if (not ReadContentLength(Headers, HasContentLength, ContentLength)) and
             (not SkipRequestProcessing) then
          begin
            Resp.StatusCode := HTTP_BAD_REQUEST;
            Resp.Body := '{"error":"Invalid Content-Length"}';
            Resp.ContentType := APPLICATION_JSON;
            Handled := True;
            SkipRequestProcessing := True;
          end;
          ContentType := Headers.Values['Content-Type'];
          TransferEncoding := LowerCase(Trim(JoinTransferEncoding(Headers)));
          { Mesma regra do parser IOCP: 'chunked' tem de ser o ULTIMO token inteiro
            (Pos aceitava 'notchunked' e 'chunked, gzip'). }
          IsChunked := LastTokenIs(TransferEncoding, 'chunked');

          if (not IsChunked) and (ContentLength > MaxRequestBodySize) then
          begin
            Resp.StatusCode := HTTP_PAYLOAD_TOO_LARGE;
            Resp.Body := '{"error":"Request body too large"}';
            Resp.ContentType := APPLICATION_JSON;
            Handled := True;
            SkipRequestProcessing := True;
          end;

          if (not IsChunked) and (ContentLength < 0) then
          begin
            Resp.StatusCode := HTTP_BAD_REQUEST;
            Resp.Body := '{"error":"Invalid Content-Length"}';
            Resp.ContentType := APPLICATION_JSON;
            Handled := True;
            SkipRequestProcessing := True;
          end;

          { RFC 7230 3.3.3: TE sem 'chunked' final, ou TE junto de Content-Length
            (base do smuggling CL.TE/TE.CL), vale 400 e fecha a conexao. }
          if (not SkipRequestProcessing) and (TransferEncoding <> '') and
             ((not IsChunked) or HasContentLength) then
          begin
            Resp.StatusCode := HTTP_BAD_REQUEST;
            Resp.Body := '{"error":"Invalid Transfer-Encoding"}';
            Resp.ContentType := APPLICATION_JSON;
            Handled := True;
            SkipRequestProcessing := True;
          end;

          { 'Expect: 100-continue': sem o interim o cliente so manda o corpo apos
            estourar o proprio timeout. }
          if (not SkipRequestProcessing) and (IsChunked or (ContentLength > 0)) and
             (Pos('100-continue', LowerCase(Headers.Values['Expect'])) > 0) then
            FClientSocket.SendString('HTTP/1.1 100 Continue'#13#10#13#10);

          if (not SkipRequestProcessing) and (IsChunked or (ContentLength > 0)) then
          begin
            BodyStream := TMemoryStream.Create;
            try
              TotalBytes := 0;

              if IsChunked then
              begin
                if not ReadChunkedBodyToStream(BodyStream, TotalBytes) then
                begin
                  Resp.StatusCode := HTTP_BAD_REQUEST;
                  Resp.Body := '{"error":"Invalid chunked request body"}';
                  Resp.ContentType := APPLICATION_JSON;
                  Handled := True;
                  SkipRequestProcessing := True;
                end;
              end
              else
              begin
                SetLength(TempBytes, Min(ContentLength, MaxBufferSize));
                while (TotalBytes < ContentLength) and (FClientSocket.LastError = 0) do
                begin
                  { Pedir exatamente o que falta: RecvBufferEx bloqueia ate encher o
                    buffer, entao pedir mais do que resta custa FTimeout no ultimo bloco. }
                  BytesRead := FClientSocket.RecvBufferEx(@TempBytes[0],
                    Min(ContentLength - TotalBytes, Length(TempBytes)), FTimeout);
                  if BytesRead <= 0 then Break;
                  BodyStream.Write(TempBytes[0], BytesRead);
                  Inc(TotalBytes, BytesRead);
                end;
              end;

              BodyStream.Position := 0;
              if (Pos('application/json', LowerCase(ContentType)) > 0) or (Pos('text/', LowerCase(ContentType)) > 0) then
              begin
                if TotalBytes > 0 then
                begin
                  SetLength(TempBytes, TotalBytes);
                  BodyStream.ReadBuffer(TempBytes[0], TotalBytes);
                  {$IFDEF Delphi2009Plus}
                  Req.Body := TEncoding.UTF8.GetString(TempBytes);
                  {$ELSE}
                  SetString(Req.Body, PAnsiChar(@TempBytes[0]), TotalBytes);
                  Req.Body := CharsetConversion(Req.Body, UTF_8, GetCurCP);
                  {$ENDIF}
                end
                else
                  Req.Body := '';
                FreeAndNil(BodyStream);
              end
              else
              begin
                Req.BodyStream := BodyStream;
                BodyStream := nil;
              end;
            except
              FreeAndNil(BodyStream);
              raise;
            end;
          end;

          { Nao recalcular quando um caminho de erro (400/413/431) ja decidiu fechar:
            caso contrario o keep-alive volta e o lixo restante no socket e lido
            como a proxima requisicao. }
          if SkipRequestProcessing then
            CloseConnection := True
          else if Pos('HTTP/1.0', FRequestLine) > 0 then
          begin
            // HTTP/1.0 s� aceita Keep-Alive se o cliente pedir explicitamente
            CloseConnection := (LowerCase(Headers.Values['Connection']) <> 'keep-alive');
          end
          else
          begin
            // HTTP/1.1 mant�m aberta a menos que pe�a para fechar
            CloseConnection := (LowerCase(Headers.Values['Connection']) = 'close');
          end;

          Origin := Headers.Values['Origin'];
          ACRM := Headers.Values['Access-Control-Request-Method'];
          ACRH := Headers.Values['Access-Control-Request-Headers'];

          if (not SkipRequestProcessing) and SameText(FMethod, 'OPTIONS') and (Origin <> '') and (ACRM <> '') and Assigned(FParentServer) and FParentServer.CorsEnabled then
          begin
            AllowWildcardOrigin := (FParentServer.CorsAllowedOrigins.IndexOf('*') >= 0);
            if FParentServer.CorsAllowCredentials then
              OriginAllowed := (FParentServer.CorsAllowedOrigins.IndexOf(Origin) >= 0)
            else
              OriginAllowed := AllowWildcardOrigin or (FParentServer.CorsAllowedOrigins.IndexOf(Origin) >= 0);

            if OriginAllowed then
            begin
              if FParentServer.CorsAllowedMethods.IndexOf(UpperCase(ACRM)) < 0 then
              begin
                Resp.StatusCode := HTTP_METHOD_NOT_ALLOWED;
                Resp.Body := '';
                Resp.ContentType := '';
                Resp.HeadersCustom.Values['Allow'] := FParentServer.CorsAllowedMethods.CommaText;
                ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, nil, Resp.ContentType, True, Resp.HeadersCustom);
                FClientSocket.SendString(ResponseHeader);
                Break;
              end;

              if ACRH <> '' then
              begin
                HeadersStr := '';
                HdrParts := TStringList.Create;
                try
                  SplitCommaTokens(ACRH, HdrParts);
                  for I := 0 to HdrParts.Count - 1 do
                  begin
                    HdrPart := Trim(HdrParts[I]);
                    if HdrPart <> '' then
                    begin
                      if FParentServer.CorsAllowedHeaders.IndexOf(HdrPart) < 0 then
                      begin
                        Resp.StatusCode := HTTP_BAD_REQUEST;
                        Resp.Body := '';
                        Resp.ContentType := '';
                        ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, nil, Resp.ContentType, True, Resp.HeadersCustom);
                        FClientSocket.SendString(ResponseHeader);
                        Break;
                      end
                      else
                      begin
                        if HeadersStr = '' then HeadersStr := HdrPart else HeadersStr := HeadersStr + ',' + HdrPart;
                      end;
                    end;
                  end;
                  if Resp.StatusCode <> 0 then Break;
                finally
                  HdrParts.Free;
                end;
              end
              else
                HeadersStr := FParentServer.CorsAllowedHeaders.CommaText;

              if FParentServer.CorsAllowCredentials then
                AllowOrigin := Origin
              else if AllowWildcardOrigin then
                AllowOrigin := '*'
              else
                AllowOrigin := Origin;

              MethodsStr := FParentServer.CorsAllowedMethods.CommaText;

              Resp.StatusCode := HTTP_NO_CONTENT;
              Resp.Body := '';
              Resp.ContentType := '';
              Resp.HeadersCustom.Values['Access-Control-Allow-Origin'] := AllowOrigin;
              Resp.HeadersCustom.Values['Access-Control-Allow-Methods'] := MethodsStr;
              Resp.HeadersCustom.Values['Access-Control-Allow-Headers'] := HeadersStr;
              if FParentServer.CorsAllowCredentials then
                Resp.HeadersCustom.Values['Access-Control-Allow-Credentials'] := 'true';
              if FParentServer.CorsMaxAge > 0 then
                Resp.HeadersCustom.Values['Access-Control-Max-Age'] := IntToStr(FParentServer.CorsMaxAge);
              if AllowOrigin <> '*' then
                Resp.HeadersCustom.Values['Vary'] := 'Origin, Access-Control-Request-Method, Access-Control-Request-Headers';

              ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, Resp.Stream, Resp.ContentType, CloseConnection, Resp.HeadersCustom);
              FClientSocket.SendString(ResponseHeader);
              Break;
            end
            else
            begin
              Resp.StatusCode := HTTP_FORBIDDEN;
              Resp.Body := 'CORS origin not allowed';
              Resp.ContentType := TEXT_PLAIN;
              ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, nil, Resp.ContentType, True, Resp.HeadersCustom);
              FClientSocket.SendString(ResponseHeader);
              Break;
            end;
          end;


          // --- BEFORE + ROUTE, then AFTER (LIFO), then send ---
          // After runs in finally so cleanup still happens if the route raises,
          // but ALWAYS before BuildHTTPResponse so body/header changes reach the client.
          try
            // --- MIDDLEWARES ---
            if not SkipRequestProcessing then
            begin
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
                    Logger.Error('Middleware exception: ' + E.Message);
                    Resp.StatusCode := HTTP_INTERNAL_SERVER_ERROR;
                    Resp.Body := '{"error":"Internal Server Error"}';
                    Resp.ContentType := APPLICATION_JSON;
                    Handled := True;
                    Break;
                  end;
                end;
              end;
            end;

            { Upgrade WebSocket depois dos middlewares: auth roda antes. Exige GET
              (RFC 6455 §4.1) e uma rota declarada com AddWebSocket. O handshake
              em si roda após o finally, para não segurar after-middlewares
              (ex.: conexão de pool) durante a sessão. }
            if (not Handled) and BadgerWsIsUpgrade(Headers) then
            begin
              Handled := True;
              CloseConnection := True;
              WsUpgradeKey := Trim(Headers.Values['Sec-WebSocket-Key']);
              if UpperCase(FMethod) <> CGET then
              begin
                Resp.StatusCode := HTTP_METHOD_NOT_ALLOWED;
                Resp.Body := '{"error":"WebSocket upgrade requires GET"}';
                Resp.ContentType := APPLICATION_JSON;
                Resp.HeadersCustom.Values['Allow'] := CGET;
              end
              else if WsUpgradeKey = '' then
              begin
                Resp.StatusCode := HTTP_BAD_REQUEST;
                Resp.Body := '{"error":"Missing Sec-WebSocket-Key"}';
                Resp.ContentType := APPLICATION_JSON;
              end
              else if Headers.Values['Sec-WebSocket-Version'] <> '13' then
              begin
                Resp.StatusCode := HTTP_BAD_REQUEST;
                Resp.Body := '{"error":"Unsupported WebSocket version"}';
                Resp.ContentType := APPLICATION_JSON;
                Resp.HeadersCustom.Values['Sec-WebSocket-Version'] := '13';
              end
              else if not FRouteManager.MatchRoute(CWS, FURI, RouteEntry, RouteParams) then
              begin
                Resp.StatusCode := HTTP_NOT_FOUND;
                Resp.Body := 'Not Found';
                Resp.ContentType := TEXT_PLAIN;
              end
              else
              begin
                Req.RouteParams.Assign(RouteParams);
                DoWsUpgrade := True;
              end;
            end;

            if not Handled then
            begin
              // --- MATCH ROTA COM :param ---
              { HEAD deve existir onde GET existe (RFC 7231 4.3.2). }
              if FRouteManager.MatchRoute(UpperCase(FMethod), FURI, RouteEntry, RouteParams) or
                 (SameText(FMethod, 'HEAD') and
                  FRouteManager.MatchRoute(CGET, FURI, RouteEntry, RouteParams)) then
              begin
                if Assigned(RouteEntry) and Assigned(TMethod(RouteEntry.Callback).Code) then
                begin
                  Req.RouteParams.Assign(RouteParams);
                  RouteEntry.Callback(Req, Resp);
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

            // --- CORS headers on Resp (before after-middleware / BuildHTTPResponse) ---
            if Assigned(FParentServer) and FParentServer.CorsEnabled and (Origin <> '') then
            begin
              AllowWildcardOrigin := (FParentServer.CorsAllowedOrigins.IndexOf('*') >= 0);
              if FParentServer.CorsAllowCredentials then
                OriginAllowed := (FParentServer.CorsAllowedOrigins.IndexOf(Origin) >= 0)
              else
                OriginAllowed := AllowWildcardOrigin or (FParentServer.CorsAllowedOrigins.IndexOf(Origin) >= 0);

              if OriginAllowed then
              begin
                if FParentServer.CorsAllowCredentials then
                  AllowOrigin := Origin
                else if AllowWildcardOrigin then
                  AllowOrigin := '*'
                else
                  AllowOrigin := Origin;
                Resp.HeadersCustom.Values['Access-Control-Allow-Origin'] := AllowOrigin;
                if FParentServer.CorsAllowCredentials then
                  Resp.HeadersCustom.Values['Access-Control-Allow-Credentials'] := 'true';
                ExposeStr := FParentServer.CorsExposeHeaders.CommaText;
                if ExposeStr <> '' then
                  Resp.HeadersCustom.Values['Access-Control-Expose-Headers'] := ExposeStr;
                if AllowOrigin <> '*' then
                  Resp.HeadersCustom.Values['Vary'] := 'Origin';
              end;
            end;
          finally
            RunAfterMiddlewares(Req, Resp);
          end;

          if DoWsUpgrade then
          begin
            if Assigned(FParentServer) then
              FParentServer.SetClientSocketURI(FClientSocket, FURI);
            ProcessWebSocketHandshakeAndLoop(FClientSocket, FURI, WsUpgradeKey);
            Break;
          end;

          // --- RESPOSTA (usa Resp já processado pelos after-middlewares) ---
          ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, Resp.Stream, Resp.ContentType, CloseConnection, Resp.HeadersCustom);
          FClientSocket.SendString(ResponseHeader);

          // --- ENVIO DO CORPO ---
          { Espelha o Content-Length de BuildHTTPResponse: com stream, o corpo em
            string é ignorado; sem stream, vai sempre, qualquer Content-Type. }
          if (Resp.Body <> '') and (not SameText(FMethod, 'HEAD')) and
             not (Assigned(Resp.Stream) and (Resp.Stream.Size > 0)) then
          begin
            {$IFDEF Delphi2009Plus}
            ResponseBodyBytes := TEncoding.UTF8.GetBytes(Resp.Body);
            {$ELSE}
            UTF8Body := UTF8Encode(Resp.Body);
            SetLength(ResponseBodyBytes, Length(UTF8Body));
            Move(UTF8Body[1], ResponseBodyBytes[0], Length(UTF8Body));
            {$ENDIF}
            if Length(ResponseBodyBytes) > 0 then
              FClientSocket.SendBuffer(@ResponseBodyBytes[0], Length(ResponseBodyBytes));
          end;

          // --- ENVIO DO STREAM ---
          if Assigned(Resp.Stream) and (Resp.Stream.Size > 0) and
             (not SameText(FMethod, 'HEAD')) then
          begin
            Resp.Stream.Position := 0;
            BufferSize := Min(MaxBufferSize, Resp.Stream.Size);
            SetLength(ResponseBodyBytes, BufferSize);
            repeat
              BytesRead := Resp.Stream.Read(ResponseBodyBytes[0], Length(ResponseBodyBytes));
              if BytesRead > 0 then
                FClientSocket.SendBuffer(@ResponseBodyBytes[0], BytesRead);
            until BytesRead = 0;
            FreeAndNil(Resp.Stream);
          end;

          // --- LOGS ---
          if FEnableEventInfo and Assigned(FOnRequest) then
          begin
            RequestInfo.Headers := TStringList.Create;
            RequestInfo.QueryParams := TStringList.Create;
            try
              RequestInfo.RemoteIP := Req.FRemoteIP;
              RequestInfo.Method := Req.Method;
              RequestInfo.URI := Req.URI;
              RequestInfo.RequestLine := Req.RequestLine;
              RequestInfo.Headers.Assign(Req.Headers);
              RequestInfo.Body := Req.Body;
              RequestInfo.QueryParams.Assign(Req.QueryParams);
              RequestInfo.Timestamp := Now;
              { A resposta ja foi enviada: excecao do callback caia no except
                externo, que mandava um segundo 500 na mesma conexao. }
              try
                FOnRequest(RequestInfo);
              except
                on E: Exception do
                  Logger.Error('OnRequest exception: ' + E.Message);
              end;
            finally
              RequestInfo.Headers.Free;
              RequestInfo.QueryParams.Free;
            end;
          end;

          if FEnableEventInfo and Assigned(FOnResponse) then
          begin
            ResponseInfo.Headers := TStringList.Create;
            try
              ResponseInfo.StatusCode := Resp.StatusCode;
              ResponseInfo.StatusText := THTTPStatus.GetStatusText(Resp.StatusCode);
              ResponseInfo.Body := Resp.Body;
              ResponseInfo.ContentType := Resp.ContentType;
              ResponseInfo.Headers.Text := ResponseHeader;
              ResponseInfo.Timestamp := Now;
              try
                FOnResponse(ResponseInfo);
              except
                on E: Exception do
                  Logger.Error('OnResponse exception: ' + E.Message);
              end;
            finally
              ResponseInfo.Headers.Free;
            end;
          end;

          if CloseConnection then Break;

        finally
          FreeAndNil(Headers);
        end;

      until False;

    except
      on E: Exception do
      begin
        Resp.StatusCode := HTTP_INTERNAL_SERVER_ERROR;
        Logger.Error(Format('Unhandled exception in request handler: %s', [E.Message]));
        Resp.Body := 'Internal Server Error';
        Resp.ContentType := TEXT_PLAIN;
        ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, nil, Resp.ContentType, True, Resp.HeadersCustom);
        if Assigned(FClientSocket) and (FClientSocket.LastError = 0) then
        begin
          FClientSocket.SendString(ResponseHeader);
          {$IFDEF Delphi2009Plus}
          ResponseBodyBytes := TEncoding.UTF8.GetBytes(Resp.Body);
          {$ELSE}
          UTF8Body := UTF8Encode(Resp.Body);
          SetLength(ResponseBodyBytes, Length(UTF8Body));
          Move(UTF8Body[1], ResponseBodyBytes[0], Length(UTF8Body));
          {$ENDIF}
          if Length(ResponseBodyBytes) > 0 then
            FClientSocket.SendBuffer(@ResponseBodyBytes[0], Length(ResponseBodyBytes));
        end;
      end;
    end;

  finally
    // --- LIMPEZA ---
    FreeAndNil(QueryParams);
    FreeAndNil(Req.QueryParams);
    FreeAndNil(Req.Headers);
    FreeAndNil(Req.RouteParams);
    FreeAndNil(Req.BodyStream);
    FreeAndNil(Resp.Stream);
    FreeAndNil(Resp.HeadersCustom);
    FreeAndNil(RouteParams);
    FreeAndNil(BodyStream);

    if Assigned(FClientSocket) then
    begin
      if FIsParallel and Assigned(FParentServer) then
      begin
        try
          FParentServer.NotifyClientSocketClosed(FClientSocket);
          FSocketNotified := True;
        except
          on E: Exception do
            Logger.Error(Format('Error in NotifyClientSocketClosed during execute cleanup: %s', [E.Message]));
        end;
      end;
      FClientSocket.CloseSocket;
      FreeAndNil(FClientSocket);
    end;
  end;
end;

end.
