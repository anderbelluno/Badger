unit BadgerRequestHandler;

{$I BadgerDefines.inc}

interface

uses
  blcksock, httpsend, synsock, SyncObjs, synachar, synautil, Math, Classes, SysUtils, StrUtils,
  BadgerRouteManager, BadgerMethods, BadgerHttpStatus, BadgerTypes, Badger, BadgerLogger;

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
    FOwnMiddlewareObjects: Boolean;
    FMiddlewareLock: TCriticalSection;
    FTimeout: Integer;
    FParentServer: TBadger;
    FIsParallel: Boolean;
    FEnableEventInfo: Boolean;
    FSocketNotified: Boolean;
  protected
    procedure ParseRequestHeader(ClientSocket: TTCPBlockSocket; aHeaders: TStringList);
    function BuildHTTPResponse(StatusCode: Integer; Body: string; Stream: TStream; ContentType: string; CloseConnection: Boolean; HeaderCustom: TStringList): string;
  public
    constructor Create(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager;
                      AMethods: TBadgerMethods; AMiddlewares: TList; AMiddlewareLock: TCriticalSection; ATimeout: Integer;
                      AOnRequest: TOnRequest; AOnResponse: TOnResponse; AParentServer: TBadger; AEnableEventInfo: Boolean);
    constructor CreateParallel(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager;
                              AMethods: TBadgerMethods; AMiddlewares: TList; AMiddlewareLock: TCriticalSection; ATimeout: Integer;
                              AOnRequest: TOnRequest; AOnResponse: TOnResponse; AParentServer: TBadger; AEnableEventInfo: Boolean);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

type
  EHeaderTooLarge = class(Exception);

{ THTTPRequestHandler }

constructor THTTPRequestHandler.Create(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager;
  AMethods: TBadgerMethods; AMiddlewares: TList; AMiddlewareLock: TCriticalSection; ATimeout: Integer;
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
  FOwnMiddlewareObjects := False;
  FTimeout := ATimeout;
  FParentServer := AParentServer;
  FIsParallel := False;
  FEnableEventInfo := AEnableEventInfo;
  FSocketNotified := False;

  if Assigned(FMiddlewareLock) then
    FMiddlewareLock.Acquire;
  try
    for I := 0 to AMiddlewares.Count - 1 do
      FMiddlewares.Add(AMiddlewares[I]);
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
  AMethods: TBadgerMethods; AMiddlewares: TList; AMiddlewareLock: TCriticalSection; ATimeout: Integer;
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
  FOwnMiddlewareObjects := False;
  FTimeout := ATimeout;
  FParentServer := AParentServer;
  FIsParallel := True;
  FEnableEventInfo := AEnableEventInfo;
  FSocketNotified := False;

  if Assigned(FMiddlewareLock) then
    FMiddlewareLock.Acquire;
  try
    for I := 0 to AMiddlewares.Count - 1 do
      FMiddlewares.Add(AMiddlewares[I]);
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
    for I := 0 to FMiddlewares.Count - 1 do
      TObject(FMiddlewares[I]).Free;
  FMiddlewares.Free;
  inherited;
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
begin
  TotalHeaderSize := 0;
  repeat
    HeaderLine := ClientSocket.RecvString(FTimeout);
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
        aHeaders.Add(Key + '=' + Value);
      end
      else
        aHeaders.Add(HeaderLine + '=');
    end;
  until HeaderLine = '';
end;

function THTTPRequestHandler.BuildHTTPResponse(StatusCode: Integer;
  Body: string; Stream: TStream; ContentType: string;
  CloseConnection: Boolean; HeaderCustom: TStringList): string;
  function StripCRLF(const S: string): string;
  begin
    Result := StringReplace(S, #13, '', [rfReplaceAll]);
    Result := StringReplace(Result, #10, '', [rfReplaceAll]);
  end;

  function SanitizeHeaderName(const S: string): string;
  var
    J: Integer;
    Ch: Char;
  begin
    Result := '';
    for J := 1 to Length(S) do
    begin
      Ch := S[J];
      if (Ord(Ch) > 31) and (Ch <> ':') then
        Result := Result + Ch
      else
        Result := Result + '_';
    end;
    Result := Trim(Result);
  end;
var
  EffectiveContentType: string;
  i: Integer;
  HeaderName, HeaderValue: string;
{$IFDEF Delphi2009Plus}
  UTF8Body: RawByteString;
{$ELSE}
  UTF8Body: string;
{$ENDIF}
begin
  Result := '';
  if ContentType = '' then
    EffectiveContentType := TEXT_PLAIN
  else
    EffectiveContentType := ContentType;

  if (AnsiContainsText(LowerCase(EffectiveContentType), TEXT_PLAIN)) or (AnsiContainsText(LowerCase(EffectiveContentType), APPLICATION_JSON)) then
  begin
    UTF8Body := UTF8Encode(Body);
    Result := Format('HTTP/1.1 %d %s', [StatusCode, THTTPStatus.GetStatusText(StatusCode)]) + CRLF +
              'Content-Type: ' + EffectiveContentType + '; charset=utf-8' + CRLF +
              'Content-Length: ' + IntToStr(Length(UTF8Body)) + CRLF;
  end
  else
  if Assigned(Stream) and (Stream.Size > 0) then
  begin
    Result := Format('HTTP/1.1 %d %s', [StatusCode, THTTPStatus.GetStatusText(StatusCode)]) + CRLF +
              'Content-Type: ' + EffectiveContentType  + CRLF +
              'Content-Length: ' + IntToStr(Stream.Size) + CRLF;
  end;

  Result := Result + 'Date: ' + Rfc822DateTime(Now) + CRLF +
                    'Server: Badger HTTP Server' + CRLF;
  if CloseConnection then
    Result := Result + 'Connection: close' + CRLF
  else
    Result := Result + 'Connection: keep-alive' + CRLF;

  if (Assigned(HeaderCustom)) and (HeaderCustom.Count > 0) then
  begin
    for i := 0 to Pred(HeaderCustom.Count) do
    begin
      HeaderName := SanitizeHeaderName(HeaderCustom.Names[i]);
      HeaderValue := StripCRLF(HeaderCustom.ValueFromIndex[i]);
      if HeaderName <> '' then
        Result := Result + HeaderName + ':' + HeaderValue + CRLF;
    end;
  end;

  Result := Result + CRLF;
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
      ChunkLine := Trim(FClientSocket.RecvString(FTimeout));
      if FClientSocket.LastError <> 0 then
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
          ChunkLine := FClientSocket.RecvString(FTimeout);
          if FClientSocket.LastError <> 0 then
            Exit;
        until ChunkLine = '';
        Result := True;
        Exit;
      end;

      if (ATotalBytes + ChunkSize) > ChunkMaxRequestBodySize then
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
      ChunkLine := FClientSocket.RecvString(FTimeout);
      if (FClientSocket.LastError <> 0) or (ChunkLine <> '') then
        Exit;
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
  LRoute: {$IFDEF Delphi2009Plus}TRoutingCallback{$ELSE}TObject{$ENDIF};
  Index: Integer;
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
  Origin, ACRM, ACRH, AllowOrigin, MethodsStr, HeadersStr, ExposeStr: string;
  HdrParts: TStringList;
  HdrPart: string;
  OriginAllowed: Boolean;
  AllowWildcardOrigin: Boolean;
  SkipRequestProcessing: Boolean;
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
        Origin := '';
        ACRM := '';
        ACRH := '';

        if FClientSocket.LastError <> 0 then Break;
        FRequestLine := FClientSocket.RecvString(FTimeout);
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
          CloseConnection := True;
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
          except
            on E: EHeaderTooLarge do
            begin
              Resp.StatusCode := HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE;
              Resp.Body := '{"error":"Request headers too large"}';
              Resp.ContentType := APPLICATION_JSON;
              Handled := True;
              CloseConnection := True;
              SkipRequestProcessing := True;
            end;
            on E: Exception do
            begin
              Resp.StatusCode := HTTP_BAD_REQUEST;
              Resp.Body := '{"error":"Invalid request headers"}';
              Resp.ContentType := APPLICATION_JSON;
              Handled := True;
              CloseConnection := True;
              SkipRequestProcessing := True;
            end;
          end;

          ContentLength := StrToIntDef(Headers.Values['Content-Length'], 0);
          ContentType := Headers.Values['Content-Type'];
          TransferEncoding := LowerCase(Trim(Headers.Values['Transfer-Encoding']));
          IsChunked := Pos('chunked', TransferEncoding) > 0;

          if (not IsChunked) and (ContentLength > MaxRequestBodySize) then
          begin
            Resp.StatusCode := HTTP_PAYLOAD_TOO_LARGE;
            Resp.Body := '{"error":"Request body too large"}';
            Resp.ContentType := APPLICATION_JSON;
            Handled := True;
            CloseConnection := True;
            SkipRequestProcessing := True;
          end;

          if (not IsChunked) and (ContentLength < 0) then
          begin
            Resp.StatusCode := HTTP_BAD_REQUEST;
            Resp.Body := '{"error":"Invalid Content-Length"}';
            Resp.ContentType := APPLICATION_JSON;
            Handled := True;
            CloseConnection := True;
            SkipRequestProcessing := True;
          end;

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
                  CloseConnection := True;
                  SkipRequestProcessing := True;
                end;
              end
              else
              begin
                SetLength(TempBytes, Min(ContentLength, MaxBufferSize));
                while (TotalBytes < ContentLength) and (FClientSocket.LastError = 0) do
                begin
                  BytesRead := FClientSocket.RecvBufferEx(@TempBytes[0], Length(TempBytes), FTimeout);
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
                  SetString(Req.Body, PChar(@TempBytes[0]), TotalBytes);
                  {$IFDEF Delphi2009Plus}
                  Req.Body := TEncoding.UTF8.GetString(TempBytes);
                  {$ELSE}
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

          if Pos('HTTP/1.0', FRequestLine) > 0 then
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
                  Resp.StatusCode := HTTP_INTERNAL_SERVER_ERROR;
                  Resp.Body := '{"error":"Middleware exception: ' + E.Message + '"}';
                  Resp.ContentType := APPLICATION_JSON;
                  Handled := True;
                  Break;
                end;
              end;
            end;
          end;

          if not Handled then
          begin
            // --- MATCH ROTA COM :param ---
            if FRouteManager.MatchRoute(UpperCase(FMethod), LowerCase(FURI), RouteEntry, RouteParams) then
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

          // --- RESPOSTA ---
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
          ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, Resp.Stream, Resp.ContentType, CloseConnection, Resp.HeadersCustom);
          FClientSocket.SendString(ResponseHeader);

          // --- ENVIO DO CORPO (TEXTO) ---
          if (Resp.Body <> '') and (
             (Resp.ContentType = '') or
             (Pos('text/', Resp.ContentType) = 1) or
             (Pos('application/json', Resp.ContentType) = 1)
          ) then
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
          if Assigned(Resp.Stream) and (Resp.Stream.Size > 0) then
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
              FOnRequest(RequestInfo);
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
              FOnResponse(ResponseInfo);
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
