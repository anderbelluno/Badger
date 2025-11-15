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
    FTimeout: Integer;
    FParentServer: TBadger;
    FIsParallel: Boolean;
  protected
    procedure ParseRequestHeader(ClientSocket: TTCPBlockSocket; aHeaders: TStringList);
    function BuildHTTPResponse(StatusCode: Integer; Body: string; Stream: TStream; ContentType: string; CloseConnection: Boolean; HeaderCustom: TStringList): string;
  public
    constructor Create(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager;
                      AMethods: TBadgerMethods; AMiddlewares: TList; ATimeout: Integer;
                      AOnRequest: TOnRequest; AOnResponse: TOnResponse);
    constructor CreateParallel(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager;
                              AMethods: TBadgerMethods; AMiddlewares: TList; ATimeout: Integer;
                              AOnRequest: TOnRequest; AOnResponse: TOnResponse; AParentServer: TBadger);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

{ THTTPRequestHandler }

constructor THTTPRequestHandler.Create(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager;
  AMethods: TBadgerMethods; AMiddlewares: TList; ATimeout: Integer;
  AOnRequest: TOnRequest; AOnResponse: TOnResponse);
var
  I: Integer;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FClientSocket := AClientSocket;
  FOnRequest := AOnRequest;
  FOnResponse := AOnResponse;
  FRouteManager := ARouteManager;
  FMethods := AMethods;
  FMiddlewares := TList.Create;
  FTimeout := ATimeout;
  FParentServer := nil;
  FIsParallel := False;

  for I := 0 to AMiddlewares.Count - 1 do
    FMiddlewares.Add(TMiddlewareWrapper.Create(TMiddlewareWrapper(AMiddlewares[I]).Middleware));
  Resume;
end;

constructor THTTPRequestHandler.CreateParallel(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager;
  AMethods: TBadgerMethods; AMiddlewares: TList; ATimeout: Integer;
  AOnRequest: TOnRequest; AOnResponse: TOnResponse; AParentServer: TBadger);
var
  I: Integer;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FClientSocket := AClientSocket;
  FOnRequest := AOnRequest;
  FOnResponse := AOnResponse;
  FRouteManager := ARouteManager;
  FMethods := AMethods;
  FMiddlewares := TList.Create;
  FTimeout := ATimeout;
  FParentServer := AParentServer;
  FIsParallel := True;

  for I := 0 to AMiddlewares.Count - 1 do
    FMiddlewares.Add(TMiddlewareWrapper.Create(TMiddlewareWrapper(AMiddlewares[I]).Middleware));
  Resume;
end;

destructor THTTPRequestHandler.Destroy;
var
  I: Integer;
begin
  if FIsParallel and Assigned(FParentServer) then
  begin
    try
      FParentServer.DecActiveConnections;
    except
      on E: Exception do
        Logger.Error(Format('Error in DecActiveConnections: %s', [E.Message]));
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

  for I := 0 to FMiddlewares.Count - 1 do
    TObject(FMiddlewares[I]).Free;
  FMiddlewares.Free;
  inherited;
end;

procedure THTTPRequestHandler.ParseRequestHeader(ClientSocket: TTCPBlockSocket; aHeaders: TStringList);
var
  HeaderLine: string;
  SeparatorPos: Integer;
  Key, Value: string;
begin
  try
    repeat
      HeaderLine := ClientSocket.RecvString(FTimeout);
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
  except
    raise;
  end;
end;

function THTTPRequestHandler.BuildHTTPResponse(StatusCode: Integer;
  Body: string; Stream: TStream; ContentType: string;
  CloseConnection: Boolean; HeaderCustom: TStringList): string;
var
  EffectiveContentType: string;
  i: Integer;
{$IFDEF VER150}
  UTF8Body: string;
{$ELSE}
  UTF8Body: RawByteString;
{$ENDIF}
begin
  if ContentType = '' then
    EffectiveContentType := TEXT_PLAIN
  else
    EffectiveContentType := ContentType;

  if (AnsiContainsText(LowerCase(EffectiveContentType), TEXT_PLAIN)) or (AnsiContainsText(LowerCase(EffectiveContentType), APPLICATION_JSON)) then
  begin
{$IFDEF VER150}
    UTF8Body := UTF8Encode(Body);
    Result := Format('HTTP/1.1 %d %s', [StatusCode, THTTPStatus.GetStatusText(StatusCode)]) + CRLF +
              'Content-Type: ' + EffectiveContentType + '; charset=utf-8' + CRLF +
              'Content-Length: ' + IntToStr(Length(UTF8Body)) + CRLF;
{$ELSE}
    UTF8Body := UTF8Encode(Body);
    Result := Format('HTTP/1.1 %d %s', [StatusCode, THTTPStatus.GetStatusText(StatusCode)]) + CRLF +
              'Content-Type: ' + EffectiveContentType + '; charset=utf-8' + CRLF +
              'Content-Length: ' + IntToStr(Length(UTF8Body)) + CRLF;
{$ENDIF}
  end
  else if Assigned(Stream) and (Stream.Size > 0) then
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
      Result := Result + HeaderCustom.Names[i] + ':' + HeaderCustom.ValueFromIndex[i]+ CRLF;
    end;
  end;

  Result := Result + CRLF;
end;

procedure THTTPRequestHandler.Execute;
  procedure Exec(ARoute: {$IFDEF Delphi2009Plus}TRoutingCallback{$ELSE}TObject{$ENDIF}; const ARequest: THTTPRequest; out Response: THTTPResponse);
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

const
  MaxBufferSize = 1048576; // 1MB
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
{$IFDEF VER150}
  TempBytes: array of Byte;
  ResponseBodyBytes: array of Byte;
  UTF8Body: string;
{$ELSE}
  TempBytes: TBytes;
  ResponseBodyBytes: TBytes;
  UTF8Body: RawByteString;
{$ENDIF}
  BufferSize: Integer;
  BytesRead: Integer;
  QueryParams: TStringList;
  ResponseHeader: string;
  ContentType: string;
  RequestInfo: TRequestInfo;
  ResponseInfo: TResponseInfo;
  Handled: Boolean;
begin
  // Logger.Info(Format('Starting THTTPRequestHandler.Execute for thread %d', [ThreadID]));

  QueryParams := TStringList.Create;
  Req.QueryParams := TStringList.Create;
  Req.Headers := TStringList.Create;
  Req.Body := '';
  Req.BodyStream := nil;
  Req.RouteParams := TStringList.Create;
  Resp.Stream := nil;
  Resp.HeadersCustom := TStringList.Create; // da branch1
  RouteParams := TStringList.Create;
  Headers := nil;
  BodyStream := nil;
  Handled := False;
  try
    try
      repeat
        if FClientSocket.LastError <> 0 then Break;
        FRequestLine := FClientSocket.RecvString(FTimeout);
        if FRequestLine = '' then Break;

        if not FMethods.ExtractMethodAndURI(FRequestLine, FMethod, FURI, QueryParams) then
        begin
          Resp.StatusCode := HTTP_BAD_REQUEST;
          Resp.Body := 'Bad Request';
          Resp.ContentType := TEXT_PLAIN;
          Break;
        end;

        Req.Method := FMethod;
        Req.URI := FURI;
        Req.RequestLine := FRequestLine;
        Req.QueryParams.Assign(QueryParams);
        Req.Socket := FClientSocket;
        Req.FRemoteIP := FClientSocket.GetRemoteSinIP;
        LRouteStr := UpperCase(Req.Method) + ' ' + LowerCase(Req.URI);

        Headers := TStringList.Create();
        try
          ParseRequestHeader(FClientSocket, Headers);
          Req.Headers.Assign(Headers);

          ContentLength := StrToIntDef(Headers.Values['Content-Length'], 0);
          ContentType := Headers.Values['Content-Type'];

          if ContentLength > 0 then
          begin
            BodyStream := TMemoryStream.Create;
            try
              TotalBytes := 0;
              SetLength(TempBytes, Min(ContentLength, MaxBufferSize));
              while (TotalBytes < ContentLength) and (FClientSocket.LastError = 0) do
              begin
                BytesRead := FClientSocket.RecvBufferEx(@TempBytes[0], Length(TempBytes), FTimeout);
                if BytesRead <= 0 then Break;
                BodyStream.Write(TempBytes[0], BytesRead);
                Inc(TotalBytes, BytesRead);
              end;
              BodyStream.Position := 0;

              if (Pos('application/json', LowerCase(ContentType)) > 0) or (Pos('text/', LowerCase(ContentType)) > 0) then
              begin
                SetLength(TempBytes, TotalBytes);
                BodyStream.ReadBuffer(TempBytes[0], TotalBytes);
                SetString(Req.Body, PChar(@TempBytes[0]), TotalBytes);
                {$IFDEF VER150}
                Req.Body := CharsetConversion(Req.Body, UTF_8, GetCurCP);
                {$ELSE}
                Req.Body := TEncoding.UTF8.GetString(TempBytes);
                {$ENDIF}
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

          CloseConnection := (Headers.Values['Connection'] = 'close') or (Pos('HTTP/1.0', FRequestLine) > 0);

          // --- MIDDLEWARES ---
          Handled := False;
          for I := 0 to FMiddlewares.Count - 1 do
          begin
            MiddlewareWrapper := TMiddlewareWrapper(FMiddlewares[I]);
            if MiddlewareWrapper.Middleware(Req, Resp) then
            begin
              Handled := True;
              Break;
            end;
          end;

          if not Handled then
          begin
            // --- MATCH ROTA COM :param ---
            if FRouteManager.MatchRoute(UpperCase(FMethod), LowerCase(FURI), RouteEntry, RouteParams) then
            begin
              Req.RouteParams.Assign(RouteParams);
              RouteEntry.Callback(Req, Resp);
            end
            else
            begin
              Resp.StatusCode := HTTP_NOT_FOUND;
              Resp.Body := 'Not Found';
              Resp.ContentType := TEXT_PLAIN;
            end;
          end;

          // --- RESPOSTA ---
          ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, Resp.Stream, Resp.ContentType, CloseConnection, Resp.HeadersCustom);
          FClientSocket.SendString(ResponseHeader);

          // --- ENVIO DO CORPO (TEXTO) ---
          if (Resp.Body <> '') and (
             (Resp.ContentType = '') or
             (Pos('text/', Resp.ContentType) = 1) or
             (Pos('application/json', Resp.ContentType) = 1)
          ) then
          begin
            {$IFDEF VER150}
            UTF8Body := UTF8Encode(Resp.Body);
            SetLength(ResponseBodyBytes,, Length(UTF8Body));
            Move(UTF8Body[1], ResponseBodyBytes[0], Length(UTF8Body));
            {$ELSE}
            ResponseBodyBytes := TEncoding.UTF8.GetBytes(Resp.Body);
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
          if Assigned(FOnRequest) then
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

          if Assigned(FOnResponse) then
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
        Resp.Body := 'Internal Server Error: ' + E.Message;
        Resp.ContentType := TEXT_PLAIN;
        ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, nil, Resp.ContentType, True, Resp.HeadersCustom);
        if Assigned(FClientSocket) and (FClientSocket.LastError = 0) then
        begin
          FClientSocket.SendString(ResponseHeader);
          {$IFDEF VER150}
          UTF8Body := UTF8Encode(Resp.Body);
          SetLength(ResponseBodyBytes, Length(UTF8Body));
          Move(UTF8Body[1], ResponseBodyBytes[0], Length(UTF8Body));
          {$ELSE}
          ResponseBodyBytes := TEncoding.UTF8.GetBytes(Resp.Body);
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
      FClientSocket.CloseSocket;
      FreeAndNil(FClientSocket);
    end;

    // Logger.Info(Format('Finished THTTPRequestHandler.Execute for thread %d', [ThreadID]));
  end;
end;

end.
