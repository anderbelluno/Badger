unit BadgerRequestHandler;

{$I BadgerDefines.inc}

interface

uses
  blcksock, httpsend, synsock, SyncObjs, synachar, synautil, Classes, SysUtils, StrUtils,
  BadgerRouteManager, BadgerMethods, BadgerHttpStatus, BadgerTypes, Badger, Math, Windows;

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
    function ParseRequestHeader(ClientSocket: TTCPBlockSocket): TStringList;
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
        OutputDebugString(PChar(Format('Error in DecActiveConnections: %s', [E.Message])));
    end;
  end;

  if Assigned(FClientSocket) then
  begin
    try
      FClientSocket.CloseSocket;
    except
      on E: Exception do
        OutputDebugString(PChar(Format('Error closing client socket: %s', [E.Message])));
    end;
    try
      FClientSocket.Free;
      FClientSocket := nil;
    except
      on E: Exception do
        OutputDebugString(PChar(Format('Error freeing client socket: %s', [E.Message])));
    end;
  end;
  for I := 0 to FMiddlewares.Count - 1 do
    TObject(FMiddlewares[I]).Free;
  FMiddlewares.Free;
  inherited;
end;

function THTTPRequestHandler.ParseRequestHeader(ClientSocket: TTCPBlockSocket): TStringList;
var
  HeaderLine: string;
  SeparatorPos: Integer;
  Key, Value: string;
begin
  Result := TStringList.Create;
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
          Result.Add(Key + '=' + Value);
        end
        else
          Result.Add(HeaderLine + '=');
      end;
    until HeaderLine = '';
  except
    Result.Free;
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

  if (ContainsText(LowerCase(EffectiveContentType), TEXT_PLAIN)) or (ContainsText(LowerCase(EffectiveContentType), APPLICATION_JSON)) then
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
              'Content-Type: ' + EffectiveContentType + CRLF +
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
      Result := Result + HeaderCustom.KeyNames[i] + ':' + HeaderCustom.ValueFromIndex[i]+ CRLF;
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
  Index, I, ContentLength, TotalBytes: Integer;
  Req: THTTPRequest;
  Resp: THTTPResponse;
  LRouteStr: string;
  LRoute: {$IFDEF Delphi2009Plus}TRoutingCallback{$ELSE}TObject{$ENDIF};
  MiddlewareWrapper: TMiddlewareWrapper;
  Headers: TStringList;
  BodyStream: TMemoryStream;
  CloseConnection: Boolean;
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
begin
  OutputDebugString(PChar(Format('Starting THTTPRequestHandler.Execute for thread %d', [ThreadID])));
  try
    QueryParams := TStringList.Create;
    try
      Req.QueryParams := TStringList.Create;
      Req.Headers := TStringList.Create;
      Req.Body := '';
      Req.BodyStream := nil;
      Resp.Stream := TMemoryStream.Create;
      Resp.HeadersCustom := TStringList.Create;

      RequestInfo.Headers := TStringList.Create;
      RequestInfo.QueryParams := TStringList.Create;
      ResponseInfo.Headers := TStringList.Create;

      try
        repeat
          if FClientSocket.LastError = 0 then
          begin
            Resp.HeadersCustom.Clear;
            FRequestLine := FClientSocket.RecvString(FTimeout);
            if FRequestLine = '' then Break;
            if FMethods.ExtractMethodAndURI(FRequestLine, FMethod, FURI, QueryParams) then
            begin
              Req.Method := FMethod;
              Req.URI := FURI;
              Req.RequestLine := FRequestLine;
              Req.FRemoteIP :=FClientSocket.GetRemoteSinIP;
              LRouteStr := UpperCase(Req.Method) + ' ' + LowerCase(Req.URI);

              RequestInfo.RemoteIP := Req.FRemoteIP;
              RequestInfo.Method := Req.Method;
              RequestInfo.URI := Req.URI;
              RequestInfo.RequestLine := Req.RequestLine;

              Headers := ParseRequestHeader(FClientSocket);
              try
                ContentLength := StrToIntDef(Headers.Values['Content-Length'], 0);
                ContentType := Headers.Values['Content-Type'];
                BodyStream := TMemoryStream.Create;
                try
                  if ContentLength > 0 then
                  begin
                    BodyStream.SetSize(ContentLength);
                    TotalBytes := 0;
                    while TotalBytes < ContentLength do
                    begin
                      BytesRead := FClientSocket.RecvBufferEx(Pointer(NativeUInt(BodyStream.Memory) + TotalBytes), ContentLength - TotalBytes, 5000);
                      if BytesRead <= 0 then Break;
                      Inc(TotalBytes, BytesRead);
                    end;
                    BodyStream.Size := TotalBytes;
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
                  end
                  else
                  begin
                    FreeAndNil(BodyStream);
                  end;

                  Req.Socket := FClientSocket;
                  Req.Headers.Assign(Headers);
                  Req.QueryParams.Assign(QueryParams);

                  RequestInfo.Headers.Assign(Headers);
                  RequestInfo.Body := Req.Body;
                  RequestInfo.QueryParams.Assign(QueryParams);

                  if Assigned(FOnRequest) then
                    FOnRequest(RequestInfo);

                  CloseConnection := (Headers.Values['Connection'] = 'close') or (Pos('HTTP/1.0', FRequestLine) = 1);

                  for I := 0 to FMiddlewares.Count - 1 do
                  begin
                    MiddlewareWrapper := TMiddlewareWrapper(FMiddlewares[I]);
                    if not MiddlewareWrapper.Middleware(Req, Resp) then
                    begin
                      ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, Resp.Stream, Resp.ContentType, CloseConnection, Resp.HeadersCustom);
                      FClientSocket.SendString(ResponseHeader);
{$IFDEF VER150}
                      UTF8Body := UTF8Encode(Resp.Body);
                      SetLength(ResponseBodyBytes, Length(UTF8Body));
                      Move(UTF8Body[1], ResponseBodyBytes[0], Length(UTF8Body));
                      if Length(ResponseBodyBytes) > 0 then
                        FClientSocket.SendBuffer(@ResponseBodyBytes[0], Length(ResponseBodyBytes));
{$ELSE}
                      ResponseBodyBytes := TEncoding.UTF8.GetBytes(Resp.Body);
                      if Length(ResponseBodyBytes) > 0 then
                        FClientSocket.SendBuffer(@ResponseBodyBytes[0], Length(ResponseBodyBytes));
{$ENDIF}
                      if Assigned(Resp.Stream) then
                      begin
                        Resp.Stream.Position := 0;
                        repeat
                          BytesRead := Resp.Stream.Read(Pointer(ResponseBodyBytes)^, Length(ResponseBodyBytes));
                          if BytesRead > 0 then
                            FClientSocket.SendBuffer(@ResponseBodyBytes[0], BytesRead);
                        until BytesRead = 0;
                        FreeAndNil(Resp.Stream);
                      end;

                      ResponseInfo.StatusCode := Resp.StatusCode;
                      ResponseInfo.StatusText := THTTPStatus.GetStatusText(Resp.StatusCode);
                      ResponseInfo.Body := Resp.Body;
                      ResponseInfo.ContentType := Resp.ContentType;
                      ResponseInfo.Headers.Text := ResponseHeader;
                      ResponseInfo.Timestamp := Now;

                      if Assigned(FOnResponse) then
                        FOnResponse(ResponseInfo);

                      if CloseConnection then Break;
                      Continue;
                    end;
                  end;

{$IFDEF Delphi2009Plus}
                  if not FRouteManager.FRoutes.TryGetValue(LRouteStr, LRoute) then
                    LRoute := nil;
{$ELSE}
                  Index := FRouteManager.FRoutes.IndexOf(LRouteStr);
                  if Index <> -1 then
                    LRoute := FRouteManager.FRoutes.Objects[Index]
                  else
                    LRoute := nil;
{$ENDIF}

                  if Assigned(LRoute) then
                  begin
                    if not Assigned(Resp.Stream) then
                      Resp.Stream := TMemoryStream.Create;
                    Exec(LRoute, Req, Resp);
                    ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, Resp.Stream, Resp.ContentType, CloseConnection, Resp.HeadersCustom);
                    FClientSocket.SendString(ResponseHeader);
{$IFDEF VER150}
                    UTF8Body := UTF8Encode(Resp.Body);
                    SetLength(ResponseBodyBytes, Length(UTF8Body));
                    Move(UTF8Body[1], ResponseBodyBytes[0], Length(UTF8Body));
                    if Length(ResponseBodyBytes) > 0 then
                      FClientSocket.SendBuffer(@ResponseBodyBytes[0], Length(ResponseBodyBytes));
{$ELSE}
                    ResponseBodyBytes := TEncoding.UTF8.GetBytes(Resp.Body);
                    if Length(ResponseBodyBytes) > 0 then
                      FClientSocket.SendBuffer(@ResponseBodyBytes[0], Length(ResponseBodyBytes));
{$ENDIF}

                    if Assigned(Resp.Stream) then
                    begin
                      BufferSize := Resp.Stream.Size;
                      if BufferSize > MaxBufferSize then
                        BufferSize := MaxBufferSize;

                      SetLength(ResponseBodyBytes, BufferSize);

                      if Resp.Stream.Size > 0 then
                      begin
                        Resp.Stream.Position := 0;
                        repeat
                          BytesRead := Resp.Stream.Read(ResponseBodyBytes[0], Length(ResponseBodyBytes));
                          if BytesRead > 0 then
                            FClientSocket.SendBuffer(@ResponseBodyBytes[0], BytesRead);
                        until BytesRead = 0;
                      end;
                      FreeAndNil(Resp.Stream);
                    end;

                    ResponseInfo.StatusCode := Resp.StatusCode;
                    ResponseInfo.StatusText := THTTPStatus.GetStatusText(Resp.StatusCode);
                    ResponseInfo.Body := Resp.Body;
                    ResponseInfo.ContentType := Resp.ContentType;
                    ResponseInfo.Headers.Text := ResponseHeader;
                    ResponseInfo.Timestamp := Now;
                    if Assigned(FOnResponse) then
                      FOnResponse(ResponseInfo);
                  end
                  else
                  begin
                    Resp.StatusCode := HTTP_NOT_FOUND;
                    Resp.Body := 'Not Found';
                    Resp.ContentType := TEXT_PLAIN;
                    ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, nil, Resp.ContentType, CloseConnection, Resp.HeadersCustom);
                    FClientSocket.SendString(ResponseHeader);
{$IFDEF VER150}
                    UTF8Body := UTF8Encode('Not Found');
                    SetLength(ResponseBodyBytes, Length(UTF8Body));
                    Move(UTF8Body[1], ResponseBodyBytes[0], Length(UTF8Body));
                    if Length(ResponseBodyBytes) > 0 then
                      FClientSocket.SendBuffer(@ResponseBodyBytes[0], Length(ResponseBodyBytes));
{$ELSE}
                    ResponseBodyBytes := TEncoding.UTF8.GetBytes('Not Found');
                    if Length(ResponseBodyBytes) > 0 then
                      FClientSocket.SendBuffer(@ResponseBodyBytes[0], Length(ResponseBodyBytes));
{$ENDIF}

                    ResponseInfo.StatusCode := Resp.StatusCode;
                    ResponseInfo.StatusText := THTTPStatus.GetStatusText(Resp.StatusCode);
                    ResponseInfo.Body := Resp.Body;
                    ResponseInfo.ContentType := Resp.ContentType;
                    ResponseInfo.Headers.Text := ResponseHeader;
                    ResponseInfo.Timestamp := Now;
                    if Assigned(FOnResponse) then
                      FOnResponse(ResponseInfo);
                  end;

                  if CloseConnection then
                    Break;
                finally
                  if Assigned(BodyStream) then
                    FreeAndNil(BodyStream);
                end;
              finally
                Headers.Free;
              end;
            end;
          end
          else
            Break;
        until False;
      except
        on E: Exception do
        begin
          Resp.StatusCode := HTTP_INTERNAL_SERVER_ERROR;
          Resp.Body := 'Internal Server Error: ' + E.Message;
          Resp.ContentType := TEXT_PLAIN;
          ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, nil, Resp.ContentType, True, Resp.HeadersCustom);
          FClientSocket.SendString(ResponseHeader);
  {$IFDEF VER150}
          UTF8Body := UTF8Encode(Resp.Body);
          SetLength(ResponseBodyBytes, Length(UTF8Body));
          Move(UTF8Body[1], ResponseBodyBytes[0], Length(UTF8Body));
          if Length(ResponseBodyBytes) > 0 then
            FClientSocket.SendBuffer(@ResponseBodyBytes[0], Length(ResponseBodyBytes));
  {$ELSE}
          ResponseBodyBytes := TEncoding.UTF8.GetBytes(Resp.Body);
          if Length(ResponseBodyBytes) > 0 then
            FClientSocket.SendBuffer(@ResponseBodyBytes[0], Length(ResponseBodyBytes));
  {$ENDIF}

          ResponseInfo.StatusCode := Resp.StatusCode;
          ResponseInfo.StatusText := THTTPStatus.GetStatusText(Resp.StatusCode);
          ResponseInfo.Body := Resp.Body;
          ResponseInfo.ContentType := Resp.ContentType;
          ResponseInfo.Headers.Text := ResponseHeader;
          ResponseInfo.Timestamp := Now;
          if Assigned(FOnResponse) then
            FOnResponse(ResponseInfo);
        end;
      end;
    finally
      if Assigned(QueryParams) then
        FreeAndNil(QueryParams);
      if Assigned(Req.QueryParams) then
        FreeAndNil(Req.QueryParams);
      if Assigned(Req.Headers) then
        FreeAndNil(Req.Headers);
      if Assigned(Req.BodyStream) then
        FreeAndNil(Req.BodyStream);
      if Assigned(Resp.Stream) then
        FreeAndNil(Resp.Stream);
      if Assigned(Resp.HeadersCustom) then
        FreeAndNil(Resp.HeadersCustom);
      if Assigned(RequestInfo.Headers) then
        FreeAndNil(RequestInfo.Headers);
      if Assigned(RequestInfo.QueryParams) then
        FreeAndNil(RequestInfo.QueryParams);
      if Assigned(ResponseInfo.Headers) then
        FreeAndNil(ResponseInfo.Headers);
      if Assigned(FClientSocket) then
      begin
        FClientSocket.CloseSocket;
        FreeAndNil(FClientSocket);
      end;
    end;
  finally
    OutputDebugString(PChar(Format('Finished THTTPRequestHandler.Execute for thread %d', [ThreadID])));
  end;
end;

end.
