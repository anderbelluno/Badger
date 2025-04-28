unit BadgerRequestHandler;

interface

uses
  blcksock, httpsend, synsock, SyncObjs, synachar, synautil, Classes, SysUtils, StrUtils,
  BadgerRouteManager, SyUtils, BadgerMethods, BadgerHttpStatus, BadgerTypes, Math;

type
  THTTPRequestHandler = class(TThread)
  private
    FLastRequest: TLastRequest;
    FLastResponse: TLastResponse;
    FClientSocket: TTCPBlockSocket;
    FRouteManager: TRouteManager;
    FURI: string;
    FMethod: string;
    FRequestLine: string;
    FResponseLine: string;
    FCriticalSection: TCriticalSection;
    FMethods: TBadgerMethods;
    FMiddlewares: TList;
    FTimeout: Integer;
  protected
    function ParseRequestHeader(ClientSocket: TTCPBlockSocket): TStringList;
    function BuildHTTPResponse(StatusCode: Integer; Body: string; Stream: TStream;
                               ContentType: string; CloseConnection: Boolean): string;
  public
    constructor Create(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager;
                       ACriticalSection: TCriticalSection; AMethods: TBadgerMethods;
                       AMiddlewares: TList; ATimeout: Integer;
                       ALastRequest: TLastRequest = nil; ALastResponse: TLastResponse = nil);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

{ THTTPRequestHandler }

constructor THTTPRequestHandler.Create(AClientSocket: TTCPBlockSocket; ARouteManager: TRouteManager;
  ACriticalSection: TCriticalSection; AMethods: TBadgerMethods; AMiddlewares: TList; ATimeout: Integer;
  ALastRequest: TLastRequest; ALastResponse: TLastResponse);
var
  I: Integer;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FClientSocket := AClientSocket;
  FCriticalSection := ACriticalSection;
  FLastRequest := ALastRequest;
  FLastResponse := ALastResponse;
  FRouteManager := ARouteManager;
  FMethods := AMethods;
  FMiddlewares := TList.Create;
  FTimeout := ATimeout;
  for I := 0 to AMiddlewares.Count - 1 do
    FMiddlewares.Add(TMiddlewareWrapper.Create(TMiddlewareWrapper(AMiddlewares[I]).Middleware));
  Resume;
end;

destructor THTTPRequestHandler.Destroy;
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

function THTTPRequestHandler.BuildHTTPResponse(StatusCode: Integer; Body: string; Stream: TStream;
  ContentType: string; CloseConnection: Boolean): string;
var
  EffectiveContentType: string;
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

  if (EffectiveContentType = TEXT_PLAIN) or (EffectiveContentType = APPLICATION_JSON) then
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
  Result := Result + CRLF;
end;

procedure THTTPRequestHandler.Execute;
  procedure Exec(LRoute: TObject; const Request: THTTPRequest; out Response: THTTPResponse);
  var
    Callback: TRoutingCallback;
    MethodPointer: TMethod;
  begin
    MethodPointer.Data := Self;
    MethodPointer.Code := Pointer(LRoute);
    Callback := TRoutingCallback(MethodPointer);
    Callback(Request, Response);
  end;

const
  MaxBufferSize = 1048576; // 1MB
var
  Index, I, ContentLength, TotalBytes: Integer;
  Req: THTTPRequest;
  Resp: THTTPResponse;
  LRoute: TObject;
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
begin
  QueryParams := TStringList.Create;
  try
    Req.QueryParams := TStringList.Create;
    Req.Headers := TStringList.Create;
    Req.Body := '';
    Req.BodyStream := nil;
    Resp.Stream := TMemoryStream.Create;
    try
      repeat
        if FClientSocket.LastError = 0 then
        begin
          FRequestLine := FClientSocket.RecvString(FTimeout);
          if FRequestLine = '' then Break;
          if FMethods.ExtractMethodAndURI(FRequestLine, FMethod, FURI, QueryParams) then
          begin
            FCriticalSection.Enter;
            try
              if Assigned(FLastRequest) then
                FLastRequest(FRequestLine);

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
                  Req.URI := FURI;
                  Req.Method := FMethod;
                  Req.RequestLine := FRequestLine;
                  Req.Headers.Assign(Headers);
                  Req.QueryParams.Assign(QueryParams);

                  CloseConnection := (Headers.Values['Connection'] = 'close') or (Pos('HTTP/1.0', FRequestLine) = 1);

                  for I := 0 to FMiddlewares.Count - 1 do
                  begin
                    MiddlewareWrapper := TMiddlewareWrapper(FMiddlewares[I]);
                    if not MiddlewareWrapper.Middleware(Req, Resp) then
                    begin
                      ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, Resp.Stream, Resp.ContentType, CloseConnection);
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
                      FResponseLine := Format('HTTP/1.1 %d %s', [Resp.StatusCode, Resp.Body]);
                      if Assigned(FLastResponse) then
                        FLastResponse(FResponseLine);
                      if CloseConnection then Break;
                      Continue;
                    end;
                  end;

{$IF CompilerVersion >= 20}
                  if not FRouteManager.FRoutes.TryGetValue(FURI.ToLower, LRoute) then
                    LRoute := nil;
{$ELSE}
                  Index := FRouteManager.FRoutes.IndexOf(FURI.ToLower);
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
                    ResponseHeader := BuildHTTPResponse(Resp.StatusCode, Resp.Body, Resp.Stream, Resp.ContentType, CloseConnection);
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
                    FResponseLine := Format('HTTP/1.1 %d %s', [Resp.StatusCode, Resp.Body]);
                  end
                  else
                  begin
                    ResponseHeader := BuildHTTPResponse(HTTP_NOT_FOUND, 'Not Found', nil, TEXT_PLAIN, CloseConnection);
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
                    FResponseLine := Format('HTTP/1.1 %d %s', [HTTP_NOT_FOUND, Resp.Body]);
                  end;

                  if Assigned(FLastResponse) then
                    FLastResponse(FResponseLine);
                  if CloseConnection then Break;
                finally
                  if Assigned(BodyStream) then
                    FreeAndNil(BodyStream);
                end;
              finally
                Headers.Free;
              end;
            finally
              FCriticalSection.Leave;
            end;
          end;
        end
        else
          Break;
      until False;
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

      if Assigned(FClientSocket) then
      begin
        FClientSocket.CloseSocket;
        FreeAndNil(FClientSocket);
      end;
    end;
  except
    on E: Exception do
    begin
      ResponseHeader := BuildHTTPResponse(HTTP_INTERNAL_SERVER_ERROR, 'Internal Server Error: ' + E.Message, nil, TEXT_PLAIN, True);
      FClientSocket.SendString(ResponseHeader);
{$IFDEF VER150}
      UTF8Body := UTF8Encode('Internal Server Error: ' + E.Message);
      SetLength(ResponseBodyBytes, Length(UTF8Body));
      Move(UTF8Body[1], ResponseBodyBytes[0], Length(UTF8Body));
      if Length(ResponseBodyBytes) > 0 then
        FClientSocket.SendBuffer(@ResponseBodyBytes[0], Length(ResponseBodyBytes));
{$ELSE}
      ResponseBodyBytes := TEncoding.UTF8.GetBytes('Internal Server Error: ' + E.Message);
      if Length(ResponseBodyBytes) > 0 then
        FClientSocket.SendBuffer(@ResponseBodyBytes[0], Length(ResponseBodyBytes));
{$ENDIF}
      FResponseLine := Format('HTTP/1.1 %d', [HTTP_INTERNAL_SERVER_ERROR]);
      if Assigned(FLastResponse) then
        FLastResponse(FResponseLine);
    end;
  end;
end;

end.
