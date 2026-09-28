unit BadgerHttpDispatch;

{ Shared HTTP/WS-upgrade dispatch for IOCP and epoll. Engines parse, send, and
  begin WebSocket; this unit runs MW, routes, CORS, and Assemble. }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, BadgerTypes, BadgerHttpParser, BadgerHttpStatus,
  BadgerRouteManager, BadgerLogger;

type
  TBadgerDispatchPipeline = record
    RouteManager: TRouteManager;
    Middlewares: TList;
    AfterMiddlewares: TList;
    CorsEnabled: Boolean;
    CorsAllowedOrigins: TStringList;
    CorsAllowedMethods: TStringList;
    CorsAllowedHeaders: TStringList;
    CorsExposeHeaders: TStringList;
    CorsAllowCredentials: Boolean;
    CorsMaxAge: Integer;
    EnableEventInfo: Boolean;
    OnRequest: TOnRequest;
    OnResponse: TOnResponse;
    { X-Real-IP / X-Forwarded-For sobrescreviam o IP do peer sempre: qualquer cliente
      se declarava o IP que quisesse em log, rate limit e regra de auth. Agora e
      opt-in, e com lista de proxies quando informada. }
    TrustProxyHeaders: Boolean;
    TrustedProxies: TStringList;
  end;

  TBadgerDispatchResult = record
    Wire: AnsiString;
    CloseConn: Boolean;
    WsUpgrade: Boolean;
    WsURI: string;
    WsKey: string;
  end;

procedure BadgerAssignDispatchPipeline(var P: TBadgerDispatchPipeline;
  ARouteManager: TRouteManager; AMiddlewares, AAfterMiddlewares: TList;
  ACorsEnabled: Boolean;
  ACorsOrigins, ACorsMethods, ACorsHeaders, ACorsExpose: TStringList;
  ACorsAllowCredentials: Boolean; ACorsMaxAge: Integer;
  AEnableEventInfo: Boolean; AOnRequest: TOnRequest; AOnResponse: TOnResponse);

function BadgerDispatchHttp(Parser: TBadgerHttpParser; Conn: TBadgerConn;
  RouteParams, RespHeaders: TStringList;
  const Pipeline: TBadgerDispatchPipeline): TBadgerDispatchResult;

implementation

procedure BadgerAssignDispatchPipeline(var P: TBadgerDispatchPipeline;
  ARouteManager: TRouteManager; AMiddlewares, AAfterMiddlewares: TList;
  ACorsEnabled: Boolean;
  ACorsOrigins, ACorsMethods, ACorsHeaders, ACorsExpose: TStringList;
  ACorsAllowCredentials: Boolean; ACorsMaxAge: Integer;
  AEnableEventInfo: Boolean; AOnRequest: TOnRequest; AOnResponse: TOnResponse);
begin
  FillChar(P, SizeOf(P), 0);
  P.RouteManager := ARouteManager;
  P.Middlewares := AMiddlewares;
  P.AfterMiddlewares := AAfterMiddlewares;
  P.CorsEnabled := ACorsEnabled;
  P.CorsAllowedOrigins := ACorsOrigins;
  P.CorsAllowedMethods := ACorsMethods;
  P.CorsAllowedHeaders := ACorsHeaders;
  P.CorsExposeHeaders := ACorsExpose;
  P.CorsAllowCredentials := ACorsAllowCredentials;
  P.CorsMaxAge := ACorsMaxAge;
  P.EnableEventInfo := AEnableEventInfo;
  P.OnRequest := AOnRequest;
  P.OnResponse := AOnResponse;
end;

procedure AttachParserBody(Parser: TBadgerHttpParser; var Req: THTTPRequest);
var
  CT: string;
  Raw: AnsiString;
begin
  if not Assigned(Parser) then
    Exit;
  Raw := Parser.Body;
  if Length(Raw) = 0 then
    Exit;
  CT := LowerCase(Req.Headers.Values['Content-Type']);
  if (Pos('application/json', CT) > 0) or (Pos('text/', CT) > 0) then
    Req.Body := Utf8BytesToString(Raw)
  else
  begin
    Req.BodyStream := TMemoryStream.Create;
    Req.BodyStream.WriteBuffer(Raw[1], Length(Raw));
    Req.BodyStream.Position := 0;
  end;
end;

procedure RunAfterMiddlewares(const Pipeline: TBadgerDispatchPipeline;
  var Req: THTTPRequest; var Resp: THTTPResponse);
var
  I: Integer;
  AfterWrapper: TAfterMiddlewareWrapper;
begin
  if not Assigned(Pipeline.AfterMiddlewares) then
    Exit;
  for I := Pipeline.AfterMiddlewares.Count - 1 downto 0 do
  begin
    AfterWrapper := TAfterMiddlewareWrapper(Pipeline.AfterMiddlewares[I]);
    if not Assigned(AfterWrapper) or not Assigned(AfterWrapper.Middleware) then
      Continue;
    try
      AfterWrapper.Middleware(Req, Resp);
    except
      on E: Exception do
        Logger.Error('After-middleware exception: ' + E.Message);
    end;
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

function HandleCorsPreflight(const Pipeline: TBadgerDispatchPipeline;
  const Req: THTTPRequest; var Resp: THTTPResponse): Boolean;
var
  Origin, ACRM, ACRH, AllowOrigin, MethodsStr, HeadersStr, HdrPart: string;
  AllowWildcardOrigin, OriginAllowed: Boolean;
  I: Integer;
  HdrParts: TStringList;
begin
  Result := False;
  if not Pipeline.CorsEnabled then
    Exit;
  if Req.Method <> 'OPTIONS' then
    Exit;
  Origin := Trim(Req.Headers.Values['Origin']);
  ACRM := Trim(Req.Headers.Values['Access-Control-Request-Method']);
  ACRH := Req.Headers.Values['Access-Control-Request-Headers'];
  if (Origin = '') or (ACRM = '') then
    Exit;

  Result := True;
  AllowWildcardOrigin := Pipeline.CorsAllowedOrigins.IndexOf('*') >= 0;
  if Pipeline.CorsAllowCredentials then
    OriginAllowed := Pipeline.CorsAllowedOrigins.IndexOf(Origin) >= 0
  else
    OriginAllowed := AllowWildcardOrigin or (Pipeline.CorsAllowedOrigins.IndexOf(Origin) >= 0);

  if not OriginAllowed then
  begin
    Resp.StatusCode := HTTP_FORBIDDEN;
    Resp.Body := 'CORS origin not allowed';
    Resp.ContentType := TEXT_PLAIN;
    Exit;
  end;

  if Pipeline.CorsAllowedMethods.IndexOf(UpperCase(ACRM)) < 0 then
  begin
    Resp.StatusCode := HTTP_METHOD_NOT_ALLOWED;
    Resp.Body := '';
    Resp.ContentType := '';
    Resp.HeadersCustom.Values['Allow'] := Pipeline.CorsAllowedMethods.CommaText;
    Exit;
  end;

  HeadersStr := '';
  if ACRH <> '' then
  begin
    HdrParts := TStringList.Create;
    try
      SplitCommaTokens(ACRH, HdrParts);
      for I := 0 to HdrParts.Count - 1 do
      begin
        HdrPart := Trim(HdrParts[I]);
        if HdrPart = '' then
          Continue;
        if Pipeline.CorsAllowedHeaders.IndexOf(HdrPart) < 0 then
        begin
          Resp.StatusCode := HTTP_BAD_REQUEST;
          Resp.Body := '';
          Resp.ContentType := '';
          Exit;
        end;
        if HeadersStr = '' then
          HeadersStr := HdrPart
        else
          HeadersStr := HeadersStr + ',' + HdrPart;
      end;
    finally
      HdrParts.Free;
    end;
  end
  else
    HeadersStr := Pipeline.CorsAllowedHeaders.CommaText;

  if Pipeline.CorsAllowCredentials then
    AllowOrigin := Origin
  else if AllowWildcardOrigin then
    AllowOrigin := '*'
  else
    AllowOrigin := Origin;

  MethodsStr := Pipeline.CorsAllowedMethods.CommaText;
  Resp.StatusCode := HTTP_NO_CONTENT;
  Resp.Body := '';
  Resp.ContentType := '';
  Resp.HeadersCustom.Values['Access-Control-Allow-Origin'] := AllowOrigin;
  Resp.HeadersCustom.Values['Access-Control-Allow-Methods'] := MethodsStr;
  Resp.HeadersCustom.Values['Access-Control-Allow-Headers'] := HeadersStr;
  if Pipeline.CorsAllowCredentials then
    Resp.HeadersCustom.Values['Access-Control-Allow-Credentials'] := 'true';
  if Pipeline.CorsMaxAge > 0 then
    Resp.HeadersCustom.Values['Access-Control-Max-Age'] := IntToStr(Pipeline.CorsMaxAge);
  if AllowOrigin <> '*' then
    Resp.HeadersCustom.Values['Vary'] :=
      'Origin, Access-Control-Request-Method, Access-Control-Request-Headers';
end;

procedure ApplyCorsHeaders(const Pipeline: TBadgerDispatchPipeline;
  const Req: THTTPRequest; var Resp: THTTPResponse);
var
  Origin, AllowOrigin, ExposeStr: string;
  AllowWildcardOrigin, OriginAllowed: Boolean;
begin
  if not Pipeline.CorsEnabled then
    Exit;
  Origin := Trim(Req.Headers.Values['Origin']);
  if Origin = '' then
    Exit;
  AllowWildcardOrigin := Pipeline.CorsAllowedOrigins.IndexOf('*') >= 0;
  if Pipeline.CorsAllowCredentials then
    OriginAllowed := Pipeline.CorsAllowedOrigins.IndexOf(Origin) >= 0
  else
    OriginAllowed := AllowWildcardOrigin or (Pipeline.CorsAllowedOrigins.IndexOf(Origin) >= 0);
  if not OriginAllowed then
    Exit;
  if Pipeline.CorsAllowCredentials then
    AllowOrigin := Origin
  else if AllowWildcardOrigin then
    AllowOrigin := '*'
  else
    AllowOrigin := Origin;
  Resp.HeadersCustom.Values['Access-Control-Allow-Origin'] := AllowOrigin;
  if Pipeline.CorsAllowCredentials then
    Resp.HeadersCustom.Values['Access-Control-Allow-Credentials'] := 'true';
  ExposeStr := Pipeline.CorsExposeHeaders.CommaText;
  if ExposeStr <> '' then
    Resp.HeadersCustom.Values['Access-Control-Expose-Headers'] := ExposeStr;
  if AllowOrigin <> '*' then
    Resp.HeadersCustom.Values['Vary'] := 'Origin';
end;

procedure FireHttpEvents(const Pipeline: TBadgerDispatchPipeline;
  const Req: THTTPRequest; const Resp: THTTPResponse; const ResponseHeader: string);
var
  RequestInfo: TRequestInfo;
  ResponseInfo: TResponseInfo;
begin
  if not Pipeline.EnableEventInfo then
    Exit;
  if Assigned(Pipeline.OnRequest) then
  begin
    FillChar(RequestInfo, SizeOf(RequestInfo), 0);
    RequestInfo.Headers := TStringList.Create;
    RequestInfo.QueryParams := TStringList.Create;
    try
      RequestInfo.RemoteIP := Req.FRemoteIP;
      RequestInfo.Method := Req.Method;
      RequestInfo.URI := Req.URI;
      RequestInfo.RequestLine := Req.RequestLine;
      if Assigned(Req.Headers) then
        RequestInfo.Headers.Assign(Req.Headers);
      RequestInfo.Body := Req.Body;
      if Assigned(Req.QueryParams) then
        RequestInfo.QueryParams.Assign(Req.QueryParams);
      RequestInfo.Timestamp := Now;
      { Callback de log do usuario: excecao aqui escapava de BadgerDispatchHttp com
        a resposta ja montada, e o cliente ficava sem ela. }
      try
        Pipeline.OnRequest(RequestInfo);
      except
        on E: Exception do
          Logger.Error('OnRequest exception: ' + E.Message);
      end;
    finally
      RequestInfo.Headers.Free;
      RequestInfo.QueryParams.Free;
    end;
  end;
  if Assigned(Pipeline.OnResponse) then
  begin
    FillChar(ResponseInfo, SizeOf(ResponseInfo), 0);
    ResponseInfo.Headers := TStringList.Create;
    try
      ResponseInfo.StatusCode := Resp.StatusCode;
      ResponseInfo.StatusText := THTTPStatus.GetStatusText(Resp.StatusCode);
      ResponseInfo.Body := Resp.Body;
      ResponseInfo.ContentType := Resp.ContentType;
      ResponseInfo.Headers.Text := ResponseHeader;
      ResponseInfo.Timestamp := Now;
      try
        Pipeline.OnResponse(ResponseInfo);
      except
        on E: Exception do
          Logger.Error('OnResponse exception: ' + E.Message);
      end;
    finally
      ResponseInfo.Headers.Free;
    end;
  end;
end;

function BadgerDispatchHttp(Parser: TBadgerHttpParser; Conn: TBadgerConn;
  RouteParams, RespHeaders: TStringList;
  const Pipeline: TBadgerDispatchPipeline): TBadgerDispatchResult;
var
  Req: THTTPRequest;
  Resp: THTTPResponse;
  RouteEntry: TRouteEntry;
  MiddlewareWrapper: TMiddlewareWrapper;
  Handled: Boolean;
  SkipRoute: Boolean;
  I: Integer;
  LForwardedFor: string;
  Header: string;
  WSKey: string;
begin
  Result.Wire := '';
  Result.CloseConn := True;
  Result.WsUpgrade := False;
  Result.WsURI := '';
  Result.WsKey := '';
  if not Assigned(Parser) then
    Exit;

  FillChar(Req, SizeOf(Req), 0);
  FillChar(Resp, SizeOf(Resp), 0);
  if Assigned(RouteParams) then
    RouteParams.Clear;
  if Assigned(RespHeaders) then
    RespHeaders.Clear;
  Req.Headers := Parser.Headers;
  Req.QueryParams := Parser.QueryParams;
  Req.RouteParams := RouteParams;
  Resp.HeadersCustom := RespHeaders;
  Handled := False;
  SkipRoute := False;
  try
    try
      Req.Socket := nil;
      Req.Method := Parser.Method;
      Req.URI := Parser.URI;
      Req.RequestLine := Parser.RequestLine;
      AttachParserBody(Parser, Req);
      if Assigned(Conn) then
        Req.FRemoteIP := Conn.RemoteIP;

      if Pipeline.TrustProxyHeaders and
         ((not Assigned(Pipeline.TrustedProxies)) or
          (Pipeline.TrustedProxies.Count = 0) or
          (Pipeline.TrustedProxies.IndexOf(Req.FRemoteIP) >= 0)) then
      begin
        LForwardedFor := Trim(Parser.RealIP);
        if LForwardedFor <> '' then
          Req.FRemoteIP := LForwardedFor
        else
        begin
          LForwardedFor := Parser.ForwardedFor;
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

      if Parser.State = hpsError then
      begin
        if Parser.Error = 'body too large' then
        begin
          Resp.StatusCode := HTTP_PAYLOAD_TOO_LARGE;
          Resp.Body := '{"error":"Request body too large"}';
          Resp.ContentType := APPLICATION_JSON;
        end
        else
        begin
          Resp.StatusCode := HTTP_BAD_REQUEST;
          Resp.Body := Parser.Error;
          Resp.ContentType := TEXT_PLAIN;
        end;
        Handled := True;
        SkipRoute := True;
      end;

      if (not SkipRoute) and HandleCorsPreflight(Pipeline, Req, Resp) then
        SkipRoute := True
      else if not SkipRoute then
      begin
        try
          if Assigned(Pipeline.Middlewares) then
          begin
            for I := 0 to Pipeline.Middlewares.Count - 1 do
            begin
              MiddlewareWrapper := TMiddlewareWrapper(Pipeline.Middlewares[I]);
              try
                if MiddlewareWrapper.Middleware(Req, Resp) then
                begin
                  Handled := True;
                  Break;
                end;
              except
                on E: Exception do
                begin
                  { Detalhe interno so no log: a mensagem crua vazava SQL/paths e
                    quebrava o JSON quando continha aspas. }
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
            (RFC 6455 §4.1) e uma rota declarada com AddWebSocket. }
          if (not Handled) and Parser.IsWebSocketUpgrade then
          begin
            Handled := True;
            WSKey := Trim(Req.Headers.Values['Sec-WebSocket-Key']);
            if Req.Method <> CGET then
            begin
              Resp.StatusCode := HTTP_METHOD_NOT_ALLOWED;
              Resp.Body := '{"error":"WebSocket upgrade requires GET"}';
              Resp.ContentType := APPLICATION_JSON;
              Resp.HeadersCustom.Values['Allow'] := CGET;
            end
            else if WSKey = '' then
            begin
              Resp.StatusCode := HTTP_BAD_REQUEST;
              Resp.Body := '{"error":"Missing Sec-WebSocket-Key"}';
              Resp.ContentType := APPLICATION_JSON;
            end
            else if Req.Headers.Values['Sec-WebSocket-Version'] <> '13' then
            begin
              Resp.StatusCode := HTTP_BAD_REQUEST;
              Resp.Body := '{"error":"Unsupported WebSocket version"}';
              Resp.ContentType := APPLICATION_JSON;
              Resp.HeadersCustom.Values['Sec-WebSocket-Version'] := '13';
            end
            else if not (Assigned(Pipeline.RouteManager) and
              Pipeline.RouteManager.MatchRoute(CWS, Parser.URI, RouteEntry, Req.RouteParams)) then
            begin
              Resp.StatusCode := HTTP_NOT_FOUND;
              Resp.Body := 'Not Found';
              Resp.ContentType := TEXT_PLAIN;
            end
            else
            begin
              Result.WsUpgrade := True;
              Result.WsURI := Req.URI;
              Result.WsKey := WSKey;
            end;
          end;

          if not Handled then
          begin
            { HEAD deve existir onde GET existe (RFC 7231 4.3.2): sem este fallback
              um health check por HEAD recebe 404. O corpo e removido no final. }
            if Assigned(Pipeline.RouteManager) and
              (Pipeline.RouteManager.MatchRoute(Req.Method, Parser.URI, RouteEntry, Req.RouteParams) or
               ((Req.Method = 'HEAD') and
                Pipeline.RouteManager.MatchRoute(CGET, Parser.URI, RouteEntry, Req.RouteParams))) then
            begin
              if Assigned(RouteEntry) and Assigned(TMethod(RouteEntry.Callback).Code) then
              begin
                try
                  RouteEntry.Callback(Req, Resp);
                except
                  on E: Exception do
                  begin
                    Logger.Error('Route exception: ' + E.Message);
                    Resp.StatusCode := HTTP_INTERNAL_SERVER_ERROR;
                    Resp.Body := '{"error":"Internal Server Error"}';
                    Resp.ContentType := APPLICATION_JSON;
                  end;
                end;
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

          if Parser.Origin <> '' then
            ApplyCorsHeaders(Pipeline, Req, Resp);
        finally
          RunAfterMiddlewares(Pipeline, Req, Resp);
        end;
      end;
    except
      on E: Exception do
      begin
        Logger.Error('Dispatch exception: ' + E.Message);
        Resp.StatusCode := HTTP_INTERNAL_SERVER_ERROR;
        Resp.Body := 'Internal Server Error';
        Resp.ContentType := TEXT_PLAIN;
      end;
    end;

    if not Result.WsUpgrade then
    begin
      Result.CloseConn := Parser.WantsClose;
      if Parser.State = hpsError then
        Result.CloseConn := True;
      { Empty DateValue: assembler writes Date on the wire without a heap Format temp
        (same role as Synapse Rfc822DateTime(Now) in THTTPRequestHandler). }
      Result.Wire := BadgerAssembleHTTPMessage(Resp.StatusCode, Resp.Body, Resp.Stream,
        Resp.ContentType, Result.CloseConn, Resp.HeadersCustom, '');
      if Req.Method = 'HEAD' then
        Result.Wire := BadgerStripBody(Result.Wire);
      if Pipeline.EnableEventInfo and (Assigned(Pipeline.OnRequest) or Assigned(Pipeline.OnResponse)) then
      begin
        Header := string(Copy(Result.Wire, 1, Pos(AnsiString(#13#10#13#10), Result.Wire) + 3));
        FireHttpEvents(Pipeline, Req, Resp, Header);
      end;
    end;
  finally
    if Assigned(Resp.Stream) then
      Resp.Stream.Free;
    if Assigned(Req.BodyStream) then
      Req.BodyStream.Free;
  end;
end;

end.
