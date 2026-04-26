unit BadgerTests;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

{
  Badger TDD test suite — boundary tests over real HTTP.

  Pattern per test:
    1. Spin up a TBadger on a unique high port.
    2. Register the routes / middlewares the test needs.
    3. Drive it via raw TTCPBlockSocket through BadgerTestClient.
    4. Assert on the wire bytes.
    5. Stop and free the server.

  Tests cover the CRIT/HIGH items from AUDIT_TODO.md verified by the
  20260423_AUDIT_DEEP_DIVE.md plus a few sanity tests. Adding a new test:
    1. Add `procedure TestXxx;` below.
    2. Add `RegisterTest('Group', 'TestXxx', TestXxx);` in RegisterAllTests.
}

interface

procedure RegisterAllTests;

implementation

uses
  SysUtils, Classes, BadgerTestSuite, BadgerTestClient,
  BadgerTestRoutes, Badger, BadgerLogger, BadgerTypes, BadgerUtils,
  BadgerBasicAuth, BadgerAuthJWT;

const
  STARTUP_DELAY_MS = 200;

type
  TServerConfig = record
    Port: Integer;
    EnableCors: Boolean;
    CorsAllowCreds: Boolean;
    CorsOrigins: string;  // Comma-separated; '*' means add wildcard.
  end;

function MakeConfig: TServerConfig;
begin
  Result.Port := NextTestPort;
  Result.EnableCors := False;
  Result.CorsAllowCreds := False;
  Result.CorsOrigins := '';
end;

procedure ApplyCors(AServer: TBadger; const ACfg: TServerConfig);
var
  TmpList: TStringList;
  I: Integer;
begin
  if not ACfg.EnableCors then
    Exit;
  AServer.CorsEnabled := True;
  AServer.CorsAllowCredentials := ACfg.CorsAllowCreds;
  if ACfg.CorsOrigins <> '' then
  begin
    TmpList := TStringList.Create;
    try
      TmpList.CommaText := ACfg.CorsOrigins;
      for I := 0 to TmpList.Count - 1 do
        AServer.CorsAllowedOrigins.Add(Trim(TmpList[I]));
    finally
      TmpList.Free;
    end;
  end;
end;

function MakeServer(const ACfg: TServerConfig): TBadger;
begin
  Result := TBadger.Create;
  Result.Port := ACfg.Port;
  Result.EnableEventInfo := False;
  Result.Timeout := 2000;
  Result.ParallelProcessing := True;
  Result.MaxConcurrentConnections := 50;
  ApplyCors(Result, ACfg);
end;

procedure StartAndWait(AServer: TBadger);
begin
  AServer.Start;
  Sleep(STARTUP_DELAY_MS);
end;

procedure StopAndFree(var AServer: TBadger);
begin
  if Assigned(AServer) then
  begin
    try
      AServer.Stop;
    except
      // swallow — test reports its own outcome
    end;
    try
      AServer.Free;
    except
    end;
    AServer := nil;
  end;
end;

{ ------------------------ Sanity ------------------------ }

procedure Test_Sanity_Ping_200_Pong;
var
  Cfg: TServerConfig;
  Server: TBadger;
  R: TTestHttpResponse;
begin
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddGet('/ping', TTestRoutes.Ping);
    StartAndWait(Server);
    R := HttpGet(Cfg.Port, '/ping');
    AssertEqualsInt(200, R.StatusCode, 'Ping must return 200');
    AssertEqualsStr('Pong', R.Body, 'Ping body');
  finally
    StopAndFree(Server);
  end;
end;

procedure Test_Sanity_Unknown_404;
var
  Cfg: TServerConfig;
  Server: TBadger;
  R: TTestHttpResponse;
begin
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddGet('/ping', TTestRoutes.Ping);
    StartAndWait(Server);
    R := HttpGet(Cfg.Port, '/no-such-route');
    AssertEqualsInt(404, R.StatusCode, 'Unknown route must be 404');
  finally
    StopAndFree(Server);
  end;
end;

procedure Test_Sanity_RouteParam_Extracted;
var
  Cfg: TServerConfig;
  Server: TBadger;
  R: TTestHttpResponse;
begin
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddGet('/items/:id', TTestRoutes.RouteParam);
    StartAndWait(Server);
    R := HttpGet(Cfg.Port, '/items/42');
    AssertEqualsInt(200, R.StatusCode, 'Route-param request must be 200');
    AssertEqualsStr('id=42', R.Body, 'Route param value');
  finally
    StopAndFree(Server);
  end;
end;

{ ------------------------ Headers ------------------------ }

procedure Test_Headers_CRLFInjection_StrippedFromValue;
var
  Cfg: TServerConfig;
  Server: TBadger;
  R: TTestHttpResponse;
begin
  // §5.1: route writes value with embedded CRLF; sanitiser must strip it
  // so the wire never carries an injected "X-Injected" header line.
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddGet('/inject', TTestRoutes.InjectHeader);
    StartAndWait(Server);
    R := HttpGet(Cfg.Port, '/inject');
    AssertEqualsInt(200, R.StatusCode, 'Inject route status');
    AssertNotContains(#13#10 + 'X-Injected:', R.Raw,
      'Response must NOT contain a CR/LF-injected header');
    AssertNotContains(#10 + 'X-Injected:', R.Raw,
      'Response must NOT contain a LF-injected header');
  finally
    StopAndFree(Server);
  end;
end;

procedure Test_Headers_OversizeLine_Rejected;
var
  Cfg: TServerConfig;
  Server: TBadger;
  R: TTestHttpResponse;
  Big: string;
  I: Integer;
  Req: string;
begin
  // §3.3: per-line cap is 16384 bytes. Send 20 KB header, expect rejection.
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddGet('/ping', TTestRoutes.Ping);
    StartAndWait(Server);
    SetLength(Big, 20480);
    for I := 1 to Length(Big) do
      Big[I] := 'A';
    Req :=
      'GET /ping HTTP/1.1' + CRLF +
      'Host: 127.0.0.1' + CRLF +
      'Connection: close' + CRLF +
      'X-Big: ' + Big + CRLF +
      CRLF;
    R := HttpRaw(Cfg.Port, Req);
    AssertTrue((R.StatusCode = 0) or (R.StatusCode >= 400),
      'Oversize header line must NOT yield 2xx (got ' + IntToStr(R.StatusCode) + ')');
  finally
    StopAndFree(Server);
  end;
end;

{ ------------------------ Keep-Alive ------------------------ }

procedure Test_KeepAlive_TwoRequests_BothSucceed;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Sess: TKeepAliveSession;
  R1, R2: TTestHttpResponse;
begin
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddGet('/ping', TTestRoutes.Ping);
    StartAndWait(Server);
    Sess := TKeepAliveSession.Create(Cfg.Port);
    try
      R1 := Sess.Exchange(
        'GET /ping HTTP/1.1' + CRLF +
        'Host: 127.0.0.1' + CRLF +
        'Connection: keep-alive' + CRLF + CRLF);
      AssertEqualsInt(200, R1.StatusCode, 'First request status');
      AssertEqualsStr('Pong', R1.Body, 'First request body');
      R2 := Sess.Exchange(
        'GET /ping HTTP/1.1' + CRLF +
        'Host: 127.0.0.1' + CRLF +
        'Connection: close' + CRLF + CRLF);
      AssertEqualsInt(200, R2.StatusCode, 'Second request status');
      AssertEqualsStr('Pong', R2.Body, 'Second request body');
    finally
      Sess.Free;
    end;
  finally
    StopAndFree(Server);
  end;
end;

procedure Test_KeepAlive_NoCustomHeaderLeak;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Sess: TKeepAliveSession;
  R1, R2: TTestHttpResponse;
begin
  // §12.1: req A sets X-Trace via custom response header; req B on the same
  // socket must NOT see that header in its response.
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddGet('/set', TTestRoutes.SetTraceHeader);
    Server.RouteManager.AddGet('/check', TTestRoutes.CheckTraceHeader);
    StartAndWait(Server);
    Sess := TKeepAliveSession.Create(Cfg.Port);
    try
      R1 := Sess.Exchange(
        'GET /set HTTP/1.1' + CRLF +
        'Host: 127.0.0.1' + CRLF +
        'Connection: keep-alive' + CRLF + CRLF);
      AssertEqualsInt(200, R1.StatusCode, '/set status');
      AssertContains('X-Trace', R1.HeadersBlock, '/set must carry X-Trace');

      R2 := Sess.Exchange(
        'GET /check HTTP/1.1' + CRLF +
        'Host: 127.0.0.1' + CRLF +
        'Connection: close' + CRLF + CRLF);
      AssertEqualsInt(200, R2.StatusCode, '/check status');
      AssertNotContains('X-Trace', R2.HeadersBlock,
        '/check must NOT inherit X-Trace from the previous iteration');
    finally
      Sess.Free;
    end;
  finally
    StopAndFree(Server);
  end;
end;

procedure Test_KeepAlive_NoQueryParamLeak;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Sess: TKeepAliveSession;
  R1, R2: TTestHttpResponse;
begin
  // Route A receives ?leak=yes; route B on same socket queries with no params
  // and must observe count=0.
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddGet('/q', TTestRoutes.EchoQuery);
    StartAndWait(Server);
    Sess := TKeepAliveSession.Create(Cfg.Port);
    try
      R1 := Sess.Exchange(
        'GET /q?leak=yes HTTP/1.1' + CRLF +
        'Host: 127.0.0.1' + CRLF +
        'Connection: keep-alive' + CRLF + CRLF);
      AssertEqualsInt(200, R1.StatusCode, 'First query status');
      AssertContains('leak=yes', R1.Body, 'First query carries the param');

      R2 := Sess.Exchange(
        'GET /q HTTP/1.1' + CRLF +
        'Host: 127.0.0.1' + CRLF +
        'Connection: close' + CRLF + CRLF);
      AssertEqualsInt(200, R2.StatusCode, 'Second query status');
      AssertContains('count=0', R2.Body,
        'Second query must not see leaked params from the first');
    finally
      Sess.Free;
    end;
  finally
    StopAndFree(Server);
  end;
end;

{ ------------------------ Body / Content-Length ------------------------ }

procedure Test_Body_NegativeContentLength_400_AndConnSync;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Sess: TKeepAliveSession;
  R1, R2: TTestHttpResponse;
begin
  // §3.2: Content-Length: -1 must be rejected with 400 and the connection
  // must remain in sync for a follow-up request.
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddGet('/ping', TTestRoutes.Ping);
    Server.RouteManager.AddPost('/echo', TTestRoutes.EchoBody);
    StartAndWait(Server);
    Sess := TKeepAliveSession.Create(Cfg.Port);
    try
      R1 := Sess.Exchange(
        'POST /echo HTTP/1.1' + CRLF +
        'Host: 127.0.0.1' + CRLF +
        'Content-Length: -1' + CRLF +
        'Connection: keep-alive' + CRLF + CRLF);
      AssertEqualsInt(400, R1.StatusCode, 'Negative Content-Length must yield 400');
      R2 := Sess.Exchange(
        'GET /ping HTTP/1.1' + CRLF +
        'Host: 127.0.0.1' + CRLF +
        'Connection: close' + CRLF + CRLF);
      AssertEqualsInt(200, R2.StatusCode,
        'Connection must remain in sync after rejected Content-Length');
    finally
      Sess.Free;
    end;
  finally
    StopAndFree(Server);
  end;
end;

procedure Test_Body_ValidPost_Echoed;
var
  Cfg: TServerConfig;
  Server: TBadger;
  R: TTestHttpResponse;
begin
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddPost('/echo', TTestRoutes.EchoBody);
    StartAndWait(Server);
    R := HttpPost(Cfg.Port, '/echo', 'text/plain', 'hello world');
    AssertEqualsInt(200, R.StatusCode, 'POST status');
    AssertEqualsStr('hello world', R.Body, 'POST body echoed');
  finally
    StopAndFree(Server);
  end;
end;

{ ------------------------ Chunked Transfer-Encoding ------------------------ }

procedure Test_Chunked_RoundTrip_KeepAlive;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Sess: TKeepAliveSession;
  R1, R2: TTestHttpResponse;
begin
  // §3.4: POST with Transfer-Encoding: chunked, then GET on same socket.
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddGet('/ping', TTestRoutes.Ping);
    Server.RouteManager.AddPost('/echo', TTestRoutes.EchoBody);
    StartAndWait(Server);
    Sess := TKeepAliveSession.Create(Cfg.Port);
    try
      R1 := Sess.Exchange(
        'POST /echo HTTP/1.1' + CRLF +
        'Host: 127.0.0.1' + CRLF +
        'Transfer-Encoding: chunked' + CRLF +
        'Content-Type: text/plain' + CRLF +
        'Connection: keep-alive' + CRLF + CRLF +
        '5' + CRLF + 'hello' + CRLF +
        '0' + CRLF + CRLF);
      AssertEqualsInt(200, R1.StatusCode, 'Chunked POST status');
      AssertContains('hello', R1.Body, 'Chunked body re-assembled');
      R2 := Sess.Exchange(
        'GET /ping HTTP/1.1' + CRLF +
        'Host: 127.0.0.1' + CRLF +
        'Connection: close' + CRLF + CRLF);
      AssertEqualsInt(200, R2.StatusCode,
        'Follow-up GET on same socket after chunked POST must succeed');
      AssertEqualsStr('Pong', R2.Body, 'Follow-up GET body');
    finally
      Sess.Free;
    end;
  finally
    StopAndFree(Server);
  end;
end;

{ ------------------------ CORS ------------------------ }

procedure Test_CORS_Disabled_NoHeaders;
var
  Cfg: TServerConfig;
  Server: TBadger;
  R: TTestHttpResponse;
begin
  Cfg := MakeConfig;
  Cfg.EnableCors := False;
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddGet('/ping', TTestRoutes.Ping);
    StartAndWait(Server);
    R := HttpGetWithHeaders(Cfg.Port, '/ping', 'Origin: https://x.test' + CRLF);
    AssertEqualsInt(200, R.StatusCode, 'Status');
    AssertEqualsStr('', HeaderValue(R, 'Access-Control-Allow-Origin'),
      'CORS disabled => no ACAO header');
  finally
    StopAndFree(Server);
  end;
end;

procedure Test_CORS_Credentials_RejectsWildcardReflection;
var
  Cfg: TServerConfig;
  Server: TBadger;
  R: TTestHttpResponse;
begin
  // §5.3: credentials=true + only '*' configured must NOT reflect Origin.
  Cfg := MakeConfig;
  Cfg.EnableCors := True;
  Cfg.CorsAllowCreds := True;
  Cfg.CorsOrigins := '*';
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddGet('/ping', TTestRoutes.Ping);
    StartAndWait(Server);
    R := HttpGetWithHeaders(Cfg.Port, '/ping',
      'Origin: https://evil.test' + CRLF);
    AssertNotContains('https://evil.test',
      HeaderValue(R, 'Access-Control-Allow-Origin'),
      'Must NOT reflect arbitrary Origin when credentials=true and only "*" is allowed');
  finally
    StopAndFree(Server);
  end;
end;

procedure Test_CORS_Credentials_AllowsExplicitOrigin;
var
  Cfg: TServerConfig;
  Server: TBadger;
  R: TTestHttpResponse;
  Allowed: string;
begin
  Cfg := MakeConfig;
  Cfg.EnableCors := True;
  Cfg.CorsAllowCreds := True;
  Cfg.CorsOrigins := 'https://good.test';
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddGet('/ping', TTestRoutes.Ping);
    StartAndWait(Server);
    R := HttpGetWithHeaders(Cfg.Port, '/ping',
      'Origin: https://good.test' + CRLF);
    Allowed := HeaderValue(R, 'Access-Control-Allow-Origin');
    AssertContains('https://good.test', Allowed,
      'Explicitly-listed origin must be returned in ACAO');
  finally
    StopAndFree(Server);
  end;
end;

procedure Test_CORS_Preflight_Allows;
var
  Cfg: TServerConfig;
  Server: TBadger;
  R: TTestHttpResponse;
begin
  Cfg := MakeConfig;
  Cfg.EnableCors := True;
  Cfg.CorsAllowCreds := False;
  Cfg.CorsOrigins := 'https://ok.test';
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddGet('/ping', TTestRoutes.Ping);
    StartAndWait(Server);
    R := HttpOptions(Cfg.Port, '/ping', 'https://ok.test', 'GET', 'Content-Type');
    AssertTrue((R.StatusCode = 200) or (R.StatusCode = 204),
      'Preflight must be 200 or 204 (got ' + IntToStr(R.StatusCode) + ')');
    AssertContains('GET', HeaderValue(R, 'Access-Control-Allow-Methods'),
      'Preflight must list GET');
  finally
    StopAndFree(Server);
  end;
end;

{ ------------------------ Basic Auth ------------------------ }

function Base64BasicCreds(const AUser, APass: string): string;
begin
  Result := 'Basic ' + CustomEncodeBase64(AUser + ':' + APass, False);
end;

procedure Test_BasicAuth_NoHeader_401;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Auth: TBasicAuth;
  R: TTestHttpResponse;
begin
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  Auth := TBasicAuth.Create('user', 'pass');
  try
    Server.RouteManager.AddGet('/private', TTestRoutes.Private1);
    Auth.RegisterProtectedRoutes(Server, ['/private']);
    StartAndWait(Server);
    R := HttpGet(Cfg.Port, '/private');
    AssertEqualsInt(401, R.StatusCode, 'Missing auth must yield 401');
  finally
    StopAndFree(Server);
    Auth.Free;
  end;
end;

procedure Test_BasicAuth_ValidCreds_200;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Auth: TBasicAuth;
  R: TTestHttpResponse;
begin
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  Auth := TBasicAuth.Create('user', 'pass');
  try
    Server.RouteManager.AddGet('/private', TTestRoutes.Private1);
    Auth.RegisterProtectedRoutes(Server, ['/private']);
    StartAndWait(Server);
    R := HttpGetWithHeaders(Cfg.Port, '/private',
      'Authorization: ' + Base64BasicCreds('user', 'pass') + CRLF);
    AssertEqualsInt(200, R.StatusCode, 'Valid creds must return 200');
  finally
    StopAndFree(Server);
    Auth.Free;
  end;
end;

procedure Test_BasicAuth_SubpathProtected;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Auth: TBasicAuth;
  R1, R2, R3: TTestHttpResponse;
begin
  // §4.3: /private protects /private/x but NOT /privateX
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  Auth := TBasicAuth.Create('user', 'pass');
  try
    Server.RouteManager.AddGet('/private', TTestRoutes.Private1);
    Server.RouteManager.AddGet('/private/sub', TTestRoutes.Private1);
    Server.RouteManager.AddGet('/privateX', TTestRoutes.Ping);
    Auth.RegisterProtectedRoutes(Server, ['/private']);
    StartAndWait(Server);
    R1 := HttpGet(Cfg.Port, '/private');
    AssertEqualsInt(401, R1.StatusCode, '/private without creds = 401');
    R2 := HttpGet(Cfg.Port, '/private/sub');
    AssertEqualsInt(401, R2.StatusCode, '/private/sub without creds = 401 (sub-path coverage)');
    R3 := HttpGet(Cfg.Port, '/privateX');
    AssertEqualsInt(200, R3.StatusCode, '/privateX must NOT match /private prefix');
  finally
    StopAndFree(Server);
    Auth.Free;
  end;
end;

procedure Test_BasicAuth_XForwardedAuthorization_NotHonored;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Auth: TBasicAuth;
  R: TTestHttpResponse;
begin
  // §4.2: only the exact "Authorization" header counts; X-Forwarded-Authorization
  // must NOT be parsed as auth.
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  Auth := TBasicAuth.Create('user', 'pass');
  try
    Server.RouteManager.AddGet('/private', TTestRoutes.Private1);
    Auth.RegisterProtectedRoutes(Server, ['/private']);
    StartAndWait(Server);
    R := HttpGetWithHeaders(Cfg.Port, '/private',
      'X-Forwarded-Authorization: ' + Base64BasicCreds('user', 'pass') + CRLF);
    AssertEqualsInt(401, R.StatusCode,
      'X-Forwarded-Authorization must not authenticate');
  finally
    StopAndFree(Server);
    Auth.Free;
  end;
end;

procedure Test_BasicAuth_VarRequest_PropagatesUserID;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Auth: TBasicAuth;
  R: TTestHttpResponse;
begin
  // §2.1: middleware sets Request.UserID; route must observe it.
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  Auth := TBasicAuth.Create('alice', 'pw');
  try
    Server.RouteManager.AddGet('/whoami', TTestRoutes.WhoAmI);
    Auth.RegisterProtectedRoutes(Server, ['/whoami']);
    StartAndWait(Server);
    R := HttpGetWithHeaders(Cfg.Port, '/whoami',
      'Authorization: ' + Base64BasicCreds('alice', 'pw') + CRLF);
    AssertEqualsInt(200, R.StatusCode, 'Auth status');
    AssertContains('UserID=alice', R.Body,
      'UserID set by middleware MUST reach the route handler');
  finally
    StopAndFree(Server);
    Auth.Free;
  end;
end;

{ ------------------------ JWT ------------------------ }

function JsonExtract(const AJSON, AKey: string): string;
var
  Needle: string;
  P, Q: Integer;
begin
  Result := '';
  Needle := '"' + AKey + '":"';
  P := Pos(Needle, AJSON);
  if P <= 0 then
    Exit;
  Inc(P, Length(Needle));
  Q := P;
  while (Q <= Length(AJSON)) and (AJSON[Q] <> '"') do
    Inc(Q);
  Result := Copy(AJSON, P, Q - P);
end;

procedure Test_JWT_NoToken_401;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Auth: TBadgerJWTAuth;
  R: TTestHttpResponse;
begin
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  Auth := TBadgerJWTAuth.Create('test-secret', '');
  try
    Server.RouteManager.AddGet('/api', TTestRoutes.Private1);
    Auth.RegisterProtectedRoutes(Server, ['/api']);
    StartAndWait(Server);
    R := HttpGet(Cfg.Port, '/api');
    AssertEqualsInt(401, R.StatusCode, 'Missing JWT must yield 401');
  finally
    StopAndFree(Server);
    Auth.Free;
  end;
end;

procedure Test_JWT_ValidToken_200;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Auth: TBadgerJWTAuth;
  R: TTestHttpResponse;
  Token, Json: string;
begin
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  Auth := TBadgerJWTAuth.Create('test-secret', '');
  try
    Server.RouteManager.AddGet('/api', TTestRoutes.Private1);
    Auth.RegisterProtectedRoutes(Server, ['/api']);
    StartAndWait(Server);
    Json := Auth.GenerateToken('bob', 'user', 1);
    Token := JsonExtract(Json, 'access_token');
    AssertTrue(Token <> '', 'Could not extract access_token from GenerateToken JSON');
    R := HttpGetWithHeaders(Cfg.Port, '/api',
      'Authorization: Bearer ' + Token + CRLF);
    AssertEqualsInt(200, R.StatusCode, 'Valid JWT must yield 200');
  finally
    StopAndFree(Server);
    Auth.Free;
  end;
end;

procedure Test_JWT_TamperedSignature_401;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Auth: TBadgerJWTAuth;
  R: TTestHttpResponse;
  Token, Json: string;
  LastDot: Integer;
begin
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  Auth := TBadgerJWTAuth.Create('test-secret', '');
  try
    Server.RouteManager.AddGet('/api', TTestRoutes.Private1);
    Auth.RegisterProtectedRoutes(Server, ['/api']);
    StartAndWait(Server);
    Json := Auth.GenerateToken('bob', 'user', 1);
    Token := JsonExtract(Json, 'access_token');
    // Flip one byte of the signature segment.
    LastDot := Length(Token);
    while (LastDot > 0) and (Token[LastDot] <> '.') do
      Dec(LastDot);
    if (LastDot > 0) and (LastDot < Length(Token)) then
    begin
      if Token[LastDot + 1] = 'A' then
        Token[LastDot + 1] := 'B'
      else
        Token[LastDot + 1] := 'A';
    end;
    R := HttpGetWithHeaders(Cfg.Port, '/api',
      'Authorization: Bearer ' + Token + CRLF);
    AssertEqualsInt(401, R.StatusCode, 'Tampered JWT signature must yield 401');
  finally
    StopAndFree(Server);
    Auth.Free;
  end;
end;

procedure Test_JWT_AlgNone_Rejected;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Auth: TBadgerJWTAuth;
  R: TTestHttpResponse;
  Header, Payload, ForgedToken: string;
begin
  // §4.4: alg must be HS256; alg=none must be rejected even with a valid-looking payload.
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  Auth := TBadgerJWTAuth.Create('test-secret', '');
  try
    Server.RouteManager.AddGet('/api', TTestRoutes.Private1);
    Auth.RegisterProtectedRoutes(Server, ['/api']);
    StartAndWait(Server);
    Header := CustomEncodeBase64('{"alg":"none","typ":"JWT"}', True);
    Payload := CustomEncodeBase64('{"user_id":"bob","role":"user","iss":1,"exp":9999999999}', True);
    ForgedToken := Header + '.' + Payload + '.';
    R := HttpGetWithHeaders(Cfg.Port, '/api',
      'Authorization: Bearer ' + ForgedToken + CRLF);
    AssertEqualsInt(401, R.StatusCode, 'alg=none must be rejected');
  finally
    StopAndFree(Server);
    Auth.Free;
  end;
end;

{ ------------------------ Multipart filename sanitization ------------------------ }

procedure Test_Sanitize_Traversal_StrippedToBaseName;
var
  Cfg: TServerConfig;
  Server: TBadger;
  R1, R2, R3: TTestHttpResponse;
begin
  // §3.1: SanitizeUploadFileName strips traversal / absolute paths.
  // Rather than constructing a real multipart body, drive the function through
  // a route that calls the sanitizer directly. Equivalent observable behavior.
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  try
    Server.RouteManager.AddPost('/sanitize', TTestRoutes.SanitizeName);
    StartAndWait(Server);
    R1 := HttpPost(Cfg.Port, '/sanitize', 'text/plain',
      '../../Windows/System32/drivers/etc/hosts');
    AssertEqualsStr('hosts', R1.Body, 'Traversal must be stripped to basename');

    R2 := HttpPost(Cfg.Port, '/sanitize', 'text/plain', '/etc/passwd');
    AssertEqualsStr('passwd', R2.Body, 'Absolute Unix path must be stripped');

    R3 := HttpPost(Cfg.Port, '/sanitize', 'text/plain', 'normal.png');
    AssertEqualsStr('normal.png', R3.Body, 'Normal filename must pass through');
  finally
    StopAndFree(Server);
  end;
end;

{ ------------------------ Base64 / Auth header edge cases ------------------------ }

procedure Test_Base64_EmptyBasicAuth_NoCrash;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Auth: TBasicAuth;
  R: TTestHttpResponse;
begin
  // §3.5 / §4.8: empty Basic auth payload must not crash the handler.
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  Auth := TBasicAuth.Create('user', 'pass');
  try
    Server.RouteManager.AddGet('/private', TTestRoutes.Private1);
    Auth.RegisterProtectedRoutes(Server, ['/private']);
    StartAndWait(Server);
    R := HttpGetWithHeaders(Cfg.Port, '/private',
      'Authorization: Basic ' + CRLF);
    AssertTrue((R.StatusCode = 401) or (R.StatusCode = 400),
      'Empty Basic auth must return 401/400, not crash. Got=' + IntToStr(R.StatusCode));
  finally
    StopAndFree(Server);
    Auth.Free;
  end;
end;

procedure Test_Base64_OneCharBasicAuth_NoCrash;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Auth: TBasicAuth;
  R: TTestHttpResponse;
begin
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  Auth := TBasicAuth.Create('user', 'pass');
  try
    Server.RouteManager.AddGet('/private', TTestRoutes.Private1);
    Auth.RegisterProtectedRoutes(Server, ['/private']);
    StartAndWait(Server);
    R := HttpGetWithHeaders(Cfg.Port, '/private',
      'Authorization: Basic A' + CRLF);
    AssertTrue((R.StatusCode = 401) or (R.StatusCode = 400),
      'One-char Basic auth must return 401/400, not crash. Got=' + IntToStr(R.StatusCode));
  finally
    StopAndFree(Server);
    Auth.Free;
  end;
end;

procedure Test_Base64_InvalidChars_NoCrash;
var
  Cfg: TServerConfig;
  Server: TBadger;
  Auth: TBasicAuth;
  R: TTestHttpResponse;
begin
  // Invalid Base64 alphabet bytes — must not propagate bogus credentials.
  Cfg := MakeConfig;
  Server := MakeServer(Cfg);
  Auth := TBasicAuth.Create('user', 'pass');
  try
    Server.RouteManager.AddGet('/private', TTestRoutes.Private1);
    Auth.RegisterProtectedRoutes(Server, ['/private']);
    StartAndWait(Server);
    R := HttpGetWithHeaders(Cfg.Port, '/private',
      'Authorization: Basic !!!@@@###' + CRLF);
    AssertTrue((R.StatusCode = 401) or (R.StatusCode = 400) or (R.StatusCode = 500),
      'Malformed Base64 must not authenticate. Got=' + IntToStr(R.StatusCode));
    AssertNotContains('private-ok', R.Body, 'Must not reach private route body');
  finally
    StopAndFree(Server);
    Auth.Free;
  end;
end;

{ ------------------------ Registration ------------------------ }

procedure RegisterAllTests;
begin
  RegisterTest('Sanity',     'Ping_returns_200_and_Pong',                        Test_Sanity_Ping_200_Pong);
  RegisterTest('Sanity',     'Unknown_route_returns_404',                        Test_Sanity_Unknown_404);
  RegisterTest('Sanity',     'Route_param_extracted_into_RouteParams',           Test_Sanity_RouteParam_Extracted);

  RegisterTest('Headers',    'CRLF_in_custom_value_is_stripped',                 Test_Headers_CRLFInjection_StrippedFromValue);
  RegisterTest('Headers',    'Oversize_header_line_does_not_yield_2xx',          Test_Headers_OversizeLine_Rejected);

  RegisterTest('KeepAlive',  'Two_sequential_requests_both_succeed',             Test_KeepAlive_TwoRequests_BothSucceed);
  RegisterTest('KeepAlive',  'Custom_response_header_does_not_leak_to_next_req', Test_KeepAlive_NoCustomHeaderLeak);
  RegisterTest('KeepAlive',  'QueryParams_do_not_leak_to_next_request',          Test_KeepAlive_NoQueryParamLeak);

  RegisterTest('Body',       'Negative_Content_Length_returns_400_keeps_sync',   Test_Body_NegativeContentLength_400_AndConnSync);
  RegisterTest('Body',       'Valid_POST_body_is_echoed',                        Test_Body_ValidPost_Echoed);

  RegisterTest('Chunked',    'Transfer_Encoding_chunked_round_trip_keepalive',   Test_Chunked_RoundTrip_KeepAlive);

  RegisterTest('CORS',       'Disabled_means_no_CORS_headers',                   Test_CORS_Disabled_NoHeaders);
  RegisterTest('CORS',       'Credentials_plus_wildcard_does_NOT_reflect_Origin',Test_CORS_Credentials_RejectsWildcardReflection);
  RegisterTest('CORS',       'Credentials_plus_explicit_origin_is_returned',     Test_CORS_Credentials_AllowsExplicitOrigin);
  RegisterTest('CORS',       'Preflight_OPTIONS_returns_method_list',            Test_CORS_Preflight_Allows);

  RegisterTest('BasicAuth',  'Missing_Authorization_header_yields_401',          Test_BasicAuth_NoHeader_401);
  RegisterTest('BasicAuth',  'Valid_credentials_yield_200',                      Test_BasicAuth_ValidCreds_200);
  RegisterTest('BasicAuth',  'Sub_paths_are_protected_but_prefix_collisions_are_not', Test_BasicAuth_SubpathProtected);
  RegisterTest('BasicAuth',  'X_Forwarded_Authorization_does_NOT_authenticate',  Test_BasicAuth_XForwardedAuthorization_NotHonored);
  RegisterTest('BasicAuth',  'UserID_set_by_middleware_reaches_route_handler',   Test_BasicAuth_VarRequest_PropagatesUserID);

  RegisterTest('JWT',        'Missing_token_yields_401',                         Test_JWT_NoToken_401);
  RegisterTest('JWT',        'Valid_token_yields_200',                           Test_JWT_ValidToken_200);
  RegisterTest('JWT',        'Tampered_signature_yields_401',                    Test_JWT_TamperedSignature_401);
  RegisterTest('JWT',        'alg_none_header_is_rejected',                      Test_JWT_AlgNone_Rejected);

  RegisterTest('Sanitize',   'Multipart_traversal_is_stripped_to_basename',      Test_Sanitize_Traversal_StrippedToBaseName);

  RegisterTest('Base64Edge', 'Empty_Basic_auth_payload_does_not_crash',          Test_Base64_EmptyBasicAuth_NoCrash);
  RegisterTest('Base64Edge', 'One_char_Basic_auth_payload_does_not_crash',       Test_Base64_OneCharBasicAuth_NoCrash);
  RegisterTest('Base64Edge', 'Invalid_Base64_alphabet_does_not_authenticate',    Test_Base64_InvalidChars_NoCrash);
end;

end.
