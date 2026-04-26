unit BadgerTestRoutes;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

{
  Route fixtures used by the TDD tests.

  Each handler is intentionally minimal — they exist to expose specific
  observable behavior to the harness (e.g. echo back UserID set by middleware,
  return the sanitized filename without ever touching disk, set a custom
  response header so the test can verify CRLF stripping at the wire).

  Do NOT use these handlers in production samples — they're test scaffolding.
}

interface

uses
  SysUtils, Classes, BadgerTypes, BadgerHttpStatus, BadgerUploadUtils;

type
  TTestRoutes = class
  public
    class procedure Ping(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure WhoAmI(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure SetTraceHeader(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure CheckTraceHeader(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure InjectHeader(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure EchoQuery(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure EchoBody(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure SanitizeName(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure RouteParam(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure Private1(Request: THTTPRequest; var Response: THTTPResponse);
  end;

implementation

class procedure TTestRoutes.Ping(Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.ContentType := TEXT_PLAIN;
  Response.Body := 'Pong';
end;

class procedure TTestRoutes.WhoAmI(Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.ContentType := TEXT_PLAIN;
  Response.Body := 'UserID=' + Request.UserID;
end;

class procedure TTestRoutes.SetTraceHeader(Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.ContentType := TEXT_PLAIN;
  Response.Body := 'set';
  if Assigned(Response.HeadersCustom) then
    Response.HeadersCustom.Values['X-Trace'] := 'leaked';
end;

class procedure TTestRoutes.CheckTraceHeader(Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.ContentType := TEXT_PLAIN;
  Response.Body := 'check';
  // Intentionally do NOT set X-Trace.
end;

class procedure TTestRoutes.InjectHeader(Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.ContentType := TEXT_PLAIN;
  Response.Body := 'ok';
  // Attempt CRLF injection via custom header value — sanitiser must strip it.
  if Assigned(Response.HeadersCustom) then
    Response.HeadersCustom.Values['X-Test'] := 'safe' + #13#10 + 'X-Injected: yes';
end;

class procedure TTestRoutes.EchoQuery(Request: THTTPRequest; var Response: THTTPResponse);
var
  Buf: string;
  I: Integer;
begin
  Response.StatusCode := HTTP_OK;
  Response.ContentType := TEXT_PLAIN;
  Buf := 'count=' + IntToStr(Request.QueryParams.Count);
  for I := 0 to Request.QueryParams.Count - 1 do
    Buf := Buf + '|' + Request.QueryParams[I];
  Response.Body := Buf;
end;

class procedure TTestRoutes.EchoBody(Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.ContentType := TEXT_PLAIN;
  Response.Body := Request.Body;
end;

class procedure TTestRoutes.SanitizeName(Request: THTTPRequest; var Response: THTTPResponse);
var
  Raw, Clean: string;
begin
  // Read raw filename from query "?name=...". Caller URL-encodes traversal.
  Raw := Request.QueryParams.Values['name'];
  if Raw = '' then
    Raw := Request.Body;
  Clean := SanitizeUploadFileName(Raw);
  Response.StatusCode := HTTP_OK;
  Response.ContentType := TEXT_PLAIN;
  Response.Body := Clean;
end;

class procedure TTestRoutes.RouteParam(Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.ContentType := TEXT_PLAIN;
  Response.Body := 'id=' + Request.RouteParams.Values['id'];
end;

class procedure TTestRoutes.Private1(Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.ContentType := TEXT_PLAIN;
  Response.Body := 'private-ok UserID=' + Request.UserID;
end;

end.
