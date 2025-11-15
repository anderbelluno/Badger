unit SampleRouteManager;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  BadgerMethods,
  BadgerTypes,
  BadgerHttpStatus,
  BadgerUtils,
  BadgerAuthJWT,
  SysUtils,
  Classes,
  superobject;

type
  TSampleRouteManager = class(TObject)
  private

  public
    class procedure upLoad(Request: THTTPRequest; out Response: THTTPResponse);
    class procedure downLoad(Request: THTTPRequest; out Response: THTTPResponse);
    class procedure rota1(Request: THTTPRequest; out Response: THTTPResponse);
    class procedure ping(Request: THTTPRequest; out Response: THTTPResponse);
    class procedure AtuImage(Request: THTTPRequest; out Response: THTTPResponse);
    class procedure Login(Request: THTTPRequest; out Response: THTTPResponse);
    class procedure RefreshToken(Request: THTTPRequest; out Response: THTTPResponse);
    class procedure produtos(Request: THTTPRequest; out Response: THTTPResponse);
  end;

var
  FJWT: TBadgerJWTAuth;

implementation

{ TSampleRouteManager }

class procedure TSampleRouteManager.downLoad(Request: THTTPRequest; out Response: THTTPResponse);
var
  SynClasses: TBadgerMethods;
  FileName: string;
  I: Integer;
begin
  SynClasses := TBadgerMethods.Create;
  try
    FileName := '.\master.png';
    for I := 0 to Request.QueryParams.Count - 1 do
    begin
      if Pos('file=', Request.QueryParams[I]) = 1 then
      begin
        FileName := 'D:\GoogleDrive\Camera\' + Copy(Request.QueryParams[I], 6, Length(Request.QueryParams[I]));
        Break;
      end;
    end;

    Response.StatusCode := HTTP_OK;
    Response.Stream := SynClasses.fDownloadStream(FileName, Response.ContentType);
  finally
    FreeAndNil(SynClasses);
  end;
end;

class procedure TSampleRouteManager.Login(Request: THTTPRequest;
  out Response: THTTPResponse);
var
  LJSON, LResponseJSON: ISuperObject;
  LUser, LPass: string;
begin
  LJSON := SO(Request.Body);
  LUser := LJSON.S['username'];
  LPass := LJSON.S['password'];

  if (LUser = 'usuario') and (LPass = 'senha123') then
  begin

    LResponseJSON := SO(FJWT.GenerateToken(LUser, 'user_role', 24));

    Response.StatusCode := 200;
    Response.ContentType := APPLICATION_JSON;
    Response.Body := LResponseJSON.AsJSON;
  end
  else
  begin
    Response.StatusCode := 401;
    Response.ContentType := APPLICATION_JSON;
    Response.Body := '{"error":"Credenciais inválidas"}';
  end;
end;

class procedure TSampleRouteManager.rota1(Request: THTTPRequest;
  out Response: THTTPResponse);
begin
  Sleep(10000) ;
  Response.StatusCode := HTTP_OK;
  Response.Body := UTF8Encode('Rota1 executada');
end;

class procedure TSampleRouteManager.upLoad(Request: THTTPRequest;
  out Response: THTTPResponse);
var
  SynClasses: TBadgerMethods;
begin
  SynClasses := TBadgerMethods.Create;
  try
    Response.StatusCode := HTTP_OK;
    Response.ContentType := Request.Headers.Values['Content-Type'];
    Response.Body := (SynClasses.fParserJsonStream(Request, Response));
  finally
    FreeAndNil(SynClasses);
  end;
end;

class procedure TSampleRouteManager.ping(Request: THTTPRequest;
  out Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.Body := UTF8Encode('Pong');
  Response.HeadersCustom.Add('Access-Control-Allow-Origin: *');
  Response.ContentType := TEXT_PLAIN;
end;

class procedure TSampleRouteManager.produtos(Request: THTTPRequest;
  out Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.ContentType := TEXT_PLAIN;

  if Request.RouteParams.Count > 0 then
    Response.Body := Format('id: %s' + sLineBreak +  'código: %s', [Request.RouteParams.Values['id'], Request.RouteParams.Values['codigo'] ])
  else
    Response.Body := Format('id: %s' + sLineBreak +  'código: %s', [Request.QueryParams.Values['id'], Request.QueryParams.Values['codigo'] ]);
end;

class procedure TSampleRouteManager.RefreshToken(Request: THTTPRequest;
  out Response: THTTPResponse);
var
  LRefreshToken: string;
  I: Integer;
begin
  LRefreshToken := '';
  for I := 0 to Request.Headers.Count - 1 do
    if Pos('Authorization=', Request.Headers[I]) > 0 then
    begin
      LRefreshToken := Trim(Copy(Request.Headers[I], Pos('=', Request.Headers[I]) + 1, Length(Request.Headers[I])));
      if Pos('Bearer ', LRefreshToken) = 1 then
        LRefreshToken := Copy(LRefreshToken, 8, MaxInt);
      Break;
    end;

  if LRefreshToken = '' then
  begin
    Response.StatusCode := HTTP_UNAUTHORIZED;
    Response.Body := '{"error":"Refresh token não fornecido"}';
    Response.ContentType := APPLICATION_JSON;
    Exit;
  end;

  try
    Response.Body := FJWT.RefreshToken(LRefreshToken);
    Response.StatusCode := HTTP_OK;
    Response.ContentType := APPLICATION_JSON;
  except
    on E: Exception do
    begin
      Response.StatusCode := HTTP_UNAUTHORIZED;
      Response.Body := '{"error":"' + E.Message + '"}';
      Response.ContentType := APPLICATION_JSON;
    end;
  end;
end;

class procedure TSampleRouteManager.AtuImage(Request: THTTPRequest;
  out Response: THTTPResponse);
var
  SynClasses: TBadgerMethods;
begin
  SynClasses := TBadgerMethods.Create;
  try
    SynClasses.AtuImage(Request, Response);
  finally
    FreeAndNil(SynClasses);
  end;
end;

end.
