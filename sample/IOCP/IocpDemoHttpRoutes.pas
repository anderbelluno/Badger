unit IocpDemoHttpRoutes;

{ Shared HTTP routes for IOCP and epoll samples. }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, Contnrs, BadgerTypes, BadgerHttpStatus, BadgerMethods,
  BadgerMultipartDataReader, BadgerHttpUtils, BadgerAuthJWT, superobject;

type
  TIocpDemoHttpRoutes = class
  public
    class procedure Ping(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure Echo(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure Produtos(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure Json(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure Download(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure Upload(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure Login(Request: THTTPRequest; var Response: THTTPResponse);
  end;

procedure IocpDemoSetAuthJWT(Auth: TBadgerJWTAuth);

implementation

var
  DemoAuthJWT: TBadgerJWTAuth;

procedure IocpDemoSetAuthJWT(Auth: TBadgerJWTAuth);
begin
  DemoAuthJWT := Auth;
end;

function EnsureDemoFile: string;
var
  SL: TStringList;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'iocp-demo.txt';
  if FileExists(Result) then
    Exit;
  SL := TStringList.Create;
  try
    SL.Text := 'Badger IOCP download demo';
    SL.SaveToFile(Result);
  finally
    SL.Free;
  end;
end;

class procedure TIocpDemoHttpRoutes.Ping(Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.Body := 'Pong';
  Response.ContentType := TEXT_PLAIN;
end;

class procedure TIocpDemoHttpRoutes.Echo(Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.ContentType := TEXT_PLAIN;
  if Request.Body <> '' then
    Response.Body := Request.Body
  else
    Response.Body := '(empty)';
end;

class procedure TIocpDemoHttpRoutes.Produtos(Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.ContentType := TEXT_PLAIN;
  if Request.RouteParams.Count > 0 then
    Response.Body := Format('id: %s%scodigo: %s',
      [Request.RouteParams.Values['id'], sLineBreak, Request.RouteParams.Values['codigo']])
  else
    Response.Body := Format('id: %s%scodigo: %s',
      [Request.QueryParams.Values['id'], sLineBreak, Request.QueryParams.Values['codigo']]);
end;

class procedure TIocpDemoHttpRoutes.Json(Request: THTTPRequest; var Response: THTTPResponse);
var
  Methods: TBadgerMethods;
begin
  Methods := TBadgerMethods.Create;
  try
    Response.StatusCode := HTTP_OK;
    Response.ContentType := APPLICATION_JSON;
    Response.Body := Methods.fParserJsonStream(Request, Response);
  finally
    Methods.Free;
  end;
end;

class procedure TIocpDemoHttpRoutes.Download(Request: THTTPRequest; var Response: THTTPResponse);
var
  Methods: TBadgerMethods;
begin
  Methods := TBadgerMethods.Create;
  try
    Response.StatusCode := HTTP_OK;
    Response.Stream := Methods.fDownloadStream(EnsureDemoFile, Response.ContentType);
    if Assigned(Response.HeadersCustom) then
      Response.HeadersCustom.Values['Content-Disposition'] :=
        'attachment; filename="iocp-demo.txt"';
  finally
    Methods.Free;
  end;
end;

class procedure TIocpDemoHttpRoutes.Upload(Request: THTTPRequest; var Response: THTTPResponse);
var
  Methods: TBadgerMethods;
  Reader: TFormDataReader;
  Files: TObjectList;
  I: Integer;
  FormFile: TFormDataFile;
  Parts: string;
begin
  Response.ContentType := APPLICATION_JSON;
  if (Request.BodyStream = nil) or (Request.BodyStream.Size = 0) then
  begin
    Response.StatusCode := HTTP_BAD_REQUEST;
    Response.Body := '{"error":"No image data provided"}';
    Exit;
  end;
  Methods := TBadgerMethods.Create;
  Reader := TFormDataReader.Create;
  Files := nil;
  try
    Request.BodyStream.Position := 0;
    Files := Reader.ProcessMultipartFormData(Request.BodyStream,
      Methods.ExtractBoundary(Request.Headers.Values['Content-Type']));
    Parts := '';

    for I := 0 to Files.Count - 1 do
    begin
      FormFile := TFormDataFile(Files[I]);

      if Parts <> '' then
        Parts := Parts + ',';

      Parts := Parts + '{"file":"' + JSONEscape(FormFile.FileName) +
        '","bytes":' + IntToStr(FormFile.Stream.Size) + '}';
    end;

    Response.StatusCode := HTTP_OK;
    Response.Body := '{"status":true,"files":[' + Parts + ']}';

    if Assigned(Response.HeadersCustom) then
      Response.HeadersCustom.Values['X-Upload-Count'] := IntToStr(Files.Count);
  finally
    Files.Free;
    Reader.Free;
    Methods.Free;
  end;
end;

class procedure TIocpDemoHttpRoutes.Login(Request: THTTPRequest; var Response: THTTPResponse);
var
  LJSON: ISuperObject;
  LUser, LPass: string;
begin
  Response.ContentType := APPLICATION_JSON;
  if not Assigned(DemoAuthJWT) then
  begin
    Response.StatusCode := HTTP_INTERNAL_SERVER_ERROR;
    Response.Body := '{"error":"JWT not initialized"}';
    Exit;
  end;
  LJSON := SO(Request.Body);
  if LJSON = nil then
  begin
    Response.StatusCode := HTTP_BAD_REQUEST;
    Response.Body := '{"error":"Invalid JSON"}';
    Exit;
  end;
  LUser := LJSON.S['username'];
  LPass := LJSON.S['password'];
  if (LUser = 'usuario') and (LPass = 'senha123') then
  begin
    Response.StatusCode := HTTP_OK;
    Response.Body := DemoAuthJWT.GenerateToken(LUser, 'user_role', 24);
  end
  else
  begin
    Response.StatusCode := HTTP_UNAUTHORIZED;
    Response.Body := '{"error":"Credenciais invalidas"}';
  end;
end;

initialization
  DemoAuthJWT := nil;

end.
