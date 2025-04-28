unit SampleRouteManager;

interface

uses
  BadgerMethods,
  BadgerTypes,
  BadgerHttpStatus,

  Classes,
  SysUtils;

type
  TSampleRouteManager = class(TObject)
  private
  public
    class procedure upLoad(Request: THTTPRequest; out Response: THTTPResponse);
    class procedure downLoad(Request: THTTPRequest; out Response: THTTPResponse);
    class procedure rota1(Request: THTTPRequest; out Response: THTTPResponse);
    class procedure ping(Request: THTTPRequest; out Response: THTTPResponse);
    class procedure AtuImage(Request: THTTPRequest; out Response: THTTPResponse);
  end;

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

class procedure TSampleRouteManager.rota1(Request: THTTPRequest; out Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.Body := UTF8Encode('Rota1 executada');
end;

class procedure TSampleRouteManager.upLoad(Request: THTTPRequest; out Response: THTTPResponse);
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

class procedure TSampleRouteManager.ping(Request: THTTPRequest;  out Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.Body := UTF8Encode('Pong');
end;

class procedure TSampleRouteManager.AtuImage(Request: THTTPRequest; out Response: THTTPResponse);
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
