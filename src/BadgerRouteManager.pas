unit BadgerRouteManager;

interface

uses
  Classes, SysUtils, BadgerMethods, BadgerTypes, BadgerHttpStatus;

type
  TRouteManager = class(TObject)
  private
    procedure RegisterRoute(const Route: string; Callback: TRoutingCallback);
  public
    FRoutes: TStringList;
    constructor Create;
    destructor Destroy; override;

    procedure upLoad(Request: THTTPRequest; out Response: THTTPResponse);
    procedure downLoad(Request: THTTPRequest; out Response: THTTPResponse);
    procedure rota1(Request: THTTPRequest; out Response: THTTPResponse);
    procedure ping(Request: THTTPRequest; out Response: THTTPResponse);
    procedure AtuImage(Request: THTTPRequest; out Response: THTTPResponse);
  end;

implementation

{ TRouteManager }

procedure TRouteManager.RegisterRoute(const Route: string; Callback: TRoutingCallback);
var
  Method: TMethod;
begin
  Method.Data := Self;
  Method.Code := @Callback;
  FRoutes.AddObject(Route, TObject(Method.Code));
end;

constructor TRouteManager.Create;
begin
  FRoutes := TStringList.Create;
  RegisterRoute('/upload', upLoad);
  RegisterRoute('/download', downLoad);
  RegisterRoute('/rota1', rota1);
  RegisterRoute('/ping', ping);
  RegisterRoute('/AtuImage', AtuImage);
end;

destructor TRouteManager.Destroy;
begin
  FRoutes.Free;
  inherited;
end;

procedure TRouteManager.downLoad(Request: THTTPRequest; out Response: THTTPResponse);
var
  SynClasses: TBadgerMethods;
  FileName: string;
  I: Integer;
begin
  SynClasses := TBadgerMethods.Create;
  try
    // Procurar o parâmetro 'file' na querystring
    FileName := '.\master.png'; // Padrão
    for I := 0 to Request.QueryParams.Count - 1 do
    begin
      if Pos('file=', Request.QueryParams[I]) = 1 then
      begin
        FileName := 'D:\GoogleDrive\Camera\' + Copy(Request.QueryParams[I], 6, Length(Request.QueryParams[I])); // Remove 'file='
        Break;
      end;
    end;

    Response.StatusCode := HTTP_OK;
    Response.Stream := SynClasses.fDownloadStream(FileName, Response.ContentType);
  finally
    FreeAndNil(SynClasses);
  end;
end;

procedure TRouteManager.rota1(Request: THTTPRequest; out Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.Body := UTF8Encode('Rota1 executada');
end;

procedure TRouteManager.upLoad(Request: THTTPRequest; out Response: THTTPResponse);
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

procedure TRouteManager.ping(Request: THTTPRequest; out Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.Body := UTF8Encode('Pong');
end;

procedure TRouteManager.AtuImage(Request: THTTPRequest; out Response: THTTPResponse);
var
  SynClasses: TBadgerMethods;
begin
  SynClasses := TBadgerMethods.Create;
  try
    SynClasses.AtuImage(Request.Body, Response.StatusCode, Response.Body);
  finally
    FreeAndNil(SynClasses);
  end;
end;

end.
