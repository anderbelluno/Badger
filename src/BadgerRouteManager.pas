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

    procedure upLoad(const URI, Method, RequestLine, Body: string; out Response: THTTPResponse);
    procedure downLoad(const URI, Method, RequestLine, Body: string; out Response: THTTPResponse);
    procedure rota1(const URI, Method, RequestLine, Body: string; out Response: THTTPResponse);
    procedure ping(const URI, Method, RequestLine, Body: string; out Response: THTTPResponse);
    procedure AtuImage(const URI, Method, RequestLine, Body: string; out Response: THTTPResponse);
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

procedure TRouteManager.downLoad(const URI, Method, RequestLine, Body: string; out Response: THTTPResponse);
var
  SynClasses: TBadgerMethods;
begin
  SynClasses := TBadgerMethods.Create;
  try
    Response.StatusCode := THTTPStatus.OK;
    Response.Stream := SynClasses.fDownloadStream('D:\GoogleDrive\Camera\0cc1b8a542673ba65bda4a151228e384.png', Response.ContentType);
  finally
    FreeAndNil(SynClasses);
  end;
end;

procedure TRouteManager.rota1(const URI, Method, RequestLine, Body: string; out Response: THTTPResponse);
begin
  Response.StatusCode := THTTPStatus.OK;
  Response.Body := UTF8Encode('Rota1 executada');
end;

procedure TRouteManager.upLoad(const URI, Method, RequestLine, Body: string; out Response: THTTPResponse);
var
  SynClasses: TBadgerMethods;
begin
  SynClasses := TBadgerMethods.Create;
  try
    Response.StatusCode := THTTPStatus.OK;
    Response.Body := SynClasses.fParserJsonStream(Body);
  finally
    FreeAndNil(SynClasses);
  end;
end;

procedure TRouteManager.ping(const URI, Method, RequestLine, Body: string; out Response: THTTPResponse);
begin
  Response.StatusCode := THTTPStatus.OK;
  Response.Body := UTF8Encode('Pong');
end;

procedure TRouteManager.AtuImage(const URI, Method, RequestLine, Body: string; out Response: THTTPResponse);
var
  SynClasses: TBadgerMethods;
begin
  SynClasses := TBadgerMethods.Create;
  try
    SynClasses.AtuImage(Body, Response.StatusCode, Response.Body);
  finally
    FreeAndNil(SynClasses);
  end;
end;

end.
