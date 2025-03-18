unit BadgerTypes;

interface

uses
  blcksock, Classes, SysUtils;

type
  TLastRequest  = procedure(Value: String) of object;
  TLastResponse = procedure(Value: String) of object;

  THTTPRequest = record
    Socket: TTCPBlockSocket;
    URI, Method, RequestLine: string;
    Headers: TStringList;
    Body: string;
    QueryParams: TStringList; // Já adicionado anteriormente
    BodyStream : TMemoryStream;
  end;
  THTTPResponse = record
    StatusCode: Integer;
    Body: string;
    Stream: TStream;
    ContentType: string;
  end;
  TMiddlewareProc = function(Request: THTTPRequest; out Response: THTTPResponse): Boolean of object;
  TRoutingCallback = procedure(Request: THTTPRequest; out Response: THTTPResponse) of object; // Ajustado

  TMiddlewareWrapper = class
  public
    Middleware: TMiddlewareProc;
    constructor Create(AMiddleware: TMiddlewareProc);
  end;

implementation

{ TMiddlewareWrapper }

constructor TMiddlewareWrapper.Create(AMiddleware: TMiddlewareProc);
begin
  inherited Create;
  Middleware := AMiddleware;
end;

end.
