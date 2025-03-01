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
  end;
  THTTPResponse = record
    StatusCode: Integer;
    Body: string;
    Stream: TStream; // Adicionado para suportar streams
    ContentType: string; // Adicionado para definir o MIME type
  end;
  TMiddlewareProc = function(Request: THTTPRequest; out Response: THTTPResponse): Boolean of object;
  TRoutingCallback = procedure(const URI, Method, RequestLine, Body: string; out Response: THTTPResponse) of object;

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
