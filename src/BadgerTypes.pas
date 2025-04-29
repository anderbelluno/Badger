unit BadgerTypes;

interface

uses
  blcksock, Classes, SysUtils, Contnrs;

type
  THTTPRequest = record
    Socket: TTCPBlockSocket;
    URI, Method, RequestLine: string;
    Headers: TStringList;
    Body: string;
    QueryParams: TStringList;
    BodyStream: TMemoryStream;
    FRemoteIP: String;
  end;

  THTTPResponse = record
    StatusCode: Integer;
    Body: string;
    Stream: TStream;
    ContentType: string;
  end;

  // Record para armazenar detalhes da última requisição
  TRequestInfo = record
    RemoteIP : string;        //Ex.: Client Request IP : 192.168.0.1
    Method: string;           // Ex.: GET, POST
    URI: string;              // Ex.: /path
    RequestLine: string;      // Ex.: GET /path HTTP/1.1
    Headers: TStringList;     // Cabeçalhos da requisição
    Body: string;             // Corpo da requisição (se texto/JSON)
    QueryParams: TStringList; // Parâmetros da query string
    Timestamp: TDateTime;     // Hora da requisição
  end;

  // Record para armazenar detalhes da última resposta
  TResponseInfo = record
    StatusCode: Integer;      // Ex.: 200, 404
    StatusText: string;       // Ex.: OK, Not Found
    Body: string;             // Corpo da resposta
    ContentType: string;      // Ex.: text/plain, application/json
    Headers: TStringList;     // Cabeçalhos da resposta
    Timestamp: TDateTime;     // Hora da resposta
  end;

  // Eventos para requisição e resposta
  TOnRequest = procedure(const RequestInfo: TRequestInfo) of object;
  TOnResponse = procedure(const ResponseInfo: TResponseInfo) of object;

  TMiddlewareProc = function(Request: THTTPRequest; out Response: THTTPResponse): Boolean of object;
  TRoutingCallback = procedure(Request: THTTPRequest; out Response: THTTPResponse) of object;

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
