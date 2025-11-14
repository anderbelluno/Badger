unit BadgerTypes;

interface

uses
  Classes, SysUtils, Contnrs, blcksock;

type
  THTTPRequest = record
    Socket: TTCPBlockSocket;
    URI, Method, RequestLine: string;
    Headers: TStringList;
    Body: string;
    QueryParams: TStringList;
    BodyStream: TMemoryStream;
    FRemoteIP: String;
    UserID   : String;
    UserRole : String;
    RouteParams: TStringList;
  end;

  THTTPResponse = record
    StatusCode: Integer;
    Body: string;
    Stream: TStream;
    ContentType: string;
  end;

  TRequestInfo = record
    RemoteIP : string;
    Method: string;
    URI: string;
    RequestLine: string;
    Headers: TStringList;
    Body: string;
    QueryParams: TStringList;
    Timestamp: TDateTime;
  end;

  TResponseInfo = record
    StatusCode: Integer;
    StatusText: string;
    Body: string;
    ContentType: string;
    Headers: TStringList;
    Timestamp: TDateTime;
  end;

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
