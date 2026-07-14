unit BadgerTypes;

interface

uses
  Classes, SysUtils, Contnrs, blcksock, SyncObjs;

type
  TClientSocketInfo = class
    Socket: TTCPBlockSocket;
    InUse: Boolean;
    URI: string;
    IOLock: TCriticalSection;
    constructor Create;
    destructor Destroy; override;
  end;

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
    { Opaque DB pool hooks — typed as TObject so Badger core stays DB-agnostic.
      Filled by TBadgerDBBridge; use AcquireConn/ReleaseConn from BadgerDBPool. }
    DbPool: TObject;
    DbConn: TObject;
  end;

  THTTPResponse = record
    StatusCode: Integer;
    Body: string;
    Stream: TStream;
    ContentType: string;
    HeadersCustom: TStringList;
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

  TMiddlewareProc = function(var Request: THTTPRequest; var Response: THTTPResponse): Boolean of object;
  { Runs after the route (and after before-middlewares), even when a before-middleware
    short-circuited with Handled=True — and BEFORE the response is written to the socket,
    so mutations to Resp reach the client. Also used for cleanup (e.g. DB pool Release). }
  TAfterMiddlewareProc = procedure(var Request: THTTPRequest; var Response: THTTPResponse) of object;
  TRoutingCallback = procedure(Request: THTTPRequest; var Response: THTTPResponse) of object;

  TWebSocketMessageEvent = procedure(ClientInfo: TClientSocketInfo; const URI, AMessage: string) of object;

  TMiddlewareWrapper = class
  public
    Middleware: TMiddlewareProc;
    constructor Create(AMiddleware: TMiddlewareProc);
  end;

  TAfterMiddlewareWrapper = class
  public
    Middleware: TAfterMiddlewareProc;
    constructor Create(AMiddleware: TAfterMiddlewareProc);
  end;

implementation

{ TClientSocketInfo }

constructor TClientSocketInfo.Create;
begin
  inherited Create;
  IOLock := TCriticalSection.Create;
end;

destructor TClientSocketInfo.Destroy;
begin
  IOLock.Free;
  inherited Destroy;
end;

{ TMiddlewareWrapper }

constructor TMiddlewareWrapper.Create(AMiddleware: TMiddlewareProc);
begin
  inherited Create;
  Middleware := AMiddleware;
end;

{ TAfterMiddlewareWrapper }

constructor TAfterMiddlewareWrapper.Create(AMiddleware: TAfterMiddlewareProc);
begin
  inherited Create;
  Middleware := AMiddleware;
end;

end.
