unit BadgerDBBridge;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

{ Bridges TBadgerDBPool into Badger HTTP requests.
  Before-middleware: injects the pool into THTTPRequest.DbPool.
  After-middleware: safety-net ReleaseConn if the route forgot to release.

  Usage:

    DbBridge := TBadgerDBBridge.Create(dm.ZConn, 15);
    DbBridge.Register(Server);

    // in a route:
    Conn := TZConnection(AcquireConn(Request));
    try
      ...
    finally
      ReleaseConn(Request);  // do NOT FreeAndNil(Conn)
    end;
}

interface

uses
  Classes, SysUtils, Badger, BadgerTypes, BadgerDBPool;

type
  TBadgerDBBridge = class
  private
    FPool: TBadgerDBPool;
    FOwnsPool: Boolean;
    function BeforeMiddleware(var Request: THTTPRequest; var Response: THTTPResponse): Boolean;
    procedure AfterMiddleware(var Request: THTTPRequest; var Response: THTTPResponse);
  public
    { ATemplate = connector no DataModule; APoolN = size of pool.
      Clones are created internally. }
    constructor Create(ATemplate: TComponent; APoolN: Integer); overload;
    constructor Create(APool: TBadgerDBPool; AOwnsPool: Boolean); overload;
    destructor Destroy; override;

    procedure Register(Badger: TBadger);

    property Pool: TBadgerDBPool read FPool;
  end;

implementation

{ TBadgerDBBridge }

constructor TBadgerDBBridge.Create(ATemplate: TComponent; APoolN: Integer);
begin
  inherited Create;
  FPool := TBadgerDBPool.Create(ATemplate, APoolN);
  FOwnsPool := True;
end;

constructor TBadgerDBBridge.Create(APool: TBadgerDBPool; AOwnsPool: Boolean);
begin
  inherited Create;
  if not Assigned(APool) then
    raise Exception.Create('TBadgerDBBridge: pool is nil');
  FPool := APool;
  FOwnsPool := AOwnsPool;
end;

destructor TBadgerDBBridge.Destroy;
begin
  if FOwnsPool then
    FreeAndNil(FPool)
  else
    FPool := nil;
  inherited Destroy;
end;

procedure TBadgerDBBridge.Register(Badger: TBadger);
begin
  if not Assigned(Badger) then
    raise Exception.Create('TBadgerDBBridge.Register: Badger is nil');
  Badger.AddMiddleware(BeforeMiddleware);
  Badger.AddAfterMiddleware(AfterMiddleware);
end;

function TBadgerDBBridge.BeforeMiddleware(var Request: THTTPRequest;
  var Response: THTTPResponse): Boolean;
begin
  Request.DbPool := FPool;
  Request.DbConn := nil;
  Result := False;
end;

procedure TBadgerDBBridge.AfterMiddleware(var Request: THTTPRequest;
  var Response: THTTPResponse);
begin
  ReleaseConn(Request);
end;

end.
