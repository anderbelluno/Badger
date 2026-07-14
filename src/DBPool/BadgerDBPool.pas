unit BadgerDBPool;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

{ Generic connection pool for Badger.
  Pass any TComponent-based connector from a DataModule (Zeos, FireDAC,
  UniDAC, TSQLConnection, ...) and the desired pool size. Cloning and
  reconnect are handled internally — no per-library helper required. }

interface

uses
  Classes, SysUtils, SyncObjs, TypInfo, BadgerTypes, BadgerLogger;

type
  TBadgerDBPool = class
  private
    FTemplate: TComponent;
    FIdle: TThreadList;
    FPoolN: Integer;
    function CloneTemplate: TComponent;
    procedure EnsureConnected(AConn: TObject);
    function CreateConnection: TObject;
    procedure Log(const AMsg: string);
  public
    { ATemplate: connector on a DataModule (not pooled itself).
      APoolN: number of clones kept idle. }
    constructor Create(ATemplate: TComponent; APoolN: Integer);
    destructor Destroy; override;

    function Acquire: TObject;
    procedure Release(AConn: TObject);

    property PoolN: Integer read FPoolN;
    property Template: TComponent read FTemplate;
  end;

  { Request helpers — do NOT Free the connection; Release returns it to the pool. }
  function AcquireConn(var Request: THTTPRequest): TObject;
  procedure ReleaseConn(var Request: THTTPRequest);

implementation

function AcquireConn(var Request: THTTPRequest): TObject;
begin
  if not Assigned(Request.DbPool) then
    raise Exception.Create('DbPool not configured on request. Register TBadgerDBBridge.');
  if Assigned(Request.DbConn) then
    raise Exception.Create('A DB connection is already acquired for this request. ReleaseConn first.');

  Result := TBadgerDBPool(Request.DbPool).Acquire;
  Request.DbConn := Result;
end;

procedure ReleaseConn(var Request: THTTPRequest);
begin
  if not Assigned(Request.DbPool) or not Assigned(Request.DbConn) then
    Exit;

  TBadgerDBPool(Request.DbPool).Release(Request.DbConn);
  Request.DbConn := nil;
end;

{ TBadgerDBPool }

procedure TBadgerDBPool.Log(const AMsg: string);
begin
  Logger.Info('[BadgerDBPool] ' + AMsg);
end;

procedure TBadgerDBPool.EnsureConnected(AConn: TObject);
var
  PropInfo: PPropInfo;
begin
  if not Assigned(AConn) then
    Exit;

  PropInfo := GetPropInfo(AConn.ClassInfo, 'Connected');
  if not Assigned(PropInfo) then
    Exit;

  try
    if GetOrdProp(AConn, PropInfo) = 0 then
      SetOrdProp(AConn, PropInfo, 1);
  except
    on E: Exception do
      raise Exception.CreateFmt('TBadgerDBPool: failed to open %s: %s',
        [AConn.ClassName, E.Message]);
  end;
end;

function TBadgerDBPool.CloneTemplate: TComponent;
var
  Stream: TMemoryStream;
  CompClass: TComponentClass;
  PropInfo: PPropInfo;
begin
  CompClass := TComponentClass(FTemplate.ClassType);
  Result := CompClass.Create(nil);
  try
    { Copy published config (Host, Database, User, Password, Params, ...)
      without tying Badger to Zeos/FireDAC/UniDAC units. }
    Stream := TMemoryStream.Create;
    try
      Stream.WriteComponent(FTemplate);
      Stream.Position := 0;
      Stream.ReadComponent(Result);
    finally
      Stream.Free;
    end;

    Result.Name := '';

    { Avoid interactive login dialogs on pooled clones. }
    PropInfo := GetPropInfo(Result.ClassInfo, 'LoginPrompt');
    if Assigned(PropInfo) then
      SetOrdProp(Result, PropInfo, 0);

    EnsureConnected(Result);
  except
    Result.Free;
    raise;
  end;
end;

constructor TBadgerDBPool.Create(ATemplate: TComponent; APoolN: Integer);
var
  I: Integer;
begin
  inherited Create;
  if not Assigned(ATemplate) then
    raise Exception.Create('TBadgerDBPool: template connection is nil');
  if APoolN < 1 then
    raise Exception.Create('TBadgerDBPool: APoolN must be >= 1');

  FTemplate := ATemplate;
  FPoolN := APoolN;
  FIdle := TThreadList.Create;

  for I := 1 to FPoolN do
    FIdle.Add(CreateConnection);

  Log(Format('pool started with %d connection(s) of %s', [FPoolN, FTemplate.ClassName]));
end;

destructor TBadgerDBPool.Destroy;
var
  List: TList;
  I: Integer;
begin
  List := FIdle.LockList;
  try
    for I := 0 to List.Count - 1 do
      TObject(List[I]).Free;
    List.Clear;
  finally
    FIdle.UnlockList;
  end;
  FreeAndNil(FIdle);
  inherited Destroy;
end;

function TBadgerDBPool.CreateConnection: TObject;
begin
  Result := CloneTemplate;
end;

function TBadgerDBPool.Acquire: TObject;
var
  List: TList;
begin
  Result := nil;
  List := FIdle.LockList;
  try
    if List.Count > 0 then
    begin
      Result := TObject(List[List.Count - 1]);
      List.Delete(List.Count - 1);
    end;
  finally
    FIdle.UnlockList;
  end;

  if not Assigned(Result) then
  begin
    Result := CreateConnection;
    Log('pool exhausted; extra connection created');
  end
  else
  begin
    try
      EnsureConnected(Result);
    except
      on E: Exception do
      begin
        Result.Free;
        Result := CreateConnection;
        Log('idle connection reopen failed; replaced: ' + E.Message);
      end;
    end;
  end;
end;

procedure TBadgerDBPool.Release(AConn: TObject);
var
  List: TList;
  KeepIdle: Boolean;
begin
  if not Assigned(AConn) then
    Exit;

  KeepIdle := False;
  List := FIdle.LockList;
  try
    KeepIdle := List.Count < FPoolN;
    if KeepIdle then
      List.Add(AConn);
  finally
    FIdle.UnlockList;
  end;

  if not KeepIdle then
  begin
    AConn.Free;
    Log('extra connection freed');
  end;
end;

end.
