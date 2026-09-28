unit BadgerDBPool;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

{ Generic connection pool for Badger.
  Pass any TComponent-based connector from a DataModule (Zeos, FireDAC,
  UniDAC, TSQLConnection, ...) and the desired pool size. Cloning and
  reconnect are handled internally — no per-library helper required.

  APoolN is a hard cap on live connections (idle + borrowed). Acquire raises
  when the pool is exhausted. Release is idempotent for unknown handles
  (double-release is a no-op). Destroy closes both idle and borrowed. }

interface

uses
  Classes, SysUtils, SyncObjs, TypInfo, BadgerTypes, BadgerLogger;

type
  TBadgerDBPool = class
  private
    FTemplate: TComponent;
    FLock: TCriticalSection;
    FIdle: TList;
    FBorrowed: TList;
    FPoolN: Integer;
    function CloneTemplate: TComponent;
    procedure EnsureConnected(AConn: TObject);
    function CreateConnection: TObject;
    procedure Log(const AMsg: string);
    procedure FreeListObjects(AList: TList);
  public
    { ATemplate: connector on a DataModule (not pooled itself).
      APoolN: hard maximum of concurrent connections. }
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

procedure TBadgerDBPool.FreeListObjects(AList: TList);
var
  I: Integer;
begin
  if not Assigned(AList) then
    Exit;
  for I := 0 to AList.Count - 1 do
    TObject(AList[I]).Free;
  AList.Clear;
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
  FLock := TCriticalSection.Create;
  FIdle := TList.Create;
  FBorrowed := TList.Create;

  for I := 1 to FPoolN do
    FIdle.Add(CreateConnection);

  Log(Format('pool started with %d connection(s) of %s (hard cap)',
    [FPoolN, FTemplate.ClassName]));
end;

destructor TBadgerDBPool.Destroy;
var
  Waited: Integer;
  Pending: Integer;
begin
  { Antes as conexoes emprestadas eram liberadas de imediato: um request ainda
    dentro da rota passava a usar objeto morto. Agora espera o retorno, com teto. }
  { Construtor que levanta (template nil, CloneTemplate sem banco) chega aqui com
    FLock/FBorrowed nil: sem a guarda, um AV mascarava o erro real. }
  Waited := 0;
  Pending := 0;
  if Assigned(FLock) and Assigned(FBorrowed) then
    repeat
      FLock.Acquire;
      try
        Pending := FBorrowed.Count;
      finally
        FLock.Release;
      end;
      if Pending = 0 then
        Break;
      Sleep(50);
      Inc(Waited, 50);
    until Waited >= 5000;
  if Pending > 0 then
    Log(Format('Destroy: %d conexao(oes) ainda emprestada(s) apos 5s; liberando assim mesmo',
      [Pending]));

  if Assigned(FLock) then
  begin
    FLock.Acquire;
    try
      FreeListObjects(FBorrowed);
      FreeListObjects(FIdle);
    finally
      FLock.Release;
    end;
  end
  else
  begin
    FreeListObjects(FBorrowed);
    FreeListObjects(FIdle);
  end;

  FreeAndNil(FBorrowed);
  FreeAndNil(FIdle);
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TBadgerDBPool.CreateConnection: TObject;
begin
  Result := CloneTemplate;
end;

function TBadgerDBPool.Acquire: TObject;
var
  Broken: TObject;
  ReplaceMsg: string;
begin
  Result := nil;
  ReplaceMsg := '';

  FLock.Acquire;
  try
    if FIdle.Count = 0 then
      raise Exception.CreateFmt(
        'TBadgerDBPool: pool exhausted (%d connection(s) in use)', [FPoolN]);

    Result := TObject(FIdle[FIdle.Count - 1]);
    FIdle.Delete(FIdle.Count - 1);
    FBorrowed.Add(Result);
  finally
    FLock.Release;
  end;

  try
    EnsureConnected(Result);
  except
    on E: Exception do
    begin
      Broken := Result;
      Result := nil;
      ReplaceMsg := E.Message;

      FLock.Acquire;
      try
        FBorrowed.Remove(Broken);
      finally
        FLock.Release;
      end;
      Broken.Free;

      try
        Result := CreateConnection;
      except
        on E2: Exception do
          raise Exception.CreateFmt(
            'TBadgerDBPool: reconnect failed after idle open error (%s): %s',
            [ReplaceMsg, E2.Message]);
      end;

      FLock.Acquire;
      try
        FBorrowed.Add(Result);
      finally
        FLock.Release;
      end;
      Log('idle connection reopen failed; replaced: ' + ReplaceMsg);
    end;
  end;
end;

procedure TBadgerDBPool.Release(AConn: TObject);
var
  Idx: Integer;
begin
  if not Assigned(AConn) then
    Exit;

  FLock.Acquire;
  try
    Idx := FBorrowed.IndexOf(AConn);
    if Idx < 0 then
    begin
      { Unknown or already released — ignore (idempotent / double-release safe). }
      Exit;
    end;

    FBorrowed.Delete(Idx);

    if FIdle.IndexOf(AConn) >= 0 then
    begin
      { Should be unreachable if Release is only used for borrowed handles. }
      Log('Release: connection already idle; ignored duplicate');
      Exit;
    end;

    FIdle.Add(AConn);
  finally
    FLock.Release;
  end;
end;

end.
