unit ConnPoolWorkers;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

{ Spawns N threads that Acquire/Release the Badger DB pool concurrently. }

interface

uses
  Classes, SysUtils, SyncObjs, BadgerDBPool;

type
  TConnPoolStress = class
  private
    FPool: TBadgerDBPool;
    FThreads: Integer;
    FLoops: Integer;
    FHoldMs: Integer;
    FLock: TCriticalSection;
    FOk: Integer;
    FFail: Integer;
    FRunning: Integer;
    FMaxBackendSeen: Integer;
    FLog: TStringList;
  public
    constructor Create(APool: TBadgerDBPool; AThreads, ALoops, AHoldMs: Integer);
    destructor Destroy; override;
    procedure Start;
    procedure WaitDone(ATimeoutMs: Integer = 120000);
    property Ok: Integer read FOk;
    property Fail: Integer read FFail;
    property StillRunning: Integer read FRunning;
    property Log: TStringList read FLog;
  end;

implementation

uses
  ZConnection, ZDataset;

type
  TStressThread = class(TThread)
  private
    FOwner: TConnPoolStress;
    FIndex: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TConnPoolStress; AIndex: Integer);
  end;

constructor TStressThread.Create(AOwner: TConnPoolStress; AIndex: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FOwner := AOwner;
  FIndex := AIndex;
end;

procedure TStressThread.Execute;
var
  I, Pid: Integer;
  Conn: TZConnection;
  Q: TZQuery;
begin
  InterlockedIncrement(FOwner.FRunning);
  try
    for I := 1 to FOwner.FLoops do
    begin
      if Terminated then Break;
      Conn := nil;
      try
        Conn := TZConnection(FOwner.FPool.Acquire);
        Q := TZQuery.Create(nil);
        try
          Q.Connection := Conn;
          Q.SQL.Text := 'SELECT pg_backend_pid()';
          Q.Open;
          Pid := Q.Fields[0].AsInteger;
          Q.Close;
          if FOwner.FHoldMs > 0 then
          begin
            Q.SQL.Text := Format('SELECT pg_sleep(%s)',
              [StringReplace(FormatFloat('0.###', FOwner.FHoldMs / 1000.0), ',', '.', [rfReplaceAll])]);
            Q.Open;
            Q.Close;
          end;
          Q.SQL.Text :=
            'INSERT INTO job_hits(job_id, worker_name, backend_pid) ' +
            'SELECT id, :wn, :pid FROM jobs ORDER BY id LIMIT 1';
          Q.ParamByName('wn').AsString := Format('thr-%d', [FIndex]);
          Q.ParamByName('pid').AsInteger := Pid;
          Q.ExecSQL;
        finally
          Q.Free;
        end;
        FOwner.FLock.Acquire;
        try
          InterlockedIncrement(FOwner.FOk);
          if Pid > FOwner.FMaxBackendSeen then
            FOwner.FMaxBackendSeen := Pid;
        finally
          FOwner.FLock.Release;
        end;
      except
        on E: Exception do
        begin
          InterlockedIncrement(FOwner.FFail);
          FOwner.FLock.Acquire;
          try
            FOwner.FLog.Add(Format('[thr %d] %s', [FIndex, E.Message]));
          finally
            FOwner.FLock.Release;
          end;
        end;
      end;
      if Assigned(Conn) then
        FOwner.FPool.Release(Conn);
    end;
  finally
    InterlockedDecrement(FOwner.FRunning);
  end;
end;

{ TConnPoolStress }

constructor TConnPoolStress.Create(APool: TBadgerDBPool; AThreads, ALoops, AHoldMs: Integer);
begin
  inherited Create;
  FPool := APool;
  FThreads := AThreads;
  FLoops := ALoops;
  FHoldMs := AHoldMs;
  FLock := TCriticalSection.Create;
  FLog := TStringList.Create;
  FOk := 0;
  FFail := 0;
  FRunning := 0;
end;

destructor TConnPoolStress.Destroy;
begin
  WaitDone(5000);
  FreeAndNil(FLog);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TConnPoolStress.Start;
var
  I: Integer;
  T: TStressThread;
begin
  for I := 1 to FThreads do
  begin
    T := TStressThread.Create(Self, I);
    T.Start;
  end;
end;

procedure TConnPoolStress.WaitDone(ATimeoutMs: Integer);
var
  Elapsed: Integer;
begin
  Elapsed := 0;
  while (FRunning > 0) and (Elapsed < ATimeoutMs) do
  begin
    Sleep(50);
    Inc(Elapsed, 50);
  end;
end;

end.
