unit BadgerLogger;

{$I BadgerDefines.inc}

interface

uses
  {$IFDEF MSWINDOWS} Windows,{$ENDIF}SysUtils, SyncObjs, Classes
  ;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);

  TBadgerLogger = class
  private
    FCS: TCriticalSection;
    FisActive : Boolean;
    FLogFileName: string;
    FLogToFile: Boolean;
    FLogToConsole: Boolean;
    FLogLevel: TLogLevel;
    procedure WriteToFile(const Msg: string);
    procedure WriteToConsole(const Msg: string);
    {$IFDEF MSWINDOWS}
    procedure WriteToDebugger(const Msg: string);
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(Level: TLogLevel; const Msg: string);
    procedure Debug(const Msg: string);
    procedure Info(const Msg: string);
    procedure Warning(const Msg: string);
    procedure Error(const Msg: string);
    procedure Critical(const Msg: string);

    property isActive : Boolean read FisActive write FisActive;
    property LogToFile: Boolean read FLogToFile write FLogToFile;
    property LogToConsole: Boolean read FLogToConsole write FLogToConsole;
    property LogLevel: TLogLevel read FLogLevel write FLogLevel;
    property LogFileName: string read FLogFileName write FLogFileName;
  end;

var
  Logger: TBadgerLogger;

implementation

{ TBadgerLogger }

constructor TBadgerLogger.Create;
begin
  FCS := TCriticalSection.Create;
  FLogToConsole := True;
  FLogToFile := False;
  FLogLevel := llInfo;
  FLogFileName := 'badger_server.log';
  FisActive := False;
  {$IFDEF DEBUG}
  FLogLevel := llDebug;
  {$ENDIF}
end;

destructor TBadgerLogger.Destroy;
begin
  FCS.Free;
  inherited;
end;

procedure TBadgerLogger.WriteToFile(const Msg: string);
var
  LFile: TextFile;
begin
  if not FLogToFile then Exit;
  AssignFile(LFile, FLogFileName);
  try
    if FileExists(FLogFileName) then
      Append(LFile)
    else
      Rewrite(LFile);
    try
      WriteLn(LFile, Msg);
    finally
      CloseFile(LFile);
    end;
  except
  end;
end;

procedure TBadgerLogger.WriteToConsole(const Msg: string);
begin
  if FLogToConsole then
    WriteLn(Msg);
end;

{$IFDEF MSWINDOWS}
procedure TBadgerLogger.WriteToDebugger(const Msg: string);
begin
  OutputDebugString(PChar(Msg));
end;
{$ENDIF}

procedure TBadgerLogger.Log(Level: TLogLevel; const Msg: string);
const
  LevelNames: array[TLogLevel] of string = ( 'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL' );
var
  LogMsg: string;
begin
  if not FisActive then Exit;
  if Level < FLogLevel then Exit;

  LogMsg := Format('[%s] %s: %s', [ FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), LevelNames[Level], Msg ]);

  FCS.Acquire;
  try
    WriteToConsole(LogMsg);
    if FLogToFile then
      WriteToFile(LogMsg);
    {$IFDEF MSWINDOWS}
    if Level <= llWarning then
      WriteToDebugger(LogMsg);
    {$ENDIF}
  finally
    FCS.Release;
  end;
end;

procedure TBadgerLogger.Debug(const Msg: string);
begin
  Log(llDebug, Msg);
end;

procedure TBadgerLogger.Info(const Msg: string);
begin
  Log(llInfo, Msg);
end;

procedure TBadgerLogger.Warning(const Msg: string);
begin
  Log(llWarning, Msg);
end;

procedure TBadgerLogger.Error(const Msg: string);
begin
  Log(llError, Msg);
end;

procedure TBadgerLogger.Critical(const Msg: string);
begin
  Log(llCritical, Msg);
end;

initialization
  Logger := TBadgerLogger.Create;

finalization
  Logger.Free;

end.
