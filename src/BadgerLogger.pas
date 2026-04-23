unit BadgerLogger;

{$I BadgerDefines.inc}

interface

uses
  {$IFDEF BADGER_WINDOWS} Windows,{$ENDIF}
  {$IFDEF FPC}{$IFDEF UNIX}BaseUnix, ctypes,{$ENDIF}{$ENDIF}
  SysUtils, SyncObjs, Classes
  ;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);

  TBadgerLogger = class
  private
    FCS: TCriticalSection;
    FisActive : Boolean;
    FLogFileName: string;
    FActiveLogFileName: string;
    FLogToFile: Boolean;
    FLogToConsole: Boolean;
    FLogLevel: TLogLevel;
    FLogStream: TFileStream;
    FFileWriteErrorReported: Boolean;
    function HasConsole: Boolean;
    procedure ReportLogWriteFailure(const Detail: string);
    procedure CloseLogStream;
    function EnsureLogStream: Boolean;
    procedure WriteToFile(const Msg: string);
    procedure WriteToConsole(const Msg: string);
    {$IFDEF BADGER_WINDOWS}
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

{$IFDEF FPC}
  {$IFDEF UNIX}
function c_isatty(fd: cint): cint; cdecl; external 'c' name 'isatty';
  {$ENDIF}
{$ENDIF}

constructor TBadgerLogger.Create;
  function ReadEnv(const Name: string): string;
  begin
    {$IFDEF FPC}
      Result := SysUtils.GetEnvironmentVariable(Name);
    {$ELSE}
      Result := SysUtils.GetEnvironmentVariable(Name);
    {$ENDIF}
  end;
var
  LogEnv: string;
begin
  FCS := TCriticalSection.Create;
  FLogToConsole := True;
  FLogToFile := False;
  FLogLevel := llWarning;
  FLogFileName := 'badger_server.log';
  FActiveLogFileName := '';
  FLogStream := nil;
  FisActive := True;
  FFileWriteErrorReported := False;
  {$IFDEF DEBUG}
  FLogLevel := llDebug;
  {$ENDIF}

  LogEnv := UpperCase(Trim(ReadEnv('BADGER_LOG')));
  if LogEnv <> '' then
  begin
    if (LogEnv = '0') or (LogEnv = 'OFF') or (LogEnv = 'FALSE') then
      FisActive := False
    else if (LogEnv = '1') or (LogEnv = 'ON') or (LogEnv = 'TRUE') then
      FisActive := True
    else if LogEnv = 'DEBUG' then
    begin
      FisActive := True;
      FLogLevel := llDebug;
    end
    else if LogEnv = 'INFO' then
    begin
      FisActive := True;
      FLogLevel := llInfo;
    end
    else if (LogEnv = 'WARN') or (LogEnv = 'WARNING') then
    begin
      FisActive := True;
      FLogLevel := llWarning;
    end
    else if LogEnv = 'ERROR' then
    begin
      FisActive := True;
      FLogLevel := llError;
    end
    else if LogEnv = 'CRITICAL' then
    begin
      FisActive := True;
      FLogLevel := llCritical;
    end;
  end;
end;

destructor TBadgerLogger.Destroy;
begin
  CloseLogStream;
  FCS.Free;
  inherited;
end;

procedure TBadgerLogger.CloseLogStream;
begin
  if Assigned(FLogStream) then
    FreeAndNil(FLogStream);
  FActiveLogFileName := '';
end;

function TBadgerLogger.EnsureLogStream: Boolean;
begin
  Result := False;
  if not FLogToFile then
  begin
    CloseLogStream;
    Exit;
  end;

  if Assigned(FLogStream) and (CompareText(FActiveLogFileName, FLogFileName) = 0) then
  begin
    Result := True;
    Exit;
  end;

  CloseLogStream;
  try
    if FileExists(FLogFileName) then
      FLogStream := TFileStream.Create(FLogFileName, fmOpenReadWrite or fmShareDenyNone)
    else
      FLogStream := TFileStream.Create(FLogFileName, fmCreate or fmShareDenyNone);
    FLogStream.Position := FLogStream.Size;
    FActiveLogFileName := FLogFileName;
    Result := True;
  except
    on E: Exception do
    begin
      ReportLogWriteFailure('EnsureLogStream failed: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TBadgerLogger.HasConsole: Boolean;
{$IFDEF BADGER_WINDOWS}
var
  H: THandle;
  Mode: DWORD;
{$ENDIF}
begin
  {$IFDEF BADGER_WINDOWS}
    H := GetStdHandle(STD_OUTPUT_HANDLE);
    Result := (H <> 0) and (H <> INVALID_HANDLE_VALUE) and GetConsoleMode(H, Mode);
    Exit;
  {$ENDIF}

  {$IFDEF FPC}
    {$IFDEF UNIX}
      Result := c_isatty(1) <> 0;
      Exit;
    {$ENDIF}
  {$ENDIF}

  Result := True;
end;

procedure TBadgerLogger.ReportLogWriteFailure(const Detail: string);
begin
  if FFileWriteErrorReported then
    Exit;

  FFileWriteErrorReported := True;
  FLogToFile := False;

  if HasConsole then
    System.WriteLn('[BADGER][LOGGER] ' + Detail + ' - file logging disabled.');

  {$IFDEF BADGER_WINDOWS}
    WriteToDebugger('[BADGER][LOGGER] ' + Detail + ' - file logging disabled.');
  {$ENDIF}
end;

procedure TBadgerLogger.WriteToFile(const Msg: string);
var
  LogLine: AnsiString;
begin
  if not EnsureLogStream then Exit;

  LogLine := AnsiString(Msg + sLineBreak);
  try
    if Length(LogLine) > 0 then
      FLogStream.WriteBuffer(LogLine[1], Length(LogLine));
    FLogStream.Position := FLogStream.Size;
  except
    on E: Exception do
    begin
      CloseLogStream;
      ReportLogWriteFailure('WriteToFile failed: ' + E.Message);
    end;
  end;
end;

procedure TBadgerLogger.WriteToConsole(const Msg: string);
begin
  if FLogToConsole and HasConsole then
    WriteLn(Msg);
end;

{$IFDEF BADGER_WINDOWS}
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
    {$IFDEF BADGER_WINDOWS}
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
