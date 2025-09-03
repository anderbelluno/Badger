unit BadgerLogger;

{$I BadgerDefines.inc}

interface

uses
  SysUtils, SyncObjs, Classes
  {$IFDEF MSWINDOWS}, Windows{$ENDIF};

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);
  
  TBadgerLogger = class
  private
    FCS: TCriticalSection;
    FisActive : Boolean;
    FLogFile: TextFile;
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
  FLogLevel := llDebug; // Log mais verboso em debug
  {$ENDIF}
end;

destructor TBadgerLogger.Destroy;
begin
  if FLogToFile then
    CloseFile(FLogFile);
  FCS.Free;
  inherited;
end;

procedure TBadgerLogger.WriteToFile(const Msg: string);
begin
  if not FLogToFile then Exit;
  
  FCS.Acquire;
  try
    if not FileExists(FLogFileName) then
    begin
      AssignFile(FLogFile, FLogFileName);
      Rewrite(FLogFile);
    end
    else
    begin
      AssignFile(FLogFile, FLogFileName);
      Append(FLogFile);
    end;
    
    WriteLn(FLogFile, Msg);
    CloseFile(FLogFile);
  finally
    FCS.Release;
  end;
end;

procedure TBadgerLogger.WriteToConsole(const Msg: string);
begin
  if not FLogToConsole then Exit;
  
  FCS.Acquire;
  try
    WriteLn(Msg);
  finally
    FCS.Release;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TBadgerLogger.WriteToDebugger(const Msg: string);
begin
  OutputDebugString(PChar(Msg));
end;
{$ENDIF}

procedure TBadgerLogger.Log(Level: TLogLevel; const Msg: string);
const
  LevelNames: array[TLogLevel] of string = (
    'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'
  );
var
  LogMsg: string;
begin
 if not FisActive then
   Exit;
   
  if Level < FLogLevel then Exit; // Filtro por nível
  
  LogMsg := Format('[%s] %s: %s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
    LevelNames[Level],
    Msg
  ]);

  // Escrita thread-safe
  FCS.Acquire;
  try
    // Console (sempre disponível)
    WriteToConsole(LogMsg);
    
    // Arquivo (se habilitado)
    if FLogToFile then
      WriteToFile(LogMsg);
      
    // Debug do Windows (apenas Windows)
    {$IFDEF MSWINDOWS}
    if Level <= llWarning then // Só debug/warning no OutputDebugString
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
