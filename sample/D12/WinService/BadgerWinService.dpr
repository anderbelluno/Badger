program BadgerWinService;

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.SvcMgr,
  UBadgerService in 'UBadgerService.pas' {FBadgerService: TService},
  SampleRouteManager in '..\..\Common\SampleRouteManager.pas';

{$R *.RES}

{ When started from the IDE (or double-click), the process is not launched by
  the Service Control Manager — Application.Run returns immediately.
  RunInteractive keeps Badger alive until Enter is pressed. }

function RunningAsService: Boolean;
var
  SessionId: DWORD;
begin
  Result := ProcessIdToSessionId(GetCurrentProcessId, SessionId) and (SessionId = 0);
end;

procedure BindStdHandles;
var
  H: THandle;
begin
  H := GetStdHandle(STD_OUTPUT_HANDLE);
  if (H = 0) or (H = INVALID_HANDLE_VALUE) then
  begin
    H := CreateFile('CONOUT$', GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    SetStdHandle(STD_OUTPUT_HANDLE, H);
  end;
  H := GetStdHandle(STD_INPUT_HANDLE);
  if (H = 0) or (H = INVALID_HANDLE_VALUE) then
  begin
    H := CreateFile('CONIN$', GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    SetStdHandle(STD_INPUT_HANDLE, H);
  end;
end;

procedure RunInteractive;
var
  Started, Stopped: Boolean;
begin
  AllocConsole;
  BindStdHandles;
  try
    FBadgerService.LogToConsole := True;
    Started := True;
    FBadgerService.ServiceStart(FBadgerService, Started);
    WriteLn('Press Enter to stop...');
    ReadLn;
    Stopped := True;
    FBadgerService.ServiceStop(FBadgerService, Stopped);
  finally
    FreeConsole;
  end;
end;

begin
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TFBadgerService, FBadgerService);

  if Application.Installing or RunningAsService then
    Application.Run
  else
    RunInteractive;
end.
