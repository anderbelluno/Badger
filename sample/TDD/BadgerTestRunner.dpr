program BadgerTestRunner;

{
  Delphi (any 7+) entry point for the Badger TDD harness.

  Open in the IDE and build, or compile from a Delphi command prompt:
    dcc32 -B sample\TDD\BadgerTestRunner.dpr ^
          -U.\src;.\src\Auth\JWT;.\src\Auth\Basic;.\sample;.\sample\TDD;^
             .\ThirdParty\Synapse;.\ThirdParty\SuperObject\D_Plus_Laz_Linux

  Run from a console window so WriteLn output is visible.

  Exit code = number of FAIL + ERROR. 0 means all green.
}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  BadgerLogger,
  BadgerTestSuite,
  BadgerTestClient,
  BadgerTestRoutes,
  BadgerTests;

begin
  Logger.isActive := False;
  Logger.LogToConsole := False;

  RegisterAllTests;
  RunAllTests;
  Halt(GetExitCode);
end.
