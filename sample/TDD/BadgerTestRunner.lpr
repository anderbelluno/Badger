program BadgerTestRunner;

{$mode delphi}{$H+}

{
  FPC / Lazarus entry point for the Badger TDD harness.

  Build (from repo root):
    fpc -Mdelphi sample\TDD\BadgerTestRunner.lpr ^
        -Fu.\src ^
        -Fu.\src\Auth\JWT ^
        -Fu.\src\Auth\Basic ^
        -Fu.\sample ^
        -Fu.\sample\TDD ^
        -Fu.\ThirdParty\Synapse ^
        -Fu.\ThirdParty\SuperObject\D_Plus_Laz_Linux ^
        -B

  Run:
    sample\TDD\BadgerTestRunner.exe

  Exit code = number of FAIL + ERROR. 0 means all green.
}

uses
  {$IFDEF UNIX}
  cmem,
  cthreads,
  {$ENDIF}
  SysUtils,
  BadgerLogger,
  BadgerTestSuite,
  BadgerTestClient,
  BadgerTestRoutes,
  BadgerTests;

begin
  // Quiet the global logger so test output stays readable.
  Logger.isActive := False;
  Logger.LogToConsole := False;

  RegisterAllTests;
  RunAllTests;
  Halt(GetExitCode);
end.
