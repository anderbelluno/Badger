unit BadgerTestSuite;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

{
  Badger TDD harness — assertion + registry + console reporter.

  Cross-compiler: Delphi 7+, Delphi 12, FPC/Lazarus on Win and Unix.
  Zero external test framework. Every dependency is RTL or Synapse.

  Pattern:
    - Test procedures take no parameters and raise ETestFailure on assertion miss.
    - Each test starts and stops its own TBadger instance on a unique port.
    - Console runner returns ExitCode = (Failures + Errors), so CI/scripts
      can detect regressions with a non-zero exit.

  See sample/TDD/README.md for run instructions.
}

interface

uses
  SysUtils, Classes;

type
  TBadgerTestProc = procedure;

  TBadgerTestEntry = record
    Name: string;
    Group: string;
    Proc: TBadgerTestProc;
  end;

  ETestFailure = class(Exception);

procedure RegisterTest(const AGroup, AName: string; AProc: TBadgerTestProc);
procedure RunAllTests;
function GetExitCode: Integer;

procedure AssertTrue(ACondition: Boolean; const AMessage: string);
procedure AssertFalse(ACondition: Boolean; const AMessage: string);

procedure AssertEqualsStr(const AExpected, AActual: string; const AMessage: string);
procedure AssertEqualsInt(AExpected, AActual: Integer; const AMessage: string);

procedure AssertContains(const ANeedle, AHaystack: string; const AMessage: string);
procedure AssertNotContains(const ANeedle, AHaystack: string; const AMessage: string);

function NextTestPort: Integer;

implementation

var
  GTests: array of TBadgerTestEntry;
  GFailures: Integer;
  GPasses: Integer;
  GErrors: Integer;
  GPortCounter: Integer = 18765;

procedure RegisterTest(const AGroup, AName: string; AProc: TBadgerTestProc);
var
  L: Integer;
begin
  L := Length(GTests);
  SetLength(GTests, L + 1);
  GTests[L].Group := AGroup;
  GTests[L].Name := AName;
  GTests[L].Proc := AProc;
end;

function NextTestPort: Integer;
begin
  Result := GPortCounter;
  Inc(GPortCounter);
end;

procedure AssertTrue(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise ETestFailure.Create(AMessage);
end;

procedure AssertFalse(ACondition: Boolean; const AMessage: string);
begin
  if ACondition then
    raise ETestFailure.Create(AMessage);
end;

procedure AssertEqualsStr(const AExpected, AActual: string; const AMessage: string);
begin
  if AExpected <> AActual then
    raise ETestFailure.Create(AMessage + ' -- expected="' + AExpected +
      '" actual="' + AActual + '"');
end;

procedure AssertEqualsInt(AExpected, AActual: Integer; const AMessage: string);
begin
  if AExpected <> AActual then
    raise ETestFailure.Create(AMessage + ' -- expected=' + IntToStr(AExpected) +
      ' actual=' + IntToStr(AActual));
end;

procedure AssertContains(const ANeedle, AHaystack: string; const AMessage: string);
var
  Snip: string;
begin
  if Pos(ANeedle, AHaystack) <= 0 then
  begin
    Snip := Copy(AHaystack, 1, 240);
    raise ETestFailure.Create(AMessage + ' -- needle="' + ANeedle +
      '" not found in: ' + Snip);
  end;
end;

procedure AssertNotContains(const ANeedle, AHaystack: string; const AMessage: string);
var
  Snip: string;
begin
  if Pos(ANeedle, AHaystack) > 0 then
  begin
    Snip := Copy(AHaystack, 1, 240);
    raise ETestFailure.Create(AMessage + ' -- needle="' + ANeedle +
      '" was unexpectedly found in: ' + Snip);
  end;
end;

function ElapsedMs(const T0: TDateTime): Integer;
begin
  Result := Round((Now - T0) * 86400000.0);
end;

procedure RunAllTests;
var
  I, Total: Integer;
  T0: TDateTime;
  Suite0: TDateTime;
  CurrentGroup: string;
begin
  Total := Length(GTests);
  WriteLn('===========================================================');
  WriteLn(' Badger TDD Harness — ' + IntToStr(Total) + ' tests');
  WriteLn('===========================================================');
  CurrentGroup := '';
  Suite0 := Now;

  for I := 0 to Total - 1 do
  begin
    if GTests[I].Group <> CurrentGroup then
    begin
      CurrentGroup := GTests[I].Group;
      WriteLn('');
      WriteLn('-- ' + CurrentGroup + ' --');
    end;

    Write(Format('  [%2d/%2d] %-52s ', [I + 1, Total, GTests[I].Name]));
    T0 := Now;
    try
      GTests[I].Proc;
      Inc(GPasses);
      WriteLn(Format('PASS (%dms)', [ElapsedMs(T0)]));
    except
      on E: ETestFailure do
      begin
        Inc(GFailures);
        WriteLn(Format('FAIL (%dms)', [ElapsedMs(T0)]));
        WriteLn('         -> ' + E.Message);
      end;
      on E: Exception do
      begin
        Inc(GErrors);
        WriteLn(Format('ERROR (%dms)', [ElapsedMs(T0)]));
        WriteLn('         -> ' + E.ClassName + ': ' + E.Message);
      end;
    end;
  end;

  WriteLn('');
  WriteLn('===========================================================');
  WriteLn(Format(' PASS: %d   FAIL: %d   ERROR: %d   (total %dms)',
    [GPasses, GFailures, GErrors, ElapsedMs(Suite0)]));
  WriteLn('===========================================================');
end;

function GetExitCode: Integer;
begin
  Result := GFailures + GErrors;
end;

end.
