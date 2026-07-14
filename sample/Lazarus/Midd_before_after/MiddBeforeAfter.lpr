program MiddBeforeAfter;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms,
  Unit1,
  DemoRoutes;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar := True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
