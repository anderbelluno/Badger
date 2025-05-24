program project1;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}
    cthreads,
    {$ENDIF}
    {$IFDEF HASAMIGA}
    athreads,
    {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, unit1, SampleRouteManager
    { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource:=True;
    Application.Title:='project1';
    Application.Scaled:=True;
    Application.Initialize;
    Application.CreateForm(TForm1, Form1);
    Application.Run;
end.

