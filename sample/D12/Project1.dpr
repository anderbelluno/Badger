program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Badger in '..\..\src\Badger.pas',
  BadgerMethods in '..\..\src\BadgerMethods.pas',
  BadgerMultipartDataReader in '..\..\src\BadgerMultipartDataReader.pas',
  BadgerRouteManager in '..\..\src\BadgerRouteManager.pas',
  SyUtils in '..\..\src\SyUtils.pas',
  BadgerBasicAuth in '..\..\src\Auth\BadgerBasicAuth.pas',
  BadgerHttpStatus in '..\..\src\BadgerHttpStatus.pas',
  BadgerTypes in '..\..\src\BadgerTypes.pas',
  BadgerRequestHandler in '..\..\src\BadgerRequestHandler.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
