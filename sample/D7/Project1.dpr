program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  BadgerBasicAuth in '..\..\src\Auth\BadgerBasicAuth.pas',
  Badger in '..\..\src\Badger.pas',
  BadgerHttpStatus in '..\..\src\BadgerHttpStatus.pas',
  BadgerMethods in '..\..\src\BadgerMethods.pas',
  BadgerMultipartDataReader in '..\..\src\BadgerMultipartDataReader.pas',
  BadgerRouteManager in '..\..\src\BadgerRouteManager.pas',
  BadgerTypes in '..\..\src\BadgerTypes.pas',
  SyUtils in '..\..\src\SyUtils.pas',
  BadgerRequestHandler in '..\..\src\BadgerRequestHandler.pas',
  SampleRouteManager in '..\SampleRouteManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
