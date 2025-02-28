program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Badger in '..\TTCPBlockSocket\src\Badger.pas',
  BadgerMethods in '..\TTCPBlockSocket\src\BadgerMethods.pas',
  BadgerMultipartDataReader in '..\TTCPBlockSocket\src\BadgerMultipartDataReader.pas',
  BadgerRouteManager in '..\TTCPBlockSocket\src\BadgerRouteManager.pas',
  SyUtils in '..\TTCPBlockSocket\src\SyUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
