program IocpPingGUI;

uses
  Forms,
  MainForm in 'MainForm.pas' {FormMain},
  BadgerWinSock2 in '..\..\..\src\IOCP\BadgerWinSock2.pas',
  BadgerHttpStatus in '..\..\..\src\BadgerHttpStatus.pas',
  BadgerHttpParser in '..\..\..\src\BadgerHttpParser.pas',
  BadgerWebSocket in '..\..\..\src\BadgerWebSocket.pas',
  blcksock in '..\..\..\ThirdParty\Synapse\blcksock.pas',
  BadgerTypes in '..\..\..\src\BadgerTypes.pas',
  BadgerRouteManager in '..\..\..\src\BadgerRouteManager.pas',
  BadgerLogger in '..\..\..\src\BadgerLogger.pas',
  BadgerHttpUtils in '..\..\..\src\BadgerHttpUtils.pas',
  BadgerUtils in '..\..\..\src\BadgerUtils.pas',
  BadgerUploadUtils in '..\..\..\src\BadgerUploadUtils.pas',
  BadgerMultipartDataReader in '..\..\..\src\BadgerMultipartDataReader.pas',
  BadgerMethods in '..\..\..\src\BadgerMethods.pas',
  BadgerHttpDispatch in '..\..\..\src\BadgerHttpDispatch.pas',
  BadgerIOCP in '..\..\..\src\IOCP\BadgerIOCP.pas',
  BadgerRequestHandler in '..\..\..\src\BadgerRequestHandler.pas',
  Badger in '..\..\..\src\Badger.pas',
  BadgerJWTUtils in '..\..\..\src\Auth\JWT\BadgerJWTUtils.pas',
  BadgerJWTClaims in '..\..\..\src\Auth\JWT\BadgerJWTClaims.pas',
  BadgerAuthJWT in '..\..\..\src\Auth\JWT\BadgerAuthJWT.pas',
  BadgerBasicAuth in '..\..\..\src\Auth\Basic\BadgerBasicAuth.pas',
  IocpDemoRoutes in '..\IocpDemoRoutes.pas',
  IocpWsChatClient in '..\IocpWsChatClient.pas',
  IocpDemoHttpRoutes in '..\IocpDemoHttpRoutes.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
