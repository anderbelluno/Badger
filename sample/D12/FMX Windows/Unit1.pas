unit Unit1;

{

  Check the Project-Search Path for more info

}

interface

uses
  Badger,
  BadgerBasicAuth,

  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Layouts,
  FMX.Controls.Presentation, FMX.Edit, FMX.ListBox;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    btnSyna: TButton;
    rdLog: TCheckBox;
    CBxNonBlockMode: TCheckBox;
    Memo1: TMemo;
    Label1: TLabel;
    edtPorta: TEdit;
    ComboAuth: TComboBox;
    Layout2: TLayout;
    Label2: TLabel;
    edtTimeOut: TEdit;
    procedure btnSynaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    ServerThread: TBadger;
    BasicAuth: TBasicAuth; // Instância da autenticação Basic

  public
    { Public declarations }

    procedure onLastRequest(Value : String);
    procedure onLastResponse(Value : String);
  end;

var
  Form1: TForm1;

implementation

uses
  SampleRouteManager;

{$R *.fmx}

procedure TForm1.btnSynaClick(Sender: TObject);
begin
  if btnSyna.Tag = 0 then
  begin
    ServerThread := TBadger.Create;
    ServerThread.Port := StrToInt(edtPorta.Text);
    ServerThread.Timeout := StrToInt(edtTimeOut.Text);
    ServerThread.NonBlockMode := CBxNonBlockMode.IsChecked;
    ServerThread.OnLastRequest := onLastRequest;
    ServerThread.OnLastResponse := onLastResponse;
    if ComboAuth.ItemIndex = 1 then
      ServerThread.AddMiddleware(BasicAuth.Check);

    ServerThread.RouteManager
      .&Register('/upload', TSampleRouteManager.upLoad)
      .&Register('/download', TSampleRouteManager.downLoad)
      .&Register('/rota1', TSampleRouteManager.rota1)
      .&Register('/ping', TSampleRouteManager.ping)
      .&Register('/AtuImage', TSampleRouteManager.AtuImage);

    ServerThread.Start; // Inicia o servidor
    edtPorta.Enabled := False;
    rdLog.Enabled := False;
    CBxNonBlockMode.Enabled := False;
    btnSyna.Tag := 1;
    btnSyna.Text := 'Parar Servidor';
    ComboAuth.Enabled := False;
    edtTimeOut.Enabled := False;
  end
  else
  begin
    ServerThread.Stop; // Para o servidor
    ServerThread := nil; // A thread já se libera com FreeOnTerminate
    btnSyna.Tag := 0;
    btnSyna.Text := 'Iniciar Servidor';
    edtPorta.Enabled := True;
    rdLog.Enabled := True;
    CBxNonBlockMode.Enabled := True;
    ComboAuth.Enabled := True;
    edtTimeOut.Enabled := True;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ServerThread := nil;
  BasicAuth := TBasicAuth.Create('andersons', 'fioris');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
if Assigned(ServerThread) then
    ServerThread.Stop; // Para o servidor ao destruir o formulário
  FreeAndNil(BasicAuth);
end;

procedure TForm1.onLastRequest(Value: String);
begin
  if rdLog.IsChecked then
    TThread.Synchronize(nil, procedure
    begin
      Memo1.Lines.Add('Client Request: ' + #13#10 + Value + #13#10);
    end);
end;

procedure TForm1.onLastResponse(Value: String);
begin
  if rdLog.IsChecked then
    TThread.Synchronize(nil, procedure
    begin
      Memo1.Lines.Add('Server Response: ' + #13#10 + Value + #13#10);
    end);
end;


end.

unit uBRSSService;

interface

uses
  Windows, Messages, SysUtils, Classes, SvcMgr
  ,xLog
  ,uIniFiles
  ,Registry
  ,ufMain,  Badger;

const
  // This has to be the same as DataModule Name:
  MY_DATASERVICE_NAME='BRSSService';

type
  TBRSSService = class(TService)
    procedure ServiceExecute(Sender: TService);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceAfterInstall(Sender: TService);
  private
    { Private declarations }
    ServerThread: TBadger;  //Server Synapse 04/10/2024
    FIniFileName : String;
//    FTcpDaemon : TTcpDaemon;
    procedure Display(Msg: String);
  public
    { Public declarations }
    //Running:boolean;

    function GetServiceController: TServiceController; override;

    (*
    procedure StartBRSSService;
    procedure StopBRSSService;
    *)
  end;

var
  BRSSService: TBRSSService;

implementation

uses SoliConst;

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  BRSSService.Controller(CtrlCode);
end;

function TBRSSService.GetServiceController: TServiceController;
begin
   Result := ServiceController;
end;

procedure TBRSSService.ServiceExecute(Sender: TService);
begin
    while not Terminated do
        ServiceThread.ProcessRequests(TRUE);
end;

procedure TBRSSService.ServiceCreate(Sender: TObject);
Var
   IniFile : TIcsIniFile;
begin
   Log('Service Create ...');

   FIniFileName := uIniFiles.GetIcsIniFileName;

   IniFile      := TIcsIniFile.Create(FIniFileName);
   Try
      {
      FTcpDaemon                    := TTcpDaemon.Create;
      FTcpDaemon.Banner             := DisplayName + ' Ready';
      FTcpDaemon.Port               := IniFile.ReadString(SectionMain,    KeyListenPort,        '9070');
      FTcpDaemon.BRTS_IPServer      := IniFile.ReadString(SectionMain,    KeyIp,                    '');
      FTcpDaemon.BRTS_IPPorta       := IniFile.ReadInteger(SectionMain,   KeyPort,                   0);
      FTcpDaemon.BRTS_Compression   := IniFile.ReadInteger(SectionMain,   KeyCompression,          Nao);
      FTcpDaemon.BRTS_CompressLevel := IniFile.ReadInteger(SectionMain,   KeyCompressionLevel,       0);
      FTcpDaemon.BRTS_Encryption    := IniFile.ReadInteger(SectionMain,   KeyEncryption,           Nao);
      FTcpDaemon.BRTS_Key           := IniFile.ReadString(SectionMain,    KeyEncryptionKey,         '');
      }
      // Le parâmetros no arquivo INI
      // Define variáveis globais para serem usadas do Rest DataModule !
      G_ListenPort    := IniFile.ReadInteger(SectionMain, KeyListenPort,       40021);
      G_AutoStart     := IniFile.ReadInteger(SectionMain, KeyAutoStart,          Nao);
      G_LogRequests   := IniFile.ReadInteger(SectionMain, KeyLogRequests,        Nao);
      G_IPServer      := IniFile.ReadString(SectionMain,  KeyIp,                  '');
      G_IPPorta       := IniFile.ReadInteger(SectionMain, KeyPort,                 0);
      G_Compression   := IniFile.ReadInteger(SectionMain, KeyCompression,        Nao);
      G_CompressLevel := IniFile.ReadInteger(SectionMain, KeyCompressionLevel,     0);
      G_Encryption    := IniFile.ReadInteger(SectionMain, KeyEncryption,         Nao);
      G_Key           := IniFile.ReadString(SectionMain,    KeyEncryptionKey,     '');

      {RESTServicePooler.ServicePort := G_ListenPort;
      RESTServicePooler.ServerMethodClass := TdmRest;}

      ServerThread.Port := G_ListenPort;
      ServerThread.Start;

//      Log('FTcpDaemon.Banner: ' + FTcpDaemon.Banner);
//      FTcpDaemon.OnDisplay := Display;
   Finally
      IniFile.Free;
   End;
end;



procedure TBRSSService.ServiceStart(Sender: TService; var Started: Boolean);
begin
   Log('Service Starting ...');
   try
//      StartBRSSService;
      //FTcpDaemon.Start;
      If Boolean( G_AutoStart ) Then
      Begin
         Started := TRUE;
         If Assigned(ServerThread) then
         Begin
            Log('Service Started: ' + FormatDateTime('dd/mm/yyyy hh:nn', Now) );
            Log('Service listining on port: ' + IntToStr(ServerThread.Port) );
         End
         Else
         Begin
            Log('Service Pooler Start failure: ' + FormatDateTime('dd/mm/yyyy hh:nn', Now) );
         End;
      End
      Else
         Log('Service is not AutoStart ');
   except
      on E:Exception do
      Begin
         Started := FALSE;
         Log('Error: '+E.ClassName+' '+E.Message);
      End;
   end;
//   Log('Service Started.');

//   Started:=Running;
end;

procedure TBRSSService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
   Log('Service Stopping ...');
   try
//      StopBRSSService;
      //FTcpDaemon.Stop;
      ServerThread.Stop;
      Stopped := TRUE;
      Log('Service Stopped: ' + FormatDateTime('dd/mm/yyyy hh:nn', Now) );
   except
   on E:Exception do
      Log('Error: '+E.ClassName+' '+E.Message);
   end;
//   Log('Service Stopped.');

//   Stopped:=not Running;
end;

procedure TBRSSService.ServiceDestroy(Sender: TObject);
begin
{
   if Assigned(FTcpDaemon) then
   begin
      FTcpDaemon.Destroy;
      FTcpDaemon := nil;
   end;
}
end;

procedure TBRSSService.ServiceShutdown(Sender: TService);
begin
   Log('Service Shutting down ...');
   try
//      StopBRSSService;
      Log('Service Shut down.');
   except
   on E:Exception do
      Log('Error: '+E.ClassName+' '+E.Message);
   end;
//   Log('Service Shut down.');
end;

procedure TBRSSService.ServiceAfterInstall(Sender: TService);
var
   regEdit: TRegINIFile;
begin
   If BRSSServiceDescription <> '' Then
   Begin
      regEdit := TRegINIFile.Create;
      try
        regEdit.RootKey := HKEY_LOCAL_MACHINE;
        if regEdit.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name,False) then
        Begin
           regEdit.WriteString('', 'Description', BRSSServiceDescription);
           regEdit.CloseKey;
        End;
      finally
        regEdit.Free;
      end;
   End;
end;

procedure TBRSSService.Display(Msg : String);
begin
    // A service has no access to the GUI.
    // Simply ignore any display :-(
    // We could log messages to Windows EventLog
end;

(*
procedure TBRSSService.StartBRSSService;
begin
   if not running then
   begin
      StartThreads;
      // Service starting, execute initialization code here ...
      running := True;
   end;
end;

procedure TBRSSService.StopBRSSService;
begin
   if running then
   begin
      StopThreads;
      // Service stopping, execute finalization code here ...
      running := False;
   end;
end;
*)



end.
