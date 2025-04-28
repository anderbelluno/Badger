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


end.
