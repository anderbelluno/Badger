unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  Badger, BadgerBasicAuth, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    btnSyna: TButton;
    rdLog: TCheckBox;
    CBxNonBlockMode: TCheckBox;
    Memo1: TMemo;
    Label1: TLabel;
    edtPorta: TEdit;
    RadioGroup1: TRadioGroup;
    Panel2: TPanel;
    Panel1: TPanel;
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

{$R *.dfm}

procedure TForm1.btnSynaClick(Sender: TObject);
begin
  if btnSyna.Tag = 0 then
  begin
    ServerThread := TBadger.Create;
    ServerThread.Port := StrToInt(edtPorta.Text);
    ServerThread.Timeout := StrToInt(edtTimeOut.Text);
    ServerThread.NonBlockMode := CBxNonBlockMode.Checked;
    ServerThread.OnLastRequest := onLastRequest;
    ServerThread.OnLastResponse := onLastResponse;
    if RadioGroup1.ItemIndex = 1 then
      ServerThread.AddMiddleware(BasicAuth.Check);
    ServerThread.Start; // Inicia o servidor
    edtPorta.Enabled := False;
    rdLog.Enabled := False;
    CBxNonBlockMode.Enabled := False;
    btnSyna.Tag := 1;
    btnSyna.Caption := 'Parar Servidor';
    RadioGroup1.Enabled := False;
  end
  else
  begin
    ServerThread.Stop; // Para o servidor
    ServerThread := nil; // A thread já se libera com FreeOnTerminate
    btnSyna.Tag := 0;
    btnSyna.Caption := 'Iniciar Servidor';
    edtPorta.Enabled := True;
    rdLog.Enabled := True;
    CBxNonBlockMode.Enabled := True;
    RadioGroup1.Enabled := True;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ServerThread := nil;
  BasicAuth := TBasicAuth.Create('andersons', 'fioris'); // Credenciais fixas para teste
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
if Assigned(ServerThread) then
    ServerThread.Stop; // Para o servidor ao destruir o formulário
  FreeAndNil(BasicAuth);
end;

procedure TForm1.onLastRequest(Value: String);
begin
if rdLog.Checked then
    Memo1.Lines.Add('Client Request: ' + #13#10 + Value);
end;

procedure TForm1.onLastResponse(Value: String);
begin
   if rdLog.Checked then
    Memo1.Lines.Add('Server Response: ' + #13#10 + Value);
end;

end.
