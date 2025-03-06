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
    procedure btnSynaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    ServerThread: TBadger;
    BasicAuth: TBasicAuth; // Instância da autenticação Basic
    procedure StartHTTPServer(var ServerThread: TBadger);
    procedure StopHTTPServer(var ServerThread: TBadger);
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
    StartHTTPServer(ServerThread);
    edtPorta.Enabled := False;
    rdLog.Enabled := False;
    CBxNonBlockMode.Enabled := False;
    btnSyna.Tag := 1;
    btnSyna.Caption := 'Parar Servidor';
    RadioGroup1.Enabled := False;
  end
  else
  begin
    StopHTTPServer(ServerThread);
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
  StopHTTPServer(ServerThread);
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

procedure TForm1.StartHTTPServer(var ServerThread: TBadger);
begin
  ServerThread := TBadger.Create;
  ServerThread.Port := StrToInt(edtPorta.Text);
  ServerThread.NonBlockMode := CBxNonBlockMode.Checked;
  ServerThread.OnLastRequest := onLastRequest;
  ServerThread.OnLastResponse := onLastResponse;

  if RadioGroup1.ItemIndex = 1 then
    ServerThread.AddMiddleware(BasicAuth.Check); // Adiciona o middleware de autenticação Basic
end;

procedure TForm1.StopHTTPServer(var ServerThread: TBadger);
begin
     if Assigned(ServerThread) then
  begin
    ServerThread.Terminate;
    ServerThread := nil;
  end;
end;

end.
