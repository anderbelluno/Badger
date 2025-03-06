unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Layouts,
  FMX.Controls.Presentation, FMX.Edit, Badger, BadgerBasicAuth, FMX.ListBox;

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

{$R *.fmx}

procedure TForm1.btnSynaClick(Sender: TObject);
begin
  if btnSyna.Tag = 0 then
  begin
    StartHTTPServer(ServerThread);

    edtPorta.Enabled := False;
    rdLog.Enabled := False;
    CBxNonBlockMode.Enabled := False;
    btnSyna.Tag := 1;
    btnSyna.Text := 'Parar Servidor';
    ComboAuth.Enabled := False;
  end
  else
  begin
    StopHTTPServer(ServerThread);
    btnSyna.Tag := 0;
    btnSyna.Text := 'Iniciar Servidor';
    edtPorta.Enabled := True;
    rdLog.Enabled := True;
    CBxNonBlockMode.Enabled := True;
    ComboAuth.Enabled := True;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ServerThread := nil;
  BasicAuth := TBasicAuth.Create('andersons', 'fioris');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  StopHTTPServer(ServerThread);
  FreeAndNil(BasicAuth);
end;

procedure TForm1.onLastRequest(Value: String);
begin
  if rdLog.IsChecked then
    TThread.Synchronize(nil, procedure
    begin
      Memo1.Lines.Add('Client Request: ' + #13#10 + Value);
    end);
end;

procedure TForm1.onLastResponse(Value: String);
begin
  if rdLog.IsChecked then
    TThread.Synchronize(nil, procedure
    begin
      Memo1.Lines.Add('Server Response: ' + #13#10 + Value);
    end);
end;

procedure TForm1.StartHTTPServer(var ServerThread: TBadger);
begin
  ServerThread := TBadger.Create;
  ServerThread.Port := StrToInt(edtPorta.Text);
  ServerThread.NonBlockMode := CBxNonBlockMode.IsChecked;
  ServerThread.OnLastRequest := onLastRequest;
  ServerThread.OnLastResponse := onLastResponse;

  if ComboAuth.ItemIndex = 1 then
    ServerThread.AddMiddleware(BasicAuth.Check);
end;

procedure TForm1.StopHTTPServer(var ServerThread: TBadger);
begin
  if Assigned(ServerThread) then
  begin
    ServerThread.Terminate;
    ServerThread:= nil;
  end;
end;

end.
