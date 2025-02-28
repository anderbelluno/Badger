unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Layouts,
  FMX.Controls.Presentation, FMX.Edit, Badger;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    btnSyna: TButton;
    rdLog: TCheckBox;
    CBxNonBlockMode: TCheckBox;
    Memo1: TMemo;
    Label1: TLabel;
    edtPorta: TEdit;
    procedure btnSynaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    ServerThread: TBadger;
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
    ServerThread.Port := StrToInt(edtPorta.Text);
    edtPorta.Enabled := False;
    rdLog.Enabled := False;
    CBxNonBlockMode.Enabled := False;
    btnSyna.Tag := 1;
    btnSyna.Text := 'Parar Servidor';
  end
  else
  begin
    StopHTTPServer(ServerThread);
    btnSyna.Tag := 0;
    btnSyna.Text := 'Iniciar Servidor';
    edtPorta.Enabled := True;
    rdLog.Enabled := True;
    CBxNonBlockMode.Enabled := True;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ServerThread := nil;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  StopHTTPServer(ServerThread);
end;

procedure TForm1.onLastRequest(Value: String);
begin
if rdLog.IsChecked then
    Memo1.Lines.Add('Client Request: ' + #13#10 + Value);
end;

procedure TForm1.onLastResponse(Value: String);
begin
   if rdLog.IsChecked then
    Memo1.Lines.Add('Server Response: ' + #13#10 + Value);
end;

procedure TForm1.StartHTTPServer(var ServerThread: TBadger);
begin
  ServerThread := TBadger.Create;
  ServerThread.NonBlockMode := CBxNonBlockMode.IsChecked;
  ServerThread.Username := 'andersons';
  ServerThread.Password := 'fioris';
  ServerThread.Token    := 'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.'+
                           'eyJpc3MiOiJhcGkuaWxvdmVwZGYuY29tIiwiY'+
                           'XVkIjoiIiwiaWF0IjoxNzI1OTI2NzIxLCJuYmY'+
                           'iOjE3MjU5MjY3MjEsImV4cCI6MTcyNTkzMDMyM'+
                           'SwianRpIjoicHJvamVjdF9wdWJsaWNfOTM4YzI1'+
                           'NmU3ZDBmOTlhNjE4MGEzZTdhMDQxYTFjMzZfZml'+
                           '0MXg3ZTNjZTQwOGY2ZWVkYWE4ODY0MjJhZjBkOD'+
                           'YxMTgwZCJ9.Ut6qSflj7QYH4DgQK-pibzoF7QB-pwb_dJqpNKvJ52k';

  ServerThread.OnLastRequest := onLastRequest;
  ServerThread.OnLastResponse := onLastResponse;

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
