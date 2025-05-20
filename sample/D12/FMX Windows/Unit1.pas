unit Unit1;

{

  Check the Project-Search Path for more info

}

interface

uses
  Badger,
  BadgerBasicAuth,
  BadgerAuthJWT,
  BadgerTypes,

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
    btnClearLog: TButton;
    procedure btnSynaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
  private
    { Private declarations }
    ServerThread: TBadger;
    BasicAuth: TBasicAuth; // Instância da autenticação Basic
    JWTAuth: TBadgerJWTAuth;
  public
    { Public declarations }

    procedure HandleRequest(const RequestInfo: TRequestInfo);
    procedure HandleResponse(const ResponseInfo: TResponseInfo);
  end;

var
  Form1: TForm1;

implementation

uses
  SampleRouteManager;

{$R *.fmx}

procedure TForm1.btnClearLogClick(Sender: TObject);
begin
   TThread.Synchronize(nil, procedure
    begin
      Memo1.Lines.Clear;
    end);
end;

procedure TForm1.btnSynaClick(Sender: TObject);
begin
  if btnSyna.Tag = 0 then
  begin
    ServerThread := TBadger.Create;
    ServerThread.Port := StrToInt(edtPorta.Text);
    ServerThread.Timeout := StrToInt(edtTimeOut.Text);
    ServerThread.NonBlockMode := CBxNonBlockMode.IsChecked;
    ServerThread.OnRequest := HandleRequest;
    ServerThread.OnResponse := HandleResponse;

   case ComboAuth.ItemIndex of
     1: ServerThread.AddMiddleware(BasicAuth.Check);
     3: begin
           JWTAuth.RegisterMiddleware(ServerThread, ['/rota1', '/ping']);
           SampleRouteManager.FJWT := JWTAuth;
        end;
    end;

    ServerThread.RouteManager
      .&Add('/upload', TSampleRouteManager.upLoad)
      .&Add('/download', TSampleRouteManager.downLoad)
      .&Add('/rota1', TSampleRouteManager.rota1)
      .&Add('/ping', TSampleRouteManager.ping)
      .&Add('/AtuImage', TSampleRouteManager.AtuImage)
      .&Add('/Login',TSampleRouteManager.Login)
      .&Add('/RefreshToken',TSampleRouteManager.RefreshToken);

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
  JWTAuth := TBadgerJWTAuth.Create('fiori88092821', 'c:\tokenss');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
if Assigned(ServerThread) then
    ServerThread.Stop; // Para o servidor ao destruir o formulário
  FreeAndNil(BasicAuth);
  FreeAndNil(JWTAuth);
end;

procedure TForm1.HandleRequest(const RequestInfo: TRequestInfo);
begin
  if rdLog.IsChecked then
    TThread.Synchronize(nil, procedure
    begin
      Memo1.Lines.Add('Client Request: ' + #13#10 + RequestInfo.RequestLine + #13#10);
      Memo1.Lines.Add('Remote Request IP: ' + #13#10 + RequestInfo.RemoteIP + #13#10);
      Memo1.GoToTextEnd;
    end);
end;

procedure TForm1.HandleResponse(const ResponseInfo: TResponseInfo);
begin
  if rdLog.IsChecked then
    TThread.Synchronize(nil, procedure
    begin
      Memo1.Lines.Add('Server Response: ' + #13#10 + ResponseInfo.StatusCode.ToString + ' ' + ResponseInfo.Body + #13#10
      + ' ' + DateTimeToStr(ResponseInfo.Timestamp) + #13#10);
      Memo1.GoToTextEnd;
    end);
end;

end.
