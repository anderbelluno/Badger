unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  Badger, BadgerBasicAuth, BadgerAuthJWT, BadgerTypes, SampleRouteManager, ExtCtrls,
  StdCtrls;

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
    btnClearLog: TButton;
    procedure btnSynaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
  private
    { Private declarations }
    ServerThread: TBadger;
    BasicAuth: TBasicAuth;
    JWTAuth: TBadgerJWTAuth;
  public
    { Public declarations }

    procedure HandleRequest(const RequestInfo: TRequestInfo);
    procedure HandleResponse(const ResponseInfo: TResponseInfo);
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
    ServerThread.OnRequest := HandleRequest;
    ServerThread.OnResponse := HandleResponse;

   case RadioGroup1.ItemIndex of
     1: BasicAuth.RegisterProtectedRoutes(ServerThread, ['/rota1', '/ping', '/download']);
     3: begin
           JWTAuth.RegisterProtectedRoutes(ServerThread, ['/rota1', '/ping']);
           SampleRouteManager.FJWT := JWTAuth;
        end;
    end;

    ServerThread.RouteManager
      .AddPost('/upload', TSampleRouteManager.upLoad)
      .AddGet('/download', TSampleRouteManager.downLoad)
      .AddGet('/rota1', TSampleRouteManager.rota1)
      .AddGet('/ping', TSampleRouteManager.ping)
      .AddPost('/AtuImage', TSampleRouteManager.AtuImage)
      .AddPost('/Login',TSampleRouteManager.Login)
      .AddGet('/RefreshToken',TSampleRouteManager.RefreshToken);

      ServerThread.ParallelProcessing := True;

    ServerThread.Start;
    edtPorta.Enabled := False;
    rdLog.Enabled := False;
    CBxNonBlockMode.Enabled := False;
    btnSyna.Tag := 1;
    btnSyna.Caption := 'Parar Servidor';
    RadioGroup1.Enabled := False;
    edtTimeOut.Enabled := False;
  end
  else
  begin
    ServerThread.Stop;
    ServerThread := nil;
    btnSyna.Tag := 0;
    btnSyna.Caption := 'Iniciar Servidor';
    edtPorta.Enabled := True;
    rdLog.Enabled := True;
    CBxNonBlockMode.Enabled := True;
    RadioGroup1.Enabled := True;
    edtTimeOut.Enabled:= True;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ServerThread := nil;
  BasicAuth := TBasicAuth.Create('username', 'password');
  JWTAuth := TBadgerJWTAuth.Create('secretekey', 'c:\tokenss');  //save token to file
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
if Assigned(ServerThread) then
    ServerThread.Stop;
  FreeAndNil(BasicAuth);
  FreeAndNil(JWTAuth);
end;

procedure TForm1.HandleRequest(const RequestInfo: TRequestInfo);
begin
if rdLog.Checked then
  begin
      Memo1.Lines.Add('Client Request: ' + #13#10 + RequestInfo.RequestLine + #13#10);
      Memo1.Lines.Add('Remote Request IP: ' + #13#10 + RequestInfo.RemoteIP + #13#10);
      memo1.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
  end;
end;

procedure TForm1.HandleResponse(const ResponseInfo: TResponseInfo);
begin
   if rdLog.Checked then
     begin
        Memo1.Lines.Add('Server Response: ' + #13#10 + IntToStr(ResponseInfo.StatusCode) + ' ' + ResponseInfo.Body + #13#10
        + DateTimeToStr(ResponseInfo.Timestamp) + #13#10);
        Memo1.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
     end;    
end;

procedure TForm1.btnClearLogClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

end.
