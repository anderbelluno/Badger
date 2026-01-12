unit Unit1;

{$mode delphi}{$H+}

interface

uses
    {$IFDEF MSWINDOWS}Windows, {$ENDIF} Messages, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
    Badger, BadgerBasicAuth, BadgerAuthJWT, BadgerTypes, BadgerLogger, SampleRouteManager;

type

    { TForm1 }

    TForm1 = class(TForm)
        btnClearLog: TButton;
        btnSyna: TButton;
        edtPorta: TEdit;
        edtTimeOut: TEdit;
        Label1: TLabel;
        Label2: TLabel;
        Memo1: TMemo;
        Panel1: TPanel;
        Panel2: TPanel;
        RadioGroup1: TRadioGroup;
        rdLog: TCheckBox;
        procedure btnClearLogClick(Sender: TObject);
        procedure btnSynaClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
    private
        ServerThread: TBadger;
        BasicAuth: TBasicAuth;
        JWTAuth: TBadgerJWTAuth;
    public
        procedure HandleRequest(const RequestInfo: TRequestInfo);
        procedure HandleResponse(const ResponseInfo: TResponseInfo);
    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnSynaClick(Sender: TObject);
begin
  Logger.isActive := False;
  Logger.LogToConsole := False;

    if btnSyna.Tag = 0 then
  begin
    ServerThread := TBadger.Create;
    ServerThread.EnableEventInfo := rdLog.Checked;
    ServerThread.Port := StrToInt(edtPorta.Text);
    ServerThread.Timeout := StrToInt(edtTimeOut.Text);

    ServerThread.OnRequest  := HandleRequest;
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
      .AddGet('/teste/ping', TSampleRouteManager.ping)
      .AddPost('/AtuImage', TSampleRouteManager.AtuImage)
      .AddPost('/Login',TSampleRouteManager.Login)
      .AddGet('/RefreshToken',TSampleRouteManager.RefreshToken)
      .AddGet('/produtos/:id/:codigo', TSampleRouteManager.produtos)
      .AddGet('/produtos', TSampleRouteManager.produtos);

    ServerThread.ParallelProcessing:= True;
    ServerThread.MaxConcurrentConnections:= 30000;

    ServerThread.Start;
    edtPorta.Enabled := False;
    rdLog.Enabled := False;
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
    RadioGroup1.Enabled := True;
    edtTimeOut.Enabled:= True;
  end;
end;

procedure TForm1.btnClearLogClick(Sender: TObject);
begin
   Memo1.Lines.Clear;
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
          memo1.SelStart := Length(Memo1.Text);
    end;
end;

procedure TForm1.HandleResponse(const ResponseInfo: TResponseInfo);
begin
    if rdLog.Checked then
    begin
      Memo1.Lines.Add('Server Response: ' + #13#10 + IntToStr(ResponseInfo.StatusCode) + ' ' + ResponseInfo.Body + #13#10
       + DateTimeToStr(ResponseInfo.Timestamp) + #13#10);
       memo1.SelStart := Length(Memo1.Text);
    end;
end;

end.

