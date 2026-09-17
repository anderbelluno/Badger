unit UBadgerService;

interface

uses
  Badger,
  BadgerTypes,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.SvcMgr,
  Vcl.Dialogs;

type
  TFBadgerService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceDestroy(Sender: TObject);
  private
    FServerThread: TBadger;
    FLogToConsole: Boolean;
    procedure HandleRequest(const RequestInfo: TRequestInfo);
    procedure HandleResponse(const ResponseInfo: TResponseInfo);
    procedure ApplyLoggerSettings;
  public
    function GetServiceController: TServiceController; override;
    procedure BadgerStart;
    procedure BadgerStop;
    property LogToConsole: Boolean read FLogToConsole write FLogToConsole;
  end;

var
  FBadgerService: TFBadgerService;

implementation

uses
  BadgerLogger, SampleRouteManager;

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  FBadgerService.Controller(CtrlCode);
end;

procedure TFBadgerService.ApplyLoggerSettings;
begin
  if FLogToConsole then
  begin
    Logger.isActive := True;
    Logger.LogToConsole := True;
    Logger.LogLevel := llInfo;
  end
  else
  begin
    Logger.isActive := False;
    Logger.LogToConsole := False;
  end;
end;

procedure TFBadgerService.HandleRequest(const RequestInfo: TRequestInfo);
begin
  WriteLn(Format('>> %s %s | %s', [RequestInfo.Method, RequestInfo.URI, RequestInfo.RemoteIP]));
end;

procedure TFBadgerService.HandleResponse(const ResponseInfo: TResponseInfo);
begin
  WriteLn(Format('<< %d %s', [ResponseInfo.StatusCode, ResponseInfo.StatusText]));
end;

procedure TFBadgerService.BadgerStart;
begin
  ApplyLoggerSettings;

  FServerThread := TBadger.Create;
  FServerThread.Port := 8080;
  FServerThread.Timeout := 10000;
  FServerThread.ParallelProcessing := True;
  FServerThread.MaxConcurrentConnections := 500;
  FServerThread.EnableEventInfo := FLogToConsole;
  FServerThread.CorsEnabled := False;

  if FLogToConsole then
  begin
    FServerThread.OnRequest := HandleRequest;
    FServerThread.OnResponse := HandleResponse;
  end;

  FServerThread.RouteManager
    .AddPost('/upload', TSampleRouteManager.upLoad)
    .AddGet('/download', TSampleRouteManager.downLoad)
    .AddGet('/rota1', TSampleRouteManager.rota1)
    .AddGet('/teste/ping', TSampleRouteManager.ping)
    .AddPost('/AtuImage', TSampleRouteManager.AtuImage);

  FServerThread.Start;
  Logger.Info(Format('Badger listening on port %d', [FServerThread.Port]));
end;

procedure TFBadgerService.BadgerStop;
begin
  if Assigned(FServerThread) then
  begin
    Logger.Info('Stopping Badger...');
    FServerThread.Stop;
    FreeAndNil(FServerThread);
  end;
end;

function TFBadgerService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TFBadgerService.ServiceDestroy(Sender: TObject);
begin
  BadgerStop;
end;

procedure TFBadgerService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  BadgerStart;
end;

procedure TFBadgerService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  BadgerStop;
end;

end.
