unit UBadgerService;

interface

uses
  Badger, // Added on Project-Search Path

  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Classes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.SvcMgr,
  Vcl.Dialogs
  ;

type
  TFBadgerService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceDestroy(Sender: TObject);
  private
    FServerThread: TBadger;
    { Private declarations }
  public
    function GetServiceController: TServiceController; override;

    procedure BadgerStart;
    procedure BadgerStop;
    { Public declarations }
  end;

var
  FBadgerService: TFBadgerService;

implementation

uses
  SampleRouteManager;

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  FBadgerService.Controller(CtrlCode);
end;

procedure TFBadgerService.BadgerStart;
begin
  FServerThread := TBadger.Create;
  FServerThread.Port := 8080;
  FServerThread.Timeout := 10000;

  FServerThread.RouteManager
    .AddPost('/upload', TSampleRouteManager.upLoad)
    .AddGet('/download', TSampleRouteManager.downLoad)
    .AddGet('/rota1', TSampleRouteManager.rota1)
    .AddGet('/ping', TSampleRouteManager.ping)
    .AddPatch('/AtuImage', TSampleRouteManager.AtuImage);

  FServerThread.Start;
end;

procedure TFBadgerService.BadgerStop;
begin
  FServerThread.Stop;
end;

function TFBadgerService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TFBadgerService.ServiceDestroy(Sender: TObject);
begin
  FServerThread.Free;
end;

procedure TFBadgerService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  BadgerStart
end;

procedure TFBadgerService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  BadgerStop;
end;

end.
