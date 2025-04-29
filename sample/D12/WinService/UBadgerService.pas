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
  FServerThread := TBadger.Create;
  FServerThread.Port := 8080;
  FServerThread.Timeout := 10000;

  FServerThread.RouteManager
    .&Add('/upload', TSampleRouteManager.upLoad)
    .&Add('/download', TSampleRouteManager.downLoad)
    .&Add('/rota1', TSampleRouteManager.rota1)
    .&Add('/ping', TSampleRouteManager.ping)
    .&Add('/AtuImage', TSampleRouteManager.AtuImage);

  FServerThread.Start;
end;

procedure TFBadgerService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  FServerThread.Stop;
end;

end.
