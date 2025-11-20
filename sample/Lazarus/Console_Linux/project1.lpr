program Project1;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, Badger, BadgerTypes, BadgerLogger, SampleRouteManager
  { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;

  private
     ServerThread: TBadger;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  Logger.isActive := False;
  Logger.LogToConsole := False;

  ServerThread := TBadger.Create;
  ServerThread.EnableEventInfo := False;
  ServerThread.Port := 8080;
  ServerThread.Timeout := 3000;

  ServerThread.RouteManager
  .AddGet('/teste/ping', TSampleRouteManager.ping) ;


    { ServerThread.ParallelProcessing:= True;
      ServerThread.MaxConcurrentConnections:= 30000;}

  ServerThread.Start;

  WriteLn('Badger start on port ' + IntToStr(ServerThread.Port));
  WriteLn('Precione Ctrl + c para terminar');
  ReadLn;

end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;


end;

destructor TMyApplication.Destroy;
begin
  ServerThread.Stop;
  ServerThread := nil;
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

