unit Badger;

interface

uses
  blcksock, SyncObjs, Classes, SysUtils, BadgerRouteManager, BadgerMethods, BadgerTypes;

type
  TBadger = class(TThread)
  private
    VLastRequest: TLastRequest;
    VLastResponse: TLastResponse;
    FServerSocket: TTCPBlockSocket;
    FRouteManager: TRouteManager;
    FCriticalSection: TCriticalSection;
    FMethods: TBadgerMethods;
    FMiddlewares: TList;
    FPort: Integer;
    FNonBlockMode: Boolean;
    FTimeout: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddMiddleware(Middleware: TMiddlewareProc);
    procedure Start;
    procedure Stop;

    property NonBlockMode: Boolean read FNonBlockMode write FNonBlockMode;
    property Port: Integer read FPort write FPort;
    property RouteManager: TRouteManager read FRouteManager;
    property Timeout: Integer read FTimeout write FTimeout default 5000;

    // Events
    property OnLastRequest: TLastRequest read VLastRequest write VLastRequest;
    property OnLastResponse: TLastResponse read VLastResponse write VLastResponse;
  end;

implementation

uses
  BadgerRequestHandler;

{ TBadger }

constructor TBadger.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FCriticalSection := TCriticalSection.Create;
  FServerSocket := TTCPBlockSocket.Create;
  FRouteManager := TRouteManager.Create;
  FMethods := TBadgerMethods.Create;
  FMiddlewares := TList.Create;
  FPort := 8080;
  FNonBlockMode := True;
  FTimeout := 5000;
end;

destructor TBadger.Destroy;
var
  I: Integer;
begin
  Stop;
  FServerSocket.Free;
  FCriticalSection.Free;
  FRouteManager.Free;
  FMethods.Free;
  for I := 0 to FMiddlewares.Count - 1 do
    TObject(FMiddlewares[I]).Free;
  FMiddlewares.Free;
  inherited;
end;

procedure TBadger.AddMiddleware(Middleware: TMiddlewareProc);
begin
  FMiddlewares.Add(TMiddlewareWrapper.Create(Middleware));
end;

procedure TBadger.Start;
begin
  if not Terminated and not Suspended then
    Exit;
  FServerSocket.CloseSocket;
  FServerSocket.CreateSocket;
  FServerSocket.setLinger(True, 10000);
  FServerSocket.NonBlockMode := FNonBlockMode;
  FServerSocket.Bind('0.0.0.0', IntToStr(FPort));
  FServerSocket.Listen;
  Resume;
end;

procedure TBadger.Stop;
begin
  if Terminated or Suspended then
    Exit;
  Terminate;
  FServerSocket.CloseSocket;
  Sleep(100);
end;

procedure TBadger.Execute;
var
  ClientSocket: TTCPBlockSocket;
begin
  while not Terminated do
  begin
    if FServerSocket.CanRead(1000) then
    begin
      ClientSocket := TTCPBlockSocket.Create;
      try
        ClientSocket.Socket := FServerSocket.Accept;
        if ClientSocket.LastError = 0 then
        begin
          THTTPRequestHandler.Create(ClientSocket, FRouteManager, FCriticalSection,
                                     FMethods, FMiddlewares, FTimeout,
                                     VLastRequest, VLastResponse);
        end
        else
        begin
          if Assigned(VLastResponse) then
            VLastResponse('Error accepting connection: ' + ClientSocket.LastErrorDesc);
          ClientSocket.Free;
        end;
      except
        ClientSocket.Free;
        if Assigned(VLastResponse) then
          VLastResponse('Exception in accept: ' + Exception(ExceptObject).Message);
      end;
    end;
  end;
  FServerSocket.CloseSocket;
end;

end.
