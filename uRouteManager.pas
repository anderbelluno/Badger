unit uRouteManager;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
 Classes, SysUtils, SynaMethods, blcksock;

 type
   TRoutingCallback = procedure(ClientSocket: TTCPBlockSocket; const URI, Method, RequestLine: string) of object;

 TRouteManager = class(TObject)
 private

    procedure RegisterRoute(const Route: string; Callback: TRoutingCallback);
 public
    FRoutes: TStringList;
    constructor Create;
    destructor Destroy; override;

    procedure upLoad(ClientSocket: TTCPBlockSocket; const URI, Method, RequestLine: string);
    procedure downLoad(ClientSocket: TTCPBlockSocket; const URI, Method, RequestLine: string);
    procedure rota1(ClientSocket: TTCPBlockSocket; const URI, Method, RequestLine: string);
    procedure ping(ClientSocket: TTCPBlockSocket; const URI, Method, RequestLine: string);
    procedure AtuImage (ClientSocket: TTCPBlockSocket; const URI, Method, RequestLine: string);
 end;

implementation

{ TRouteManager }

procedure TRouteManager.RegisterRoute(const Route: string;
  Callback: TRoutingCallback);
var
  Method: TMethod;
begin
  Method.Data := Self;
  Method.Code := @Callback;
  FRoutes.AddObject(Route, TObject(Method.Code));
end;

constructor TRouteManager.Create;
begin
  FRoutes := TStringList.Create;
  RegisterRoute('/upload',   upLoad);
  RegisterRoute('/download', download);
  RegisterRoute('/rota1',    rota1);
  RegisterRoute('/ping', ping);
  RegisterRoute('/AtuImage', AtuImage);
end;

destructor TRouteManager.Destroy;
begin
  FRoutes.Free;
  inherited;
end;

procedure TRouteManager.downLoad(ClientSocket: TTCPBlockSocket; const URI,
  Method, RequestLine: string);
var
 SynClasses : TSynaMethods;
begin
  SynClasses := TSynaMethods.Create;
  try
    SynClasses.fDownloadStream(ClientSocket,URI, Method, RequestLine);
  finally
    FreeAndNil(SynClasses);
  end;
end;



procedure TRouteManager.rota1(ClientSocket: TTCPBlockSocket; const URI,
  Method, RequestLine: string);
var
 SynClasses : TSynaMethods;
begin
  SynClasses := TSynaMethods.Create;
  try
    ClientSocket.SendString('HTTP/1.1 200 ' + CRLF + 'Content-Type: text/plain' + CRLF + CRLF + UTF8Encode( 'Rota1 executada') );
  finally
    FreeAndNil(SynClasses);
  end;
end;

procedure TRouteManager.upLoad(ClientSocket: TTCPBlockSocket; const URI,
  Method, RequestLine: string);
var
 SynClasses : TSynaMethods;
begin
  SynClasses := TSynaMethods.Create;
  try
    ClientSocket.SendString('HTTP/1.1 200 OK' + CRLF + 'Content-Type: application/json' + CRLF + CRLF + SynClasses.fParserJsonStream( ClientSocket,URI, Method, RequestLine ) )
  finally
    FreeAndNil(SynClasses);
  end;
end;

procedure TRouteManager.ping(ClientSocket: TTCPBlockSocket; const URI,
  Method, RequestLine: string);
begin
   ClientSocket.SendString('HTTP/1.1 200 ' + CRLF + 'Content-Type: text/plain' + CRLF + CRLF + UTF8Encode( 'Pong') );
end;

procedure TRouteManager.AtuImage(ClientSocket: TTCPBlockSocket; const URI,
  Method, RequestLine: string);
var
 SynClasses : TSynaMethods;
begin
  SynClasses := TSynaMethods.Create;
  try
    SynClasses.AtuImage(ClientSocket, URI, Method, RequestLine);
  finally
    FreeAndNil(SynClasses);
  end;
end;

end.
