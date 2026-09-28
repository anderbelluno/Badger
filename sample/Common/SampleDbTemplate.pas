unit SampleDbTemplate;

{ Creates a Zeos TZConnection template for BadgerDBPool (no DataModule). }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, ZConnection;

procedure ApplySamplePgSettings(AConn: TZConnection; const AHost, ADatabase, AUser,
  APassword, AProtocol: string; APort: Integer);
function CreateSamplePgTemplate: TZConnection;
function TestSamplePgTemplate(AConn: TZConnection; out AMsg: string): Boolean;

implementation

procedure ApplySamplePgSettings(AConn: TZConnection; const AHost, ADatabase, AUser,
  APassword, AProtocol: string; APort: Integer);
begin
  if AConn.Connected then
    AConn.Connected := False;
  AConn.Protocol := AProtocol;
  AConn.HostName := AHost;
  AConn.Port := APort;
  AConn.Database := ADatabase;
  AConn.User := AUser;
  AConn.Password := APassword;
  AConn.LoginPrompt := False;
  AConn.Properties.Values['controls_cp'] := 'CP_UTF8';
end;

function CreateSamplePgTemplate: TZConnection;
begin
  Result := TZConnection.Create(nil);
  ApplySamplePgSettings(Result, '127.0.0.1', 'badger_pool', 'postgres', 'postgres',
    'postgresql', 5432);
end;

function TestSamplePgTemplate(AConn: TZConnection; out AMsg: string): Boolean;
begin
  Result := False;
  AMsg := '';
  try
    if AConn.Connected then
      AConn.Connected := False;
    AConn.Connected := True;
    AMsg := Format('OK %s://%s:%d/%s',
      [AConn.Protocol, AConn.HostName, AConn.Port, AConn.Database]);
    Result := True;
  except
    on E: Exception do
      AMsg := 'FAIL: ' + E.Message;
  end;
end;

end.
