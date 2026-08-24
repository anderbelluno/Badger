unit Unit2;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, ZConnection, ZDataset;

type
  { TDataModule1 — holds the Zeos *template* connection (never used by workers).
    BadgerDBPool clones this component internally. }

  TDataModule1 = class(TDataModule)
    ZConnection1: TZConnection;
    ZQuery1: TZQuery;
  private
  public
    procedure ApplyDefaults;
    procedure ApplySettings(const AHost, ADatabase, AUser, APassword, AProtocol: string;
      APort: Integer);
    function TestTemplateConnection(out AMsg: string): Boolean;
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.ApplyDefaults;
begin
  ApplySettings('127.0.0.1', 'badger_pool', 'postgres', 'postgres', 'postgresql', 5432);
end;

procedure TDataModule1.ApplySettings(const AHost, ADatabase, AUser, APassword,
  AProtocol: string; APort: Integer);
begin
  if ZConnection1.Connected then
    ZConnection1.Connected := False;

  ZConnection1.Protocol := AProtocol;
  ZConnection1.HostName := AHost;
  ZConnection1.Port := APort;
  ZConnection1.Database := ADatabase;
  ZConnection1.User := AUser;
  ZConnection1.Password := APassword;
  ZConnection1.LoginPrompt := False;
  { Zeos / libpq: allow enough sockets for pool + extras under stress. }
  ZConnection1.Properties.Values['controls_cp'] := 'CP_UTF8';
end;

function TDataModule1.TestTemplateConnection(out AMsg: string): Boolean;
begin
  Result := False;
  AMsg := '';
  try
    if ZConnection1.Connected then
      ZConnection1.Connected := False;
    ZConnection1.Connected := True;
    AMsg := Format('OK %s://%s:%d/%s (template connected)',
      [ZConnection1.Protocol, ZConnection1.HostName, ZConnection1.Port, ZConnection1.Database]);
    Result := True;
  except
    on E: Exception do
      AMsg := 'FAIL: ' + E.Message;
  end;
end;

end.
