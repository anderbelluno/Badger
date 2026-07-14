unit ConnPoolRoutes;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

{ HTTP routes that exercise BadgerDBPool via AcquireConn / ReleaseConn. }

interface

uses
  Classes, SysUtils, BadgerTypes;

type
  TConnPoolRoutes = class
  public
    class procedure Ping(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure DbPing(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure DbWork(Request: THTTPRequest; var Response: THTTPResponse);
    class procedure DbStats(Request: THTTPRequest; var Response: THTTPResponse);
  end;

implementation

uses
  ZConnection, ZDataset, BadgerDBPool, BadgerHttpStatus;

function QueryInt(AConn: TZConnection; const ASQL: string): Integer;
var
  Q: TZQuery;
begin
  Q := TZQuery.Create(nil);
  try
    Q.Connection := AConn;
    Q.SQL.Text := ASQL;
    Q.Open;
    if Q.IsEmpty then
      Result := 0
    else
      Result := Q.Fields[0].AsInteger;
  finally
    Q.Free;
  end;
end;

function QueryStr(AConn: TZConnection; const ASQL: string): string;
var
  Q: TZQuery;
begin
  Q := TZQuery.Create(nil);
  try
    Q.Connection := AConn;
    Q.SQL.Text := ASQL;
    Q.Open;
    if Q.IsEmpty then
      Result := ''
    else
      Result := Q.Fields[0].AsString;
  finally
    Q.Free;
  end;
end;

class procedure TConnPoolRoutes.Ping(Request: THTTPRequest; var Response: THTTPResponse);
begin
  Response.StatusCode := HTTP_OK;
  Response.ContentType := 'application/json';
  Response.Body := '{"ok":true,"service":"ConnPool"}';
end;

class procedure TConnPoolRoutes.DbPing(Request: THTTPRequest; var Response: THTTPResponse);
var
  Conn: TZConnection;
  Pid: Integer;
begin
  Conn := TZConnection(AcquireConn(Request));
  try
    Pid := QueryInt(Conn, 'SELECT pg_backend_pid()');
    Response.StatusCode := HTTP_OK;
    Response.ContentType := 'application/json';
    Response.Body := Format('{"ok":true,"backend_pid":%d}', [Pid]);
  finally
    ReleaseConn(Request);
  end;
end;

class procedure TConnPoolRoutes.DbWork(Request: THTTPRequest; var Response: THTTPResponse);
var
  Conn: TZConnection;
  Ms, JobId, Pid, Hits: Integer;
  Q: TZQuery;
begin
  Ms := StrToIntDef(Request.QueryParams.Values['ms'], 50);
  if Ms < 0 then Ms := 0;
  if Ms > 5000 then Ms := 5000;

  Conn := TZConnection(AcquireConn(Request));
  try
    Pid := QueryInt(Conn, 'SELECT pg_backend_pid()');
    JobId := QueryInt(Conn, 'SELECT id FROM jobs ORDER BY id LIMIT 1');

    Q := TZQuery.Create(nil);
    try
      Q.Connection := Conn;
      if JobId > 0 then
      begin
        Q.SQL.Text :=
          'INSERT INTO job_hits(job_id, worker_name, backend_pid) ' +
          'VALUES (:jid, :wn, :pid)';
        Q.ParamByName('jid').AsInteger := JobId;
        Q.ParamByName('wn').AsString := Format('http-%d', [Pid]);
        Q.ParamByName('pid').AsInteger := Pid;
        Q.ExecSQL;
      end;

      { Hold the pooled connection briefly to force concurrency pressure. }
      if Ms > 0 then
      begin
        Q.SQL.Text := Format('SELECT pg_sleep(%s)',
          [StringReplace(FormatFloat('0.###', Ms / 1000.0), ',', '.', [rfReplaceAll])]);
        Q.Open;
        Q.Close;
      end;

      Hits := QueryInt(Conn, 'SELECT COUNT(*) FROM job_hits');
    finally
      Q.Free;
    end;

    Response.StatusCode := HTTP_OK;
    Response.ContentType := 'application/json';
    Response.Body := Format(
      '{"ok":true,"backend_pid":%d,"job_id":%d,"ms":%d,"hits":%d}',
      [Pid, JobId, Ms, Hits]);
  finally
    ReleaseConn(Request);
  end;
end;

class procedure TConnPoolRoutes.DbStats(Request: THTTPRequest; var Response: THTTPResponse);
var
  Conn: TZConnection;
  Active, Hits, Jobs, Pid: Integer;
  DbName: string;
begin
  Conn := TZConnection(AcquireConn(Request));
  try
    Pid := QueryInt(Conn, 'SELECT pg_backend_pid()');
    DbName := QueryStr(Conn, 'SELECT current_database()');
    Active := QueryInt(Conn,
      'SELECT COUNT(*) FROM pg_stat_activity WHERE datname = current_database()');
    Jobs := QueryInt(Conn, 'SELECT COUNT(*) FROM jobs');
    Hits := QueryInt(Conn, 'SELECT COUNT(*) FROM job_hits');

    Response.StatusCode := HTTP_OK;
    Response.ContentType := 'application/json';
    Response.Body := Format(
      '{"ok":true,"database":"%s","backend_pid":%d,"pg_backends":%d,"jobs":%d,"hits":%d}',
      [DbName, Pid, Active, Jobs, Hits]);
  finally
    ReleaseConn(Request);
  end;
end;

end.
