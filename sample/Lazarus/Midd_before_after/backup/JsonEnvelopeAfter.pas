unit JsonEnvelopeAfter;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

{ After-middleware with route list — same RegisterProtectedRoutes shape as TBasicAuth. }

interface

uses
  Classes, SysUtils, Badger, BadgerTypes, superobject;

type
  TJsonEnvelopeAfter = class
  private
    FProtectedRoutes: TStringList;
    function IsProtectedRoute(const AURI: string): Boolean;
    procedure Middleware(var Request: THTTPRequest; var Response: THTTPResponse);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterProtectedRoutes(Badger: TBadger; const ProtectedRoutes: array of string);
  end;

implementation

function NormalizeRoute(const ARoute: string): string;
begin
  Result := Trim(ARoute);
  while (Length(Result) > 1) and (Result[Length(Result)] = '/') do
    Delete(Result, Length(Result), 1);
end;

function RouteMatches(const ARequestURI, AProtectedRoute: string): Boolean;
var
  RequestURI, ProtectedRoute: string;
begin
  RequestURI := NormalizeRoute(ARequestURI);
  ProtectedRoute := NormalizeRoute(AProtectedRoute);

  if ProtectedRoute = '' then
  begin
    Result := False;
    Exit;
  end;

  if ProtectedRoute = '/' then
  begin
    Result := True;
    Exit;
  end;

  if SameText(RequestURI, ProtectedRoute) then
  begin
    Result := True;
    Exit;
  end;

  Result :=
    (Length(RequestURI) > Length(ProtectedRoute)) and
    (CompareText(Copy(RequestURI, 1, Length(ProtectedRoute)), ProtectedRoute) = 0) and
    (RequestURI[Length(ProtectedRoute) + 1] = '/');
end;

constructor TJsonEnvelopeAfter.Create;
begin
  inherited Create;
  FProtectedRoutes := TStringList.Create;
  FProtectedRoutes.CaseSensitive := False;
end;

destructor TJsonEnvelopeAfter.Destroy;
begin
  FreeAndNil(FProtectedRoutes);
  inherited Destroy;
end;

function TJsonEnvelopeAfter.IsProtectedRoute(const AURI: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FProtectedRoutes.Count - 1 do
    if RouteMatches(AURI, FProtectedRoutes[I]) then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TJsonEnvelopeAfter.RegisterProtectedRoutes(Badger: TBadger;
  const ProtectedRoutes: array of string);
var
  I: Integer;
begin
  if not Assigned(Badger) then
    raise Exception.Create('TJsonEnvelopeAfter.RegisterProtectedRoutes: Badger is nil');

  FProtectedRoutes.Clear;
  for I := Low(ProtectedRoutes) to High(ProtectedRoutes) do
    FProtectedRoutes.Add(ProtectedRoutes[I]);

  Badger.AddAfterMiddleware(Middleware);
end;

procedure TJsonEnvelopeAfter.Middleware(var Request: THTTPRequest;
  var Response: THTTPResponse);
var
  Root, Meta, Data: ISuperObject;
begin
  if not IsProtectedRoute(Request.URI) then
    Exit;

  Root := SO();
  Data := nil;
  if Trim(Response.Body) <> '' then
  begin
    try
      Data := SO(Response.Body);
    except
      Data := nil;
    end;
  end;
  if Assigned(Data) then
    Root.O['data'] := Data
  else
    Root.S['data'] := Response.Body;

  Meta := SO();
  Meta.S['method'] := Request.Method;
  Meta.S['uri'] := Request.URI;
  Meta.I['status'] := Response.StatusCode;
  Root.O['meta'] := Meta;

  Response.ContentType := 'application/json';
  Response.Body := Root.AsJSON;
end;

end.
