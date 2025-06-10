unit BadgerRouteManager;

{$I BadgerDefines.inc}

interface

uses
  Classes,
  SysUtils,
{$IFDEF Delphi2009Plus}
  System.Generics.Collections,
{$ENDIF}
  BadgerMethods,
  BadgerTypes,
  BadgerHttpStatus;

type
  TRouteManager = class(TObject)
  private
  public
    FRoutes:
{$IFDEF Delphi2009Plus}
    TDictionary<string, TRoutingCallback>
{$ELSE}
    TStringList
{$ENDIF};
    constructor Create;
    destructor Destroy; override;
{$IFDEF Delphi2009Plus}
      function &Add(const Route: string; Callback: TRoutingCallback): TRouteManager;
{$ELSE}
      function Add(const Route: string; Callback: TRoutingCallback): TRouteManager;
{$ENDIF}

    function Unregister(const Route: string): TRouteManager;
  end;

implementation

uses
  Windows;

{ TRouteManager }

{$IFDEF Delphi2009Plus}
function TRouteManager.&Add(const Route: string; Callback: TRoutingCallback): TRouteManager;
begin
  Result := Self;
  if Assigned(Callback) then
    FRoutes.AddOrSetValue(Route.ToLower, Callback);
end;
{$ELSE}
function TRouteManager.Add(const Route: string; Callback: TRoutingCallback): TRouteManager;
var
  Method: TMethod;
begin
  Result := Self;
  Method.Data := Self;
  Method.Code := @Callback;
  FRoutes.AddObject(Route, TObject(Method.Code));
end;
{$ENDIF}

function TRouteManager.Unregister(const Route: string): TRouteManager;
var
  LIndex: Integer;
begin
  Result := Self;
{$IFDEF Delphi2009Plus}
  if FRoutes.ContainsKey(Route.ToLower) then
    FRoutes.Remove(Route.ToLower);
{$ELSE}
  LIndex := FRoutes.IndexOf( LowerCase(Route) );
  if LIndex <> -1 then
    FRoutes.Delete(LIndex);
{$ENDIF}
end;

constructor TRouteManager.Create;
begin
  FRoutes :=
{$IFDEF Delphi2009Plus}
   TDictionary<string, TRoutingCallback>.Create
{$ELSE}
   TStringList.Create
{$ENDIF}
  ;

  OutputDebugString(PChar('TRouteManager created'));
end;

destructor TRouteManager.Destroy;
begin
  try
    if Assigned(FRoutes) then
    begin
      FreeAndNil(FRoutes);
      OutputDebugString(PChar('FRoutes freed'));
    end;
  except
    on E: Exception do
      OutputDebugString(PChar(Format('Error freeing FRoutes: %s', [E.Message])));
  end;
  OutputDebugString(PChar('TRouteManager destroyed'));
  inherited;
end;

end.
