unit BadgerRouteManager;

interface

uses
  Classes,
  SysUtils,

{$IF CompilerVersion >= 20} // Delphi 2009+
  System.Generics.Collections,
{$IFEND}

  BadgerMethods,
  BadgerTypes,
  BadgerHttpStatus;

type
  TRouteManager = class(TObject)
  private
  public
    FRoutes: {$IF CompilerVersion >= 20}TDictionary<string, TObject>{$ELSE}TStringList{$IFEND};
    constructor Create;
    destructor Destroy; override;
    {$IF CompilerVersion >= 20}
      function &Add(const Route: string; Callback: TRoutingCallback): TRouteManager;
    {$ELSE}
      function Add(const Route: string; Callback: TRoutingCallback): TRouteManager;
    {$IFEND}
    function Unregister(const Route: string): TRouteManager;
  end;

implementation

{ TRouteManager }
{$IF CompilerVersion >= 20}
  function TRouteManager.&Add(const Route: string; Callback: TRoutingCallback): TRouteManager;
{$ELSE}
  function TRouteManager.Add(const Route: string; Callback: TRoutingCallback): TRouteManager;
{$IFEND}
var
  Method: TMethod;
begin
  Result := Self;
  Method.Data := Self;
  Method.Code := @Callback;
{$IF CompilerVersion >= 20}
  FRoutes.AddOrSetValue(Route.ToLower, TObject(Method.Code));
{$ELSE}
  FRoutes.AddObject(Route, TObject(Method.Code));
{$IFEND}
end;

function TRouteManager.Unregister(const Route: string): TRouteManager;
{$IF CompilerVersion < 20}
var
  LIndex: Integer;
{$IFEND}
begin
  Result := Self;
{$IF CompilerVersion >= 20}
  if FRoutes.ContainsKey(Route.ToLower) then
    FRoutes.Remove(Route.ToLower);
{$ELSE}
  LIndex := FRoutes.IndexOf( LowerCase(Route) );
  if LIndex <> -1 then
    FRoutes.Delete(LIndex);
{$IFEND}
end;

constructor TRouteManager.Create;
begin
  FRoutes := {$IF CompilerVersion >= 20}TDictionary<string, TObject>.Create{$ELSE}TStringList.Create{$IFEND};
end;

destructor TRouteManager.Destroy;
begin
  FRoutes.Free;
  inherited;
end;

end.
