unit BadgerRouteManager;

interface

uses
  Classes,
  SysUtils,

{$IF CompilerVersion >= 20} // Delphi 2009+
  System.Generics.Collections,
{$ENDIF}

  BadgerMethods,
  BadgerTypes,
  BadgerHttpStatus;

type
  TRouteManager = class(TObject)
  private
  public
    FRoutes: {$IF CompilerVersion >= 20}TDictionary<string, TObject>{$ELSE}TStringList{$ENDIF};
    constructor Create;
    destructor Destroy; override;

    function &Register(const Route: string; Callback: TRoutingCallback): TRouteManager;
    function Unregister(const Route: string): TRouteManager;
  end;

implementation

{ TRouteManager }

function TRouteManager.&Register(const Route: string; Callback: TRoutingCallback): TRouteManager;
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
{$ENDIF}
end;

function TRouteManager.Unregister(const Route: string): TRouteManager;
{$IF CompilerVersion < 20}
var
  LIndex: Integer;
{$ENDIF}
begin
  Result := Self;
{$IF CompilerVersion >= 20}
  if FRoutes.ContainsKey(Route.ToLower) then
    FRoutes.Remove(Route.ToLower);
{$ELSE}
  LIndex := FRoutes.IndexOf(Route.ToLower);
  if LIndex <> -1 then
    FRoutes..Delete(LIndex);
{$ENDIF}
end;

constructor TRouteManager.Create;
begin
  FRoutes := {$IF CompilerVersion >= 20}TDictionary<string, TObject>.Create{$ELSE}TStringList.Create{$ENDIF};
end;

destructor TRouteManager.Destroy;
begin
  FRoutes.Free;
  inherited;
end;

end.
