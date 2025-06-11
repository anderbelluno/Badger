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
    function AddMethod(const AVerb, ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddDel(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddGet(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddPatch(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddPost(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddPut(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;


    function Unregister(const Route: string): TRouteManager;
  end;

implementation

uses
  Windows;

{ TRouteManager }



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

function TRouteManager.AddDel(const ARoute: string;
  ACallback: TRoutingCallback): TRouteManager;
begin
  Result := AddMethod('DELETE', ARoute, ACallback);
end;

function TRouteManager.AddGet(const ARoute: string;
  ACallback: TRoutingCallback): TRouteManager;
begin
  Result := AddMethod('GET', ARoute, ACallback);
end;

function TRouteManager.AddMethod(const AVerb, ARoute: string;
  ACallback: TRoutingCallback): TRouteManager;
var
  LRoute: string;
{$IFnDEF Delphi2009Plus}
  LMethod: TMethod;
{$ENDIF}
begin
  Result := Self;
  LRoute := UpperCase(AVerb) + ' ' + LowerCase(ARoute);
{$IFDEF Delphi2009Plus}
  if Assigned(ACallback) then
    FRoutes.AddOrSetValue(LRoute, ACallback);
{$ELSE}
  LMethod.Data := Self;
  LMethod.Code := @ACallback;
  FRoutes.AddObject(LRoute, TObject(LMethod.Code));
{$ENDIF}
end;

function TRouteManager.AddPatch(const ARoute: string;
  ACallback: TRoutingCallback): TRouteManager;
begin
  Result := AddMethod('PATCH', ARoute, ACallback);
end;

function TRouteManager.AddPost(const ARoute: string;
  ACallback: TRoutingCallback): TRouteManager;
begin
  Result := AddMethod('POST', ARoute, ACallback);
end;

function TRouteManager.AddPut(const ARoute: string;
  ACallback: TRoutingCallback): TRouteManager;
begin
  Result := AddMethod('PUT', ARoute, ACallback);
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
