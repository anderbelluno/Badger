unit BadgerRouteManager;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
 {$IFNDEF FPC}
    {$IF CompilerVersion >= 20} // Delphi 2009+
      System.Generics.Collections,
    {$IFEND}
 {$ENDIF}
  BadgerMethods,
  BadgerTypes,
  BadgerHttpStatus;

type
  TRouteManager = class(TObject)
  private
  public
    FRoutes:
            {$IFNDEF FPC}
             {$IF CompilerVersion >= 20}
                TDictionary<string, TObject>
             {$ELSE}
                TStringList
             {$IFEND}
            {$ELSE}
                TStringList
            {$ENDIF};
    constructor Create;
    destructor Destroy; override;
   {$IFNDEF FPC}
    {$IF CompilerVersion >= 20}
      function &Add(const Route: string; Callback: TRoutingCallback): TRouteManager;
    {$ELSE}
      function Add(const Route: string; Callback: TRoutingCallback): TRouteManager;
    {$IFEND}
   {$ELSE}
      function Add(const Route: string; Callback: TRoutingCallback): TRouteManager;
    {$ENDIF}

    function Unregister(const Route: string): TRouteManager;
  end;

implementation

{ TRouteManager }

{$IFNDEF FPC}
  {$IF CompilerVersion >= 20}
     function TRouteManager.&Add(const Route: string; Callback: TRoutingCallback): TRouteManager;
  {$ELSE}
     function TRouteManager.Add(const Route: string; Callback: TRoutingCallback): TRouteManager;
  {$IFEND}
{$ELSE}
  function TRouteManager.Add(const Route: string; Callback: TRoutingCallback): TRouteManager;
{$ENDIF}
var
  Method: TMethod;
begin
  Result := Self;
  Method.Data := Self;
  Method.Code := @Callback;
  {$IFNDEF FPC}
    {$IF CompilerVersion >= 20}
      FRoutes.AddOrSetValue(Route.ToLower, TObject(Method.Code));
    {$ELSE}
      FRoutes.AddObject(Route, TObject(Method.Code));
    {$IFEND}
  {$ELSE}
      FRoutes.AddObject(Route, TObject(Method.Code));
    {$ENDIF}
end;

function TRouteManager.Unregister(const Route: string): TRouteManager;
var
  LIndex: Integer;
begin
  Result := Self;
  {$IFNDEF FPC}
    {$IF CompilerVersion >= 20}
      if FRoutes.ContainsKey(Route.ToLower) then
        FRoutes.Remove(Route.ToLower);
    {$ELSE}
      LIndex := FRoutes.IndexOf( LowerCase(Route) );
      if LIndex <> -1 then
        FRoutes.Delete(LIndex);
    {$IFEND}
  {$ELSE}
      LIndex := FRoutes.IndexOf( LowerCase(Route) );
      if LIndex <> -1 then
        FRoutes.Delete(LIndex);
    {$ENDIF}
end;

constructor TRouteManager.Create;
begin
  FRoutes :=
          {$IFNDEF FPC}
            {$IF CompilerVersion >= 20}
               TDictionary<string, TObject>.Create
            {$ELSE}
               TStringList.Create
            {$IFEND}
          {$ELSE}
               TStringList.Create
            {$ENDIF}
            ;
end;

destructor TRouteManager.Destroy;
begin
  FRoutes.Free;
  inherited;
end;

end.
