unit BadgerRouteManager;

{$I BadgerDefines.inc}

interface

uses
  Classes,
  SysUtils,
  BadgerMethods,
  BadgerTypes,
  BadgerHttpStatus,
  Contnrs;

type
  TRouteEntry = class
    Verb: string;
    Pattern: string;
    Callback: TRoutingCallback;
    ParamNames: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  TRouteManager = class(TObject)
  private
    FRoutes: TObjectList;
    function SplitString(const S, Delim: string): TStringList;

  public
    const
    CDEL = 'DELETE';
    CGET = 'GET';
    CPATCH = 'PATCH';
    CPOST = 'POST';
    CPUT = 'PUT';
    
    constructor Create;
    destructor Destroy; override;
    function AddMethod(const AVerb, ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddDel(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddGet(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddPatch(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddPost(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddPut(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;

    function Unregister(const Route: string): TRouteManager;
    function MatchRoute(const AVerb, APath: string; out Entry: TRouteEntry; out Params: TStringList): Boolean;
  end;

const
  CGET   = 'GET';
  CPOST  = 'POST';
  CPUT   = 'PUT';
  CPATCH = 'PATCH';
  CDEL   = 'DELETE';

implementation

{ TRouteEntry }

constructor TRouteEntry.Create;
begin
  inherited Create;
  ParamNames := TStringList.Create;
end;

destructor TRouteEntry.Destroy;
begin
  FreeAndNil(ParamNames);
  inherited;
end;

{ TRouteManager }

function TRouteManager.Unregister(const Route: string): TRouteManager;
var
  I: Integer;
  Entry: TRouteEntry;
  FullRoute: string;
begin
  Result := Self;
  for I := FRoutes.Count - 1 downto 0 do
  begin
    Entry := TRouteEntry(FRoutes[I]);
    FullRoute := Entry.Verb + ' ' + Entry.Pattern;
    if SameText(FullRoute, Route) then
    begin
      FRoutes.Delete(I);
      Break;
    end;
  end;
end;

function TRouteManager.AddDel(const ARoute: string;
  ACallback: TRoutingCallback): TRouteManager;
begin
  Result := AddMethod(CDEL, ARoute, ACallback);
end;

function TRouteManager.AddGet(const ARoute: string;
  ACallback: TRoutingCallback): TRouteManager;
begin
  Result := AddMethod(CGET, ARoute, ACallback);
end;

function TRouteManager.AddMethod(const AVerb, ARoute: string; ACallback: TRoutingCallback): TRouteManager;
var
  Entry: TRouteEntry;
  CleanRoute: string;
begin
  Result := Self;
  CleanRoute := StringReplace(ARoute, '//', '/', [rfReplaceAll]);
  if Copy(CleanRoute, 1, 1) <> '/' then
    CleanRoute := '/' + CleanRoute;
  if (Length(CleanRoute) > 1) and (CleanRoute[Length(CleanRoute)] = '/') then
    SetLength(CleanRoute, Length(CleanRoute) - 1);

  Entry := TRouteEntry.Create;
  Entry.Verb := UpperCase(AVerb);
  Entry.Pattern := LowerCase(CleanRoute);
  Entry.Callback := ACallback;
  FRoutes.Add(Entry);
end;

function TRouteManager.AddPatch(const ARoute: string;
  ACallback: TRoutingCallback): TRouteManager;
begin
  Result := AddMethod(CPATCH, ARoute, ACallback);
end;

function TRouteManager.AddPost(const ARoute: string;
  ACallback: TRoutingCallback): TRouteManager;
begin
  Result := AddMethod(CPOST, ARoute, ACallback);
end;

function TRouteManager.AddPut(const ARoute: string;
  ACallback: TRoutingCallback): TRouteManager;
begin
  Result := AddMethod(CPUT, ARoute, ACallback);
end;

constructor TRouteManager.Create;
begin
  inherited Create;
  FRoutes := TObjectList.Create(True);
end;

destructor TRouteManager.Destroy;
begin
  FreeAndNil(FRoutes);
  inherited;
end;

function TRouteManager.SplitString(const S, Delim: string): TStringList;
var
  P: Integer;
  Part: string;
begin
  Result := TStringList.Create;
  Part := S;
  while Part <> '' do
  begin
    P := Pos(Delim, Part);
    if P > 0 then
    begin
      Result.Add(Copy(Part, 1, P - 1));
      Delete(Part, 1, P);
    end
    else
    begin
      Result.Add(Part);
      Break;
    end;
  end;
end;

function TRouteManager.MatchRoute(const AVerb, APath: string; out Entry: TRouteEntry; out Params: TStringList): Boolean;
var
  I, J: Integer;
  PatternParts, PathParts: TStringList;
  Part, ParamName: string;
begin
  Result := False;
  Params.Clear;
  Entry := nil;

  for I := 0 to FRoutes.Count - 1 do
  begin
    Entry := TRouteEntry(FRoutes[I]);
    if Entry.Verb <> AVerb then Continue;

    PatternParts := SplitString(Entry.Pattern, '/');
    PathParts := SplitString(APath, '/');
    try
      if PatternParts.Count <> PathParts.Count then
        Continue;

      Params.Clear;
      Result := True;
      for J := 0 to PatternParts.Count - 1 do
      begin
        Part := PatternParts[J];
        if (Part = '') and (J = 0) then
          Continue;

        if Copy(Part, 1, 1) = ':' then
        begin
          ParamName := Copy(Part, 2, MaxInt);
          Params.Add(ParamName + '=' + PathParts[J]);
        end
        else if Part <> PathParts[J] then
        begin
          Result := False;
          Break;
        end;
      end;

      if Result then
        Exit;
    finally
      PatternParts.Free;
      PathParts.Free;
    end;
  end;
end;

end.
