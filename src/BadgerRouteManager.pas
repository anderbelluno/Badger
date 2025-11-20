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
    FContextIndex: TStringList;
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
  Parts: TStringList;
  ContextKey: string;
  CtxIdx: Integer;
  Bucket: TObjectList;
begin
  Result := Self;
  for I := FRoutes.Count - 1 downto 0 do
  begin
    Entry := TRouteEntry(FRoutes[I]);
    FullRoute := Entry.Verb + ' ' + Entry.Pattern;
    if SameText(FullRoute, Route) then
    begin
      FRoutes.Delete(I);
      Parts := SplitString(Entry.Pattern, '/');
      try
        if (Parts.Count > 1) and (Copy(Parts[1], 1, 1) <> ':') then
          ContextKey := Parts[1]
        else
          ContextKey := '';
        CtxIdx := FContextIndex.IndexOf(ContextKey);
        if CtxIdx >= 0 then
        begin
          Bucket := TObjectList(FContextIndex.Objects[CtxIdx]);
          Bucket.Remove(Entry);
        end;
      finally
        Parts.Free;
      end;
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
  Parts: TStringList;
  ContextKey: string;
  CtxIdx: Integer;
  Bucket: TObjectList;
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

  Parts := SplitString(Entry.Pattern, '/');
  try
    if (Parts.Count > 1) and (Copy(Parts[1], 1, 1) <> ':') then
      ContextKey := Parts[1]
    else
      ContextKey := '';

    CtxIdx := FContextIndex.IndexOf(ContextKey);
    if CtxIdx < 0 then
    begin
      Bucket := TObjectList.Create(False);
      CtxIdx := FContextIndex.AddObject(ContextKey, Bucket);
    end
    else
      Bucket := TObjectList(FContextIndex.Objects[CtxIdx]);
    Bucket.Add(Entry);
  finally
    Parts.Free;
  end;
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
  FContextIndex := TStringList.Create;
  FContextIndex.CaseSensitive := False;
end;

destructor TRouteManager.Destroy;
begin
  if Assigned(FContextIndex) then
  begin
    while FContextIndex.Count > 0 do
    begin
      if Assigned(FContextIndex.Objects[0]) then
        TObject(FContextIndex.Objects[0]).Free;
      FContextIndex.Delete(0);
    end;
    FreeAndNil(FContextIndex);
  end;
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
  I, J, CtxIdx: Integer;
  PatternParts, PathParts: TStringList;
  Part, ParamName, ContextKey: string;
  Bucket: TObjectList;
begin
  Result := False;
  Params.Clear;
  Entry := nil;

  PathParts := SplitString(APath, '/');
  try
    if (PathParts.Count > 1) then
      ContextKey := PathParts[1]
    else
      ContextKey := '';

    CtxIdx := FContextIndex.IndexOf(ContextKey);
    if CtxIdx >= 0 then
      Bucket := TObjectList(FContextIndex.Objects[CtxIdx])
    else
      Bucket := FRoutes;

    for I := 0 to Bucket.Count - 1 do
  begin
    Entry := TRouteEntry(Bucket[I]);
    if Entry.Verb <> AVerb then Continue;

    PatternParts := SplitString(Entry.Pattern, '/');
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
    end;
  end;
  finally
    PathParts.Free;
  end;
end;

end.
