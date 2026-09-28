unit BadgerRouteManager;

{$I BadgerDefines.inc}

interface

uses
  Classes,
  SysUtils,
  BadgerTypes,
  BadgerHttpStatus,
  Contnrs;

type
  TRouteEntry = class
    Verb: string;
    Pattern: string;
    Callback: TRoutingCallback;
    ParamNames: TStringList;
    HasParams: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TRouteManager = class(TObject)
  private
    FRoutes: TObjectList;
    FContextIndex: TStringList;
    FSealed: Boolean;
    function SplitString(const S, Delim: string): TStringList;
    function ScanBucket(ABucket: TObjectList; const AVerb, APath: string;
      out Entry: TRouteEntry; var Params: TStringList): Boolean;

  public
    constructor Create;
    destructor Destroy; override;
    function AddMethod(const AVerb, ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddDel(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    { Alias de AddDel: 'AddDel' nao aparece em busca por 'Delete'. }
    function AddDelete(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddGet(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddPatch(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddPost(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    function AddPut(const ARoute: string; ACallback: TRoutingCallback): TRouteManager;
    { Declara que ARoute aceita upgrade WebSocket. Sem callback: o handshake é do
      motor, e as mensagens chegam por TBadger.OnWebSocketMessage. }
    function AddWebSocket(const ARoute: string): TRouteManager;

    function Unregister(const Route: string): TRouteManager;
    { Chamado pelo Start do servidor. Depois disso Add*/Unregister levantam excecao:
      TObjectList nao tem lock e realocar a lista enquanto um worker a percorre e
      access violation ou middleware pulado. }
    procedure Seal;
    function Sealed: Boolean;
    function MatchRoute(const AVerb, APath: string; out Entry: TRouteEntry; var Params: TStringList): Boolean;
  end;

{ True quando APath esta sob APattern, segmento a segmento: ':nome' no padrao casa
  qualquer segmento nao vazio e os demais comparam sem caixa. Usado pelos
  middlewares de auth: a comparacao literal deixava '/users/:id' protegido so no
  texto, e '/users/5' passava sem autenticacao. }
function BadgerPathUnderPattern(const APath, APattern: string): Boolean;

const
  CGET   = 'GET';
  CPOST  = 'POST';
  CPUT   = 'PUT';
  CPATCH = 'PATCH';
  CDEL   = 'DELETE';
  CWS    = 'WS';

implementation

uses
  BadgerLogger;

procedure SplitPathSegments(const S: string; L: TStringList);
var
  I, St: Integer;
begin
  L.Clear;
  St := 1;
  for I := 1 to Length(S) do
    if S[I] = '/' then
    begin
      L.Add(Copy(S, St, I - St));
      St := I + 1;
    end;
  L.Add(Copy(S, St, MaxInt));
end;

function BadgerPathUnderPattern(const APath, APattern: string): Boolean;
var
  P, R: TStringList;
  I: Integer;
begin
  P := TStringList.Create;
  R := TStringList.Create;
  try
    SplitPathSegments(APattern, P);
    SplitPathSegments(APath, R);
    Result := R.Count >= P.Count;
    I := 0;
    while Result and (I < P.Count) do
    begin
      if Copy(P[I], 1, 1) = ':' then
        Result := R[I] <> ''
      else
        Result := SameText(P[I], R[I]);
      Inc(I);
    end;
  finally
    R.Free;
    P.Free;
  end;
end;

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
  RemovedCount: Integer;
begin
  Result := Self;
  if FSealed then
    raise Exception.Create(
      'TRouteManager: Unregister com o servidor no ar nao e suportado.');
  RemovedCount := 0;
  for I := FRoutes.Count - 1 downto 0 do
  begin
    Entry := TRouteEntry(FRoutes[I]);
    FullRoute := Entry.Verb + ' ' + Entry.Pattern;
    if SameText(FullRoute, Route) then
    begin
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
      FRoutes.Delete(I);
      Inc(RemovedCount);
    end;
  end;

  if RemovedCount = 0 then
    raise Exception.CreateFmt('Route not found for unregister: %s', [Route]);
end;

function TRouteManager.AddDel(const ARoute: string;
  ACallback: TRoutingCallback): TRouteManager;
begin
  Result := AddMethod(CDEL, ARoute, ACallback);
end;

function TRouteManager.AddDelete(const ARoute: string;
  ACallback: TRoutingCallback): TRouteManager;
begin
  Result := AddMethod(CDEL, ARoute, ACallback);
end;

function TRouteManager.AddGet(const ARoute: string;
  ACallback: TRoutingCallback): TRouteManager;
begin
  Result := AddMethod(CGET, ARoute, ACallback);
end;

procedure TRouteManager.Seal;
begin
  FSealed := True;
end;

function TRouteManager.Sealed: Boolean;
begin
  Result := FSealed;
end;

function TRouteManager.AddMethod(const AVerb, ARoute: string; ACallback: TRoutingCallback): TRouteManager;
var
  I: Integer;
  Entry: TRouteEntry;
  CleanRoute: string;
  Parts: TStringList;
  ContextKey: string;
  CtxIdx: Integer;
  Bucket: TObjectList;
begin
  Result := Self;
  if FSealed then
    raise Exception.CreateFmt(
      'TRouteManager: rota "%s %s" registrada com o servidor no ar. Registre as ' +
      'rotas antes de Start.', [AVerb, ARoute]);
  { Um StringReplace so deixava '///' como '//' e a rota nunca casava. }
  CleanRoute := ARoute;
  while Pos('//', CleanRoute) > 0 do
    CleanRoute := StringReplace(CleanRoute, '//', '/', [rfReplaceAll]);
  if Copy(CleanRoute, 1, 1) <> '/' then
    CleanRoute := '/' + CleanRoute;
  if (Length(CleanRoute) > 1) and (CleanRoute[Length(CleanRoute)] = '/') then
    SetLength(CleanRoute, Length(CleanRoute) - 1);

  { Mesma rota duas vezes: a primeira vence (ScanBucket para no primeiro) e a
    segunda nunca roda. Mantido, mas avisado: costuma ser copia-e-cola. }
  for I := 0 to FRoutes.Count - 1 do
    if (TRouteEntry(FRoutes[I]).Verb = UpperCase(AVerb)) and
       (TRouteEntry(FRoutes[I]).Pattern = LowerCase(CleanRoute)) then
    begin
      Logger.Warning(Format('TRouteManager: route "%s %s" registered twice; ' +
        'the first registration wins', [UpperCase(AVerb), CleanRoute]));
      Break;
    end;

  Entry := TRouteEntry.Create;
  Entry.Verb := UpperCase(AVerb);
  Entry.Pattern := LowerCase(CleanRoute);
  Entry.HasParams := Pos(':', Entry.Pattern) > 0;
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

function TRouteManager.AddWebSocket(const ARoute: string): TRouteManager;
begin
  Result := AddMethod(CWS, ARoute, nil);
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

function RouteContextKey(const Path: string): string;
var
  P, Q: Integer;
begin
  Result := '';
  if (Length(Path) < 2) or (Path[1] <> '/') then
    Exit;
  P := 2;
  Q := P;
  while (Q <= Length(Path)) and (Path[Q] <> '/') do
    Inc(Q);
  Result := Copy(Path, P, Q - P);
  if (Result <> '') and (Result[1] = ':') then
    Result := '';
end;

{ Varre um bucket. Os valores de :param saem do APath original — o padrão já está
  em minúsculas (AddMethod), então a comparação é SameText e a caixa da URI é
  preservada nos parâmetros. }
function TRouteManager.ScanBucket(ABucket: TObjectList; const AVerb, APath: string;
  out Entry: TRouteEntry; var Params: TStringList): Boolean;
var
  I, J: Integer;
  PatternParts, PathParts: TStringList;
  Part, ParamName: string;
begin
  Result := False;
  Entry := nil;
  for I := 0 to ABucket.Count - 1 do
  begin
    Entry := TRouteEntry(ABucket[I]);
    if not Assigned(Entry) then Continue;
    if Entry.Verb <> AVerb then Continue;

    Params.Clear;
    if not Entry.HasParams then
    begin
      if SameText(Entry.Pattern, APath) then
      begin
        Result := True;
        Exit;
      end;
      Continue;
    end;

    PathParts := SplitString(APath, '/');
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
        else if not SameText(Part, PathParts[J]) then
        begin
          Result := False;
          Break;
        end;
      end;

      if Result then
        Exit;

      Entry := nil;
    finally
      PatternParts.Free;
      PathParts.Free;
    end;
  end;
  Entry := nil;
end;

function TRouteManager.MatchRoute(const AVerb, APath: string; out Entry: TRouteEntry; var Params: TStringList): Boolean;
var
  CtxIdx: Integer;
  Bucket: TObjectList;
  Path: string;
begin
  Params.Clear;
  Entry := nil;

  { Rota registrada perde a barra final (AddMethod); o path da requisicao nao
    perdia, entao /users/ nunca casava com /users. }
  Path := APath;
  if (Length(Path) > 1) and (Path[Length(Path)] = '/') then
    SetLength(Path, Length(Path) - 1);

  CtxIdx := FContextIndex.IndexOf(RouteContextKey(Path));
  if CtxIdx >= 0 then
    Bucket := TObjectList(FContextIndex.Objects[CtxIdx])
  else
    Bucket := FRoutes;

  Result := ScanBucket(Bucket, AVerb, Path, Entry, Params);
  { O bucket de contexto não guarda as rotas cujo primeiro segmento é :param
    (vivem na chave ''). Sem este segundo passe, /:a/:b nunca casa quando existe
    qualquer rota estática com o mesmo primeiro segmento. }
  if (not Result) and (Bucket <> FRoutes) then
    Result := ScanBucket(FRoutes, AVerb, Path, Entry, Params);
  if not Result then
    Entry := nil;
end;

end.
