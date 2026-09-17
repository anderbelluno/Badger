unit BadgerHttpParser;

{ HTTP wire helpers with no sockets — parser + response builder.
  IOCP, epoll or threads Feed() bytes; send the AnsiString from Assemble. }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, StrUtils, BadgerHttpStatus
  {$IFDEF MSWINDOWS}, Windows{$ENDIF};

const
  HTTP_MAX_HEADER_LINE = 16384;
  HTTP_MAX_HEADER_TOTAL = 65536;
  HTTP_MAX_BODY = 52428800; { 50MB — same cap as THTTPRequestHandler }

type
  { Engine-owned connection. Routes read RemoteIP; they must not assume a
    TTCPBlockSocket. }
  TBadgerConn = class
  private
    FRemoteIP: string;
  public
    property RemoteIP: string read FRemoteIP write FRemoteIP;
  end;

  THttpParseState = (
    hpsHeaders,
    hpsBodyLength,
    hpsChunkSize,
    hpsChunkData,
    hpsChunkCRLF,
    hpsChunkTrailers,
    hpsDone,
    hpsError
  );

  TBadgerHttpParser = class
  private
    FState: THttpParseState;
    FRaw: AnsiString;
    FError: string;
    FRequestLine: string;
    FMethod: string;
    FURI: string;
    FHeaders: TStringList;
    FQueryParams: TStringList;
    FBody: AnsiString;
    FContentLength: Integer;
    FChunked: Boolean;
    FChunkLeft: Integer;
    FHttp10: Boolean;
    FWantsClose: Boolean;
    FWsUpgrade: Boolean;
    FURILower: string;
    FConnection: string;
    FUpgrade: string;
    FOrigin: string;
    FRealIP: string;
    FForwardedFor: string;
    procedure SetError(const Msg: string);
    procedure AppendRaw(Buf: Pointer; Len: Integer);
    procedure ParseRequestLineAndHeaders;
    procedure ParseRequestLine(P: PAnsiChar; Len: Integer);
    procedure ConsumeBody;
    procedure ConsumeChunked;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    { Returns True when the request is complete or failed (see State). }
    function Feed(Buf: Pointer; Len: Integer): Boolean;
    property State: THttpParseState read FState;
    property Error: string read FError;
    property RequestLine: string read FRequestLine;
    property Method: string read FMethod;
    property URI: string read FURI;
    property Headers: TStringList read FHeaders;
    property QueryParams: TStringList read FQueryParams;
    property Body: AnsiString read FBody;
    property Leftover: AnsiString read FRaw;
    property WantsClose: Boolean read FWantsClose;
    property IsWebSocketUpgrade: Boolean read FWsUpgrade;
    property URILower: string read FURILower;
    property Origin: string read FOrigin;
    property RealIP: string read FRealIP;
    property ForwardedFor: string read FForwardedFor;
  end;

function Utf8BytesToString(const Raw: AnsiString): string;
function BadgerRfc822Date(const DateValue: string): string;
function BadgerBuildHTTPResponse(StatusCode: Integer; const Body: string; Stream: TStream;
  const ContentType: string; CloseConnection: Boolean; HeaderCustom: TStringList;
  const DateValue: string): string;
function BadgerAssembleHTTPMessage(StatusCode: Integer; const Body: string; Stream: TStream;
  const ContentType: string; CloseConnection: Boolean; HeaderCustom: TStringList): AnsiString;

implementation

const
  W_CRLF: AnsiString = #13#10;
  W_HTTP11: AnsiString = 'HTTP/1.1 ';
  W_CT: AnsiString = 'Content-Type: ';
  W_CLEN: AnsiString = 'Content-Length: ';
  W_CHARSET: AnsiString = '; charset=utf-8';
  W_DATE: AnsiString = 'Date: ';
  W_SERVER: AnsiString = 'Server: Badger HTTP Server';
  W_CONN_CLOSE: AnsiString = 'Connection: close';
  W_CONN_KA: AnsiString = 'Connection: keep-alive';
  W_COLON: AnsiString = ':';
  W_SP: AnsiString = ' ';
  H_HTTP10: AnsiString = 'HTTP/1.0';
  H_CONNECTION: AnsiString = 'connection';
  H_UPGRADE: AnsiString = 'upgrade';
  H_ORIGIN: AnsiString = 'origin';
  H_XREALIP: AnsiString = 'x-real-ip';
  H_XFF: AnsiString = 'x-forwarded-for';
  H_CLEN: AnsiString = 'content-length';
  H_TE: AnsiString = 'transfer-encoding';

threadvar
  GDateTick: LongWord;
  GDateWire: AnsiString;

function HttpIsTextual(const ContentType: string): Boolean;
var
  Effective: string;
begin
  if ContentType = '' then
    Effective := TEXT_PLAIN
  else
    Effective := ContentType;
  Result := AnsiContainsText(Effective, TEXT_PLAIN) or
            AnsiContainsText(Effective, APPLICATION_JSON);
end;

function ResponseUsesStream(Stream: TStream): Boolean;
begin
  Result := Assigned(Stream) and (Stream.Size > 0);
end;

function Utf8PtrToString(P: PAnsiChar; Len: Integer): string;
{$IFDEF FPC}
begin
  SetString(Result, P, Len);
end;
{$ELSE}
{$IFDEF UNICODE}
var
  I, N: Integer;
{$IFDEF MSWINDOWS}
begin
  if (P = nil) or (Len <= 0) then
  begin
    Result := '';
    Exit;
  end;
  for I := 0 to Len - 1 do
    if Ord(P[I]) > 127 then
    begin
      N := MultiByteToWideChar(CP_UTF8, 0, P, Len, nil, 0);
      SetLength(Result, N);
      if N > 0 then
        MultiByteToWideChar(CP_UTF8, 0, P, Len, PWideChar(Result), N);
      Exit;
    end;
  SetLength(Result, Len);
  for I := 0 to Len - 1 do
    Result[I + 1] := WideChar(Ord(P[I]));
end;
{$ELSE}
  Bytes: TBytes;
begin
  if (P = nil) or (Len <= 0) then
  begin
    Result := '';
    Exit;
  end;
  for I := 0 to Len - 1 do
    if Ord(P[I]) > 127 then
    begin
      SetLength(Bytes, Len);
      Move(P^, Bytes[0], Len);
      Result := TEncoding.UTF8.GetString(Bytes);
      Exit;
    end;
  SetLength(Result, Len);
  for I := 0 to Len - 1 do
    Result[I + 1] := WideChar(Ord(P[I]));
end;
{$ENDIF}
{$ELSE}
var
  Tmp: AnsiString;
  I: Integer;
begin
  if (P = nil) or (Len <= 0) then
  begin
    Result := '';
    Exit;
  end;
  for I := 0 to Len - 1 do
    if Ord(P[I]) > 127 then
    begin
      SetString(Tmp, P, Len);
      Result := Utf8ToAnsi(Tmp);
      Exit;
    end;
  SetString(Result, P, Len);
end;
{$ENDIF}
{$ENDIF}

function StringToUtf8Bytes(const S: string): AnsiString;
{$IFDEF FPC}
begin
  Result := AnsiString(S);
end;
{$ELSE}
{$IFDEF UNICODE}
var
  I, L, N: Integer;
{$IFDEF MSWINDOWS}
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := '';
    Exit;
  end;
  SetLength(Result, L);
  for I := 1 to L do
  begin
    if Ord(S[I]) > 127 then
    begin
      N := WideCharToMultiByte(CP_UTF8, 0, PWideChar(S), L, nil, 0, nil, nil);
      SetLength(Result, N);
      if N > 0 then
        WideCharToMultiByte(CP_UTF8, 0, PWideChar(S), L, PAnsiChar(Result), N, nil, nil);
      Exit;
    end;
    Result[I] := AnsiChar(Ord(S[I]));
  end;
end;
{$ELSE}
  U: UTF8String;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := '';
    Exit;
  end;
  for I := 1 to L do
    if Ord(S[I]) > 127 then
    begin
      U := UTF8Encode(S);
      SetLength(Result, Length(U));
      if Length(U) > 0 then
        Move(PAnsiChar(U)^, Result[1], Length(U));
      Exit;
    end;
  SetLength(Result, L);
  for I := 1 to L do
    Result[I] := AnsiChar(Ord(S[I]));
end;
{$ENDIF}
{$ELSE}
begin
  Result := AnsiString(UTF8Encode(S));
end;
{$ENDIF}
{$ENDIF}

function Utf8BytesToString(const Raw: AnsiString): string;
begin
  if Raw = '' then
    Result := ''
  else
    Result := Utf8PtrToString(PAnsiChar(Raw), Length(Raw));
end;

function StripCRLF(const S: string): string;
begin
  Result := StringReplace(S, #13, '', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);
end;

function SanitizeHeaderName(const S: string): string;
var
  J, N: Integer;
  Ch: Char;
begin
  N := Length(S);
  SetLength(Result, N);
  for J := 1 to N do
  begin
    Ch := S[J];
    if (Ord(Ch) > 31) and (Ch <> ':') then
      Result[J] := Ch
    else
      Result[J] := '_';
  end;
  Result := Trim(Result);
end;

function BadgerRfc822Date(const DateValue: string): string;
const
  DayNames: array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  MonthNames: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  Y, M, D, H, N, S, Ms: Word;
begin
  if DateValue <> '' then
  begin
    Result := DateValue;
    Exit;
  end;
  DecodeDate(Now, Y, M, D);
  DecodeTime(Now, H, N, S, Ms);
  Result := Format('%s, %d %s %.4d %.2d:%.2d:%.2d',
    [DayNames[DayOfWeek(Now)], D, MonthNames[M], Y, H, N, S]);
end;

procedure WireGrow(var D: AnsiString; var Len, Cap: Integer; Need: Integer);
begin
  if Len + Need <= Cap then
    Exit;
  Cap := (Len + Need) * 2 + 128;
  SetLength(D, Cap);
end;

procedure WireA(var D: AnsiString; var Len, Cap: Integer; const T: AnsiString);
var
  N: Integer;
begin
  N := Length(T);
  if N <= 0 then
    Exit;
  WireGrow(D, Len, Cap, N);
  Move(T[1], D[Len + 1], N);
  Inc(Len, N);
end;

procedure WireS(var D: AnsiString; var Len, Cap: Integer; const T: string);
begin
  WireA(D, Len, Cap, StringToUtf8Bytes(T));
end;

procedure WireInt(var D: AnsiString; var Len, Cap: Integer; V: Integer);
var
  Buf: array[0..11] of AnsiChar;
  P, N: Integer;
  U: Cardinal;
  Neg: Boolean;
begin
  Neg := V < 0;
  if V = 0 then
  begin
    WireGrow(D, Len, Cap, 1);
    Inc(Len);
    D[Len] := '0';
    Exit;
  end;
  if Neg then
    U := Cardinal(-V)
  else
    U := Cardinal(V);
  P := High(Buf);
  repeat
    Buf[P] := AnsiChar(Ord('0') + (U mod 10));
    U := U div 10;
    Dec(P);
  until U = 0;
  if Neg then
  begin
    Buf[P] := '-';
    Dec(P);
  end;
  N := High(Buf) - P;
  WireGrow(D, Len, Cap, N);
  Move(Buf[P + 1], D[Len + 1], N);
  Inc(Len, N);
end;

function Rfc822DateWire(const DateValue: string): AnsiString;
{$IFDEF MSWINDOWS}
var
  T: DWORD;
begin
  if DateValue <> '' then
  begin
    Result := StringToUtf8Bytes(DateValue);
    Exit;
  end;
  T := GetTickCount;
  if (GDateWire <> '') and ((T - GDateTick) < 1000) then
  begin
    Result := GDateWire;
    Exit;
  end;
  Result := StringToUtf8Bytes(BadgerRfc822Date(''));
  GDateWire := Result;
  GDateTick := T;
end;
{$ELSE}
begin
  if DateValue <> '' then
    Result := StringToUtf8Bytes(DateValue)
  else
    Result := StringToUtf8Bytes(BadgerRfc822Date(''));
end;
{$ENDIF}

function BuildHeaderBlock(StatusCode, BodyByteLen: Integer; HasStream: Boolean;
  const ContentType: string; CloseConnection: Boolean; HeaderCustom: TStringList;
  const DateValue: string): AnsiString;
var
  EffectiveContentType: string;
  I, Len, Cap: Integer;
  HeaderName, HeaderValue: string;
begin
  if ContentType = '' then
    EffectiveContentType := TEXT_PLAIN
  else
    EffectiveContentType := ContentType;

  Len := 0;
  Cap := 256;
  SetLength(Result, Cap);
  WireA(Result, Len, Cap, W_HTTP11);
  WireInt(Result, Len, Cap, StatusCode);
  WireA(Result, Len, Cap, W_SP);
  WireS(Result, Len, Cap, THTTPStatus.GetStatusText(StatusCode));
  WireA(Result, Len, Cap, W_CRLF);
  if HasStream then
  begin
    WireA(Result, Len, Cap, W_CT);
    WireS(Result, Len, Cap, EffectiveContentType);
    WireA(Result, Len, Cap, W_CRLF);
    WireA(Result, Len, Cap, W_CLEN);
    WireInt(Result, Len, Cap, BodyByteLen);
    WireA(Result, Len, Cap, W_CRLF);
  end
  else if HttpIsTextual(EffectiveContentType) then
  begin
    WireA(Result, Len, Cap, W_CT);
    WireS(Result, Len, Cap, EffectiveContentType);
    WireA(Result, Len, Cap, W_CHARSET);
    WireA(Result, Len, Cap, W_CRLF);
    WireA(Result, Len, Cap, W_CLEN);
    WireInt(Result, Len, Cap, BodyByteLen);
    WireA(Result, Len, Cap, W_CRLF);
  end;

  WireA(Result, Len, Cap, W_DATE);
  WireA(Result, Len, Cap, Rfc822DateWire(DateValue));
  WireA(Result, Len, Cap, W_CRLF);
  WireA(Result, Len, Cap, W_SERVER);
  WireA(Result, Len, Cap, W_CRLF);
  if CloseConnection then
    WireA(Result, Len, Cap, W_CONN_CLOSE)
  else
    WireA(Result, Len, Cap, W_CONN_KA);
  WireA(Result, Len, Cap, W_CRLF);

  if Assigned(HeaderCustom) and (HeaderCustom.Count > 0) then
  begin
    for I := 0 to Pred(HeaderCustom.Count) do
    begin
      HeaderName := SanitizeHeaderName(HeaderCustom.Names[I]);
      HeaderValue := StripCRLF(HeaderCustom.ValueFromIndex[I]);
      if HeaderName <> '' then
      begin
        WireS(Result, Len, Cap, HeaderName);
        WireA(Result, Len, Cap, W_COLON);
        WireS(Result, Len, Cap, HeaderValue);
        WireA(Result, Len, Cap, W_CRLF);
      end;
    end;
  end;

  WireA(Result, Len, Cap, W_CRLF);
  SetLength(Result, Len);
end;

function BadgerBuildHTTPResponse(StatusCode: Integer; const Body: string; Stream: TStream;
  const ContentType: string; CloseConnection: Boolean; HeaderCustom: TStringList;
  const DateValue: string): string;
var
  BodyLen: Integer;
  UseStream: Boolean;
  Effective: string;
begin
  UseStream := ResponseUsesStream(Stream);
  if ContentType = '' then
    Effective := TEXT_PLAIN
  else
    Effective := ContentType;
  if UseStream then
    BodyLen := Integer(Stream.Size)
  else if HttpIsTextual(Effective) then
    BodyLen := Length(StringToUtf8Bytes(Body))
  else
    BodyLen := 0;
  Result := Utf8BytesToString(BuildHeaderBlock(StatusCode, BodyLen, UseStream, ContentType,
    CloseConnection, HeaderCustom, DateValue));
end;

function BadgerAssembleHTTPMessage(StatusCode: Integer; const Body: string; Stream: TStream;
  const ContentType: string; CloseConnection: Boolean; HeaderCustom: TStringList): AnsiString;
var
  Hdr, Utf8: AnsiString;
  Extra: Integer;
  P: Integer;
  UseStream: Boolean;
  Effective: string;
begin
  Result := '';
  Utf8 := '';
  Extra := 0;
  UseStream := ResponseUsesStream(Stream);
  if ContentType = '' then
    Effective := TEXT_PLAIN
  else
    Effective := ContentType;
  if UseStream then
    Extra := Integer(Stream.Size)
  else if HttpIsTextual(Effective) then
    Utf8 := StringToUtf8Bytes(Body);
  Hdr := BuildHeaderBlock(StatusCode, Extra + Length(Utf8), UseStream,
    ContentType, CloseConnection, HeaderCustom, '');
  SetLength(Result, Length(Hdr) + Length(Utf8) + Extra);
  P := 1;
  if Length(Hdr) > 0 then
  begin
    Move(Hdr[1], Result[P], Length(Hdr));
    Inc(P, Length(Hdr));
  end;
  if Length(Utf8) > 0 then
  begin
    Move(Utf8[1], Result[P], Length(Utf8));
    Inc(P, Length(Utf8));
  end;
  if Extra > 0 then
  begin
    Stream.Position := 0;
    Stream.Read(Result[P], Extra);
  end;
end;

constructor TBadgerHttpParser.Create;
begin
  inherited Create;
  FHeaders := TStringList.Create;
  FQueryParams := TStringList.Create;
  Reset;
end;

destructor TBadgerHttpParser.Destroy;
begin
  FQueryParams.Free;
  FHeaders.Free;
  inherited Destroy;
end;

function AsciiHeaderIs(P: PAnsiChar; Len: Integer; const Name: AnsiString): Boolean;
var
  I: Integer;
  A, B: Byte;
begin
  if Len <> Length(Name) then
  begin
    Result := False;
    Exit;
  end;
  for I := 1 to Len do
  begin
    A := Ord(P[I - 1]);
    B := Ord(Name[I]);
    if (A >= 65) and (A <= 90) then
      Inc(A, 32);
    if (B >= 65) and (B <= 90) then
      Inc(B, 32);
    if A <> B then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

procedure PtrTrim(var P: PAnsiChar; var Len: Integer);
begin
  while (Len > 0) and ((P^ = ' ') or (P^ = #9)) do
  begin
    Inc(P);
    Dec(Len);
  end;
  while (Len > 0) and ((P[Len - 1] = ' ') or (P[Len - 1] = #9)) do
    Dec(Len);
end;

function AsciiPtrToCasedString(P: PAnsiChar; Len: Integer; ToUpper: Boolean): string;
var
  I: Integer;
  C: Byte;
begin
  if (P = nil) or (Len <= 0) then
  begin
    Result := '';
    Exit;
  end;
  SetLength(Result, Len);
  for I := 0 to Len - 1 do
  begin
    C := Ord(P[I]);
    if ToUpper then
    begin
      if (C >= 97) and (C <= 122) then
        Dec(C, 32);
    end
    else if (C >= 65) and (C <= 90) then
      Inc(C, 32);
    Result[I + 1] := Char(C);
  end;
end;

function StrAsciiLower(const S: string): string;
var
  I, C: Integer;
begin
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
  begin
    C := Ord(S[I]);
    if (C >= 65) and (C <= 90) then
      Inc(C, 32);
    Result[I] := Char(C);
  end;
end;

function PtrToInt(P: PAnsiChar; Len: Integer): Integer;
var
  I: Integer;
  Neg: Boolean;
  C: Byte;
begin
  Result := 0;
  Neg := False;
  I := 0;
  while (I < Len) and ((P[I] = ' ') or (P[I] = #9)) do
    Inc(I);
  if (I < Len) and (P[I] = '-') then
  begin
    Neg := True;
    Inc(I);
  end;
  while I < Len do
  begin
    C := Ord(P[I]);
    if (C < Ord('0')) or (C > Ord('9')) then
      Break;
    Result := Result * 10 + (C - Ord('0'));
    Inc(I);
  end;
  if Neg then
    Result := -Result;
end;

procedure TBadgerHttpParser.Reset;
begin
  FState := hpsHeaders;
  FRaw := '';
  FError := '';
  FRequestLine := '';
  FMethod := '';
  FURI := '';
  FURILower := '';
  FHeaders.Clear;
  FQueryParams.Clear;
  FBody := '';
  FContentLength := 0;
  FChunked := False;
  FChunkLeft := 0;
  FHttp10 := False;
  FWantsClose := True;
  FWsUpgrade := False;
  FConnection := '';
  FUpgrade := '';
  FOrigin := '';
  FRealIP := '';
  FForwardedFor := '';
end;

procedure TBadgerHttpParser.SetError(const Msg: string);
begin
  FState := hpsError;
  FError := Msg;
end;

procedure TBadgerHttpParser.AppendRaw(Buf: Pointer; Len: Integer);
var
  OldLen: Integer;
begin
  if (Buf = nil) or (Len <= 0) then
    Exit;
  OldLen := Length(FRaw);
  SetLength(FRaw, OldLen + Len);
  Move(Buf^, FRaw[OldLen + 1], Len);
end;

procedure TBadgerHttpParser.ParseRequestLine(P: PAnsiChar; Len: Integer);
var
  I, MethodLen, UriStart, UriLen, QueryPos: Integer;
  QueryString, ParamPair: string;
  SpacePos: Integer;
begin
  FQueryParams.Clear;
  if (P = nil) or (Len <= 0) then
  begin
    SetError('bad request line');
    Exit;
  end;
  FRequestLine := Utf8PtrToString(P, Len);
  I := 0;
  while (I < Len) and (P[I] <> ' ') do
    Inc(I);
  MethodLen := I;
  if MethodLen <= 0 then
  begin
    SetError('bad request line');
    Exit;
  end;
  FMethod := AsciiPtrToCasedString(P, MethodLen, True);
  Inc(I);
  while (I < Len) and (P[I] = ' ') do
    Inc(I);
  UriStart := I;
  while (I < Len) and (P[I] <> ' ') do
    Inc(I);
  UriLen := I - UriStart;
  if UriLen <= 0 then
  begin
    SetError('bad request line');
    Exit;
  end;
      FURI := Utf8PtrToString(@P[UriStart], UriLen);
  if I < Len then
  begin
    Inc(I);
    while (I < Len) and (P[I] = ' ') do
      Inc(I);
    FHttp10 := AsciiHeaderIs(@P[I], Len - I, H_HTTP10);
  end;
  while Pos('//', FURI) > 0 do
    FURI := StringReplace(FURI, '//', '/', [rfReplaceAll]);
  QueryPos := Pos('?', FURI);
  if QueryPos > 0 then
  begin
    QueryString := Copy(FURI, QueryPos + 1, Length(FURI));
    FURI := Copy(FURI, 1, QueryPos - 1);
    while QueryString <> '' do
    begin
      SpacePos := Pos('&', QueryString);
      if SpacePos > 0 then
      begin
        ParamPair := Copy(QueryString, 1, SpacePos - 1);
        Delete(QueryString, 1, SpacePos);
      end
      else
      begin
        ParamPair := QueryString;
        QueryString := '';
      end;
      if ParamPair <> '' then
        FQueryParams.Add(ParamPair);
    end;
  end;
  FURILower := StrAsciiLower(FURI);
end;

procedure TBadgerHttpParser.ParseRequestLineAndHeaders;
var
  RawLen, HeadEnd, LineStart, LineEnd, Sep: Integer;
  NameP, ValP: PAnsiChar;
  NameLen, ValLen: Integer;
  TE: string;
begin
  RawLen := Length(FRaw);
  HeadEnd := Pos(AnsiString(#13#10#13#10), FRaw);
  if HeadEnd <= 0 then
  begin
    SetError('bad headers');
    Exit;
  end;
  LineEnd := 1;
  while (LineEnd < RawLen) and not ((FRaw[LineEnd] = #13) and (FRaw[LineEnd + 1] = #10)) do
    Inc(LineEnd);
  if (LineEnd = 1) or (LineEnd > HeadEnd) then
  begin
    SetError('bad headers');
    Exit;
  end;
  ParseRequestLine(@FRaw[1], LineEnd - 1);
  if FState = hpsError then
    Exit;

  TE := '';
  LineStart := LineEnd + 2;
  while LineStart < HeadEnd do
  begin
    LineEnd := LineStart;
    while (LineEnd < RawLen) and not ((FRaw[LineEnd] = #13) and (FRaw[LineEnd + 1] = #10)) do
      Inc(LineEnd);
    if LineEnd = LineStart then
      Break;
    if (LineEnd - LineStart) > HTTP_MAX_HEADER_LINE then
    begin
      SetError('header line too large');
      Exit;
    end;
    Sep := LineStart;
    while (Sep < LineEnd) and (FRaw[Sep] <> ':') do
      Inc(Sep);
    if Sep < LineEnd then
    begin
      NameP := @FRaw[LineStart];
      NameLen := Sep - LineStart;
      ValP := @FRaw[Sep + 1];
      ValLen := LineEnd - (Sep + 1);
      PtrTrim(NameP, NameLen);
      PtrTrim(ValP, ValLen);
      if AsciiHeaderIs(NameP, NameLen, H_CONNECTION) then
        FConnection := AsciiPtrToCasedString(ValP, ValLen, False)
      else if AsciiHeaderIs(NameP, NameLen, H_UPGRADE) then
        FUpgrade := AsciiPtrToCasedString(ValP, ValLen, False)
      else if AsciiHeaderIs(NameP, NameLen, H_ORIGIN) then
        FOrigin := Utf8PtrToString(ValP, ValLen)
      else if AsciiHeaderIs(NameP, NameLen, H_XREALIP) then
        FRealIP := Utf8PtrToString(ValP, ValLen)
      else if AsciiHeaderIs(NameP, NameLen, H_XFF) then
        FForwardedFor := Utf8PtrToString(ValP, ValLen)
      else if AsciiHeaderIs(NameP, NameLen, H_CLEN) then
        FContentLength := PtrToInt(ValP, ValLen)
      else if AsciiHeaderIs(NameP, NameLen, H_TE) then
        TE := AsciiPtrToCasedString(ValP, ValLen, False);
      FHeaders.Add(Utf8PtrToString(NameP, NameLen) + '=' + Utf8PtrToString(ValP, ValLen));
    end;
    LineStart := LineEnd + 2;
  end;

  FWsUpgrade := (FUpgrade = 'websocket') and (Pos('upgrade', FConnection) > 0);
  if FHttp10 then
    FWantsClose := Pos('keep-alive', FConnection) = 0
  else
    FWantsClose := Pos('close', FConnection) > 0;

  FChunked := Pos('chunked', TE) > 0;
  if FContentLength < 0 then
    FContentLength := 0;
  if FContentLength > HTTP_MAX_BODY then
  begin
    SetError('body too large');
    Exit;
  end;

  FRaw := Copy(FRaw, HeadEnd + 4, MaxInt);

  if FChunked then
    FState := hpsChunkSize
  else if FContentLength > 0 then
    FState := hpsBodyLength
  else
    FState := hpsDone;
end;

procedure TBadgerHttpParser.ConsumeBody;
var
  Need, Take, OldLen: Integer;
begin
  if FState <> hpsBodyLength then
    Exit;
  Need := FContentLength - Length(FBody);
  if Need <= 0 then
  begin
    FState := hpsDone;
    Exit;
  end;
  if Length(FRaw) = 0 then
    Exit;
  Take := Length(FRaw);
  if Take > Need then
    Take := Need;
  OldLen := Length(FBody);
  SetLength(FBody, OldLen + Take);
  Move(FRaw[1], FBody[OldLen + 1], Take);
  Delete(FRaw, 1, Take);
  if Length(FBody) >= FContentLength then
    FState := hpsDone;
end;

procedure TBadgerHttpParser.ConsumeChunked;
var
  P, Size, Take, OldLen: Integer;
  Line: AnsiString;
begin
  while (FState <> hpsDone) and (FState <> hpsError) and (Length(FRaw) > 0) do
  begin
    case FState of
      hpsChunkSize:
        begin
          P := Pos(AnsiString(#13#10), FRaw);
          if P <= 0 then
            Exit;
          Line := Copy(FRaw, 1, P - 1);
          Delete(FRaw, 1, P + 1);
          Size := Pos(AnsiString(';'), Line);
          if Size > 0 then
            Line := Copy(Line, 1, Size - 1);
          Size := StrToIntDef('$' + Trim(string(Line)), -1);
          if Size < 0 then
          begin
            SetError('bad chunk size');
            Exit;
          end;
          if Size = 0 then
            FState := hpsChunkTrailers
          else
          begin
            if Length(FBody) + Size > HTTP_MAX_BODY then
            begin
              SetError('body too large');
              Exit;
            end;
            FChunkLeft := Size;
            FState := hpsChunkData;
          end;
        end;
      hpsChunkData:
        begin
          if Length(FRaw) = 0 then
            Exit;
          Take := Length(FRaw);
          if Take > FChunkLeft then
            Take := FChunkLeft;
          OldLen := Length(FBody);
          SetLength(FBody, OldLen + Take);
          Move(FRaw[1], FBody[OldLen + 1], Take);
          Delete(FRaw, 1, Take);
          Dec(FChunkLeft, Take);
          if FChunkLeft = 0 then
            FState := hpsChunkCRLF;
        end;
      hpsChunkCRLF:
        begin
          if Length(FRaw) < 2 then
            Exit;
          if Copy(FRaw, 1, 2) <> AnsiString(#13#10) then
          begin
            SetError('bad chunk crlf');
            Exit;
          end;
          Delete(FRaw, 1, 2);
          FState := hpsChunkSize;
        end;
      hpsChunkTrailers:
        begin
          P := Pos(AnsiString(#13#10), FRaw);
          if P <= 0 then
            Exit;
          Line := Copy(FRaw, 1, P - 1);
          Delete(FRaw, 1, P + 1);
          if Line = '' then
            FState := hpsDone;
        end;
    else
      Exit;
    end;
  end;
end;

function TBadgerHttpParser.Feed(Buf: Pointer; Len: Integer): Boolean;
begin
  Result := False;
  if FState = hpsError then
  begin
    Result := True;
    Exit;
  end;
  if FState = hpsDone then
  begin
    Result := True;
    Exit;
  end;

  AppendRaw(Buf, Len);

  if FState = hpsHeaders then
  begin
    if Length(FRaw) > HTTP_MAX_HEADER_TOTAL then
    begin
      SetError('headers too large');
      Result := True;
      Exit;
    end;
    if Pos(AnsiString(#13#10#13#10), FRaw) > 0 then
      ParseRequestLineAndHeaders;
  end;

  if FState = hpsBodyLength then
    ConsumeBody
  else if FState in [hpsChunkSize, hpsChunkData, hpsChunkCRLF, hpsChunkTrailers] then
    ConsumeChunked;

  Result := (FState = hpsDone) or (FState = hpsError);
end;

end.
