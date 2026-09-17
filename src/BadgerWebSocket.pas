unit BadgerWebSocket;

{ RFC 6455 handshake + frames. No sockets — classic handler and IOCP both use this. }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes;

const
  WS_MAX_PAYLOAD = 65535;
  WS_OP_TEXT = 1;
  WS_OP_CLOSE = 8;
  WS_OP_PING = 9;
  WS_OP_PONG = 10;

{$IFDEF UNICODE}
type
  TBadgerWsBytes = RawByteString;
{$ELSE}
type
  TBadgerWsBytes = AnsiString;
{$ENDIF}

function BadgerWsAcceptKey(const SecWebSocketKey: string): string;
function BadgerWsHandshakeMessage(const SecWebSocketKey: string): AnsiString;
function BadgerWsTextFrame(const AMessage: string): TBadgerWsBytes;
function BadgerWsMaskedTextFrame(const AMessage: string): TBadgerWsBytes;
function BadgerWsPongFrame(const Payload: TBadgerWsBytes): TBadgerWsBytes;
function BadgerWsRandomKey: string;
function BadgerWsUtf8ToString(P: Pointer; Len: Integer): string;
function BadgerWsIsUpgrade(Headers: TStringList): Boolean;

type
  TBadgerWsParser = class
  private
    FRaw: TBadgerWsBytes;
    FOpcode: Byte;
    FPayload: TBadgerWsBytes;
    FFailed: Boolean;
    procedure AppendRaw(Buf: Pointer; Len: Integer);
  public
    procedure Feed(Buf: Pointer; Len: Integer);
    function TryParse: Boolean;
    function Text: string;
    property Opcode: Byte read FOpcode;
    property Payload: TBadgerWsBytes read FPayload;
    property Failed: Boolean read FFailed;
  end;

implementation

uses
  synacode;

const
  WS_MAGIC = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
  CP_RAW = 65535;

procedure TagWsRaw(var S: TBadgerWsBytes);
begin
{$IFDEF UNICODE}
  if Pointer(S) <> nil then
    SetCodePage(RawByteString(S), CP_RAW, False);
{$ENDIF}
end;

{ UnicodeString -> UTF-8 bytes. TEncoding, never AnsiString() of UTF-8 (that recodes to ACP). }
function Utf8Wire(const S: string): TBadgerWsBytes;
{$IFDEF FPC}
begin
  Result := TBadgerWsBytes(S);
  TagWsRaw(Result);
end;
{$ELSE}
{$IFDEF UNICODE}
var
  Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(S);
  SetLength(Result, Length(Bytes));
  if Length(Bytes) > 0 then
    Move(Bytes[0], Result[1], Length(Bytes));
  TagWsRaw(Result);
end;
{$ELSE}
begin
  Result := AnsiString(UTF8Encode(S));
end;
{$ENDIF}
{$ENDIF}

{ UTF-8 bytes -> string. D12 must not use string(RawByteString): CP_NONE maps each byte to U+00xx (OlÃ¡). }
function BadgerWsUtf8ToString(P: Pointer; Len: Integer): string;
{$IFDEF FPC}
begin
  SetString(Result, PAnsiChar(P), Len);
end;
{$ELSE}
{$IFDEF UNICODE}
var
  Bytes: TBytes;
begin
  if (P = nil) or (Len <= 0) then
  begin
    Result := '';
    Exit;
  end;
  SetLength(Bytes, Len);
  Move(P^, Bytes[0], Len);
  Result := TEncoding.UTF8.GetString(Bytes);
end;
{$ELSE}
var
  Tmp: AnsiString;
begin
  if (P = nil) or (Len <= 0) then
  begin
    Result := '';
    Exit;
  end;
  SetLength(Tmp, Len);
  Move(P^, Tmp[1], Len);
  Result := Utf8ToAnsi(Tmp);
end;
{$ENDIF}
{$ENDIF}

function BadgerWsAcceptKey(const SecWebSocketKey: string): string;
begin
  Result := Trim(string(EncodeBase64(SHA1(AnsiString(SecWebSocketKey) + AnsiString(WS_MAGIC)))));
end;

function BadgerWsHandshakeMessage(const SecWebSocketKey: string): AnsiString;
begin
  Result := AnsiString(
    'HTTP/1.1 101 Switching Protocols' + #13#10 +
    'Upgrade: websocket' + #13#10 +
    'Connection: Upgrade' + #13#10 +
    'Sec-WebSocket-Accept: ' + BadgerWsAcceptKey(SecWebSocketKey) + #13#10 +
    #13#10);
end;

function BadgerWsFrame(Opcode: Byte; const Payload: TBadgerWsBytes; Masked: Boolean): TBadgerWsBytes;
var
  Len, HeaderSize, I: Integer;
  Header: array[0..7] of Byte;
  Mask: array[0..3] of Byte;
begin
  Len := Length(Payload);
  if Len > WS_MAX_PAYLOAD then
  begin
    Result := '';
    Exit;
  end;
  Header[0] := $80 or (Opcode and $0F);
  if Len <= 125 then
  begin
    Header[1] := Byte(Len);
    HeaderSize := 2;
  end
  else
  begin
    Header[1] := 126;
    Header[2] := Byte((Len shr 8) and $FF);
    Header[3] := Byte(Len and $FF);
    HeaderSize := 4;
  end;
  if Masked then
  begin
    Header[1] := Header[1] or $80;
    for I := 0 to 3 do
    begin
      Mask[I] := Byte(Random(256));
      Header[HeaderSize + I] := Mask[I];
    end;
    Inc(HeaderSize, 4);
  end;
  SetLength(Result, HeaderSize + Len);
  TagWsRaw(Result);
  Move(Header[0], Result[1], HeaderSize);
  if Len > 0 then
  begin
    Move(Payload[1], Result[HeaderSize + 1], Len);
    if Masked then
      for I := 1 to Len do
        Result[HeaderSize + I] :=
          AnsiChar(Byte(Result[HeaderSize + I]) xor Mask[(I - 1) mod 4]);
  end;
end;

function BadgerWsTextFrame(const AMessage: string): TBadgerWsBytes;
begin
  Result := BadgerWsFrame(WS_OP_TEXT, Utf8Wire(AMessage), False);
end;

function BadgerWsMaskedTextFrame(const AMessage: string): TBadgerWsBytes;
begin
  Result := BadgerWsFrame(WS_OP_TEXT, Utf8Wire(AMessage), True);
end;

function BadgerWsPongFrame(const Payload: TBadgerWsBytes): TBadgerWsBytes;
begin
  Result := BadgerWsFrame(WS_OP_PONG, Payload, False);
end;

function BadgerWsRandomKey: string;
var
  Raw: AnsiString;
  I: Integer;
begin
  SetLength(Raw, 16);
  for I := 1 to 16 do
    Raw[I] := AnsiChar(Random(256));
  Result := Trim(string(EncodeBase64(Raw)));
end;

function BadgerWsIsUpgrade(Headers: TStringList): Boolean;
begin
  Result := Assigned(Headers) and
    SameText(Headers.Values['Upgrade'], 'websocket') and
    (Pos('upgrade', LowerCase(Headers.Values['Connection'])) > 0);
end;

procedure TBadgerWsParser.AppendRaw(Buf: Pointer; Len: Integer);
var
  Old: Integer;
begin
  if (Buf = nil) or (Len <= 0) then
    Exit;
  Old := Length(FRaw);
  SetLength(FRaw, Old + Len);
  Move(Buf^, FRaw[Old + 1], Len);
  TagWsRaw(FRaw);
end;

procedure TBadgerWsParser.Feed(Buf: Pointer; Len: Integer);
begin
  AppendRaw(Buf, Len);
end;

function TBadgerWsParser.TryParse: Boolean;
var
  B1, B2: Byte;
  Masked: Boolean;
  PayLen: Int64;
  Need, I, Ext: Integer;
  MaskKey: array[0..3] of Byte;
  P: Integer;
begin
  Result := False;
  FFailed := False;
  FOpcode := 0;
  FPayload := '';
  if Length(FRaw) < 2 then
    Exit;
  B1 := Byte(FRaw[1]);
  B2 := Byte(FRaw[2]);
  FOpcode := B1 and $0F;
  Masked := (B2 and $80) <> 0;
  PayLen := B2 and $7F;
  Ext := 0;
  if PayLen = 126 then
    Ext := 2
  else if PayLen = 127 then
    Ext := 8;
  Need := 2 + Ext;
  if Masked then
    Inc(Need, 4);
  if Length(FRaw) < Need then
    Exit;
  P := 3;
  if PayLen = 126 then
  begin
    PayLen := (Int64(Byte(FRaw[3])) shl 8) or Byte(FRaw[4]);
    P := 5;
  end
  else if PayLen = 127 then
  begin
    PayLen := 0;
    for I := 0 to 7 do
      PayLen := (PayLen shl 8) or Int64(Byte(FRaw[3 + I]));
    P := 11;
  end;
  if PayLen > WS_MAX_PAYLOAD then
  begin
    FFailed := True;
    Result := True;
    Exit;
  end;
  if Masked then
  begin
    for I := 0 to 3 do
      MaskKey[I] := Byte(FRaw[P + I]);
    Inc(P, 4);
  end;
  Inc(Need, Integer(PayLen));
  if Length(FRaw) < Need then
    Exit;
  if PayLen > 0 then
  begin
    FPayload := Copy(FRaw, P, Integer(PayLen));
    if Masked then
      for I := 1 to Integer(PayLen) do
        FPayload[I] := AnsiChar(Ord(FPayload[I]) xor MaskKey[(I - 1) mod 4]);
    TagWsRaw(FPayload);
  end;
  Delete(FRaw, 1, Need);
  TagWsRaw(FRaw);
  Result := True;
end;

function TBadgerWsParser.Text: string;
begin
  Result := BadgerWsUtf8ToString(Pointer(FPayload), Length(FPayload));
end;

end.
