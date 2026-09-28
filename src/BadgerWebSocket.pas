unit BadgerWebSocket;

{ RFC 6455 handshake + frames. No sockets — classic handler and IOCP both use this. }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes;

const
  { Teto de recepcao. Nao limita mais o envio: BadgerWsFrame emite comprimento de
    64 bits quando preciso, em vez de descartar o frame em silencio. }
  WS_MAX_PAYLOAD = 65535;
  WS_OP_CONT = 0;
  WS_OP_TEXT = 1;
  WS_OP_BINARY = 2;
  WS_OP_CLOSE = 8;
  WS_OP_PING = 9;
  WS_OP_PONG = 10;
  { Status codes RFC 6455 7.4.1 }
  WS_CLOSE_NORMAL = 1000;
  WS_CLOSE_PROTOCOL = 1002;
  WS_CLOSE_UNSUPPORTED = 1003;
  WS_CLOSE_TOO_BIG = 1009;

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
function BadgerWsBinaryFrame(const Payload: TBadgerWsBytes): TBadgerWsBytes;
{ Close com status: o servidor derrubava o TCP sem frame de close. }
function BadgerWsCloseFrame(Code: Word): TBadgerWsBytes;
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
    FCloseCode: Word;
    FComplete: Boolean;
    FFrag: TBadgerWsBytes;
    FFragOpcode: Byte;
    FFragging: Boolean;
    FMaxPayload: Integer;
    procedure AppendRaw(Buf: Pointer; Len: Integer);
    procedure Fail(ACode: Word);
  public
    constructor Create;
    procedure Feed(Buf: Pointer; Len: Integer);
    { True quando um frame foi consumido. Complete diz se ha mensagem inteira:
      fragmento intermediario (FIN=0) consome bytes e devolve Complete=False. }
    function TryParse: Boolean;
    function Text: string;
    property Opcode: Byte read FOpcode;
    property Payload: TBadgerWsBytes read FPayload;
    property Failed: Boolean read FFailed;
    property Complete: Boolean read FComplete;
    property CloseCode: Word read FCloseCode;
    property MaxPayload: Integer read FMaxPayload write FMaxPayload;
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
  Header: array[0..13] of Byte;
  Mask: array[0..3] of Byte;
begin
  Len := Length(Payload);
  Header[0] := $80 or (Opcode and $0F);
  if Len <= 125 then
  begin
    Header[1] := Byte(Len);
    HeaderSize := 2;
  end
  else if Len <= 65535 then
  begin
    Header[1] := 126;
    Header[2] := Byte((Len shr 8) and $FF);
    Header[3] := Byte(Len and $FF);
    HeaderSize := 4;
  end
  else
  begin
    { Extensao de 64 bits. Antes o frame acima de 64 KiB era descartado devolvendo
      string vazia e o chamador saia em silencio: perda de dados sem erro. }
    Header[1] := 127;
    Header[2] := 0;
    Header[3] := 0;
    Header[4] := 0;
    Header[5] := 0;
    Header[6] := Byte((Len shr 24) and $FF);
    Header[7] := Byte((Len shr 16) and $FF);
    Header[8] := Byte((Len shr 8) and $FF);
    Header[9] := Byte(Len and $FF);
    HeaderSize := 10;
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

function BadgerWsBinaryFrame(const Payload: TBadgerWsBytes): TBadgerWsBytes;
begin
  Result := BadgerWsFrame(WS_OP_BINARY, Payload, False);
end;

function BadgerWsCloseFrame(Code: Word): TBadgerWsBytes;
var
  P: TBadgerWsBytes;
begin
  SetLength(P, 2);
  TagWsRaw(P);
  P[1] := AnsiChar(Byte(Code shr 8));
  P[2] := AnsiChar(Byte(Code and $FF));
  Result := BadgerWsFrame(WS_OP_CLOSE, P, False);
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

constructor TBadgerWsParser.Create;
begin
  inherited Create;
  FMaxPayload := WS_MAX_PAYLOAD;
end;

procedure TBadgerWsParser.Fail(ACode: Word);
begin
  FFailed := True;
  FCloseCode := ACode;
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
  Masked, Fin: Boolean;
  PayLen: Int64;
  Need, I, Ext: Integer;
  MaskKey: array[0..3] of Byte;
  P: Integer;
  Op: Byte;
  Frame: TBadgerWsBytes;
begin
  Result := False;
  FFailed := False;
  FComplete := False;
  FCloseCode := 0;
  FOpcode := 0;
  FPayload := '';
  if Length(FRaw) < 2 then
    Exit;
  B1 := Byte(FRaw[1]);
  B2 := Byte(FRaw[2]);
  Fin := (B1 and $80) <> 0;
  Op := B1 and $0F;
  Masked := (B2 and $80) <> 0;
  PayLen := B2 and $7F;

  { RSV1..3: sem extensao negociada devem ser zero. }
  if (B1 and $70) <> 0 then
  begin
    Fail(WS_CLOSE_PROTOCOL);
    Result := True;
    Exit;
  end;
  if not ((Op = WS_OP_CONT) or (Op = WS_OP_TEXT) or (Op = WS_OP_BINARY) or
          (Op = WS_OP_CLOSE) or (Op = WS_OP_PING) or (Op = WS_OP_PONG)) then
  begin
    Fail(WS_CLOSE_PROTOCOL);
    Result := True;
    Exit;
  end;
  { RFC 6455 5.1: frame de cliente sem mascara obriga a fechar a conexao. }
  if not Masked then
  begin
    Fail(WS_CLOSE_PROTOCOL);
    Result := True;
    Exit;
  end;

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

  { Negativo (bit 63 ligado) passava pelo teste de tamanho, nao copiava payload e
    deixava Delete andar um valor truncado: o resto do buffer virava frame novo e a
    sessao dessincronizava. }
  if (PayLen < 0) or (PayLen > FMaxPayload) then
  begin
    Fail(WS_CLOSE_TOO_BIG);
    Result := True;
    Exit;
  end;
  { Frame de controle: FIN obrigatorio e no maximo 125 bytes. }
  if (Op >= WS_OP_CLOSE) and ((not Fin) or (PayLen > 125)) then
  begin
    Fail(WS_CLOSE_PROTOCOL);
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

  Frame := '';
  if PayLen > 0 then
  begin
    Frame := Copy(FRaw, P, Integer(PayLen));
    for I := 1 to Integer(PayLen) do
      Frame[I] := AnsiChar(Ord(Frame[I]) xor MaskKey[(I - 1) mod 4]);
    TagWsRaw(Frame);
  end;
  Delete(FRaw, 1, Need);
  TagWsRaw(FRaw);
  Result := True;

  { Controle atravessa a montagem de fragmentos sem interferir nela. }
  if Op >= WS_OP_CLOSE then
  begin
    FOpcode := Op;
    FPayload := Frame;
    FComplete := True;
    if (Op = WS_OP_CLOSE) and (Length(Frame) >= 2) then
      FCloseCode := (Byte(Frame[1]) shl 8) or Byte(Frame[2]);
    Exit;
  end;

  if Op = WS_OP_CONT then
  begin
    if not FFragging then
    begin
      Fail(WS_CLOSE_PROTOCOL);
      Exit;
    end;
  end
  else
  begin
    if FFragging then
    begin
      { Frame de dados novo no meio de uma mensagem fragmentada. }
      Fail(WS_CLOSE_PROTOCOL);
      Exit;
    end;
    FFragOpcode := Op;
    FFrag := '';
    FFragging := True;
  end;

  if Length(FFrag) + Length(Frame) > FMaxPayload then
  begin
    FFragging := False;
    FFrag := '';
    Fail(WS_CLOSE_TOO_BIG);
    Exit;
  end;
  FFrag := FFrag + Frame;
  TagWsRaw(FFrag);

  if Fin then
  begin
    FOpcode := FFragOpcode;
    FPayload := FFrag;
    FComplete := True;
    FFragging := False;
    FFrag := '';
  end;
end;

function TBadgerWsParser.Text: string;
begin
  Result := BadgerWsUtf8ToString(Pointer(FPayload), Length(FPayload));
end;

end.
