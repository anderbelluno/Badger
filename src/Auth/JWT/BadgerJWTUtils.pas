unit BadgerJWTUtils;

interface

uses
  SysUtils, Classes;

function CreateSignature(const AHeader, APayload, ASecret: string): string;
function CustomEncodeBase64(const Input: string; URLSafe: Boolean): string;
function CustomDecodeBase64(const Input: string): string;
procedure SaveToken(const AUserID, AToken, AStoragePath: string);
function LoadToken(const AUserID, AStoragePath: string): string;
procedure SaveRefreshToken(const AUserID, AToken, AStoragePath: string);
function LoadRefreshToken(const AUserID, AStoragePath: string): string;
function DateTimeToUnix(ADateTime: TDateTime): Int64;

implementation

type
  TBytes = array of Byte;
  TDWordArray = array [0 .. 63] of LongWord;

const
  // Alfabeto Base64
  Base64Alphabet: array [0 .. 63] of Char = ('A', 'B', 'C', 'D', 'E', 'F', 'G',
    'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
    'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
    'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/');

  // Constantes para SHA256
  K: array [0 .. 63] of LongWord = ($428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5,
    $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5, $D807AA98, $12835B01, $243185BE,
    $550C7DC3, $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174, $E49B69C1, $EFBE4786,
    $0FC19DC6, $240CA1CC, $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA, $983E5152,
    $A831C66D, $B00327C8, $BF597FC7, $C6E00BF3, $D5A79147, $06CA6351, $14292967,
    $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13, $650A7354, $766A0ABB, $81C2C92E,
    $92722C85, $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3, $D192E819, $D6990624,
    $F40E3585, $106AA070, $19A4C116, $1E376C08, $2748774C, $34B0BCB5, $391C0CB3,
    $4ED8AA4A, $5B9CCA4F, $682E6FF3, $748F82EE, $78A5636F, $84C87814, $8CC70208,
    $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2);
  CRLF = #13#10;

function DateTimeToUnix(ADateTime: TDateTime): Int64;
const
  UnixStartDate: TDateTime = 25569.0; // 01/01/1970
begin
  Result := Round((ADateTime - UnixStartDate) * 86400);
end;

procedure SaveToken(const AUserID, AToken, AStoragePath: string);
var
  LFileName: string;
  FS: TFileStream;
{$IFDEF UNICODE}
  Buffer: TArray<Byte>;
{$ELSE}
  Buffer: TBytes;
{$ENDIF}
begin
  if AStoragePath = '' then
    Exit;
  ForceDirectories(AStoragePath);
  LFileName := IncludeTrailingPathDelimiter(AStoragePath) + AUserID + '.token';
  If FileExists(LFileName) then
    DeleteFile(LFileName);
  FS := TFileStream.Create(LFileName, fmCreate);
  try
{$IFDEF UNICODE}
    Buffer := TEncoding.ANSI.GetBytes(AToken);
{$ELSE}
    Buffer := TBytes(AToken);
{$ENDIF}
    if Length(Buffer) > 0 then
      FS.WriteBuffer(Buffer[0], Length(Buffer));
  finally
    FS.Free;
  end;
end;

function LoadToken(const AUserID, AStoragePath: string): string;
var
  LFileName: string;
  FS: TFileStream;
  Buffer: TBytes;
begin
  Result := '';
  if AStoragePath = '' then Exit;
  LFileName := IncludeTrailingPathDelimiter(AStoragePath) + AUserID + '.token';
  if not FileExists(LFileName) then Exit;

  FS := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyNone);
  try
    if FS.Size > 0 then
    begin
      SetLength(Buffer, FS.Size);
      FS.ReadBuffer(Buffer[0], FS.Size);
      {$IF CompilerVersion >= 20}
      Result := TEncoding.ANSI.GetString(Buffer);
      {$ELSE}
      SetString(Result, PAnsiChar(@Buffer[0]), Length(Buffer));
      {$IFEND}
    end;
  finally
    FS.Free;
  end;
end;

procedure SaveRefreshToken(const AUserID, AToken, AStoragePath: string);
var
  LFileName: string;
  FS: TFileStream;
{$IFDEF UNICODE}
  Buffer: TArray<Byte>;
{$ELSE}
  Buffer: TBytes;
{$ENDIF}
begin
  if AStoragePath = '' then
    Exit;
  ForceDirectories(AStoragePath);
  LFileName := IncludeTrailingPathDelimiter(AStoragePath) + AUserID + '.refreshtoken';
  If FileExists(LFileName) then
    DeleteFile(LFileName);
  FS := TFileStream.Create(LFileName, fmCreate);
  try
{$IF CompilerVersion >= 20}
    Buffer := TEncoding.ANSI.GetBytes(AToken);
{$ELSE}
    Buffer := TBytes(AToken);
{$IFEND}
    if Length(Buffer) > 0 then
      FS.WriteBuffer(Buffer[0], Length(Buffer));
  finally
    FS.Free;
  end;
end;

function LoadRefreshToken(const AUserID, AStoragePath: string): string;
var
  LFileName: string;
  FS: TFileStream;
  Buffer: TBytes;
begin
  Result := '';
  if AStoragePath = '' then
    Exit;
  LFileName := IncludeTrailingPathDelimiter(AStoragePath) + AUserID +
    '.refreshtoken';
  if not FileExists(LFileName) then
    Exit;

 FS := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyNone);
  try
    if FS.Size > 0 then
    begin
      SetLength(Buffer, FS.Size);
      FS.ReadBuffer(Buffer[0], FS.Size);
      {$IF CompilerVersion >= 20}
      Result := TEncoding.ANSI.GetString(Buffer);
      {$ELSE}
      SetString(Result, PAnsiChar(@Buffer[0]), Length(Buffer));
      {$IFEND}
    end;
  finally
    FS.Free;
  end;
end;

function RotateRight(Value: LongWord; Bits: Integer): LongWord;
begin
  Result := (Value shr Bits) or (Value shl (32 - Bits));
end;

function RotateLeft(Value: LongWord; Bits: Integer): LongWord;
begin
  Result := (Value shl Bits) or (Value shr (32 - Bits));
end;

function Ch(x, y, z: LongWord): LongWord;
begin
  Result := (x and y) xor ((not x) and z);
end;

function Maj(x, y, z: LongWord): LongWord;
begin
  Result := (x and y) xor (x and z) xor (y and z);
end;

function Sigma0(x: LongWord): LongWord;
begin
  Result := RotateRight(x, 2) xor RotateRight(x, 13) xor RotateRight(x, 22);
end;

function Sigma1(x: LongWord): LongWord;
begin
  Result := RotateRight(x, 6) xor RotateRight(x, 11) xor RotateRight(x, 25);
end;

function Gamma0(x: LongWord): LongWord;
begin
  Result := RotateRight(x, 7) xor RotateRight(x, 18) xor (x shr 3);
end;

function Gamma1(x: LongWord): LongWord;
begin
  Result := RotateRight(x, 17) xor RotateRight(x, 19) xor (x shr 10);
end;

function SHA256(const Data: TBytes): TBytes;
var
  L: array [0 .. 7] of LongWord;
  W: TDWordArray;
  a, b, c, d, e, f, g, h, T1, T2: LongWord;
  DataLen, PadLen, i, j: Integer;
  PaddedData: TBytes;
  LenInBits: UInt64;
  TempSum: Int64;
begin
  L[0] := $6A09E667;
  L[1] := $BB67AE85;
  L[2] := $3C6EF372;
  L[3] := $A54FF53A;
  L[4] := $510E527F;
  L[5] := $9B05688C;
  L[6] := $1F83D9AB;
  L[7] := $5BE0CD19;

  DataLen := Length(Data);
  PadLen := (56 - (DataLen + 1) mod 64) mod 64;
  SetLength(PaddedData, DataLen + 1 + PadLen + 8);
  Move(Data[0], PaddedData[0], DataLen);
  PaddedData[DataLen] := $80;
  for i := DataLen + 1 to DataLen + PadLen do
    PaddedData[i] := 0;

  LenInBits := UInt64(DataLen) * 8;
  PaddedData[Length(PaddedData) - 8] := Byte(LenInBits shr 56);
  PaddedData[Length(PaddedData) - 7] := Byte(LenInBits shr 48);
  PaddedData[Length(PaddedData) - 6] := Byte(LenInBits shr 40);
  PaddedData[Length(PaddedData) - 5] := Byte(LenInBits shr 32);
  PaddedData[Length(PaddedData) - 4] := Byte(LenInBits shr 24);
  PaddedData[Length(PaddedData) - 3] := Byte(LenInBits shr 16);
  PaddedData[Length(PaddedData) - 2] := Byte(LenInBits shr 8);
  PaddedData[Length(PaddedData) - 1] := Byte(LenInBits);

  for i := 0 to (Length(PaddedData) div 64) - 1 do
  begin
    for j := 0 to 15 do
      W[j] := (LongWord(PaddedData[i * 64 + j * 4]) shl 24) or
        (LongWord(PaddedData[i * 64 + j * 4 + 1]) shl 16) or
        (LongWord(PaddedData[i * 64 + j * 4 + 2]) shl 8) or
        LongWord(PaddedData[i * 64 + j * 4 + 3]);

    for j := 16 to 63 do
    begin
      TempSum := Int64(Gamma1(W[j - 2]));
      TempSum := (TempSum + Int64(W[j - 7])) and $FFFFFFFF;
      TempSum := (TempSum + Int64(Gamma0(W[j - 15]))) and $FFFFFFFF;
      TempSum := (TempSum + Int64(W[j - 16])) and $FFFFFFFF;
      W[j] := LongWord(TempSum);
    end;

    a := L[0];
    b := L[1];
    c := L[2];
    d := L[3];
    e := L[4];
    f := L[5];
    g := L[6];
    h := L[7];

    for j := 0 to 63 do
    begin
      TempSum := Int64(h) + Int64(Sigma1(e)) + Int64(Ch(e, f, g)) + Int64(K[j])
        + Int64(W[j]);
      T1 := LongWord(TempSum and $FFFFFFFF);
      TempSum := Int64(Sigma0(a)) + Int64(Maj(a, b, c));
      T2 := LongWord(TempSum and $FFFFFFFF);
      h := g;
      g := f;
      f := e;
      TempSum := Int64(d) + Int64(T1);
      e := LongWord(TempSum and $FFFFFFFF);
      d := c;
      c := b;
      b := a;
      TempSum := Int64(T1) + Int64(T2);
      a := LongWord(TempSum and $FFFFFFFF);
    end;

    TempSum := Int64(L[0]) + Int64(a);
    L[0] := LongWord(TempSum and $FFFFFFFF);
    TempSum := Int64(L[1]) + Int64(b);
    L[1] := LongWord(TempSum and $FFFFFFFF);
    TempSum := Int64(L[2]) + Int64(c);
    L[2] := LongWord(TempSum and $FFFFFFFF);
    TempSum := Int64(L[3]) + Int64(d);
    L[3] := LongWord(TempSum and $FFFFFFFF);
    TempSum := Int64(L[4]) + Int64(e);
    L[4] := LongWord(TempSum and $FFFFFFFF);
    TempSum := Int64(L[5]) + Int64(f);
    L[5] := LongWord(TempSum and $FFFFFFFF);
    TempSum := Int64(L[6]) + Int64(g);
    L[6] := LongWord(TempSum and $FFFFFFFF);
    TempSum := Int64(L[7]) + Int64(h);
    L[7] := LongWord(TempSum and $FFFFFFFF);
  end;

  SetLength(Result, 32);
  for i := 0 to 7 do
  begin
    Result[i * 4] := (L[i] shr 24) and $FF;
    Result[i * 4 + 1] := (L[i] shr 16) and $FF;
    Result[i * 4 + 2] := (L[i] shr 8) and $FF;
    Result[i * 4 + 3] := L[i] and $FF;
  end;
end;

function BytesToRawString(const ABytes: TBytes): string;
var
  i: Integer;
begin
  SetLength(Result, Length(ABytes));
  for i := 0 to Length(ABytes) - 1 do
    Result[i + 1] := Chr(ABytes[i]);
end;

function RawStringToBytes(const S: string): TBytes;
var
  i: Integer;
begin
  SetLength(Result, Length(S));
  for i := 1 to Length(S) do
    Result[i - 1] := Byte(AnsiChar(S[i]));
end;

function HMAC_SHA256(const Key, Message: string): TBytes;
const
  BlockSize = 64;
var
  LKey, LMessage: TBytes;
  InnerPad, OuterPad: TBytes;
  i: Integer;
  TempHash: TBytes;
begin
  LKey := RawStringToBytes(Key);
  LMessage := RawStringToBytes(Message);

  if Length(LKey) > BlockSize then
    LKey := SHA256(LKey);

  if Length(LKey) < BlockSize then
    SetLength(LKey, BlockSize);

  SetLength(InnerPad, BlockSize);
  SetLength(OuterPad, BlockSize);

  for i := 0 to BlockSize - 1 do
  begin
    InnerPad[i] := LKey[i] xor $36;
    OuterPad[i] := LKey[i] xor $5C;
  end;

  SetLength(TempHash, Length(InnerPad) + Length(LMessage));
  Move(InnerPad[0], TempHash[0], Length(InnerPad));
  Move(LMessage[0], TempHash[Length(InnerPad)], Length(LMessage));
  TempHash := SHA256(TempHash);

  SetLength(LMessage, Length(OuterPad) + Length(TempHash));
  Move(OuterPad[0], LMessage[0], Length(OuterPad));
  Move(TempHash[0], LMessage[Length(OuterPad)], Length(TempHash));
  Result := SHA256(LMessage);
end;

function CustomEncodeBase64(const Input: string; URLSafe: Boolean): string;
var
  Bytes: TBytes;
  i, Len, Pos: Integer;
  OutLen: Integer;
  Buffer: array [0 .. 3] of Char;
  RemainingBytes: Integer;
  TempIndex: Integer;
begin
  SetLength(Bytes, Length(Input));
  for i := 1 to Length(Input) do
    Bytes[i - 1] := Byte(AnsiChar(Input[i]));

  Len := Length(Bytes);
  OutLen := ((Len + 2) div 3) * 4;
  SetLength(Result, OutLen);
  Pos := 1;
  i := 0;

  while i < Len do
  begin
    RemainingBytes := Len - i;

    Buffer[0] := Base64Alphabet[(Bytes[i] shr 2) and 63];

    if RemainingBytes > 1 then
      Buffer[1] := Base64Alphabet
        [((Bytes[i] shl 4) or ((Bytes[i + 1] shr 4) and 15)) and 63]
    else
      Buffer[1] := Base64Alphabet[(Bytes[i] shl 4) and 63];

    if RemainingBytes > 1 then
    begin
      TempIndex := (Bytes[i + 1] shl 2) and 63;
      if RemainingBytes > 2 then
        TempIndex := TempIndex or ((Bytes[i + 2] shr 6) and 3);
      Buffer[2] := Base64Alphabet[TempIndex and 63];
    end
    else
      Buffer[2] := '=';

    if RemainingBytes > 2 then
      Buffer[3] := Base64Alphabet[Bytes[i + 2] and 63]
    else
      Buffer[3] := '=';

    Result[Pos] := Buffer[0];
    Result[Pos + 1] := Buffer[1];
    Result[Pos + 2] := Buffer[2];
    Result[Pos + 3] := Buffer[3];

    Inc(i, 3);
    Inc(Pos, 4);
  end;

  if URLSafe then
  begin
    Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
    Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
    Result := StringReplace(Result, '=', '', [rfReplaceAll]);
  end;
end;

function CustomDecodeBase64(const Input: string): string;
var
  Bytes: TBytes;
  i, Len, Pos: Integer;
  InBuf: array [0 .. 3] of Byte;
  OutBuf: array [0 .. 2] of Byte;
  Base64Table: array [Char] of Byte;
  CleanInput: string;
begin
  FillChar(Base64Table, SizeOf(Base64Table), 255);
  for i := 0 to 63 do
    Base64Table[Base64Alphabet[i]] := i;
  Base64Table['-'] := Base64Table['+'];
  Base64Table['_'] := Base64Table['/'];

  CleanInput := StringReplace(Input, '-', '+', [rfReplaceAll]);
  CleanInput := StringReplace(CleanInput, '_', '/', [rfReplaceAll]);
  case Length(CleanInput) mod 4 of
    2:
      CleanInput := CleanInput + '==';
    3:
      CleanInput := CleanInput + '=';
  end;

  Len := Length(CleanInput);
  SetLength(Bytes, (Len * 3) div 4);
  Pos := 0;

  i := 1;
  while i <= Len do
  begin
    InBuf[0] := Base64Table[CleanInput[i]];
    InBuf[1] := Base64Table[CleanInput[i + 1]];
    InBuf[2] := Base64Table[CleanInput[i + 2]];
    InBuf[3] := Base64Table[CleanInput[i + 3]];

    OutBuf[0] := (InBuf[0] shl 2) or ((InBuf[1] shr 4) and 3);
    OutBuf[1] := ((InBuf[1] shl 4) and $F0) or ((InBuf[2] shr 2) and $0F);
    OutBuf[2] := ((InBuf[2] shl 6) and $C0) or (InBuf[3] and $3F);

    Bytes[Pos] := OutBuf[0];
    if CleanInput[i + 2] <> '=' then
      Bytes[Pos + 1] := OutBuf[1];
    if CleanInput[i + 3] <> '=' then
      Bytes[Pos + 2] := OutBuf[2];

    Inc(i, 4);
    Inc(Pos, 3);
  end;

  if CleanInput[Len] = '=' then
    Dec(Pos);
  if CleanInput[Len - 1] = '=' then
    Dec(Pos);
  SetLength(Bytes, Pos);

  SetLength(Result, Length(Bytes));
  for i := 0 to Length(Bytes) - 1 do
    Result[i + 1] := Chr(Bytes[i]);
end;

function CreateSignature(const AHeader, APayload, ASecret: string): string;
var
  Data: string;
  HashBytes: TBytes;
begin
  Data := AHeader + '.' + APayload;
  HashBytes := HMAC_SHA256(ASecret, Data);
  Result := CustomEncodeBase64(BytesToRawString(HashBytes), True);
end;

end.
