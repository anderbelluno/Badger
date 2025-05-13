unit BadgerJWTUtils;

interface

uses
  SysUtils, Classes, EncdDecd;

function CreateSignature(const AHeader, APayload, ASecret: string): string;
procedure SaveToken(const AUserID, AToken, AStoragePath: string);
function LoadToken(const AUserID, AStoragePath: string): string;
function DateTimeToUnix(ADateTime: TDateTime): Int64;

implementation

type
  TBytes = array of Byte;
  TDWordArray = array[0..63] of LongWord;

const
  // Constantes para SHA256
  K: array[0..63] of LongWord = (
    $428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5, $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5,
    $D807AA98, $12835B01, $243185BE, $550C7DC3, $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174,
    $E49B69C1, $EFBE4786, $0FC19DC6, $240CA1CC, $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA,
    $983E5152, $A831C66D, $B00327C8, $BF597FC7, $C6E00BF3, $D5A79147, $06CA6351, $14292967,
    $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13, $650A7354, $766A0ABB, $81C2C92E, $92722C85,
    $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3, $D192E819, $D6990624, $F40E3585, $106AA070,
    $19A4C116, $1E376C08, $2748774C, $34B0BCB5, $391C0CB3, $4ED8AA4A, $5B9CCA4F, $682E6FF3,
    $748F82EE, $78A5636F, $84C87814, $8CC70208, $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2
  );
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
begin
  if AStoragePath = '' then Exit;
  ForceDirectories(AStoragePath);
  LFileName := IncludeTrailingPathDelimiter(AStoragePath) + AUserID + '.token';
  FS := TFileStream.Create(LFileName, fmCreate);
  try
    FS.WriteBuffer(Pointer(AToken)^, Length(AToken));
  finally
    FS.Free;
  end;
end;

function LoadToken(const AUserID, AStoragePath: string): string;
var
  LFileName: string;
  FS: TFileStream;
begin
  Result := '';
  if AStoragePath = '' then Exit;
  LFileName := IncludeTrailingPathDelimiter(AStoragePath) + AUserID + '.token';
  if not FileExists(LFileName) then Exit;
  FS := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, FS.Size);
    FS.ReadBuffer(Pointer(Result)^, FS.Size);
  finally
    FS.Free;
  end;
end;

function ToBase64URL(const S: string): string;
begin
  Result := StringReplace(EncodeString(S), '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
  Result := StringReplace(Result, CRLF, '', [rfReplaceAll]);
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
  Result := (RotateRight(x, 7) and $FFFFFFFF) xor
            (RotateRight(x, 18) and $FFFFFFFF) xor
            ((x shr 3) and $FFFFFFFF);
end;

function Gamma1(x: LongWord): LongWord;
begin
  Result := (RotateRight(x, 17) and $FFFFFFFF) xor
            (RotateRight(x, 19) and $FFFFFFFF) xor
            ((x shr 10) and $FFFFFFFF);
end;

function SHA256(const Data: TBytes): TBytes;
var
  L: array[0..7] of LongWord;
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
      W[j] := (LongWord(PaddedData[i*64 + j*4]) shl 24) or
              (LongWord(PaddedData[i*64 + j*4 + 1]) shl 16) or
              (LongWord(PaddedData[i*64 + j*4 + 2]) shl 8) or
              LongWord(PaddedData[i*64 + j*4 + 3]);

    for j := 16 to 63 do
    begin
      TempSum := Int64(Gamma1(W[j-2]));
      TempSum := (TempSum + Int64(W[j-7])) and $FFFFFFFF;
      TempSum := (TempSum + Int64(Gamma0(W[j-15]))) and $FFFFFFFF;
      TempSum := (TempSum + Int64(W[j-16])) and $FFFFFFFF;
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
      TempSum := Int64(h) + Int64(Sigma1(e)) + Int64(Ch(e, f, g)) + Int64(K[j]) + Int64(W[j]);
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
    Result[i*4] := (L[i] shr 24) and $FF;
    Result[i*4 + 1] := (L[i] shr 16) and $FF;
    Result[i*4 + 2] := (L[i] shr 8) and $FF;
    Result[i*4 + 3] := L[i] and $FF;
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

function CreateSignature(const AHeader, APayload, ASecret: string): string;
var
  EncHeader, EncPayload, Data: string;
  HashBytes: TBytes;
begin
  // Codifica header e payload em Base64URL se não estiverem já codificados
  EncHeader := ToBase64URL(AHeader);
  EncPayload := ToBase64URL(APayload);
  Data := EncHeader + '.' + EncPayload;
  HashBytes := HMAC_SHA256(ASecret, Data);
  Result := ToBase64URL(EncodeString(BytesToRawString(HashBytes)));
end;

end.
