unit BadgerJWTUtils;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

{$I ..\..\BadgerPlatform.inc}

interface

uses
  {$IFDEF BADGER_WINDOWS}
    Windows,
  {$ENDIF}
  {$IFDEF FPC}
    {$IFDEF UNIX}
      BaseUnix,
    {$ENDIF}
  {$ENDIF}
  SysUtils, Classes, BadgerUtils;

    function CreateSignature(const AHeader, APayload, ASecret: string): string;
    procedure SaveToken(const AUserID, AToken, AStoragePath: string);
    function LoadToken(const AUserID, AStoragePath: string): string;
    procedure SaveRefreshToken(const AUserID, AToken, AStoragePath: string);
    function LoadRefreshToken(const AUserID, AStoragePath: string): string;
    function DateTimeToUnix(ADateTime: TDateTime): Int64;
    { Epoch Unix em UTC. DateTimeToUnix(Now) usa hora local e produz exp/iat
      deslocados pelo fuso (3h no Brasil) para qualquer outra biblioteca JWT. }
    function UnixNowUtc: Int64;
    { Normaliza AUserID para nome de arquivo: só [A-Za-z0-9._-], demais viram '_'.
      Sem isso, um AUserID vindo do login escreve fora de AStoragePath. }
    function SanitizeTokenOwner(const AUserID: string): string;
    { Nome de arquivo INJETIVO para AUserID (usado na gravacao dos tokens). IDs so
      com [A-Za-z0-9._-] mantem o mesmo nome de SanitizeTokenOwner; o resto vira %XX
      dos bytes UTF-8. SanitizeTokenOwner trocava tudo por '_': 'joao' acentuado e
      'jo_o', ou 'a@b.com' e 'a_b.com', dividiam o arquivo e o login de um invalidava
      a sessao do outro. Tambem escapa ponto inicial e nomes de dispositivo do
      Windows (CON, NUL, COM1...), que quebravam o armazenamento desses usuarios. }
    function TokenOwnerFileName(const AUserID: string): string;

implementation

{$IFDEF BADGER_WINDOWS}
const
  SDDL_REVISION_1 = 1;

type
  PSECURITY_DESCRIPTOR = Pointer;

function ConvertStringSecurityDescriptorToSecurityDescriptorA(
  StringSecurityDescriptor: PAnsiChar;
  StringSDRevision: DWORD;
  out SecurityDescriptor: PSECURITY_DESCRIPTOR;
  SecurityDescriptorSize: Pointer
): BOOL; stdcall; external 'advapi32.dll' name 'ConvertStringSecurityDescriptorToSecurityDescriptorA';

function SetFileSecurityA(
  lpFileName: PAnsiChar;
  SecurityInformation: DWORD;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR
): BOOL; stdcall; external 'advapi32.dll' name 'SetFileSecurityA';
{$ENDIF}

type
  TJWTBytes = array of Byte;
  TDWordArray = array [0 .. 63] of LongWord;

const
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

function DateTimeToUnix(ADateTime: TDateTime): Int64;
const
  UnixStartDate: TDateTime = 25569.0;
begin
  Result := Round((ADateTime - UnixStartDate) * 86400);
end;

function UnixNowUtc: Int64;
{$IFDEF BADGER_WINDOWS}
var
  ST: TSystemTime;
begin
  { GetSystemTime já devolve UTC. }
  GetSystemTime(ST);
  Result := DateTimeToUnix(EncodeDate(ST.wYear, ST.wMonth, ST.wDay) +
                           EncodeTime(ST.wHour, ST.wMinute, ST.wSecond, 0));
end;
{$ELSE}
{$IFDEF FPC}
begin
  { GetLocalTimeOffset: minutos a somar ao local para chegar a UTC. }
  Result := DateTimeToUnix(Now) + Int64(GetLocalTimeOffset) * 60;
end;
{$ELSE}
begin
  Result := DateTimeToUnix(TTimeZone.Local.ToUniversalTime(Now));
end;
{$ENDIF}
{$ENDIF}

function SanitizeTokenOwner(const AUserID: string): string;
var
  I: Integer;
  Ch: Char;
begin
  Result := '';
  for I := 1 to Length(AUserID) do
  begin
    Ch := AUserID[I];
    if ((Ch >= 'A') and (Ch <= 'Z')) or ((Ch >= 'a') and (Ch <= 'z')) or
       ((Ch >= '0') and (Ch <= '9')) or (Ch = '.') or (Ch = '-') or (Ch = '_') then
      Result := Result + Ch
    else
      Result := Result + '_';
  end;
  { '.' e '..' resolveriam para diretório; qualquer nome só de pontos é inseguro. }
  while (Result <> '') and (Result[1] = '.') do
    Result[1] := '_';
  if Result = '' then
    Result := '_';
end;

function TokenOwnerFileName(const AUserID: string): string;
const
  HexDigits: array[0..15] of Char = '0123456789ABCDEF';
  Devices: array[0..21] of string = ('CON', 'PRN', 'AUX', 'NUL',
    'COM1', 'COM2', 'COM3', 'COM4', 'COM5', 'COM6', 'COM7', 'COM8', 'COM9',
    'LPT1', 'LPT2', 'LPT3', 'LPT4', 'LPT5', 'LPT6', 'LPT7', 'LPT8', 'LPT9');
var
  U: AnsiString;
  I, P: Integer;
  B: Byte;
  Base: string;
begin
  U := Utf8Bytes(AUserID);
  Result := '';
  for I := 1 to Length(U) do
  begin
    B := Ord(U[I]);
    if ((B >= Ord('A')) and (B <= Ord('Z'))) or ((B >= Ord('a')) and (B <= Ord('z'))) or
       ((B >= Ord('0')) and (B <= Ord('9'))) or (B = Ord('-')) or (B = Ord('_')) or
       ((B = Ord('.')) and (I > 1)) then
      Result := Result + Char(B)
    else
      Result := Result + '%' + HexDigits[B shr 4] + HexDigits[B and 15];
  end;
  { '%' sozinho nunca sai do laco (todo '%' gerado leva dois hex): ID vazio nao
    colide com ninguem. }
  if Result = '' then
  begin
    Result := '%';
    Exit;
  end;
  P := Pos('.', Result);
  if P > 0 then
    Base := Copy(Result, 1, P - 1)
  else
    Base := Result;
  for I := Low(Devices) to High(Devices) do
    if SameText(Base, Devices[I]) then
    begin
      B := Ord(Result[1]);
      Result := '%' + HexDigits[B shr 4] + HexDigits[B and 15] + Copy(Result, 2, MaxInt);
      Break;
    end;
end;

{ Gravacao usa sempre o nome novo. Leitura aceita o nome antigo quando o novo nao
  existe: sessoes emitidas antes da troca seguem validas ate o proximo login do
  usuario (que grava no nome novo). A assinatura do JWT carrega o user_id, entao
  ler o arquivo legado de outro usuario so resulta em 'Token not found'. }
function TokenFilePath(const AStoragePath, AUserID, AExt: string; AForRead: Boolean): string;
var
  Legacy: string;
begin
  Result := IncludeTrailingPathDelimiter(AStoragePath) + TokenOwnerFileName(AUserID) + AExt;
  if AForRead and not FileExists(Result) then
  begin
    Legacy := IncludeTrailingPathDelimiter(AStoragePath) + SanitizeTokenOwner(AUserID) + AExt;
    if FileExists(Legacy) then
      Result := Legacy;
  end;
end;

procedure ApplyTokenFilePermissions(const AFileName: string);
{$IFDEF BADGER_WINDOWS}
const
  // Owner/SYSTEM/Admins full control, inheritance blocked. IU (Interactive Users)
  // e SU (Service) ficaram fora: dariam o token a qualquer sessão logada na máquina.
  TokenFileSDDL = 'D:P(A;;FA;;;OW)(A;;FA;;;SY)(A;;FA;;;BA)';
var
  SD: PSECURITY_DESCRIPTOR;
  AFileNameAnsi: AnsiString;
{$ENDIF}
begin
  {$IFDEF FPC}
    {$IFDEF UNIX}
      fpchmod(PChar(AFileName), &700);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF BADGER_WINDOWS}
    SD := nil;
    AFileNameAnsi := AnsiString(AFileName);
    if ConvertStringSecurityDescriptorToSecurityDescriptorA(
         PAnsiChar(AnsiString(TokenFileSDDL)),
         SDDL_REVISION_1,
         SD,
         nil) then
    begin
      try
        SetFileSecurityA(PAnsiChar(AFileNameAnsi), DACL_SECURITY_INFORMATION, SD);
      finally
        LocalFree(HLOCAL(SD));
      end;
    end;
  {$ENDIF}
end;

function StringToAnsiBytes(const S: string): TJWTBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
    Result[I - 1] := Byte(AnsiChar(S[I]));
end;

procedure SaveToken(const AUserID, AToken, AStoragePath: string);
var
  LFileName: string;
  FS: TFileStream;
  Buffer: TJWTBytes;
begin
  if AStoragePath = '' then
    Exit;
  ForceDirectories(AStoragePath);
  LFileName := TokenFilePath(AStoragePath, AUserID, '.token', False);
  if FileExists(LFileName) then
    DeleteFile(LFileName);
  FS := TFileStream.Create(LFileName, fmCreate);
  try
    Buffer := StringToAnsiBytes(AToken);
    if Length(Buffer) > 0 then
      FS.WriteBuffer(Buffer[0], Length(Buffer));
  finally
    FS.Free;
  end;
  ApplyTokenFilePermissions(LFileName);
end;

function LoadToken(const AUserID, AStoragePath: string): string;
var
  LFileName: string;
  FS: TFileStream;
  Buffer: TBytes;
begin
  Result := '';
  if AStoragePath = '' then Exit;
  LFileName := TokenFilePath(AStoragePath, AUserID, '.token', True);
  if not FileExists(LFileName) then Exit;

  FS := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyNone);
  try
    if FS.Size > 0 then
    begin
      SetLength(Buffer, FS.Size);
      FS.ReadBuffer(Buffer[0], FS.Size);
      {$IFNDEF FPC}
          {$IF CompilerVersion >= 20}
          Result := TEncoding.ANSI.GetString(Buffer);
          {$ELSE}
          SetString(Result, PAnsiChar(@Buffer[0]), Length(Buffer));
          {$IFEND}
      {$ELSE}
          SetString(Result, PAnsiChar(@Buffer[0]), Length(Buffer));
      {$ENDIF}
    end;
  finally
    FS.Free;
  end;
end;

procedure SaveRefreshToken(const AUserID, AToken, AStoragePath: string);
var
  LFileName: string;
  FS: TFileStream;
  Buffer: TJWTBytes;
begin
  if AStoragePath = '' then
    Exit;
  ForceDirectories(AStoragePath);
  LFileName := TokenFilePath(AStoragePath, AUserID, '.refreshtoken', False);
  if FileExists(LFileName) then
    DeleteFile(LFileName);
  FS := TFileStream.Create(LFileName, fmCreate);
  try
    Buffer := StringToAnsiBytes(AToken);
    if Length(Buffer) > 0 then
      FS.WriteBuffer(Buffer[0], Length(Buffer));
  finally
    FS.Free;
  end;
  ApplyTokenFilePermissions(LFileName);
end;

function LoadRefreshToken(const AUserID, AStoragePath: string): string;
var
  LFileName: string;
  FS: TFileStream;
  Buffer: TBytes;
begin
  Result := '';
  if AStoragePath = '' then Exit;
  LFileName := TokenFilePath(AStoragePath, AUserID, '.refreshtoken', True);
  if not FileExists(LFileName) then Exit;

  FS := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyNone);
  try
    if FS.Size > 0 then
    begin
      SetLength(Buffer, FS.Size);
      FS.ReadBuffer(Buffer[0], FS.Size);
      {$IFNDEF FPC}
          {$IF CompilerVersion >= 20}
          Result := TEncoding.ANSI.GetString(Buffer);
          {$ELSE}
          SetString(Result, PAnsiChar(@Buffer[0]), Length(Buffer));
          {$IFEND}
      {$ELSE}
          SetString(Result, PAnsiChar(@Buffer[0]), Length(Buffer));
      {$ENDIF}
    end;
  finally
    FS.Free;
  end;
end;

function RotateRight(Value: LongWord; Bits: Integer): LongWord;
begin
  Result := (Value shr Bits) or (Value shl (32 - Bits));
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

function SHA256(const Data: TJWTBytes): TJWTBytes;
var
  L: array [0 .. 7] of LongWord;
  W: TDWordArray;
  a, b, c, d, e, f, g, h, T1, T2: LongWord;
  DataLen, PadLen, i, j: Integer;
  PaddedData: TJWTBytes;
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

    a := L[0]; b := L[1]; c := L[2]; d := L[3];
    e := L[4]; f := L[5]; g := L[6]; h := L[7];

    for j := 0 to 63 do
    begin
      TempSum := Int64(h) + Int64(Sigma1(e)) + Int64(Ch(e, f, g)) + Int64(K[j]) + Int64(W[j]);
      T1 := LongWord(TempSum and $FFFFFFFF);
      TempSum := Int64(Sigma0(a)) + Int64(Maj(a, b, c));
      T2 := LongWord(TempSum and $FFFFFFFF);
      h := g; g := f; f := e;
      TempSum := Int64(d) + Int64(T1);
      e := LongWord(TempSum and $FFFFFFFF);
      d := c; c := b; b := a;
      TempSum := Int64(T1) + Int64(T2);
      a := LongWord(TempSum and $FFFFFFFF);
    end;

    TempSum := Int64(L[0]) + Int64(a); L[0] := LongWord(TempSum and $FFFFFFFF);
    TempSum := Int64(L[1]) + Int64(b); L[1] := LongWord(TempSum and $FFFFFFFF);
    TempSum := Int64(L[2]) + Int64(c); L[2] := LongWord(TempSum and $FFFFFFFF);
    TempSum := Int64(L[3]) + Int64(d); L[3] := LongWord(TempSum and $FFFFFFFF);
    TempSum := Int64(L[4]) + Int64(e); L[4] := LongWord(TempSum and $FFFFFFFF);
    TempSum := Int64(L[5]) + Int64(f); L[5] := LongWord(TempSum and $FFFFFFFF);
    TempSum := Int64(L[6]) + Int64(g); L[6] := LongWord(TempSum and $FFFFFFFF);
    TempSum := Int64(L[7]) + Int64(h); L[7] := LongWord(TempSum and $FFFFFFFF);
  end;

  SetLength(Result, 32);
  for i := 0 to 7 do
  begin
    Result[i * 4]     := (L[i] shr 24) and $FF;
    Result[i * 4 + 1] := (L[i] shr 16) and $FF;
    Result[i * 4 + 2] := (L[i] shr 8)  and $FF;
    Result[i * 4 + 3] :=  L[i]         and $FF;
  end;
end;

function BytesToRawStr(const ABytes: TJWTBytes): string;
var
  i: Integer;
begin
  SetLength(Result, Length(ABytes));
  for i := 0 to Length(ABytes) - 1 do
    Result[i + 1] := Chr(ABytes[i]);
end;

function RawStrToBytes(const S: string): TJWTBytes;
var
  i: Integer;
begin
  SetLength(Result, Length(S));
  for i := 1 to Length(S) do
    Result[i - 1] := Byte(AnsiChar(S[i]));
end;

{ Chave e mensagem em bytes UTF-8. RawStrToBytes truncava o byte alto de cada
  caractere: segredo ou claim com acento gerava assinatura que nenhuma outra
  biblioteca JWT reproduz. }
function Utf8StrToJwtBytes(const S: string): TJWTBytes;
var
  Raw: AnsiString;
  I: Integer;
begin
  Raw := Utf8Bytes(S);
  SetLength(Result, Length(Raw));
  for I := 1 to Length(Raw) do
    Result[I - 1] := Byte(Raw[I]);
end;

function BytesToAnsi(const ABytes: TJWTBytes): AnsiString;
var
  I: Integer;
begin
  SetLength(Result, Length(ABytes));
  for I := 0 to Length(ABytes) - 1 do
    Result[I + 1] := AnsiChar(ABytes[I]);
end;

function HMAC_SHA256(const Key, Message: string): TJWTBytes;
const
  BlockSize = 64;
var
  LKey, LMessage: TJWTBytes;
  InnerPad, OuterPad: TJWTBytes;
  i: Integer;
  TempHash: TJWTBytes;
begin
  LKey := Utf8StrToJwtBytes(Key);
  LMessage := Utf8StrToJwtBytes(Message);

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
  Data: string;
  HashBytes: TJWTBytes;
begin
  Data := AHeader + '.' + APayload;
  HashBytes := HMAC_SHA256(ASecret, Data);
  { Assinatura e binaria: base64 sobre os bytes, sem passar por UTF-8. }
  Result := Base64EncodeBytes(BytesToAnsi(HashBytes), True);
end;

end.
