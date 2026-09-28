unit BadgerUploadUtils;

interface

uses
  SysUtils;

function SanitizeUploadFileName(const AFileName: string): string;

implementation

function SanitizeUploadFileName(const AFileName: string): string;
var
  I: Integer;
  Ch: Char;
  NameOnly: string;
  G: TGUID;
  GuidText: string;
begin
  NameOnly := Trim(AFileName);
  NameOnly := StringReplace(NameOnly, '/', PathDelim, [rfReplaceAll]);
  NameOnly := StringReplace(NameOnly, '\', PathDelim, [rfReplaceAll]);
  NameOnly := ExtractFileName(NameOnly);

  Result := '';
  for I := 1 to Length(NameOnly) do
  begin
    Ch := NameOnly[I];
    { Comparacao explicita em vez de 'in': com WideChar o compilador reduz a byte
      (W1050) e um caractere acima de 255 podia casar pelo byte baixo. }
    if (Ord(Ch) < 32) or (Ord(Ch) > 126) or
       (Ch = '\') or (Ch = '/') or (Ch = ':') or (Ch = '*') or (Ch = '?') or
       (Ch = '"') or (Ch = '<') or (Ch = '>') or (Ch = '|') then
      Result := Result + '_'
    else
      Result := Result + Ch;
  end;

  while Pos('..', Result) > 0 do
    Result := StringReplace(Result, '..', '_', [rfReplaceAll]);

  Result := Trim(Result);
  if (Result = '') or (Result = '.') or (Result = '..') then
  begin
    if CreateGUID(G) = 0 then
    begin
      GuidText := GUIDToString(G);
      GuidText := StringReplace(GuidText, '{', '', [rfReplaceAll]);
      GuidText := StringReplace(GuidText, '}', '', [rfReplaceAll]);
      GuidText := StringReplace(GuidText, '-', '', [rfReplaceAll]);
    end
    else
      GuidText := FormatDateTime('yyyymmddhhnnsszzz', Now);
    Result := 'unnamed_file_' + GuidText + '.bin';
  end;
end;

end.
