unit BadgerHttpUtils;

interface

uses
  SysUtils, Classes;

function URLDecode(const Value: string): string;
function JSONEscape(const Value: string): string;
function TryGetHeaderValue(Headers: TStringList; const HeaderName: string; out HeaderValue: string): Boolean;
function TryGetHeaderInt(Headers: TStringList; const HeaderName: string; out HeaderValue: Integer): Boolean;

implementation

function URLDecode(const Value: string): string;
var
  I: Integer;
  HexValue: string;
  Code: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(Value) do
  begin
    if Value[I] = '+' then
      Result := Result + ' '
    else if (Value[I] = '%') and (I + 2 <= Length(Value)) then
    begin
      HexValue := Copy(Value, I + 1, 2);
      Code := StrToIntDef('$' + HexValue, -1);
      if (Code >= 0) and (Code <= 255) then
      begin
        Result := Result + Chr(Code);
        Inc(I, 2);
      end
      else
        Result := Result + Value[I];
    end
    else
      Result := Result + Value[I];
    Inc(I);
  end;
end;

function JSONEscape(const Value: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(Value) do
  begin
    C := Value[I];
    case C of
      '"':  Result := Result + '\"';
      '\':  Result := Result + '\\';
      '/':  Result := Result + '\/';
      #8:   Result := Result + '\b';
      #9:   Result := Result + '\t';
      #10:  Result := Result + '\n';
      #12:  Result := Result + '\f';
      #13:  Result := Result + '\r';
    else
      if Ord(C) < 32 then
        Result := Result + '\u' + IntToHex(Ord(C), 4)
      else
        Result := Result + C;
    end;
  end;
end;

function TryGetHeaderValue(Headers: TStringList; const HeaderName: string; out HeaderValue: string): Boolean;
var
  HeaderLine: string;
  ParsedHeaderName: string;
  I, PosColon: Integer;
begin
  Result := False;
  HeaderValue := '';
  for I := 0 to Headers.Count - 1 do
  begin
    HeaderLine := Headers[I];
    PosColon := Pos(':', HeaderLine);
    if PosColon > 0 then
    begin
      ParsedHeaderName := Trim(Copy(HeaderLine, 1, PosColon - 1));
      if SameText(ParsedHeaderName, HeaderName) then
      begin
        HeaderValue := Trim(Copy(HeaderLine, PosColon + 1, Length(HeaderLine)));
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TryGetHeaderInt(Headers: TStringList; const HeaderName: string; out HeaderValue: Integer): Boolean;
var
  RawValue: string;
begin
  Result := TryGetHeaderValue(Headers, HeaderName, RawValue);
  if Result then
    HeaderValue := StrToIntDef(RawValue, 0)
  else
    HeaderValue := 0;
end;

end.
