unit BadgerHttpUtils;

interface

uses
  SysUtils, Classes;

function URLDecode(const Value: string): string;
function JSONEscape(const Value: string): string;
function TryGetHeaderValue(Headers: TStringList; const HeaderName: string; out HeaderValue: string): Boolean;
function TryGetHeaderInt(Headers: TStringList; const HeaderName: string; out HeaderValue: Integer): Boolean;

implementation

uses
  BadgerHttpParser;

function URLDecode(const Value: string): string;
begin
  { Implementacao unica em BadgerHttpParser, compartilhada pelos tres motores. }
  Result := BadgerUrlDecode(Value);
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
  Idx: Integer;
begin
  { Os headers sao gravados como 'Nome=Valor'. Procurar ':' aqui fazia esta funcao
    (e ParseRequestHeaderStr/Int, que dependem dela) devolver vazio para header
    que existe. }
  Result := False;
  HeaderValue := '';
  if not Assigned(Headers) then
    Exit;
  Idx := Headers.IndexOfName(HeaderName);
  if Idx < 0 then
    Exit;
  HeaderValue := Trim(Headers.ValueFromIndex[Idx]);
  Result := True;
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
