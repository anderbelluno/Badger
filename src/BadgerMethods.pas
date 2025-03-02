unit BadgerMethods;

interface

uses
  blcksock, SysUtils, Classes, DB, BadgerMultipartDataReader, Contnrs, SyUtils;

type
  TBadgerMethods = class(TObject)
  private
    function getMime(aFilePath: String): String;
  public
    function ParseRequestHeaderInt(Headers: TStringList; aRequestHeader: String): Integer;
    function ParseRequestHeaderStr(Headers: TStringList; aRequestHeader: String): String;
    function fParserJsonStream(const Body: string): string;
    function fDownloadStream(const FilePath: string; out MimeType: string): TStream;
    procedure AtuImage(const Body: string; out StatusCode: Integer; out ResponseBody: string);
    function ExtractMethodAndURI(const RequestLine: string; out Method, URI: string; out QueryParams: TStringList): Boolean;
  end;

implementation

function TBadgerMethods.ExtractMethodAndURI(const RequestLine: string; out Method, URI: string; out QueryParams: TStringList): Boolean;
var
  SpacePos, QueryPos: Integer;
  VRequestLine, QueryString, ParamPair: string;
begin
  Result := False;
  QueryParams.Clear; // Limpa os parâmetros anteriores
  VRequestLine := RequestLine;

  // Extrair o método
  SpacePos := Pos(' ', VRequestLine);
  if SpacePos > 0 then
  begin
    Method := Copy(VRequestLine, 1, SpacePos - 1);
    Delete(VRequestLine, 1, SpacePos);

    // Extrair a URI e a querystring
    SpacePos := Pos(' ', VRequestLine);
    if SpacePos > 0 then
    begin
      URI := Copy(VRequestLine, 1, SpacePos - 1);
      QueryPos := Pos('?', URI);
      if QueryPos > 0 then
      begin
        QueryString := Copy(URI, QueryPos + 1, Length(URI));
        URI := Copy(URI, 1, QueryPos - 1); // Remove a querystring da URI

        // Separar os parâmetros da querystring
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
          QueryParams.Add(ParamPair);
        end;
      end;
      Result := True;
    end;
  end;
end;

function TBadgerMethods.getMime(aFilePath: String): String;
var
  sUtils: TSyUtils;
begin
  sUtils := TSyUtils.Create;
  try
    Result := sUtils.GetFileMIMEType(aFilePath);
  finally
    sUtils.Free;
  end;
end;

function TBadgerMethods.ParseRequestHeaderStr(Headers: TStringList; aRequestHeader: String): String;
var
  HeaderLine: string;
  i, PosColon: Integer;
begin
  Result := '';
  for i := 0 to Headers.Count - 1 do
  begin
    HeaderLine := Headers[i];
    if Pos(aRequestHeader, HeaderLine) > 0 then
    begin
      PosColon := Pos(':', HeaderLine);
      if PosColon > 0 then
        Result := Trim(Copy(HeaderLine, PosColon + 1, Length(HeaderLine)));
    end;
  end;
end;

function TBadgerMethods.ParseRequestHeaderInt(Headers: TStringList; aRequestHeader: String): Integer;
var
  HeaderLine: string;
  i, PosColon: Integer;
begin
  Result := 0;
  for i := 0 to Headers.Count - 1 do
  begin
    HeaderLine := Headers[i];
    if Pos(aRequestHeader, HeaderLine) > 0 then
    begin
      PosColon := Pos(':', HeaderLine);
      if PosColon > 0 then
        Result := StrToIntDef(Trim(Copy(HeaderLine, PosColon + 1, Length(HeaderLine))), 0);
    end;
  end;
end;

function TBadgerMethods.fParserJsonStream(const Body: string): string;
begin
  if UpperCase(Body) = 'POST' then
    Result := '{"status":true, "message":"Recebimento concluído com sucesso"}'
  else
    Result := '{"status":false, "message":"Método não aceito, usar POST"}';
end;

function TBadgerMethods.fDownloadStream(const FilePath: string; out MimeType: string): TStream;
var
  FileStream: TFileStream;
begin
  Result := nil;
  if FileExists(FilePath) then
  begin
    FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
    Result := FileStream;
    MimeType := getMime(FilePath);
  end
  else
  begin
    Result := TStringStream.Create('File not found');
    MimeType := 'text/plain';
  end;
end;

procedure TBadgerMethods.AtuImage(const Body: string; out StatusCode: Integer; out ResponseBody: string);
var
  Stream: TMemoryStream;
  Reader: TFormDataReader;
  Files: TObjectList;
  i: Integer;
  FormDataFile: TFormDataFile;
  UniqueName: string;
begin
  Stream := TMemoryStream.Create;
  Reader := TFormDataReader.Create;
  Files := nil;
  try
    if Body <> '' then
    begin
      Stream.WriteBuffer(Body[1], Length(Body));
      Stream.Position := 0;
      Files := Reader.ProcessMultipartFormData(Stream, ''); // Boundary precisa ser ajustado
      for i := 0 to Files.Count - 1 do
      begin
        FormDataFile := TFormDataFile(Files[i]);
        UniqueName := Reader.UniqueFileName(FormDataFile.FileName);
        FormDataFile.Stream.SaveToFile(UniqueName);
      end;
      StatusCode := 200; // OK
      ResponseBody := 'Image processed successfully';
    end
    else
    begin
      StatusCode := 400; // Bad Request
      ResponseBody := 'No image data provided';
    end;
  finally
    Stream.Free;
    if Assigned(Reader) then Reader.Free;
    if Assigned(Files) then Files.Free;
  end;
end;

end.
