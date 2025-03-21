unit BadgerMethods;

interface

uses
  blcksock, SysUtils, Classes, DB, BadgerMultipartDataReader, Contnrs, SyUtils,
  BadgerTypes, BadgerHttpStatus;

type
  TBadgerMethods = class(TObject)
  private
    function getMime(aFilePath: String): String;
  public
    function ParseRequestHeaderInt(Headers: TStringList; aRequestHeader: String): Integer;
    function ParseRequestHeaderStr(Headers: TStringList; aRequestHeader: String): String;
    function fParserJsonStream(Request: THTTPRequest; Response : THTTPResponse): string;
    function fDownloadStream(const FilePath: string; out MimeType: string): TStream;
    procedure AtuImage(Request: THTTPRequest; out Response: THTTPResponse);
    function ExtractMethodAndURI(const RequestLine: string; out Method, URI: string; out QueryParams: TStringList): Boolean;
    function ExtractBoundary(const ContentType: string): string;
  end;

implementation

function TBadgerMethods.ExtractMethodAndURI(const RequestLine: string; out Method, URI: string; out QueryParams: TStringList): Boolean;
var
  SpacePos, QueryPos: Integer;
  VRequestLine, QueryString, ParamPair: string;
begin
  Result := False;
  QueryParams.Clear;
  VRequestLine := RequestLine;

  SpacePos := Pos(' ', VRequestLine);
  if SpacePos > 0 then
  begin
    Method := Copy(VRequestLine, 1, SpacePos - 1);
    Delete(VRequestLine, 1, SpacePos);

    SpacePos := Pos(' ', VRequestLine);
    if SpacePos > 0 then
    begin
      URI := Copy(VRequestLine, 1, SpacePos - 1);
      QueryPos := Pos('?', URI);
      if QueryPos > 0 then
      begin
        QueryString := Copy(URI, QueryPos + 1, Length(URI));
        URI := Copy(URI, 1, QueryPos - 1);

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

function TBadgerMethods.fParserJsonStream( Request: THTTPRequest; Response : THTTPResponse ): string;
begin
  if UpperCase(Request.Method) = 'POST' then
    Result := '{"status":true, "message":"Recebimento concluído com sucesso", "Vc me mandou":"' + Request.Body + '"}'
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
    MimeType := TEXT_PLAIN;
  end;
end;

procedure TBadgerMethods.AtuImage(Request: THTTPRequest; out Response: THTTPResponse);
var
  Reader: TFormDataReader;
  Files: TObjectList;
  i: Integer;
  FormDataFile: TFormDataFile;
  UniqueName: string;
  V_Boundary    : string;
begin
  Reader := TFormDataReader.Create;
  Files := nil;
  try
    if (Request.BodyStream <> nil) and (Request.BodyStream.Size > 0)then
    begin
      V_Boundary := ExtractBoundary(Request.Headers.Values['Content-Type']);
      Files := Reader.ProcessMultipartFormData(Request.BodyStream, V_Boundary);
      for i := 0 to Files.Count - 1 do
      begin
        FormDataFile := TFormDataFile(Files[i]);
        UniqueName := Reader.UniqueFileName(FormDataFile.FileName);
        FormDataFile.Stream.SaveToFile(UniqueName);
      end;
      Response.StatusCode := HTTP_OK; // OK
      Response.Body := 'Image processed successfully';
    end
    else
    begin
      Response.StatusCode := HTTP_BAD_REQUEST;//   404; // Bad Request
      Response.Body := 'No image data provided';
    end;
  finally
    if Assigned(Reader) then Reader.Free;
    if Assigned(Files) then Files.Free;
  end;
end;

function PosEx(const SubStr, S: string; Offset: Integer = 1): Integer;
var
  I: Integer;
begin
  if Offset > Length(S) then
  begin
    Result := 0;
    Exit;
  end;
  for I := Offset to Length(S) - Length(SubStr) + 1 do
    if Copy(S, I, Length(SubStr)) = SubStr then
    begin
      Result := I;
      Exit;
    end;
  Result := 0;
end;

function TBadgerMethods.ExtractBoundary(const ContentType: string): string;
var
  BoundaryStart, BoundaryEnd: Integer;
begin
  Result := '';
  BoundaryStart := Pos('boundary=', ContentType);
  if BoundaryStart > 0 then
  begin
    BoundaryStart := BoundaryStart + Length('boundary=');
    BoundaryEnd := PosEx(' ', ContentType, BoundaryStart);
    if BoundaryEnd = 0 then
      BoundaryEnd := Length(ContentType) + 1;
    Result := Copy(ContentType, BoundaryStart, BoundaryEnd - BoundaryStart);
  end;
end;

end.
