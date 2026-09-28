unit BadgerMethods;

{$IFDEF FPC}
  {$mode delphi}{$H+}
  {$codepage utf8}
{$ENDIF}

interface

uses
  SysUtils, Classes, BadgerMultipartDataReader, Contnrs, BadgerUtils,
  BadgerTypes, BadgerHttpStatus, blcksock, BadgerHttpUtils;

type
  TBadgerMethods = class(TObject)
  private
    FUtils: TBadgerUtils;
    FUploadDir: string;
    function getMime(aFilePath: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    function ParseRequestHeaderInt(Headers: TStringList; aRequestHeader: String): Integer;
    function ParseRequestHeaderStr(Headers: TStringList; aRequestHeader: String): String;
    function fParserJsonStream(Request: THTTPRequest; Response : THTTPResponse): string;
    function fDownloadStream(const FilePath: string; out MimeType: string): TStream;
    procedure AtuImage(Request: THTTPRequest; var Response: THTTPResponse);
    function ExtractMethodAndURI(const RequestLine: string; out Method, URI: string; var QueryParams: TStringList): Boolean;
    function ExtractBoundary(const ContentType: string): string;
    { Diretorio de destino do upload. Vazio = diretorio corrente do processo, que
      costuma ser a pasta do servico ou do executavel. }
    property UploadDir: string read FUploadDir write FUploadDir;
  end;

implementation

uses StrUtils;

constructor TBadgerMethods.Create;
begin
  inherited Create;
  FUtils := TBadgerUtils.Create;
end;

destructor TBadgerMethods.Destroy;
begin
  FUtils.Free;
  inherited;
end;

function TBadgerMethods.ExtractMethodAndURI(const RequestLine: string; out Method, URI: string; var QueryParams: TStringList): Boolean;
var
  SpacePos, QueryPos: Integer;
  VRequestLine, QueryString, ParamPair, DoubleSlash: string;
begin
  Result := False;
  QueryParams.Clear;
  VRequestLine := RequestLine;
  DoubleSlash := '//';

  SpacePos := Pos(' ', VRequestLine);
  if SpacePos > 0 then
  begin
    Method := Copy(VRequestLine, 1, SpacePos - 1);
    Delete(VRequestLine, 1, SpacePos);

    SpacePos := Pos(' ', VRequestLine);
    if SpacePos > 0 then
    begin
      URI := Copy(VRequestLine, 1, SpacePos - 1);
      while Pos(DoubleSlash, URI)>0 do
        URI := StringReplace(URI, DoubleSlash, '/', [rfReplaceAll]);

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
          { Split antes do decode: '%3D' no valor viraria '=' e deslocaria a
            fronteira chave/valor. }
          SpacePos := Pos('=', ParamPair);
          if SpacePos > 0 then
            QueryParams.Add(URLDecode(Copy(ParamPair, 1, SpacePos - 1)) + '=' +
                            URLDecode(Copy(ParamPair, SpacePos + 1, MaxInt)))
          else
            QueryParams.Add(URLDecode(ParamPair));
        end;
      end;
      Result := True;
    end;
  end;
end;

function TBadgerMethods.getMime(aFilePath: String): String;
begin
  Result := FUtils.GetFileMIMEType(aFilePath);
end;

function TBadgerMethods.ParseRequestHeaderStr(Headers: TStringList; aRequestHeader: String): String;
begin
  if not TryGetHeaderValue(Headers, aRequestHeader, Result) then
    Result := '';
end;

function TBadgerMethods.ParseRequestHeaderInt(Headers: TStringList; aRequestHeader: String): Integer;
begin
  if not TryGetHeaderInt(Headers, aRequestHeader, Result) then
    Result := 0;
end;

function CodepointToString(Code: Word): string;
begin
  {$IFDEF UNICODE}
  Result := WideChar(Code);
  {$ELSE}
    {$IFDEF FPC}
    if Code <= $7F then
      Result := Char(Code)
    else
      Result := Char($C0 or Byte(Code shr 6)) + Char($80 or Byte(Code and $3F));
    {$ELSE}
    Result := Chr(Byte(Code));
    {$ENDIF}
  {$ENDIF}
end;

function TBadgerMethods.fParserJsonStream( Request: THTTPRequest; Response : THTTPResponse ): string;
begin
  if UpperCase(Request.Method) = 'POST' then
    Result := '{"status":true, "message":"Recebimento conclu' + CodepointToString($00ED) +
      'do com sucesso", "Vc me mandou":"' + JSONEscape(Request.Body) + '"}'
  else
    Result := '{"status":false, "message":"M' + CodepointToString($00E9) +
      'todo n' + CodepointToString($00E3) + 'o aceito, usar POST"}';
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

procedure TBadgerMethods.AtuImage(Request: THTTPRequest; var Response: THTTPResponse);
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
        if FUploadDir <> '' then
        begin
          ForceDirectories(FUploadDir);
          UniqueName := IncludeTrailingPathDelimiter(FUploadDir) + UniqueName;
        end;
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
const
  BoundaryPrefix = 'boundary=';
var
  BoundaryStart, BoundaryEnd: Integer;
begin
  Result := '';
  BoundaryStart := Pos(BoundaryPrefix, ContentType);
  if BoundaryStart = 0 then Exit;

  BoundaryStart := BoundaryStart + Length(BoundaryPrefix);
  BoundaryEnd := PosEx(';', ContentType, BoundaryStart);
  if BoundaryEnd = 0 then
    BoundaryEnd := Length(ContentType) + 1;
  Result := Trim(Copy(ContentType, BoundaryStart, BoundaryEnd - BoundaryStart));

  if (Length(Result) >= 2) and (Result[1] = '"') and (Result[Length(Result)] = '"') then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

end.
