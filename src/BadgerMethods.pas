unit BadgerMethods;

interface

uses
 blcksock, SysUtils, Classes, {DBClient, VirtualTable,} DB, {$IFDEF VER150}Dialogs{$ELSE}FMX.Dialogs{$ENDIF} ,
 BadgerMultipartDataReader, Contnrs, SyUtils;

type
 TBadgerMethods = class(TObject)
 private
   function ParseRequestHeaderInt(Headers: TStringList; aRequestHeader: String) : Integer;
   function ParseRequestHeaderStr(Headers: TStringList; aRequestHeader: String): String;
   function getMime(aFilePath: String): String;
   function ReadHeaders(ClientSocket: TTCPBlockSocket): TStringList;

 public
   function fParserJsonStream(const ClientSocket: TTCPBlockSocket; const URI, Method, RequestLine: String): string;
   function fDownloadStream(const ClientSocket: TTCPBlockSocket; const URI, Method, RequestLine, FilePath: String): Boolean;
   procedure AtuImage (ClientSocket: TTCPBlockSocket; const URI, Method, RequestLine: string);
   function getRequestStream(ClientSocket: TTCPBlockSocket; ContentLength: Integer): TMemoryStream;
   function ExtractMethodAndURI(const RequestLine: string; out Method, URI: string): Boolean;
 end;
implementation



{ TSynaMethods }
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

function ExtractBoundary(const ContentType: string): string;
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

procedure TBadgerMethods.AtuImage(ClientSocket: TTCPBlockSocket; const URI, Method, RequestLine: string);
var
   V_ErrMsg      : String;
   V_Stream 	 : TMemoryStream;
   ContentLength : Integer;
   V_HeaderList  : TStringList;
   V_Boundary    : string;

   Reader: TFormDataReader;
   Files: TObjectList;
   i: Integer;
   FormDataFile: TFormDataFile;
   UniqueName: string;
begin

   V_Stream := nil;
   V_HeaderList := nil;

   Reader := TFormDataReader.Create;
   Files := nil;

   try
      V_HeaderList := ReadHeaders(ClientSocket);
      ContentLength:= ParseRequestHeaderInt(V_HeaderList, 'Content-Length');
      V_Boundary   := ParseRequestHeaderStr(V_HeaderList, 'boundary');
      V_Boundary := ExtractBoundary(V_Boundary);
      V_Stream := getRequestStream(ClientSocket, ContentLength);
      V_Stream.Position := 0;

      Files := Reader.ProcessMultipartFormData(V_Stream, V_Boundary) ;

      try
         for i := 0 to Files.Count - 1 do
         begin
            FormDataFile := TFormDataFile(Files[i]);
            UniqueName := Reader.UniqueFileName(FormDataFile.FileName);
            FormDataFile.Stream.SaveToFile(UniqueName);
         end;

       ClientSocket.SendString('HTTP/1.1 200 OK' + CRLF + 'Content-Type: text/plain' + CRLF + CRLF );
      except
         ClientSocket.SendString( 'HTTP/1.1 500 ' + UTF8Encode(V_ErrMsg) + CRLF + 'Content-Type: text/plain' + CRLF + CRLF );
      end;

   finally
      if (Assigned(V_HeaderList)) then
         FreeAndNil( V_HeaderList );

      if (Assigned(V_Stream)) then
         FreeAndNil( V_Stream );

      if (Assigned(Reader)) then
         FreeAndNil( Reader );

      if (Assigned(Files)) then
         FreeAndNil(Files);
   end;
end;

function TBadgerMethods.ExtractMethodAndURI(const RequestLine: string;
  out Method, URI: string): Boolean;
var
  SpacePos: Integer;
  VRequestLine: String;
begin
  Result := False;
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
      Result := True;
    end;
  end;
end;


function TBadgerMethods.fDownloadStream(const ClientSocket: TTCPBlockSocket; const URI, Method, RequestLine, FilePath: String): Boolean;
var
  Header: string;
  Buffer: array[0..8191] of Byte;
  BytesRead: Integer;
  FileStream: TFileStream;
begin
  Result := False;
  try
    FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
    try
      Header := 'HTTP/1.0 200 OK' + CRLF +
                'Content-Type: ' + getMime(FilePath) + CRLF +
                'Content-Length: ' + IntToStr(FileStream.Size) + CRLF + CRLF;
      ClientSocket.SendString(Header);

      FileStream.Position := 0;
      repeat
        BytesRead := FileStream.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
          ClientSocket.SendBuffer(@Buffer, BytesRead);
      until BytesRead = 0;
      Result := True;
    finally
      FileStream.Free;
    end;
  except
    ClientSocket.SendString('HTTP/1.1 500 Internal Server Error' + CRLF + 'Content-Type: text/plain' + CRLF + CRLF);
  end;
end;

function TBadgerMethods.fParserJsonStream(const ClientSocket: TTCPBlockSocket; const URI, Method, RequestLine: String): String;
var
  ContentLength: Integer;
  BodyStream: TMemoryStream;
  LBodyData: TStringStream;
  teste : String;
  V_HeaderList : TStringList;
begin
  V_HeaderList := nil;
  V_HeaderList := ReadHeaders(ClientSocket);
  if UpperCase(Method) = 'POST' then
  begin
    BodyStream := TMemoryStream.Create;
    LBodyData := TStringStream.Create('');
    ContentLength := ParseRequestHeaderInt(V_HeaderList, 'Content-Length');
    try
      if ContentLength > 0 then
      begin
        BodyStream.SetSize(ContentLength);
        ClientSocket.RecvBufferEx(BodyStream.Memory, ContentLength, 5000); // Timeout 5 seconds
        BodyStream.Position := 0;
        BodyStream.SaveToStream(LBodyData);
        teste := LBodyData.DataString;
       // BodyStream.SaveToFile('uploaded_file.dat');
      end;

      Result := '{"status":true, "message":"Recebimento concluído com sucesso"}';
      finally
        BodyStream.Free;
        LBodyData.Free;
      end;
    end
    else
    begin
      Result := '{"status":false, "message":"Método não aceito, usar POST"}';
    end;
end;

function TBadgerMethods.getMime(aFilePath: String): String;
var
  sUtils : TSyUtils;
begin
  sUtils := nil;
  sUtils := TSyUtils.Create;
  try
     Result:= sUtils.GetFileMIMEType(aFilePath);
  finally
     sUtils.Free;
  end;
end;

function TBadgerMethods.getRequestStream(ClientSocket: TTCPBlockSocket; ContentLength: Integer): TMemoryStream;
begin
   Result := TMemoryStream.Create;

   try
      if ContentLength > 0 then
      begin
         Result.SetSize(ContentLength);
         ClientSocket.RecvBufferEx(Result.Memory, ContentLength, 5000); // Timeout 5 seconds
         Result.Position := 0;
      end;
   except
      Result.Free;
      raise;
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
        Result := Trim( Copy(HeaderLine, PosColon + 1, Length(HeaderLine) ) ) ;
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


function TBadgerMethods.ReadHeaders(ClientSocket: TTCPBlockSocket): TStringList;
var
  HeaderLine: string;
  Headers: TStringList;
begin
  Headers := TStringList.Create;
  try
    repeat
      HeaderLine := ClientSocket.RecvString(5000);
      if HeaderLine <> '' then
        Headers.Add(HeaderLine);
    until HeaderLine = '';
    Result := Headers;
  except
    Headers.Free;
    raise;
  end;
end;

end.
