unit uFormDataReader;

interface

uses
  SysUtils, Classes, Contnrs;

type
  TFormDataFile = class
  private
    FFileName: string;
    FExtension: string;
    FStream: TMemoryStream;
  public
    constructor Create(const AFileName: string; AStream: TMemoryStream);
    destructor Destroy; override;
    property FileName: string read FFileName;
    property Extension: string read FExtension;
    property Stream: TMemoryStream read FStream;
  end;

  TFormDataReader = class
  private
{$IFDEF VER150} // Delphi 7
    function ReadStreamAsAnsi(Stream: TStream; StartPos, Size: Int64): string;
{$ELSE} // Delphi 2009+ e Lazarus
    function ReadStreamAsBytes(Stream: TStream; StartPos, Size: Int64): TBytes;

{$ENDIF}

     function FindBoundary(Stream: TStream; const Boundary: string; StartPos: Int64): Int64;
     function ExtractFileName(const Stream: TStream; StartPos, EndPos: Int64): string;
  public
    function ProcessMultipartFormData(const AStream: TStream; const ABoundary: string): TObjectList;
    function UniqueFileName(const BaseName: string): string;
  end;

implementation

{ TFormDataFile }

constructor TFormDataFile.Create(const AFileName: string; AStream: TMemoryStream);
begin
  FFileName := AFileName;
  FExtension := ExtractFileExt(AFileName);
  FStream := TMemoryStream.Create;
  if Assigned(AStream) and (AStream.Size > 0) then
  begin
    AStream.Position := 0;
    FStream.CopyFrom(AStream, AStream.Size);
  end;
end;

destructor TFormDataFile.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

{ TFormDataReader }

{$IFDEF VER150} // Delphi 7
function TFormDataReader.ReadStreamAsAnsi(Stream: TStream; StartPos, Size: Int64): string;
begin
  SetLength(Result, Size);
  Stream.Position := StartPos;
  Stream.ReadBuffer(Result[1], Size);
end;
{$ELSE} // Delphi 2009+ e Lazarus
function TFormDataReader.ReadStreamAsBytes(Stream: TStream; StartPos, Size: Int64): TBytes;
begin
  SetLength(Result, Size);
  Stream.Position := StartPos;
  Stream.ReadBuffer(Result[0], Size);
end;
{$ENDIF}

function TFormDataReader.FindBoundary(Stream: TStream; const Boundary: string; StartPos: Int64): Int64;
var
  MemoryStream: TMemoryStream;
{$IFDEF VER150} // Delphi 7
  i: Integer; // Usando Integer para o loop no Delphi 7
  Buffer: PChar;
  BoundaryLen: Integer;
{$ELSE} // Delphi 2009+ e Lazarus
  i: Int64;   // Usando Int64 para streams grandes em versões novas
  BoundaryBytes: TBytes;
  Buffer: PByte;
  BoundaryLen: Integer;
{$ENDIF}
begin
  Result := -1;

{$IFDEF VER150} // Delphi 7
  BoundaryLen := Length(Boundary);
{$ELSE} // Delphi 2009+ e Lazarus
  BoundaryBytes := TEncoding.ANSI.GetBytes(Boundary);
  BoundaryLen := Length(BoundaryBytes);
{$ENDIF}

  if Stream is TMemoryStream then
    MemoryStream := Stream as TMemoryStream
  else
  begin
    MemoryStream := TMemoryStream.Create;
    try
      MemoryStream.CopyFrom(Stream, Stream.Size);
    except
      MemoryStream.Free;
      Exit;
    end;
  end;

  try
{$IFDEF VER150} // Delphi 7
    GetMem(Buffer, BoundaryLen);
{$ELSE} // Delphi 2009+ e Lazarus
    GetMem(Buffer, BoundaryLen);
{$ENDIF}
    try
{$IFDEF VER150} // Delphi 7
      for i := StartPos to MemoryStream.Size - BoundaryLen do
      begin
        MemoryStream.Position := i;
        MemoryStream.ReadBuffer(Buffer^, BoundaryLen);
        if CompareMem(Buffer, PChar(Boundary), BoundaryLen) then
        begin
          Result := i;
          Break;
        end;
      end;
{$ELSE} // Delphi 2009+ e Lazarus
      i := StartPos;
      while i <= MemoryStream.Size - BoundaryLen do
      begin
        MemoryStream.Position := i;
        MemoryStream.ReadBuffer(Buffer^, BoundaryLen);
        if CompareMem(Buffer, @BoundaryBytes[0], BoundaryLen) then
        begin
          Result := i;
          Break;
        end;
        Inc(i);
      end;
{$ENDIF}
    finally
      FreeMem(Buffer);
    end;
  finally
    if not (Stream is TMemoryStream) then
      MemoryStream.Free;
  end;
end;

function TFormDataReader.ExtractFileName(const Stream: TStream; StartPos, EndPos: Int64): string;
var
  HeaderStr, SubString: string;
  PosFilename, PosQuoteStart, PosQuoteEnd: Integer;
begin
  Result := '';
{$IFDEF VER150} // Delphi 7
  HeaderStr := ReadStreamAsAnsi(Stream, StartPos, EndPos - StartPos);
{$ELSE} // Delphi 2009+ e Lazarus
  HeaderStr := TEncoding.ANSI.GetString(ReadStreamAsBytes(Stream, StartPos, EndPos - StartPos));
{$ENDIF}

  PosFilename := Pos('filename="', HeaderStr);
  if PosFilename > 0 then
  begin
    PosQuoteStart := PosFilename + Length('filename="');
    SubString := Copy(HeaderStr, PosQuoteStart, Length(HeaderStr) - PosQuoteStart + 1);
    PosQuoteEnd := Pos('"', SubString);
    if PosQuoteEnd > 0 then
      Result := Copy(SubString, 1, PosQuoteEnd - 1)
    else
      Result := SubString;
  end;

  if Trim(Result) = '' then
    Result := 'unnamed_file_' + IntToStr(Random(10000)) + '.bin';
end;

function TFormDataReader.UniqueFileName(const BaseName: string): string;
var
  Counter: Integer;
begin
  Result := BaseName;
  Counter := 1;
  while FileExists(Result) do
  begin
    Result := ChangeFileExt(BaseName, '') + '_' + IntToStr(Counter) + ExtractFileExt(BaseName);
    Inc(Counter);
  end;
end;

function TFormDataReader.ProcessMultipartFormData(const AStream: TStream; const ABoundary: string): TObjectList;
var
  MemoryContent: TMemoryStream;
  BoundaryStart, BoundaryEnd: string;
  PartStart, PartEnd, HeaderEndPos: Int64;
  DataStream: TMemoryStream;
  TempByte: Byte;
  FileName: string;
  FormDataFile: TFormDataFile;
begin
  Result := TObjectList.Create(True);
  MemoryContent := TMemoryStream.Create;
  try
   { if (not Assigned(AStream)) or (AStream.Size = 0) then
    begin
      ShowMessage('Stream de entrada inválido ou vazio.');
      Exit;
    end;  }

    MemoryContent.CopyFrom(AStream, AStream.Size);
    MemoryContent.Position := 0;

    BoundaryStart := '--' + ABoundary;
    BoundaryEnd := '--' + ABoundary + '--';

    PartStart := 0;
    while PartStart >= 0 do
    begin
      PartStart := FindBoundary(MemoryContent, BoundaryStart, PartStart);
      if PartStart = -1 then
      begin
        PartStart := FindBoundary(MemoryContent, BoundaryEnd, 0);
        if PartStart > 0 then Break;
        Break;
      end;

      HeaderEndPos := FindBoundary(MemoryContent, #13#10#13#10, PartStart);
      if HeaderEndPos = -1 then Break;
      HeaderEndPos := HeaderEndPos + 4;

      PartEnd := FindBoundary(MemoryContent, BoundaryStart, HeaderEndPos);
      if PartEnd = -1 then
        PartEnd := FindBoundary(MemoryContent, BoundaryEnd, HeaderEndPos);
      if PartEnd = -1 then
        PartEnd := MemoryContent.Size;

      if PartEnd > HeaderEndPos + 2 then
      begin
        MemoryContent.Position := PartEnd - 2;
        MemoryContent.Read(TempByte, 1);
        if TempByte = 13 then
        begin
          MemoryContent.Read(TempByte, 1);
          if TempByte = 10 then
            PartEnd := PartEnd - 2;
        end;
      end;

      if PartEnd > HeaderEndPos then
      begin
        DataStream := TMemoryStream.Create;
        try
          FileName := ExtractFileName(MemoryContent, PartStart, HeaderEndPos - 4);
          MemoryContent.Position := HeaderEndPos;
          DataStream.CopyFrom(MemoryContent, PartEnd - HeaderEndPos);
          FormDataFile := TFormDataFile.Create(FileName, DataStream);
          Result.Add(FormDataFile);

          //ShowMessage(Format('Parte processada - PartStart: %d, PartEnd: %d, FileName: %s, Tamanho: %d', [PartStart, PartEnd, FileName, DataStream.Size]));
        finally
          DataStream.Free;
        end;
      end;

      PartStart := PartEnd;
    end;

    {if Result.Count = 0 then
      ShowMessage('Nenhum arquivo encontrado no multipart/form-data.');}
  finally
    MemoryContent.Free;
  end;
end;

end.
