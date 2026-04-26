unit BadgerTestClient;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

{
  Raw HTTP client for the TDD harness.

  Uses Synapse's TTCPBlockSocket directly so tests assert on the actual wire
  bytes instead of trusting any HTTP-aware client. This is intentional: the
  audit found CRLF injection, header-splitting, and chunked-parser bugs that
  a smart client would silently work around. Stay close to the wire.

  All public functions are stateless and create/destroy their own socket,
  except TKeepAliveSession which deliberately holds one socket across
  multiple requests so keep-alive tests can assert no state bleeds across
  iterations.
}

interface

uses
  SysUtils, Classes, blcksock;

const
  CRLF = #13#10;
  TEST_HOST = '127.0.0.1';
  DEFAULT_TIMEOUT_MS = 4000;

type
  TTestHttpResponse = record
    Raw: string;
    StatusLine: string;
    StatusCode: Integer;
    HeadersBlock: string;
    Body: string;
    Closed: Boolean;
  end;

  TKeepAliveSession = class
  private
    FSock: TTCPBlockSocket;
    FPort: Integer;
    FTimeout: Integer;
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;
    procedure Send(const ARequest: string);
    function Receive: TTestHttpResponse;
    function Exchange(const ARequest: string): TTestHttpResponse;
  end;

function MakeEmptyResponse: TTestHttpResponse;

function HttpRaw(APort: Integer; const ARequest: string): TTestHttpResponse;
function HttpGet(APort: Integer; const APath: string): TTestHttpResponse;
function HttpGetWithHeaders(APort: Integer; const APath, AExtraHeaders: string): TTestHttpResponse;
function HttpPost(APort: Integer; const APath, AContentType, ABody: string): TTestHttpResponse;
function HttpOptions(APort: Integer; const APath, AOrigin, AReqMethod, AReqHeaders: string): TTestHttpResponse;

function HeaderValue(const AResponse: TTestHttpResponse; const AHeaderName: string): string;
function ParseStatusCode(const AStatusLine: string): Integer;

implementation

uses StrUtils;

function MakeEmptyResponse: TTestHttpResponse;
begin
  Result.Raw := '';
  Result.StatusLine := '';
  Result.StatusCode := 0;
  Result.HeadersBlock := '';
  Result.Body := '';
  Result.Closed := False;
end;

function ParseStatusCode(const AStatusLine: string): Integer;
var
  P1, P2: Integer;
begin
  Result := 0;
  P1 := Pos(' ', AStatusLine);
  if P1 <= 0 then
    Exit;
  P2 := PosEx(' ', AStatusLine, P1 + 1);
  if P2 <= 0 then
    P2 := Length(AStatusLine) + 1;
  Result := StrToIntDef(Trim(Copy(AStatusLine, P1 + 1, P2 - P1 - 1)), 0);
end;

function FindHeaderValueIn(const AHeadersBlock, AName: string): string;
var
  Lower, LName: string;
  I, P, EOL, ColonPos: Integer;
  Line: string;
begin
  Result := '';
  Lower := LowerCase(AHeadersBlock);
  LName := LowerCase(AName);
  I := 1;
  while I <= Length(Lower) do
  begin
    EOL := PosEx(#10, Lower, I);
    if EOL <= 0 then
      EOL := Length(Lower) + 1;
    Line := Copy(AHeadersBlock, I, EOL - I);
    while (Length(Line) > 0) and ((Line[Length(Line)] = #13) or (Line[Length(Line)] = #10)) do
      SetLength(Line, Length(Line) - 1);

    ColonPos := Pos(':', Line);
    if ColonPos > 0 then
    begin
      P := Pos(LName + ':', Lower);
      if P = I then
      begin
        Result := Trim(Copy(Line, ColonPos + 1, Length(Line)));
        Exit;
      end
      else if (LowerCase(Trim(Copy(Line, 1, ColonPos - 1))) = LName) then
      begin
        Result := Trim(Copy(Line, ColonPos + 1, Length(Line)));
        Exit;
      end;
    end;

    I := EOL + 1;
  end;
end;

function HeaderValue(const AResponse: TTestHttpResponse; const AHeaderName: string): string;
begin
  Result := FindHeaderValueIn(AResponse.HeadersBlock, AHeaderName);
end;

procedure ConnectSocket(ASock: TTCPBlockSocket; APort, ATimeoutMs: Integer);
begin
  ASock.ConnectionTimeout := ATimeoutMs;
  ASock.Connect(TEST_HOST, IntToStr(APort));
end;

function ReadHeadersBlock(ASock: TTCPBlockSocket; ATimeoutMs: Integer; out AHeaders: string): Boolean;
var
  Buffer: string;
  Chunk: string;
  EndMark: Integer;
begin
  Result := False;
  Buffer := '';
  while True do
  begin
    Chunk := ASock.RecvPacket(ATimeoutMs);
    if (ASock.LastError <> 0) and (Chunk = '') then
      Break;
    Buffer := Buffer + Chunk;
    EndMark := Pos(CRLF + CRLF, Buffer);
    if EndMark > 0 then
    begin
      AHeaders := Copy(Buffer, 1, EndMark - 1);
      Buffer := Copy(Buffer, EndMark + 4, Length(Buffer));
      Result := True;
      Break;
    end;
    if ASock.LastError <> 0 then
      Break;
  end;
  // Whatever remained in Buffer is the start of the body — return it via global.
  AHeaders := AHeaders + #1 + Buffer;
end;

function ReadResponseFromSocket(ASock: TTCPBlockSocket; ATimeoutMs: Integer): TTestHttpResponse;
var
  Combined: string;
  SplitMark: Integer;
  RawHeaders: string;
  AlreadyRead: string;
  CL: Integer;
  CLStr: string;
  TE: string;
  Need: Integer;
  Got: string;
  ChunkSize: Integer;
  Hex: string;
  P: Integer;
  EOL: Integer;
  CrPos: Integer;
  HeadersOnly: string;
begin
  Result := MakeEmptyResponse;

  if not ReadHeadersBlock(ASock, ATimeoutMs, Combined) then
  begin
    Result.Closed := True;
    Exit;
  end;

  SplitMark := Pos(#1, Combined);
  if SplitMark > 0 then
  begin
    RawHeaders := Copy(Combined, 1, SplitMark - 1);
    AlreadyRead := Copy(Combined, SplitMark + 1, Length(Combined));
  end
  else
  begin
    RawHeaders := Combined;
    AlreadyRead := '';
  end;

  HeadersOnly := RawHeaders;
  EOL := Pos(CRLF, HeadersOnly);
  if EOL > 0 then
  begin
    Result.StatusLine := Copy(HeadersOnly, 1, EOL - 1);
    Result.HeadersBlock := Copy(HeadersOnly, EOL + 2, Length(HeadersOnly));
  end
  else
  begin
    Result.StatusLine := HeadersOnly;
    Result.HeadersBlock := '';
  end;
  Result.StatusCode := ParseStatusCode(Result.StatusLine);

  Result.Raw := RawHeaders + CRLF + CRLF;
  Result.Body := '';

  // Pick body strategy.
  TE := LowerCase(FindHeaderValueIn(Result.HeadersBlock, 'Transfer-Encoding'));
  CLStr := FindHeaderValueIn(Result.HeadersBlock, 'Content-Length');

  if Pos('chunked', TE) > 0 then
  begin
    // Drain chunked body. Read until 0-length chunk + trailers.
    Got := AlreadyRead;
    while True do
    begin
      // Need a CRLF in Got that ends the chunk-size line.
      CrPos := Pos(CRLF, Got);
      while CrPos <= 0 do
      begin
        Got := Got + ASock.RecvPacket(ATimeoutMs);
        if ASock.LastError <> 0 then
          Break;
        CrPos := Pos(CRLF, Got);
      end;
      if CrPos <= 0 then
        Break;
      Hex := Copy(Got, 1, CrPos - 1);
      P := Pos(';', Hex);
      if P > 0 then
        Hex := Copy(Hex, 1, P - 1);
      ChunkSize := StrToIntDef('$' + Trim(Hex), -1);
      Delete(Got, 1, CrPos + 1);
      if ChunkSize <= 0 then
      begin
        // Drain trailers up to empty line.
        while Pos(CRLF, Got) <> 1 do
        begin
          Got := Got + ASock.RecvPacket(ATimeoutMs);
          if ASock.LastError <> 0 then
            Break;
          if Pos(CRLF, Got) = 1 then
            Break;
        end;
        Break;
      end;
      while Length(Got) < ChunkSize + 2 do
      begin
        Got := Got + ASock.RecvPacket(ATimeoutMs);
        if ASock.LastError <> 0 then
          Break;
      end;
      Result.Body := Result.Body + Copy(Got, 1, ChunkSize);
      Delete(Got, 1, ChunkSize + 2);
    end;
    Result.Raw := Result.Raw + Result.Body;
  end
  else if CLStr <> '' then
  begin
    CL := StrToIntDef(CLStr, 0);
    Result.Body := AlreadyRead;
    Need := CL - Length(Result.Body);
    while Need > 0 do
    begin
      Got := ASock.RecvPacket(ATimeoutMs);
      if ASock.LastError <> 0 then
        Break;
      Result.Body := Result.Body + Got;
      Need := CL - Length(Result.Body);
    end;
    if Length(Result.Body) > CL then
      Result.Body := Copy(Result.Body, 1, CL);
    Result.Raw := Result.Raw + Result.Body;
  end
  else
  begin
    // Read until close.
    Result.Body := AlreadyRead;
    while True do
    begin
      Got := ASock.RecvPacket(ATimeoutMs);
      if (ASock.LastError <> 0) and (Got = '') then
        Break;
      Result.Body := Result.Body + Got;
      if ASock.LastError <> 0 then
        Break;
    end;
    Result.Raw := Result.Raw + Result.Body;
    Result.Closed := True;
  end;
end;

function HttpRaw(APort: Integer; const ARequest: string): TTestHttpResponse;
var
  Sock: TTCPBlockSocket;
begin
  Result := MakeEmptyResponse;
  Sock := TTCPBlockSocket.Create;
  try
    ConnectSocket(Sock, APort, DEFAULT_TIMEOUT_MS);
    if Sock.LastError <> 0 then
      raise Exception.Create('Connect failed: ' + Sock.LastErrorDesc);
    Sock.SendString(ARequest);
    if Sock.LastError <> 0 then
      raise Exception.Create('Send failed: ' + Sock.LastErrorDesc);
    Result := ReadResponseFromSocket(Sock, DEFAULT_TIMEOUT_MS);
  finally
    Sock.CloseSocket;
    Sock.Free;
  end;
end;

function BuildSimpleRequest(const AMethod, APath, AExtraHeaders, ABody: string;
  APort: Integer; const AContentType: string; AKeepAlive: Boolean): string;
var
  ConnHdr: string;
  CTHdr: string;
  CLHdr: string;
begin
  if AKeepAlive then
    ConnHdr := 'Connection: keep-alive' + CRLF
  else
    ConnHdr := 'Connection: close' + CRLF;

  if AContentType <> '' then
    CTHdr := 'Content-Type: ' + AContentType + CRLF
  else
    CTHdr := '';

  if ABody <> '' then
    CLHdr := 'Content-Length: ' + IntToStr(Length(ABody)) + CRLF
  else
    CLHdr := '';

  Result :=
    AMethod + ' ' + APath + ' HTTP/1.1' + CRLF +
    'Host: ' + TEST_HOST + ':' + IntToStr(APort) + CRLF +
    ConnHdr +
    CTHdr +
    CLHdr +
    AExtraHeaders +
    CRLF +
    ABody;
end;

function HttpGet(APort: Integer; const APath: string): TTestHttpResponse;
begin
  Result := HttpRaw(APort, BuildSimpleRequest('GET', APath, '', '', APort, '', False));
end;

function HttpGetWithHeaders(APort: Integer; const APath, AExtraHeaders: string): TTestHttpResponse;
begin
  Result := HttpRaw(APort, BuildSimpleRequest('GET', APath, AExtraHeaders, '', APort, '', False));
end;

function HttpPost(APort: Integer; const APath, AContentType, ABody: string): TTestHttpResponse;
begin
  Result := HttpRaw(APort, BuildSimpleRequest('POST', APath, '', ABody, APort, AContentType, False));
end;

function HttpOptions(APort: Integer; const APath, AOrigin, AReqMethod, AReqHeaders: string): TTestHttpResponse;
var
  Extra: string;
begin
  Extra := '';
  if AOrigin <> '' then
    Extra := Extra + 'Origin: ' + AOrigin + CRLF;
  if AReqMethod <> '' then
    Extra := Extra + 'Access-Control-Request-Method: ' + AReqMethod + CRLF;
  if AReqHeaders <> '' then
    Extra := Extra + 'Access-Control-Request-Headers: ' + AReqHeaders + CRLF;
  Result := HttpRaw(APort, BuildSimpleRequest('OPTIONS', APath, Extra, '', APort, '', False));
end;

{ TKeepAliveSession }

constructor TKeepAliveSession.Create(APort: Integer);
begin
  inherited Create;
  FPort := APort;
  FTimeout := DEFAULT_TIMEOUT_MS;
  FSock := TTCPBlockSocket.Create;
  ConnectSocket(FSock, APort, FTimeout);
  if FSock.LastError <> 0 then
    raise Exception.Create('KeepAlive connect failed: ' + FSock.LastErrorDesc);
end;

destructor TKeepAliveSession.Destroy;
begin
  if Assigned(FSock) then
  begin
    FSock.CloseSocket;
    FSock.Free;
  end;
  inherited;
end;

procedure TKeepAliveSession.Send(const ARequest: string);
begin
  FSock.SendString(ARequest);
  if FSock.LastError <> 0 then
    raise Exception.Create('KeepAlive send failed: ' + FSock.LastErrorDesc);
end;

function TKeepAliveSession.Receive: TTestHttpResponse;
begin
  Result := ReadResponseFromSocket(FSock, FTimeout);
end;

function TKeepAliveSession.Exchange(const ARequest: string): TTestHttpResponse;
begin
  Send(ARequest);
  Result := Receive;
end;

end.
