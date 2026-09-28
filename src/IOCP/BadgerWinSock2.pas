unit BadgerWinSock2;

{ Winsock 2 + AcceptEx declarations for Delphi 7 and FPC/Lazarus (Windows).
  Does not use the RTL WinSock/WinSock2 units, to avoid 1.1 vs 2.2 clashes. }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Windows;

{$IFNDEF MSWINDOWS}
  {$IFNDEF WINDOWS}
    {$IFDEF FPC}
      {$ERROR BadgerWinSock2 is Windows-only}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

const
  WINSOCK_VERSION = $0202;

  SOCKET_ERROR = -1;
  INVALID_SOCKET = {$IFDEF FPC}PtrUInt(not PtrUInt(0)){$ELSE}{$IFDEF WIN64}NativeUInt(not NativeUInt(0)){$ELSE}LongWord($FFFFFFFF){$ENDIF}{$ENDIF};

  AF_INET = 2;
  SOCK_STREAM = 1;
  IPPROTO_TCP = 6;

  SOL_SOCKET = $FFFF;
  SO_REUSEADDR = $0004;
  SO_UPDATE_ACCEPT_CONTEXT = $700B;
  SO_EXCLUSIVEADDRUSE = not 4; { ((int)(~SO_REUSEADDR)) }

  TCP_NODELAY = 1;

  SD_BOTH = 2;

  SOMAXCONN = $7FFFFFFF;

  WSA_FLAG_OVERLAPPED = $01;
  WSA_IO_PENDING = 997;

  SIO_GET_EXTENSION_FUNCTION_POINTER = DWORD($C8000006);

  WSAID_ACCEPTEX: TGUID = (
    D1: $B5367DF1;
    D2: $CBAC;
    D3: $11CF;
    D4: ($95, $CA, $00, $80, $5F, $48, $A1, $92)
  );

type
  TBadgerSocket = {$IFDEF FPC}PtrUInt{$ELSE}{$IFDEF WIN64}NativeUInt{$ELSE}LongWord{$ENDIF}{$ENDIF};

  { WSADATA is 400 bytes (Win32) / 408 bytes (Win64). Oversized blob is safe. }
  TBadgerWSAData = record
    Bytes: array[0..511] of Byte;
  end;

  TBadgerSockAddrIn = packed record
    sin_family: Word;
    sin_port: Word;
    sin_addr: Cardinal;
    sin_zero: array[0..7] of AnsiChar;
  end;

  { Must match WSABUF: len is 4 bytes; pointer is aligned (padding on 64-bit). }
  TBadgerWsaBuf = record
    len: Cardinal;
    buf: Pointer;
  end;
  PBadgerWsaBuf = ^TBadgerWsaBuf;

  TAcceptExProc = function(sListenSocket, sAcceptSocket: TBadgerSocket;
    lpOutputBuffer: Pointer;
    dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD;
    var lpdwBytesReceived: DWORD;
    lpOverlapped: POverlapped): BOOL; stdcall;

function WSAStartup(wVersionRequested: Word; var lpWSAData: TBadgerWSAData): Integer; stdcall;
function WSACleanup: Integer; stdcall;
function WSAGetLastError: Integer; stdcall;
function WSASocketA(af, aType, protocol: Integer; lpProtocolInfo: Pointer;
  g: Cardinal; dwFlags: Cardinal): TBadgerSocket; stdcall;
function WSAIoctl(s: TBadgerSocket; dwIoControlCode: DWORD; lpvInBuffer: Pointer;
  cbInBuffer: DWORD; lpvOutBuffer: Pointer; cbOutBuffer: DWORD;
  var lpcbBytesReturned: DWORD; lpOverlapped: POverlapped;
  lpCompletionRoutine: Pointer): Integer; stdcall;
function WSARecv(s: TBadgerSocket; lpBuffers: PBadgerWsaBuf; dwBufferCount: DWORD;
  var lpNumberOfBytesRecvd: DWORD; var lpFlags: DWORD;
  lpOverlapped: POverlapped; lpCompletionRoutine: Pointer): Integer; stdcall;
function WSASend(s: TBadgerSocket; lpBuffers: PBadgerWsaBuf; dwBufferCount: DWORD;
  var lpNumberOfBytesSent: DWORD; dwFlags: DWORD;
  lpOverlapped: POverlapped; lpCompletionRoutine: Pointer): Integer; stdcall;
function bind(s: TBadgerSocket; name: Pointer; namelen: Integer): Integer; stdcall;
function listen(s: TBadgerSocket; backlog: Integer): Integer; stdcall;
function closesocket(s: TBadgerSocket): Integer; stdcall;
function shutdown(s: TBadgerSocket; how: Integer): Integer; stdcall;
function setsockopt(s: TBadgerSocket; level, optname: Integer; optval: Pointer;
  optlen: Integer): Integer; stdcall;
function htons(hostshort: Word): Word; stdcall;
function connect(s: TBadgerSocket; name: Pointer; namelen: Integer): Integer; stdcall;
function send(s: TBadgerSocket; buf: Pointer; len, flags: Integer): Integer; stdcall;
{ Envio bloqueante curto, para respostas interinas (100 Continue). }
function BadgerSendAll(s: TBadgerSocket; const Data: AnsiString): Boolean;
function recv(s: TBadgerSocket; buf: Pointer; len, flags: Integer): Integer; stdcall;
function inet_addr(cp: PAnsiChar): Cardinal; stdcall;
function getpeername(s: TBadgerSocket; name: Pointer; var namelen: Integer): Integer; stdcall;

function BadgerWSAAddRef: Boolean;
procedure BadgerWSARelease;
function BadgerCreateOverlappedSocket: TBadgerSocket;
function BadgerLoadAcceptEx(ListenSocket: TBadgerSocket; out Proc: TAcceptExProc): Boolean;
function BadgerHttpGetLocal(APort: Integer; const APath: AnsiString;
  out AStatus: Integer; out ABody: AnsiString): Boolean;

implementation

const
  WS2_DLL = 'ws2_32.dll';

function WSAStartup(wVersionRequested: Word; var lpWSAData: TBadgerWSAData): Integer; stdcall;
  external WS2_DLL name 'WSAStartup';
function WSACleanup: Integer; stdcall;
  external WS2_DLL name 'WSACleanup';
function WSAGetLastError: Integer; stdcall;
  external WS2_DLL name 'WSAGetLastError';
function WSASocketA(af, aType, protocol: Integer; lpProtocolInfo: Pointer;
  g: Cardinal; dwFlags: Cardinal): TBadgerSocket; stdcall;
  external WS2_DLL name 'WSASocketA';
function WSAIoctl(s: TBadgerSocket; dwIoControlCode: DWORD; lpvInBuffer: Pointer;
  cbInBuffer: DWORD; lpvOutBuffer: Pointer; cbOutBuffer: DWORD;
  var lpcbBytesReturned: DWORD; lpOverlapped: POverlapped;
  lpCompletionRoutine: Pointer): Integer; stdcall;
  external WS2_DLL name 'WSAIoctl';
function WSARecv(s: TBadgerSocket; lpBuffers: PBadgerWsaBuf; dwBufferCount: DWORD;
  var lpNumberOfBytesRecvd: DWORD; var lpFlags: DWORD;
  lpOverlapped: POverlapped; lpCompletionRoutine: Pointer): Integer; stdcall;
  external WS2_DLL name 'WSARecv';
function WSASend(s: TBadgerSocket; lpBuffers: PBadgerWsaBuf; dwBufferCount: DWORD;
  var lpNumberOfBytesSent: DWORD; dwFlags: DWORD;
  lpOverlapped: POverlapped; lpCompletionRoutine: Pointer): Integer; stdcall;
  external WS2_DLL name 'WSASend';
function bind(s: TBadgerSocket; name: Pointer; namelen: Integer): Integer; stdcall;
  external WS2_DLL name 'bind';
function listen(s: TBadgerSocket; backlog: Integer): Integer; stdcall;
  external WS2_DLL name 'listen';
function closesocket(s: TBadgerSocket): Integer; stdcall;
  external WS2_DLL name 'closesocket';
function shutdown(s: TBadgerSocket; how: Integer): Integer; stdcall;
  external WS2_DLL name 'shutdown';
function setsockopt(s: TBadgerSocket; level, optname: Integer; optval: Pointer;
  optlen: Integer): Integer; stdcall;
  external WS2_DLL name 'setsockopt';
function htons(hostshort: Word): Word; stdcall;
  external WS2_DLL name 'htons';
function connect(s: TBadgerSocket; name: Pointer; namelen: Integer): Integer; stdcall;
  external WS2_DLL name 'connect';
function send(s: TBadgerSocket; buf: Pointer; len, flags: Integer): Integer; stdcall;
  external WS2_DLL name 'send';
function recv(s: TBadgerSocket; buf: Pointer; len, flags: Integer): Integer; stdcall;
  external WS2_DLL name 'recv';
function inet_addr(cp: PAnsiChar): Cardinal; stdcall;
  external WS2_DLL name 'inet_addr';
function getpeername(s: TBadgerSocket; name: Pointer; var namelen: Integer): Integer; stdcall;
  external WS2_DLL name 'getpeername';

var
  GWsaRefCount: Integer;

function BadgerWSAAddRef: Boolean;
var
  Data: TBadgerWSAData;
begin
  Result := False;
  FillChar(Data, SizeOf(Data), 0);
  if WSAStartup(WINSOCK_VERSION, Data) <> 0 then
    Exit;
  Inc(GWsaRefCount);
  Result := True;
end;

procedure BadgerWSARelease;
begin
  if GWsaRefCount <= 0 then
    Exit;
  Dec(GWsaRefCount);
  if GWsaRefCount = 0 then
    WSACleanup;
end;

function BadgerCreateOverlappedSocket: TBadgerSocket;
begin
  Result := WSASocketA(AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);
end;

function BadgerLoadAcceptEx(ListenSocket: TBadgerSocket; out Proc: TAcceptExProc): Boolean;
var
  Bytes: DWORD;
  Guid: TGUID;
  Fn: Pointer;
begin
  Proc := nil;
  Fn := nil;
  Guid := WSAID_ACCEPTEX;
  Bytes := 0;
  { @Fn is the address of the pointer slot. @Proc on a procedural type is the
    code pointer (often nil) — WSAIoctl then faults with WSAEFAULT 10014. }
  Result := WSAIoctl(ListenSocket, SIO_GET_EXTENSION_FUNCTION_POINTER,
    @Guid, SizeOf(Guid), @Fn, SizeOf(Fn), Bytes, nil, nil) = 0;
  if Result and (Fn <> nil) then
    @Proc := Fn
  else
  begin
    Result := False;
    Proc := nil;
  end;
end;

function BadgerHttpGetLocal(APort: Integer; const APath: AnsiString;
  out AStatus: Integer; out ABody: AnsiString): Boolean;
var
  S: TBadgerSocket;
  Addr: TBadgerSockAddrIn;
  Req: AnsiString;
  Raw: AnsiString;
  Chunk: array[0..1023] of AnsiChar;
  N, Total, P, Code: Integer;
  NeedWsa: Boolean;
begin
  Result := False;
  AStatus := 0;
  ABody := '';
  NeedWsa := GWsaRefCount = 0;
  if NeedWsa and not BadgerWSAAddRef then
    Exit;
  S := WSASocketA(AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, 0);
  if S = INVALID_SOCKET then
  begin
    if NeedWsa then
      BadgerWSARelease;
    Exit;
  end;
  try
    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(Word(APort));
    Addr.sin_addr := inet_addr(PAnsiChar(AnsiString('127.0.0.1')));
    if connect(S, @Addr, SizeOf(Addr)) = SOCKET_ERROR then
      Exit;
    Req := 'GET ' + APath + ' HTTP/1.0'#13#10 +
      'Host: 127.0.0.1'#13#10 +
      'Connection: close'#13#10#13#10;
    if send(S, PAnsiChar(Req), Length(Req), 0) = SOCKET_ERROR then
      Exit;
    Total := 0;
    Raw := '';
    repeat
      N := recv(S, @Chunk[0], SizeOf(Chunk), 0);
      if N <= 0 then
        Break;
      SetLength(Raw, Total + N);
      Move(Chunk[0], Raw[Total + 1], N);
      Inc(Total, N);
    until False;
    if Total = 0 then
      Exit;
    P := Pos(AnsiString(' '), Raw);
    if (P = 0) or (P + 3 > Length(Raw)) then
      Exit;
    Code := (Ord(Raw[P + 1]) - Ord('0')) * 100 +
            (Ord(Raw[P + 2]) - Ord('0')) * 10 +
            (Ord(Raw[P + 3]) - Ord('0'));
    AStatus := Code;
    P := Pos(AnsiString(#13#10#13#10), Raw);
    if P > 0 then
      ABody := Copy(Raw, P + 4, Length(Raw))
    else
      ABody := Raw;
    Result := True;
  finally
    closesocket(S);
    if NeedWsa then
      BadgerWSARelease;
  end;
end;

function BadgerSendAll(s: TBadgerSocket; const Data: AnsiString): Boolean;
var
  Sent, Total, Len: Integer;
begin
  Len := Length(Data);
  Total := 0;
  while Total < Len do
  begin
    Sent := send(s, @PAnsiChar(Data)[Total], Len - Total, 0);
    if Sent <= 0 then
    begin
      Result := False;
      Exit;
    end;
    Inc(Total, Sent);
  end;
  Result := True;
end;

end.
