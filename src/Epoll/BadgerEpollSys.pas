unit BadgerEpollSys;

{ Linux sockets + epoll. Delphi 12: Posix.* + libc. FPC: BaseUnix/sockets + libc
  for epoll_* so the packed event layout matches the kernel. }

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

{$IFDEF LINUX}

const
  BADGER_SYS_INVALID = -1;
  EPOLLIN = $00000001;
  EPOLLOUT = $00000004;
  EPOLLERR = $00000008;
  EPOLLHUP = $00000010;
  EPOLLET = $80000000;
  EPOLLONESHOT = $40000000;
  EPOLL_CTL_ADD = 1;
  EPOLL_CTL_DEL = 2;
  EPOLL_CTL_MOD = 3;

type
  TBadgerEpollData = record
    case Integer of
      0: (ptr: Pointer);
      1: (fd: Integer);
      2: (u32: Cardinal);
      3: (u64: UInt64);
  end;

  TBadgerEpollEvent = packed record
    events: Cardinal;
    data: TBadgerEpollData;
  end;
  PBadgerEpollEvent = ^TBadgerEpollEvent;

function BadgerSysSocket: Integer;
function BadgerSysListen(Fd, Port: Integer): Boolean;
function BadgerSysAccept(ListenFd: Integer; out RemoteIP: string): Integer;
function BadgerSysClose(Fd: Integer): Integer;
function BadgerSysSetNonBlock(Fd: Integer): Boolean;
function BadgerSysSetNoDelay(Fd: Integer): Boolean;
function BadgerSysRecv(Fd: Integer; Buf: Pointer; Len: Integer): Integer;
function BadgerSysSend(Fd: Integer; Buf: Pointer; Len: Integer): Integer;
function BadgerSysWrite(Fd: Integer; Buf: Pointer; Len: Integer): Integer;
function BadgerSysWouldBlock: Boolean;
function BadgerSysInterrupted: Boolean;
function BadgerSysEpollCreate: Integer;
function BadgerSysEpollCtl(Epfd, Op, Fd: Integer; Events: Cardinal; Data: Pointer): Integer;
function BadgerSysEpollDel(Epfd, Fd: Integer): Integer;
function BadgerSysEpollWait(Epfd: Integer; Events: PBadgerEpollEvent;
  MaxEvents, TimeoutMs: Integer): Integer;
function BadgerSysPipe(out ReadFd, WriteFd: Integer): Boolean;
function BadgerSysTick: LongWord;

{$ENDIF}

implementation

{$IFDEF LINUX}

uses
  SysUtils
{$IFDEF FPC}
  , BaseUnix, Sockets
{$ELSE}
  , Posix.Base, Posix.SysSocket, Posix.Unistd, Posix.Fcntl, Posix.ArpaInet,
    Posix.NetinetIn, Posix.Errno
{$ENDIF};

const
  SO_REUSEPORT = 15;
  LINUX_TCP_NODELAY = 1;
  EAGAIN = 11;
  EINTR = 4;
  MSG_NOSIGNAL = $4000;
  LINUX_CLOCK_MONOTONIC = 1;
{$IFDEF FPC}
  libc = 'libc.so.6';
{$ENDIF}

type
  TBadgerTimeSpec = record
    tv_sec: Int64;
    tv_nsec: Int64;
  end;

function epoll_create1(flags: Integer): Integer; cdecl; external libc name 'epoll_create1';
function epoll_ctl(epfd, op, fd: Integer; event: PBadgerEpollEvent): Integer; cdecl;
  external libc name 'epoll_ctl';
function epoll_wait(epfd: Integer; events: PBadgerEpollEvent; maxevents, timeout: Integer): Integer;
  cdecl; external libc name 'epoll_wait';
function sys_pipe(filedes: PInteger): Integer; cdecl; external libc name 'pipe';
function sys_write(fd: Integer; buf: Pointer; count: NativeUInt): NativeInt; cdecl;
  external libc name 'write';
function sys_clock_gettime(clk_id: Integer; tp: Pointer): Integer; cdecl;
  external libc name 'clock_gettime';

function BadgerSysSocket: Integer;
begin
{$IFDEF FPC}
  Result := fpSocket(AF_INET, SOCK_STREAM, 0);
{$ELSE}
  Result := socket(AF_INET, SOCK_STREAM, 0);
{$ENDIF}
end;

function BadgerSysListen(Fd, Port: Integer): Boolean;
var
  Opt: Integer;
{$IFDEF FPC}
  Addr: TInetSockAddr;
{$ELSE}
  Addr: sockaddr_in;
{$ENDIF}
begin
  Result := False;
  Opt := 1;
{$IFDEF FPC}
  fpSetSockOpt(Fd, SOL_SOCKET, SO_REUSEADDR, @Opt, SizeOf(Opt));
  fpSetSockOpt(Fd, SOL_SOCKET, SO_REUSEPORT, @Opt, SizeOf(Opt));
{$ELSE}
  setsockopt(Fd, SOL_SOCKET, SO_REUSEADDR, Opt, SizeOf(Opt));
  setsockopt(Fd, SOL_SOCKET, SO_REUSEPORT, Opt, SizeOf(Opt));
{$ENDIF}
  if not BadgerSysSetNonBlock(Fd) then
    Exit;
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
{$IFDEF FPC}
  Addr.sin_port := htons(Port);
  Addr.sin_addr.s_addr := 0;
  if fpBind(Fd, PSockAddr(@Addr), SizeOf(Addr)) < 0 then
    Exit;
  if fpListen(Fd, 4096) < 0 then
    Exit;
{$ELSE}
  Addr.sin_port := htons(Word(Port));
  Addr.sin_addr.s_addr := INADDR_ANY;
  if bind(Fd, Psockaddr(@Addr)^, SizeOf(Addr)) < 0 then
    Exit;
  if listen(Fd, 4096) < 0 then
    Exit;
{$ENDIF}
  Result := True;
end;

function BadgerSysAccept(ListenFd: Integer; out RemoteIP: string): Integer;
var
{$IFDEF FPC}
  Addr: TInetSockAddr;
  Len: TSockLen;
{$ELSE}
  Addr: sockaddr_in;
  Len: socklen_t;
{$ENDIF}
begin
  RemoteIP := '';
  Len := SizeOf(Addr);
  FillChar(Addr, SizeOf(Addr), 0);
{$IFDEF FPC}
  Result := fpAccept(ListenFd, PSockAddr(@Addr), @Len);
  if Result >= 0 then
    RemoteIP := NetAddrToStr(Addr.sin_addr);
{$ELSE}
  Result := accept(ListenFd, Psockaddr(@Addr)^, Len);
  if Result >= 0 then
    RemoteIP := string(AnsiString(inet_ntoa(Addr.sin_addr)));
{$ENDIF}
end;

function BadgerSysClose(Fd: Integer): Integer;
begin
  if Fd < 0 then
  begin
    Result := 0;
    Exit;
  end;
{$IFDEF FPC}
  Result := fpClose(Fd);
{$ELSE}
  Result := __close(Fd);
{$ENDIF}
end;

function BadgerSysSetNonBlock(Fd: Integer): Boolean;
begin
{$IFDEF FPC}
  Result := fpFcntl(Fd, F_SETFL, O_NONBLOCK) >= 0;
{$ELSE}
  Result := fcntl(Fd, F_SETFL, O_NONBLOCK) >= 0;
{$ENDIF}
end;

function BadgerSysSetNoDelay(Fd: Integer): Boolean;
var
  Opt: Integer;
begin
  Opt := 1;
{$IFDEF FPC}
  Result := fpSetSockOpt(Fd, IPPROTO_TCP, LINUX_TCP_NODELAY, @Opt, SizeOf(Opt)) >= 0;
{$ELSE}
  Result := setsockopt(Fd, IPPROTO_TCP, LINUX_TCP_NODELAY, Opt, SizeOf(Opt)) >= 0;
{$ENDIF}
end;

function BadgerSysRecv(Fd: Integer; Buf: Pointer; Len: Integer): Integer;
begin
{$IFDEF FPC}
  Result := fpRecv(Fd, Buf, Len, 0);
{$ELSE}
  Result := recv(Fd, Buf^, Len, 0);
{$ENDIF}
end;

function BadgerSysSend(Fd: Integer; Buf: Pointer; Len: Integer): Integer;
begin
{$IFDEF FPC}
  Result := fpSend(Fd, Buf, Len, MSG_NOSIGNAL);
{$ELSE}
  Result := send(Fd, Buf^, Len, MSG_NOSIGNAL);
{$ENDIF}
end;

function BadgerSysWrite(Fd: Integer; Buf: Pointer; Len: Integer): Integer;
begin
  Result := Integer(sys_write(Fd, Buf, NativeUInt(Len)));
end;

function BadgerSysWouldBlock: Boolean;
begin
{$IFDEF FPC}
  Result := fpGetErrno = EAGAIN;
{$ELSE}
  Result := errno = EAGAIN;
{$ENDIF}
end;

function BadgerSysInterrupted: Boolean;
begin
{$IFDEF FPC}
  Result := fpGetErrno = EINTR;
{$ELSE}
  Result := errno = EINTR;
{$ENDIF}
end;

function BadgerSysEpollCreate: Integer;
begin
  Result := epoll_create1(0);
end;

function BadgerSysEpollCtl(Epfd, Op, Fd: Integer; Events: Cardinal; Data: Pointer): Integer;
var
  Ev: TBadgerEpollEvent;
begin
  FillChar(Ev, SizeOf(Ev), 0);
  Ev.events := Events;
  Ev.data.ptr := Data;
  Result := epoll_ctl(Epfd, Op, Fd, @Ev);
end;

function BadgerSysEpollDel(Epfd, Fd: Integer): Integer;
begin
  Result := epoll_ctl(Epfd, EPOLL_CTL_DEL, Fd, nil);
end;

function BadgerSysEpollWait(Epfd: Integer; Events: PBadgerEpollEvent;
  MaxEvents, TimeoutMs: Integer): Integer;
begin
  Result := epoll_wait(Epfd, Events, MaxEvents, TimeoutMs);
end;

function BadgerSysPipe(out ReadFd, WriteFd: Integer): Boolean;
var
  Fds: array[0..1] of Integer;
begin
  ReadFd := BADGER_SYS_INVALID;
  WriteFd := BADGER_SYS_INVALID;
  if sys_pipe(@Fds[0]) < 0 then
  begin
    Result := False;
    Exit;
  end;
  ReadFd := Fds[0];
  WriteFd := Fds[1];
  BadgerSysSetNonBlock(ReadFd);
  Result := True;
end;

function BadgerSysTick: LongWord;
var
  Ts: TBadgerTimeSpec;
begin
  FillChar(Ts, SizeOf(Ts), 0);
  if sys_clock_gettime(LINUX_CLOCK_MONOTONIC, @Ts) = 0 then
    Result := LongWord((Int64(Ts.tv_sec) * 1000) + (Ts.tv_nsec div 1000000))
  else
    Result := 0;
end;

{$ENDIF}

end.
