# Badger — Security / Quality / Performance Audit ToDo

**Audited on:** 2026-04-22
**Scope:** `src/**/*.pas` (Badger.pas, BadgerRequestHandler.pas, BadgerMultipartDataReader.pas, BadgerRouteManager.pas, BadgerMethods.pas, BadgerLogger.pas, BadgerUtils.pas, BadgerTypes.pas, BadgerHttpStatus.pas, BadgerDefines.inc, Auth/JWT/*, Auth/Basic/*)
**Auditor lens:** senior Delphi engineer, security / privacy / performance focus.
**Out of scope for this pass:** Synapse and SuperObject third-party trees, samples (checked only as callers), build scripts.

## How to read this file

Each item is:
- **Where** — unit + line range + short quote/context so you can jump to it.
- **Why** — concrete impact (AV, leak, auth bypass, DoS, perf collapse, portability break).
- **How** — general direction only. Detailed fixes happen item-by-item in later passes.
- **Prompt** — a self-contained prompt to paste into a second developer's AI agent. The agent has **no context from this audit**; the prompt tells it exactly what to open, what claim to verify, and what answer shape to produce.

## Using the per-item prompts

Each prompt is designed to be run in isolation. Give the target AI access to the Badger repo at the path it finds locally, paste one prompt, and expect a short verdict of the form:

> **Confirmed / Refuted / Partially-confirmed** — <one line> · **Suggested severity:** <CRIT|HIGH|MED|LOW> · **Exploit path / repro:** <bullets> · **Recommended action:** <fix-now | plan-fix | defer | wontfix> · **Evidence:** <file:line refs, test commands, or reasoning>.

If the agent cannot confirm from code alone, it should say so and list what's missing (sample client, running server, specific Delphi version, etc.) rather than guess.

**Severity legend** (first word of each item):
- **[CRIT]** — security impact, data loss, crash under normal traffic, or silent auth bypass.
- **[HIGH]** — memory/resource corruption path, race, logic bug that breaks a documented feature.
- **[MED]**  — portability, robustness, significant perf regression under load.
- **[LOW]**  — code hygiene, minor perf, dead code, docs.

Items within a category are ordered by severity.

---

## 1. Memory & Resource Leaks

### [HIGH] Keep-alive: `Resp.HeadersCustom` and `Resp.Stream` leak across iterations
- **Where:** [BadgerRequestHandler.pas:263-275](src/BadgerRequestHandler.pas#L263) (allocation is done **once** before the `repeat` loop at 277) and [BadgerRequestHandler.pas:631-641](src/BadgerRequestHandler.pas#L631) (single `finally` after the loop).
- **Why:** On a keep-alive connection the `repeat` loop handles request N, request N+1, etc., but `Resp.HeadersCustom` / `Resp.Stream` / `Req.BodyStream` are created only once. Route callbacks that call `Resp.HeadersCustom.Values[...] := ...` or assign `Resp.Stream` accumulate stale state and/or keep references to streams the route expected to own, producing leaks and cross-response header bleed.
- **How:** Allocate/release the response artifacts **inside** the `repeat` loop per request; reset `Resp := Default(THTTPResponse)` (or explicit nil + Clear) at start of each iteration.
- **Prompt:** Open `src/BadgerRequestHandler.pas` lines 263-275 and 631-641. Claim: response/request containers are allocated once **before** the keep-alive `repeat` loop and freed only **after** it, so state bleeds across iterations on the same connection. Verify: (1) confirm the allocations are outside the loop; (2) run a mental trace of a keep-alive client sending req A (route sets `Resp.HeadersCustom.Values['X-Trace']:='A'`) then req B on the same socket and note whether B's response carries A's header; (3) check whether `Resp.Stream` assigned by route A can still be referenced by iteration B. Report confirm/refute, severity, minimal repro (two pipelined requests + expected vs actual headers), and whether a per-iteration reset or full per-iteration alloc is safer. ≤150 words.

### [HIGH] `TClientSocketInfo` entries are never removed in parallel mode
- **Where:** [Badger.pas:589-596](src/Badger.pas#L589) adds to `FClientSockets`; no matching `RemoveClientSocket` in the parallel branch. [BadgerRequestHandler.pas:93-127](src/BadgerRequestHandler.pas#L93) (handler destructor) only calls `DecActiveConnections`, not `NotifyClientSocketClosed`.
- **Why:** In parallel processing mode, `FClientSockets` grows unbounded for the server lifetime, each entry holding a `TTCPBlockSocket` pointer that is freed by the handler's destructor. Subsequent `CleanupClientSockets` (server shutdown) dereferences freed memory → AV on stop. Also a steady-state memory leak.
- **How:** Have the handler destructor call `FParentServer.NotifyClientSocketClosed(FClientSocket)` **before** freeing the socket.
- **Prompt:** Open `src/Badger.pas` lines 589-604 (parallel vs non-parallel paths) and `src/BadgerRequestHandler.pas` lines 93-127 (handler destructor). Claim: in parallel mode the server adds each `TClientSocketInfo` to `FClientSockets` but no code path removes it; the handler destructor frees the socket without notifying the parent, leaving dangling pointers that `CleanupClientSockets` later dereferences on shutdown. Verify: (1) grep for every call site of `RemoveClientSocket` and `NotifyClientSocketClosed`; (2) confirm the parallel branch has no removal; (3) simulate shutdown after N parallel requests and describe what `CleanupClientSockets` would access. Report confirm/refute, whether this is a steady-state leak vs shutdown-only AV, and proposed fix location. ≤150 words.

### [HIGH] Pre-try allocations can leak on partial failure
- **Where:** [BadgerRequestHandler.pas:263-274](src/BadgerRequestHandler.pas#L263) — 8 allocations before `try` at line 275.
- **Why:** If `TStringList.Create` #N raises (OOM / corrupted heap), the already-created N-1 objects leak because the outer `finally` block has not yet been entered.
- **How:** Move each allocation inside the try/finally, or use a single construction helper that guarantees cleanup on partial failure.
- **Prompt:** Open `src/BadgerRequestHandler.pas` lines 263-275. Claim: eight `TStringList.Create` / `TMemoryStream.Create` calls run **before** the outer `try` at line 275, so if allocation #N raises the already-created N-1 objects leak. Verify: (1) list every allocation and confirm none are inside a try; (2) judge realistic likelihood (OOM, corrupted heap) vs theoretical; (3) propose either moving into the try or a helper. Report confirm/refute, realistic severity (is OOM here recoverable?), and chosen fix pattern. ≤120 words.

### [HIGH] `CleanupClientSockets` never closes sockets because `InUse` is always `True`
- **Where:** [Badger.pas:273-283](src/Badger.pas#L273) sets `SocketInfo.InUse := True` on add, **never reassigned to False**. [Badger.pas:321-345](src/Badger.pas#L321) guards CloseSocket with `if not SocketInfo.InUse`.
- **Why:** On shutdown the guard is always false, so sockets are freed (via `SocketInfo.Free`) without being closed — file descriptors leak until process exit on Linux, and dangling accept state on Windows.
- **How:** Either drop the `InUse` flag entirely (always close on cleanup) or flip it in `RemoveClientSocket` / handler teardown.
- **Prompt:** Open `src/Badger.pas` lines 273-283 (`AddClientSocket`) and 321-345 (`CleanupClientSockets`). Claim: `SocketInfo.InUse` is set to `True` on creation and never reassigned, so the `if not SocketInfo.InUse` guard in cleanup is always false and `CloseSocket` is never called on shutdown. Verify: grep the whole codebase for `InUse :=` — confirm only the `True` assignment exists. Report confirm/refute; state whether this leaks OS handles on Linux only, Windows only, or both; propose either removing the flag or wiring it. ≤120 words.

### [MED] `fDownloadStream` returns a `TStream` whose ownership is undocumented
- **Where:** [BadgerMethods.pas:133-149](src/BadgerMethods.pas#L133).
- **Why:** Callers must know to `FreeAndNil(Resp.Stream)` after `SendBuffer`. The request handler does free `Resp.Stream` at line 559/638, but user route code that calls this helper without assigning to `Resp.Stream` (e.g. diagnostic use) will silently leak.
- **How:** Document ownership in the function comment; consider returning an `IInterface`-wrapped stream or adding an `AOwnership: TStreamOwnership` parameter.
- **Prompt:** Open `src/BadgerMethods.pas` lines 133-149 (`fDownloadStream`). Claim: the returned `TStream` has no documented owner; request-handler frees `Resp.Stream` at `BadgerRequestHandler.pas:559/638` but callers who use the helper off-band will leak. Verify: grep every call site of `fDownloadStream`; confirm whether each frees the result. Report confirm/refute, whether there is an actual leak today (not just theoretical), and preferred API change. ≤100 words.

### [MED] `BadgerUtils.GetFileMIMEType` rebuilds the whole MIME list on every call
- **Where:** [BadgerUtils.pas:543-559](src/BadgerUtils.pas#L543) — calls `BuildMimelist(FMimeList)` unconditionally. `BuildMimelist` adds ~400 hard-coded pairs **and** walks HKEY_CLASSES_ROOT (hundreds of keys).
- **Why:** Both a perf bomb (file download throughput) and a memory-churn source (rebuilds a sorted TStringList every time).
- **How:** Build once in the constructor; guard with a `FBuilt: Boolean` flag.
- **Prompt:** Open `src/BadgerUtils.pas` lines 543-559 (`GetFileMIMEType`) and trace `BuildMimelist` / `GetMIMETableFromOS`. Claim: every MIME lookup walks HKEY_CLASSES_ROOT and rebuilds a ~400-entry sorted `TStringList`. Verify: (1) confirm the rebuild is unconditional; (2) estimate per-call cost on Windows (registry opens) vs Linux (no-op + hardcoded adds); (3) check whether this fires on every static file served. Report confirm/refute, estimated ms per call under cold cache, and the minimal cache pattern. ≤120 words.

### [LOW] `TBadgerUtils` is instantiated per call inside `TBadgerMethods.getMime`
- **Where:** [BadgerMethods.pas:77-87](src/BadgerMethods.pas#L77).
- **Why:** Every MIME lookup creates/destroys the whole util object (which then triggers #1.6 above). Combined, this is multiplicative.
- **How:** Cache a single `TBadgerUtils` instance per server lifetime.
- **Prompt:** Open `src/BadgerMethods.pas` lines 77-87 (`getMime`). Claim: `TBadgerUtils` is created/freed on every MIME lookup, compounding the rebuild cost documented in §1.6. Verify: confirm the `Create`/`Free` pair is per-call; check whether any caller already holds a long-lived instance. Report confirm/refute and suggested cache scope (server-wide singleton vs per-handler). ≤80 words.

---

## 2. Threading / Concurrency

### [CRIT] `Request` is passed by value to middleware — `UserID` / `UserRole` set inside **do not reach** the route
- **Where:** [BadgerTypes.pas:53](src/BadgerTypes.pas#L53) `TMiddlewareProc = function(Request: THTTPRequest; var Response: THTTPResponse): Boolean of object;` — `Request` has no `var`. Assignments in [BadgerAuthJWT.pas:267-268](src/Auth/JWT/BadgerAuthJWT.pas#L267) (`Request.UserID := LClaims.UserID;`) and [BadgerBasicAuth.pas:91](src/Auth/Basic/BadgerBasicAuth.pas#L91) (`Request.UserID := vUsername;`) mutate a local copy only.
- **Why:** Downstream route handlers receive `Req.UserID = ''`, breaking any authorization check that relies on it. This is a silent auth-context drop, not a compile error. For applications using role-based logic inside routes this is effectively an **authorization bypass** (routes cannot tell who the user is).
- **How:** Change `TMiddlewareProc` signature to take `var Request`; likewise update `TRoutingCallback`. Cascading change in all middleware and route implementations.
- **Prompt:** Open `src/BadgerTypes.pas:53` (`TMiddlewareProc`), `src/Auth/JWT/BadgerAuthJWT.pas:267-268`, `src/Auth/Basic/BadgerBasicAuth.pas:91`, and the call site in `src/BadgerRequestHandler.pas:462-464`. Claim: `Request` is passed **by value** to middlewares, so assignments to `Request.UserID` / `Request.UserRole` inside the middleware never reach the route handler — authorization context is silently dropped. Verify: (1) confirm there is no `var` on the `Request` parameter of `TMiddlewareProc`; (2) write a 10-line test route that logs `Req.UserID` after JWT middleware runs and predict the output; (3) confirm `THTTPRequest` is a record, not a class. Report confirm/refute with Delphi semantics citation, real-world severity (are any deployed routes relying on `UserID`?), and the blast radius of flipping the signature to `var`. ≤200 words.

### [HIGH] `FMiddlewares` mutated and iterated without locking
- **Where:** [Badger.pas:399-403](src/Badger.pas#L399) adds at runtime; [BadgerRequestHandler.pas:64-66, 88-90](src/BadgerRequestHandler.pas#L64) clone the list on each request without holding a lock; destructor loop at [Badger.pas:201-212](src/Badger.pas#L201) iterates/frees during shutdown.
- **Why:** A middleware added while a request is being accepted can cause the handler constructor to read a growing list mid-realloc (AV or partial iteration). On shutdown a handler thread can still be cloning while the destructor frees items → use-after-free.
- **How:** Protect `FMiddlewares` with the existing `FSocketLock` (or a dedicated CS) for add/clone/teardown, or take a snapshot once after `Start` and freeze the list.
- **Prompt:** Open `src/Badger.pas:399-403` (`AddMiddleware`), `src/BadgerRequestHandler.pas:58-66` and `:82-90` (clone loops), and `src/Badger.pas:201-212` (destructor). Claim: `FMiddlewares` is read (cloned) by request-handler threads and mutated by `AddMiddleware` / destructor with **no lock**. Verify: grep all access paths; identify whether middlewares are ever added after `Start` in any sample; assess whether a TList realloc during a clone is reachable. Report confirm/refute, realistic exposure (is `AddMiddleware`-during-serving a supported use case per `README`?), and minimum-surface fix. ≤150 words.

### [HIGH] `FSocketLock` held for the entire accept + handler spawn path
- **Where:** [Badger.pas:567-634](src/Badger.pas#L567) — `FSocketLock.Acquire` wraps CanRead, Accept, AddClientSocket, handler creation, and even the `Sleep(10)` backoff at line 577.
- **Why:** Serializes the entire accept loop, including a 10 ms sleep when max connections are reached — i.e. the server holds the global socket lock while sleeping. Under load this is a throughput ceiling far below the documented "30k req/s".
- **How:** Narrow the critical section to guarding the socket list only; the accept itself should happen outside the lock. Move the backoff sleep outside any critical section.
- **Prompt:** Open `src/Badger.pas:554-651` (`TBadger.Execute`). Claim: `FSocketLock` is held for the entire accept + handler spawn + `Sleep(10)` backoff path — the server holds its global lock while sleeping, capping real throughput far below the README's 30 k req/s. Verify: confirm the lock scope; run (mentally or with a benchmark) a 4-concurrent-client test and state whether accepts can happen in parallel. Report confirm/refute, measured-or-estimated throughput impact, and the smallest section of code that actually needs the lock. ≤150 words.

### [MED] Destructor waits 15 s for `FActiveConnections > 0` but `DecActiveConnections` no-ops during shutdown
- **Where:** [Badger.pas:155-166](src/Badger.pas#L155) waits while `FActiveConnections > 0`; [Badger.pas:377-391](src/Badger.pas#L377) skips `InterlockedDecrement` when `FIsShuttingDown`.
- **Why:** Once shutdown is initiated, the counter is frozen. If handlers are still live when shutdown starts, the wait always times out (15 s added to every Stop in parallel mode). Also produces misleading log counts.
- **How:** Decrement unconditionally; gate only the "notify" side effects on `FIsShuttingDown`.
- **Prompt:** Open `src/Badger.pas:155-166` (destructor wait) and `src/Badger.pas:377-391` (`DecActiveConnections`). Claim: once `FIsShuttingDown = True`, `DecActiveConnections` becomes a no-op; the destructor then spins waiting for `FActiveConnections > 0` to go to zero — it never does if any handler was live at shutdown, so every stop pays the full 15 s. Verify: trace the order of events on `Stop` and confirm handlers hit the guarded branch. Report confirm/refute, whether this is observed in samples (check `sample/Lazarus/Console_Linux/project1.lpr` shutdown behaviour), and the safer decrement pattern. ≤130 words.

### [MED] `Logger` singleton's critical section serializes every log call across every server instance
- **Where:** [BadgerLogger.pas:103-126](src/BadgerLogger.pas#L103).
- **Why:** Under concurrent load the CS becomes a hot-spot; compounded by file reopen per call (§8.1). Multiple `TBadger` instances share the same `Logger` — they contend on the same lock.
- **How:** Per-instance logger, or a bounded async queue + single writer thread.
- **Prompt:** Open `src/BadgerLogger.pas:103-126`. Claim: `Logger` is a unit-level singleton; every `Log` call acquires a single global critical section, so all `TBadger` instances in the same process contend on one lock. Verify: confirm singleton scope, confirm every write path goes through `FCS`, and decide whether any code path calls `Log` from inside another held lock (deadlock risk). Report confirm/refute and estimate contention under concurrent logging. ≤100 words.

---

## 3. Input Parsing & DoS Surface

### [CRIT] Multipart filename is used as-is for `SaveToFile` — path traversal / arbitrary file write
- **Where:** [BadgerMultipartDataReader.pas:155-181](src/BadgerMultipartDataReader.pas#L155) extracts raw filename; [BadgerMethods.pas:167-172](src/BadgerMethods.pas#L167) calls `Reader.UniqueFileName(FormDataFile.FileName)` and then `FormDataFile.Stream.SaveToFile(UniqueName)` relative to CWD.
- **Why:** A malicious client can send `filename="../../Windows/System32/drivers/etc/hosts"` (or any absolute path on Unix) and overwrite files with the process's privileges. `UniqueFileName` only appends `_1` on collision — it does not strip path separators. **Remote arbitrary file write.**
- **How:** Sanitize — `ExtractFileName` to strip directory components, reject names containing `\`, `/`, `..`, colon, NUL, and reserved device names; write only to a caller-supplied safe root.
- **Prompt:** Open `src/BadgerMultipartDataReader.pas:155-181` (`ExtractFileName`) and `src/BadgerMethods.pas:151-185` (`AtuImage`). Claim: the multipart `filename` is taken from the client and passed into `SaveToFile` after only `UniqueFileName` (which just appends `_N` on collision) — allowing `../../...` or absolute paths to write anywhere the server process can write. Verify: (1) trace the filename from the HTTP boundary parser to the disk write; (2) confirm no path-stripping occurs; (3) construct a curl repro that sends `filename="../../etc/passwd"` and describe what happens on Linux and on Windows (`C:\Windows\...`). Report confirm/refute, exploit complexity (auth required?), severity, and minimal sanitization recommendation. ≤200 words.

### [CRIT] Negative `Content-Length` bypasses size cap and overflows `SetLength`
- **Where:** [BadgerRequestHandler.pas:303-319](src/BadgerRequestHandler.pas#L303). `StrToIntDef(..., 0)` accepts negative Int32. `if ContentLength > MaxRequestBodySize` passes for any negative. `Min(ContentLength, MaxBufferSize)` returns the negative. `SetLength(TempBytes, negative)` raises ERangeError or allocates pathologically depending on RTL → handler thread dies or consumes huge memory.
- **Why:** Trivially reachable by sending `Content-Length: -1`. Crashes/DoS per request.
- **How:** Reject `ContentLength < 0` with 400 before any allocation.
- **Prompt:** Open `src/BadgerRequestHandler.pas:303-319`. Claim: `Content-Length` is parsed with `StrToIntDef(..., 0)` (signed Int32), never checked `< 0`, and flows into `Min(ContentLength, MaxBufferSize)` + `SetLength(TempBytes, ...)`. A client sending `Content-Length: -1` either range-errors or allocates pathologically. Verify: trace the value end-to-end and confirm no `< 0` guard; run a mental test with `-1`, `-2147483648`, and a value exceeding Int32 range. Report confirm/refute, which Delphi/FPC RTLs raise ERangeError vs allocate, and whether the handler thread death cascades (socket leak? server crash?). ≤150 words.

### [HIGH] `ParseRequestHeader` has no per-line size cap
- **Where:** [BadgerRequestHandler.pas:130-155](src/BadgerRequestHandler.pas#L130) — `RecvString(FTimeout)` returns a single CRLF-terminated string of unbounded length.
- **Why:** A slow-loris or memory-exhaustion attacker can send one header line of gigabytes and OOM the process before the 1 MB / 50 MB body caps are even reached (headers are read before body logic runs).
- **How:** Cap total header bytes (commonly 8–16 KB per line, 64 KB cumulative); count and `Break` + 431 Request Header Fields Too Large.
- **Prompt:** Open `src/BadgerRequestHandler.pas:130-155`. Claim: `RecvString(FTimeout)` reads each header line with no length cap; a client sending one 1 GB header line OOMs the process before the 1 MB / 50 MB body caps apply. Verify: confirm no per-line or cumulative cap, identify `RecvString`'s allocation behaviour in Synapse (`blcksock`), and assess slow-loris exposure. Report confirm/refute, whether Synapse itself caps internally, and recommended limit values (8-16 KB/line, 64 KB total is typical). ≤130 words.

### [HIGH] No `Transfer-Encoding: chunked` support
- **Where:** [BadgerRequestHandler.pas:314-350](src/BadgerRequestHandler.pas#L314) always reads `ContentLength` bytes.
- **Why:** HTTP/1.1 clients that send chunked (curl with no length, many reverse proxies) are read as a 0-byte body followed by chunk markers parsed as the next request → corrupt or hung connection.
- **How:** Detect `Transfer-Encoding: chunked` and implement the chunk parser, or return 411 Length Required for chunked requests until it's supported.
- **Prompt:** Open `src/BadgerRequestHandler.pas:314-350`. Claim: the handler reads exactly `ContentLength` bytes and ignores `Transfer-Encoding: chunked`, so HTTP/1.1 chunked clients (many proxies, curl in some modes) are mis-parsed — chunk markers leak into the next request. Verify: grep for `Transfer-Encoding`/`chunked` anywhere in the repo; confirm no handler exists. Report confirm/refute, whether a 411 fallback is trivially addable, and the risk of header-smuggling if a proxy converts chunked → content-length mid-stream. ≤130 words.

### [HIGH] `CustomDecodeBase64` range-errors on short / empty input
- **Where:** [BadgerUtils.pas:623-682](src/BadgerUtils.pas#L623). Lines 673-675 index `CleanInput[Len]` and `CleanInput[Len-1]` without checking `Len >= 2`.
- **Why:** An empty or 1-char `Authorization: Basic ` value reaches this function and indexes `CleanInput[0]` / `CleanInput[-1]` → AV / ERangeError in the auth middleware thread. Reachable by unauthenticated clients.
- **How:** Guard `if Len < 2 then Exit('');` at function entry; also validate all chars are valid Base64 before decoding.
- **Prompt:** Open `src/BadgerUtils.pas:623-682` (`CustomDecodeBase64`) and `src/Auth/Basic/BadgerBasicAuth.pas:71-95` (header extraction). Claim: `CleanInput[Len]` and `CleanInput[Len-1]` are read without a `Len >= 2` guard; an empty/1-char Basic header reaches here unauthenticated and raises ERangeError / AV in the middleware thread. Verify: confirm the index pattern; confirm the call path from an `Authorization: Basic` header with empty/short value reaches this function before any validation. Report confirm/refute, whether the exception kills the handler thread (DoS amplification per connection) and the single-line fix. ≤130 words.

### [MED] `HdrParts.StrictDelimiter` only set for non-Delphi7; D7 splits on whitespace too
- **Where:** [BadgerRequestHandler.pas:388-390](src/BadgerRequestHandler.pas#L388).
- **Why:** On D7 the CORS `Access-Control-Request-Headers` parsing splits on spaces and commas, so `X-Custom Auth,Content-Type` breaks into 3 parts instead of 2. Not a security hole on its own but can reject valid preflights.
- **How:** Roll a manual CSV splitter that works across compilers, or tokenize with a `TStringList` using a known API.
- **Prompt:** Open `src/BadgerRequestHandler.pas:388-390`. Claim: `StrictDelimiter := True` is only set for non-D7 compilers, so on Delphi 7 the CORS preflight `Access-Control-Request-Headers` splitter additionally splits on whitespace, rejecting valid multi-word header lists. Verify: inspect the `{$IFDEF}` guard and confirm TStringList's D7 delimiter behaviour. Report confirm/refute and whether anyone still builds Badger on D7. ≤100 words.

### [MED] `QueryParams` not URL-decoded and not split into key/value
- **Where:** [BadgerMethods.pas:56-70](src/BadgerMethods.pas#L56) — adds the raw `a=b` pair to the list; `%20` / `+` remain encoded.
- **Why:** Route handlers that `QueryParams.Values['foo']` will not decode percent-encoding, causing silent data corruption on any unicode or special char.
- **How:** URL-decode each segment; split on `=`; feed as `key=value` to a `TStringList` with `NameValueSeparator = '='`.
- **Prompt:** Open `src/BadgerMethods.pas:56-70`. Claim: query string segments are added raw to `QueryParams` without URL-decoding and without `NameValueSeparator` configured, so `QueryParams.Values['foo']` returns `%20`-encoded garbage for any non-ASCII value. Verify: trace a request `GET /x?msg=hello%20world` and describe what `QueryParams.Values['msg']` returns. Report confirm/refute and whether route callbacks in samples rely on decoded values. ≤100 words.

### [LOW] Random filename uses unseeded `Random(10000)` (collision + predictability)
- **Where:** [BadgerMultipartDataReader.pas:180](src/BadgerMultipartDataReader.pas#L180).
- **Why:** Same sequence per process start; also tiny range → practical collisions on moderate upload volume. Not a security issue, but a reliability one.
- **How:** Add `Randomize` at init; or use `CreateGUIDString` / timestamp suffix.
- **Prompt:** Open `src/BadgerMultipartDataReader.pas:180` (`UniqueFileName`). Claim: filename suffix uses `Random(10000)` without `Randomize`, producing the same sequence every process start and only 10 k distinct values — birthday-collision probable after ~125 uploads. Verify: grep for any `Randomize` call in the repo. Report confirm/refute and preferred replacement (GUID vs nanosecond timestamp). ≤80 words.

---

## 4. Crypto / Auth Correctness

### [CRIT] JWT signature comparison is not constant-time
- **Where:** [BadgerAuthJWT.pas:147](src/Auth/JWT/BadgerAuthJWT.pas#L147) `if LSignature <> LExpectedSignature then raise`; same in [BadgerAuthJWT.pas:188](src/Auth/JWT/BadgerAuthJWT.pas#L188).
- **Why:** Delphi `<>` on strings short-circuits byte-by-byte, so validation time correlates with how many leading bytes match. A network attacker can brute-force the signature one byte at a time via timing side-channel. In-process this is usually impractical; across LAN/internet it is a known, exploitable class of attack on JWT libraries.
- **How:** Compare byte-by-byte with `xor | accumulator` to force constant-time; only branch on the accumulator at the end.
- **Prompt:** Open `src/Auth/JWT/BadgerAuthJWT.pas:147` and `:188`. Claim: JWT signature comparison uses Delphi's `<>` on strings, which short-circuits at the first differing byte; validation latency correlates with how many leading bytes match, enabling network-timing brute-force one byte at a time. Verify: confirm the operator used, and confirm no constant-time helper is wrapped around it. Report confirm/refute, realistic exploitability across LAN/WAN (cite known JWT timing CVEs if relevant), and the standard constant-time-compare pattern in Delphi. ≤150 words.

### [CRIT] JWT / Basic Auth `Authorization` header lookup uses `Pos > 0` (substring match)
- **Where:** [BadgerAuthJWT.pas:247](src/Auth/JWT/BadgerAuthJWT.pas#L247) and [BadgerBasicAuth.pas:71](src/Auth/Basic/BadgerBasicAuth.pas#L71) both use `if Pos('Authorization=', Request.Headers[I]) > 0`.
- **Why:** Any header whose name or value contains the substring `Authorization=` will match — including attacker-controlled headers like `X-Forwarded-Authorization=Bearer forged-token`. Combined with ordering (first match wins after `Break`), this may let a client supply a bearer/basic value in an unexpected header field.
- **How:** Parse headers into a key → value map keyed by exact (case-insensitive) name; do `if SameText(HeaderName, 'Authorization')`.
- **Prompt:** Open `src/Auth/JWT/BadgerAuthJWT.pas:247` and `src/Auth/Basic/BadgerBasicAuth.pas:71`. Claim: both auth middlewares search the `Headers` string list with `Pos('Authorization=', Headers[I]) > 0` — a **substring** test that matches any header whose name or value contains `Authorization=` (e.g. `X-Forwarded-Authorization`, or `X-Note: Authorization=fake`). Verify: (1) confirm the `Pos > 0` pattern in both files; (2) determine the literal delimiter used by the header parser (is it `=` or `:`?); (3) construct an HTTP request where a non-`Authorization` header triggers the match. Report confirm/refute, whether it constitutes auth bypass (e.g. spoofed forwarded headers from a reverse proxy), and the minimal exact-name-match pattern. ≤180 words.

### [CRIT] JWT protected-route match is exact (`SameText`) — sub-paths are unprotected
- **Where:** [BadgerAuthJWT.pas:231-237](src/Auth/JWT/BadgerAuthJWT.pas#L231). Same pattern in [BadgerBasicAuth.pas:54-60](src/Auth/Basic/BadgerBasicAuth.pas#L54).
- **Why:** Registering `/api/admin` as protected leaves `/api/admin/users/1` wide open. Any real REST surface is therefore undefended unless every endpoint is enumerated.
- **How:** Support prefix match (with trailing `/`) and/or glob/regex; document behaviour. Fail closed on ambiguous match.
- **Prompt:** Open `src/Auth/JWT/BadgerAuthJWT.pas:231-237` and `src/Auth/Basic/BadgerBasicAuth.pas:54-60`. Claim: the protected-route check uses `SameText(Request.URI, FProtectedRoutes[I])` — exact-match only. Registering `/api/admin` as protected leaves `/api/admin/users/1` (and every other sub-path) completely unauthenticated. Verify: (1) confirm `SameText` (exact) vs prefix; (2) send a mental request to `/api/admin/anything` with no token and describe whether the middleware returns 401 or falls through; (3) check whether any sample depends on sub-path coverage. Report confirm/refute, severity (effectively "auth is opt-in per endpoint"), and recommended prefix-match behaviour. ≤180 words.

### [HIGH] JWT `alg` header not validated / not pinned
- **Where:** [BadgerAuthJWT.pas:127-166](src/Auth/JWT/BadgerAuthJWT.pas#L127) — signature is always recomputed with HS256 regardless of the received header.
- **Why:** Recomputation saves the library from the classic `alg: none` bypass, but does not reject tokens whose header claims a different algorithm. A future change that defers to the header (or a downstream library that does) inherits the trap. Also: iss/aud/nbf claims are never checked.
- **How:** Parse the header, require `alg = HS256` and `typ in {JWT, Refresh}`; validate `nbf`, `iss` and `aud` (configurable).
- **Prompt:** Open `src/Auth/JWT/BadgerAuthJWT.pas:127-166`. Claim: the JWT header's `alg`, `typ`, `iss`, `aud`, `nbf` claims are never validated — the server always recomputes with HS256 regardless of what the token declares. Verify: grep `Header.alg`, `iss`, `aud`, `nbf` usage in the auth unit. Report confirm/refute; while `alg=none` bypass is blocked by recomputation, list the remaining concrete risks (cross-tenant token reuse, pre-activation tokens) and recommend the minimum claim set to enforce. ≤150 words.

### [HIGH] Token files written with default permissions; contents are plaintext JWT
- **Where:** [BadgerJWTUtils.pas:44-72 and 106-138](src/Auth/JWT/BadgerJWTUtils.pas#L44).
- **Why:** On Unix the refresh token is world-readable (default umask). On Windows the file inherits ACLs from the parent folder (often user-writable). Anyone with local read access can steal and replay tokens until expiry.
- **How:** `fpchmod(FileName, &600)` on Unix; SetNamedSecurityInfo restricting to current user on Windows; ideally encrypt at rest with a key derived from FSecret + OS DPAPI.
- **Prompt:** Open `src/Auth/JWT/BadgerJWTUtils.pas:44-72` and `:106-138`. Claim: access/refresh tokens are persisted as plaintext JWT with default file permissions (world-readable on Unix under default umask; inherited ACLs on Windows). Verify: grep for any `fpchmod`, `SetFileAttributes`, DPAPI, or encryption call. Report confirm/refute, real-world risk given typical deployment (running as non-root service user? multi-tenant host?), and the minimum OS-level permission fix. ≤130 words.

### [HIGH] Basic Auth stores and compares passwords in plaintext (non-constant-time)
- **Where:** [BadgerBasicAuth.pas:34-40, 89](src/Auth/Basic/BadgerBasicAuth.pas#L34). `(vUsername = FUsername) and (vPassword = FPassword)`.
- **Why:** Password is kept in memory verbatim for the server lifetime and compared with short-circuit `=`. Same timing side-channel as §4.1, plus plaintext at rest in a running process (dumpable).
- **How:** Store only a salted hash (bcrypt/argon2/PBKDF2-SHA256 via a well-known lib) and use constant-time hash comparison.
- **Prompt:** Open `src/Auth/Basic/BadgerBasicAuth.pas:34-40` and `:89`. Claim: username and password are kept as plaintext strings in the middleware instance for the server lifetime and compared with `=` (non-constant-time). Any memory dump or core-file leaks credentials; timing side-channel also applies. Verify: confirm the storage layout and comparison operator; check whether any constructor accepts a pre-hashed form. Report confirm/refute and recommend a minimal hash-based credential API. ≤130 words.

### [MED] `ValidateToken` leaks storage-path info via error messages
- **Where:** [BadgerAuthJWT.pas:157-162, 198-203](src/Auth/JWT/BadgerAuthJWT.pas#L157) — raises `'Token not found'` only when `FStoragePath <> ''`, and the middleware at line 277 echoes the exception `E.Message` straight into the JSON response.
- **Why:** Tells an attacker whether server-side persistence is enabled and which state a token is in (not found vs expired vs invalid signature). Enables token oracle attacks.
- **How:** Map all auth failures to a single generic 401 body server-side; log details internally only.
- **Prompt:** Open `src/Auth/JWT/BadgerAuthJWT.pas:157-162`, `:198-203`, and `:277` (middleware `E.Message` passthrough). Claim: auth error messages differentiate "token not found", "expired", "invalid signature", "storage path" — all echoed verbatim to the client, enabling token-state oracle attacks. Verify: list every distinct exception message and confirm the middleware's JSON response includes `E.Message`. Report confirm/refute and list the minimum generic-error mapping. ≤120 words.

### [MED] `CustomDecodeBase64` silently accepts invalid alphabet bytes
- **Where:** [BadgerUtils.pas:632-657](src/BadgerUtils.pas#L632) — `Base64Table` init fills with 255; invalid input bytes produce 255 values that OR into output bytes.
- **Why:** Malformed Basic / JWT headers decode into garbage strings rather than an error, masking bugs and feeding unexpected bytes into downstream string comparisons.
- **How:** Validate each char against the alphabet before decoding; raise on invalid input.
- **Prompt:** Open `src/BadgerUtils.pas:632-657` (`CustomDecodeBase64`). Claim: the lookup table is initialised to 255 for non-alphabet bytes; those 255s are then OR'd into output bytes, so malformed Base64 silently decodes to garbage rather than raising. Verify: inspect the init loop and the decode inner loop; construct a short input with invalid chars and describe output bytes. Report confirm/refute and a one-line validation fix. ≤100 words.

---

## 5. CORS & Header Injection

### [CRIT] Response header values are not CRLF-sanitized — response splitting
- **Where:** [BadgerRequestHandler.pas:197-203](src/BadgerRequestHandler.pas#L197) concatenates `HeaderCustom.Names[i] + ':' + HeaderCustom.ValueFromIndex[i] + CRLF`. Values such as `Access-Control-Allow-Origin` are set to the client-supplied `Origin` at [BadgerRequestHandler.pas:432, 517](src/BadgerRequestHandler.pas#L432) without validation.
- **Why:** A client sending `Origin: evil.com\r\nSet-Cookie: x=y\r\n\r\n<html>` plants a forged Set-Cookie and HTML body in the response (classic HTTP response splitting) → session fixation, cache poisoning, XSS on same-origin endpoints.
- **How:** Reject / strip CR/LF from every header value before writing the response. Validate `Origin` against an allow-list pattern (scheme + host only).
- **Prompt:** Open `src/BadgerRequestHandler.pas:197-203` (header serialization) and `:432, :517` (where `Access-Control-Allow-Origin` is set from request `Origin`). Claim: response header values are concatenated with `:` and `CRLF` without stripping embedded CR/LF — a client sending `Origin: a\r\nSet-Cookie: x=y\r\n\r\n<html>...` injects an extra Set-Cookie and HTML body (HTTP response splitting). Verify: (1) confirm no sanitisation on the value side; (2) confirm `Origin` flows unvalidated into `Access-Control-Allow-Origin`; (3) build a curl repro (requires a tool that lets raw CRLF through; netcat). Report confirm/refute, severity (session fixation, cache poisoning, XSS via same-origin trust), and minimal sanitizer. ≤200 words.

### [HIGH] `CorsAllowedOrigins` defaults to `*` even when CORS is disabled
- **Where:** [Badger.pas:111, 120](src/Badger.pas#L111) — constructor adds `'*'` unconditionally; `FCorsEnabled := False` but the list is populated.
- **Why:** A future call site that flips `CorsEnabled := True` (or a future refactor that reads the list directly) immediately exposes an allow-all policy. Secure-default principle violated.
- **How:** Do not pre-populate with `*`; require the caller to opt in explicitly.
- **Prompt:** Open `src/Badger.pas:100-125` (constructor). Claim: `CorsAllowedOrigins.Add('*')` runs unconditionally in the constructor even though `FCorsEnabled := False`, so any later code that flips `CorsEnabled := True` (or reads the list directly) gets an allow-all policy by surprise. Verify: confirm the Add happens regardless of `CorsEnabled`; check whether any consumer reads `CorsAllowedOrigins` outside `CorsEnabled` branches. Report confirm/refute and recommended secure-default. ≤100 words.

### [HIGH] `AllowCredentials=true` combined with `*` reflects the **request Origin** — CSRF anywhere
- **Where:** [BadgerRequestHandler.pas:420-425, 511-516](src/BadgerRequestHandler.pas#L420).
- **Why:** The code correctly notices that `*` + credentials is illegal per the CORS spec, but "fixes" it by echoing the caller's `Origin`. Net effect: any origin on the internet can make credentialed cross-origin calls. Worse than failing closed.
- **How:** If `AllowCredentials=true`, require the origin to appear explicitly in `CorsAllowedOrigins` (no `*` fallback) — otherwise return 403.
- **Prompt:** Open `src/BadgerRequestHandler.pas:420-425` and `:511-516`. Claim: when `AllowCredentials=true` combined with `*` in allow-list, the code "fixes" the CORS spec violation by **reflecting the caller's `Origin` verbatim** into `Access-Control-Allow-Origin` — effectively allowing any internet origin to make credentialed cross-site requests. Verify: trace the branch; confirm `Origin` is echoed without allow-list check. Report confirm/refute, full CSRF surface implications (attacker site can read authenticated responses), and the correct behaviour (require explicit origin listing, no wildcard fallback when credentials are on). ≤180 words.

### [MED] Preflight echoes `Access-Control-Request-Headers` values rather than the server's canonical names
- **Where:** [BadgerRequestHandler.pas:384-418](src/BadgerRequestHandler.pas#L384) — `HeadersStr` is built from user-supplied `ACRH`.
- **Why:** After CRLF sanitization (§5.1), this is not directly exploitable, but it still means the response contradicts the allow-list (e.g. case mismatch) and is harder to audit.
- **How:** Reply with the server-configured canonical header names, not whatever the client sent.
- **Prompt:** Open `src/BadgerRequestHandler.pas:384-418`. Claim: the preflight response builds `Access-Control-Allow-Headers` from the client's `Access-Control-Request-Headers`, not from the server's configured allow-list. After CRLF sanitization this is not directly exploitable, but it means the response contradicts the policy. Verify: trace `ACRH` into `HeadersStr`. Report confirm/refute and whether any test relies on reflection semantics. ≤100 words.

---

## 6. Socket Lifecycle & Error Paths

### [HIGH] Keep-alive loop does not reset `Handled` / response state between iterations
- **Where:** [BadgerRequestHandler.pas:274](src/BadgerRequestHandler.pas#L274) sets `Handled := False` before the loop; [BadgerRequestHandler.pas:459-479](src/BadgerRequestHandler.pas#L459) sets it `True` inside the middleware loop but it is never reset on the next iteration.
- **Why:** After the first request on a keep-alive connection is handled by a middleware (e.g. auth short-circuit), every subsequent request on the same connection skips route matching entirely and returns the previous response body.
- **How:** Move `Handled := False` to the top of the `repeat` body; same for `Resp`, `QueryParams.Clear`, etc.
- **Prompt:** Open `src/BadgerRequestHandler.pas:274` (`Handled := False` pre-loop) and `:459-479` (middleware sets `Handled := True`). Claim: `Handled` is initialised **once** before the keep-alive `repeat` loop and never reset per iteration, so after any middleware short-circuits the first request, every later request on the same connection skips route matching and returns stale response state. Verify: confirm no reset inside the loop; run a mental test of `req1 → auth-fail 401; req2 → normal /ping` on the same socket; predict what req2 returns. Report confirm/refute, reproducibility, and fix location (top of `repeat` body). ≤160 words.

### [HIGH] On `Accept` failure, `OnResponse` is invoked with an uninitialized/half-set `TResponseInfo`
- **Where:** [Badger.pas:607-616](src/Badger.pas#L607) — only 3 of the record's fields are populated before `FOnResponse(ResponseInfo)`; `Headers` (TStringList pointer) is whatever garbage was on the stack.
- **Why:** Accessing `ResponseInfo.Headers` from user code AVs; even reading `Timestamp` returns garbage. Also this event fires *inside* the critical section — user code can deadlock the server.
- **How:** Zero-init the record (`FillChar(ResponseInfo, SizeOf(ResponseInfo), 0);`), set every field, and fire the event outside the critical section.
- **Prompt:** Open `src/Badger.pas:607-616`. Claim: on `Accept` failure the code fires `FOnResponse(ResponseInfo)` with a `TResponseInfo` whose `Headers` pointer and other fields are uninitialised stack garbage; the event also fires **inside** `FSocketLock`, so user code can deadlock the server. Verify: list every field of `TResponseInfo` set before the event call vs left untouched; confirm the lock is held. Report confirm/refute, likely AV sites (`ResponseInfo.Headers.Count`), and the two-part fix (FillChar + move fire outside lock). ≤150 words.

### [MED] `Destroy` on the handler depends on `FreeOnTerminate = True` racing with external references
- **Where:** [BadgerRequestHandler.pas:52, 76](src/BadgerRequestHandler.pas#L52); [Badger.pas:600-604](src/Badger.pas#L600) — non-parallel branch calls `RemoveClientSocket(ClientSocket)` immediately after `THTTPRequestHandler.Create` resumes, before the handler thread has run a single instruction.
- **Why:** In non-parallel mode the server thread removes the socket from `FClientSockets` before the handler has actually consumed it. Works today because the handler owns the socket via its field, but it means `CleanupClientSockets` has no knowledge of active connections in non-parallel mode (inconsistent with the parallel branch).
- **How:** Decide on one ownership model (handler-owned, server-tracked) and unify both branches; the simplest is "server tracks lifetime, handler only borrows".
- **Prompt:** Open `src/BadgerRequestHandler.pas:52`, `:76` and `src/Badger.pas:600-604`. Claim: in non-parallel mode, the server calls `RemoveClientSocket` right after `THTTPRequestHandler.Create` resumes — before the handler thread has run. The two modes have inconsistent ownership models; shutdown cleanup sees no active connections in non-parallel mode even when handlers are mid-request. Verify: diff the two branches; state who owns the socket lifetime in each. Report confirm/refute and recommend a single unified ownership pattern. ≤150 words.

### [MED] Errors inside `Execute` repeat-loop break out but still use `FClientSocket` after the catch
- **Where:** [BadgerRequestHandler.pas:607-627](src/BadgerRequestHandler.pas#L607) — `except` block calls `FClientSocket.SendString`, but an exception during body read could have already invalidated the socket's state (LastError != 0 guarded). If `SendBuffer` fails mid-send the exception path is not re-entered.
- **Why:** Edge cases where the client drops mid-request produce a 500 response sent on a socket in an unknown state — occasional hang or zombie connection.
- **How:** Audit every `Send*` path to tolerate `LastError <> 0` and `Break` rather than retry.
- **Prompt:** Open `src/BadgerRequestHandler.pas:607-627`. Claim: the outer `except` attempts to send a 500 response on a socket whose state may already be invalid (client dropped mid-request). A second failure inside the except path is not caught, potentially leaving the handler thread in an undefined state. Verify: examine every `Send*` call in the except path; check if they all check `LastError` and return gracefully. Report confirm/refute and whether the affected case is reachable under normal client disconnect. ≤130 words.

---

## 7. Platform / Compiler Portability

### [MED] Mixed `{$IFDEF MSWINDOWS}` vs `{$IFDEF WINDOWS}`
- **Where:** [Badger.pas:8](src/Badger.pas#L8) uses `MSWINDOWS`; [BadgerUtils.pas:60, 68](src/BadgerUtils.pas#L60) uses `WINDOWS`; [BadgerLogger.pas:8, 24, 96, 119](src/BadgerLogger.pas#L8) uses `MSWINDOWS`.
- **Why:** `WINDOWS` is defined for FPC but not always for older Delphi targets. The two symbols drift silently; registry MIME enumeration is only compiled when `WINDOWS` is set, potentially excluding 32-bit Delphi Windows builds depending on settings.
- **How:** Pick one symbol across the codebase (`MSWINDOWS` is the Delphi convention, `WINDOWS` works for FPC; use `{$IF DEFINED(MSWINDOWS) OR DEFINED(WINDOWS)}` once and alias).
- **Prompt:** Grep the repo for `{$IFDEF MSWINDOWS}` vs `{$IFDEF WINDOWS}`. Claim: different units use different Windows-identification symbols inconsistently; on some compiler configs only one is defined, silently excluding code. Verify: list every occurrence, identify which units use which, and state when each symbol is defined in Delphi and FPC. Report confirm/refute and recommend a single convention (alias via `BadgerDefines.inc`). ≤120 words.

### [MED] `BadgerMethods` brings in `DB` for no visible reason
- **Where:** [BadgerMethods.pas:6](src/BadgerMethods.pas#L6).
- **Why:** Drags `DB.pas` into every consumer — adds runtime footprint, breaks minimal-dependency builds (e.g. console Linux with no db-aware VCL).
- **How:** Remove `DB` from the uses clause if unused (confirm with build).
- **Prompt:** Open `src/BadgerMethods.pas:6`. Claim: `DB` is in the uses clause but nothing from `DB.pas` is referenced in the unit. This drags DB runtime into every consumer. Verify: grep the unit for any `TField`, `TDataSet`, `TClientDataSet`, or similar symbols. Report confirm/refute and whether removal breaks any downstream unit (check `sample/*`). ≤80 words.

### [LOW] Unsafe TMethod pointer construction for pre-Delphi2009
- **Where:** [BadgerRequestHandler.pas:215-223](src/BadgerRequestHandler.pas#L215) and [BadgerRequestHandler.pas:240, 486](src/BadgerRequestHandler.pas#L240).
- **Why:** `MethodPointer.Code := Pointer(ARoute)` assumes plain procedural pointer layout; will break if anyone passes a true method pointer via D7 compat path.
- **How:** Drop D7 support or document the callback must be a plain `procedure` on D7.
- **Prompt:** Open `src/BadgerRequestHandler.pas:215-223`, `:240`, `:486`. Claim: on pre-Delphi2009 compilers, `TMethod` records are constructed with `MethodPointer.Code := Pointer(ARoute)` — assuming a plain procedural pointer layout, not a method-of-object. If a consumer actually supplies a method pointer on D7 it will crash. Verify: confirm the `{$IFDEF}` branch and whether any D7 sample uses method-of-object routes. Report confirm/refute and recommend either dropping D7 support or documenting the restriction. ≤120 words.

### [LOW] `{$IFDEF UNICODE}` vs `{$IFDEF FPC}` both set `Buffer` type to `TBytes` / `TArray<Byte>` inconsistently
- **Where:** [BadgerJWTUtils.pas:48-52, 110-114](src/Auth/JWT/BadgerJWTUtils.pas#L48).
- **Why:** Two parallel branches that end up writing the same code; future edits may drift.
- **How:** Consolidate to a single `{$IF Declared(TArray)}` branch or just `TBytes`.
- **Prompt:** Open `src/Auth/JWT/BadgerJWTUtils.pas:48-52` and `:110-114`. Claim: `{$IFDEF UNICODE}` and `{$IFDEF FPC}` branches both end up selecting `TBytes` / `TArray<Byte>` via parallel code that can drift. Verify: compare both branches byte-for-byte. Report confirm/refute and propose a single consolidated branch. ≤80 words.

---

## 8. Logging & Observability

### [HIGH] Log file is opened and closed per message
- **Where:** [BadgerLogger.pas:70-88](src/BadgerLogger.pas#L70). Uses `AssignFile / Append / Rewrite / CloseFile` for every call.
- **Why:** At moderate traffic, file open/close dominates request latency; on Windows it additionally hits the filesystem cache sync path. `Rewrite` vs `Append` decision is based on `FileExists` racing with concurrent writers.
- **How:** Keep the file open for the logger's lifetime (TFileStream in append mode) and flush periodically; optionally move file I/O to a background writer.
- **Prompt:** Open `src/BadgerLogger.pas:70-88`. Claim: every log message calls `AssignFile / Append or Rewrite / WriteLn / CloseFile`; at moderate traffic, file open/close dominates latency and there is a `FileExists` race between concurrent writers. Verify: confirm open/close per call; note whether `Append` vs `Rewrite` is decided on a racy `FileExists`. Report confirm/refute, measured-or-estimated per-call cost (μs), and the minimal persistent-handle pattern. ≤130 words.

### [MED] Exceptions in `WriteToFile` are silently swallowed
- **Where:** [BadgerLogger.pas:86-87](src/BadgerLogger.pas#L86) — bare `except end;`.
- **Why:** Disk full, permission error, or locked file produces no warning anywhere; operators think logs are being written. Classic debug nightmare.
- **How:** On first failure, disable file logging and surface via console/Debugger output; reset on user action.
- **Prompt:** Open `src/BadgerLogger.pas:86-87`. Claim: a bare `except end;` swallows every file-write exception (disk full, permission denied, locked). Operators see no warning. Verify: confirm the empty except; check whether `LogToFile` has any liveness probe. Report confirm/refute and the minimal "disable + surface once" pattern. ≤80 words.

### [MED] Commented-out `Logger.Info` calls litter `Badger.pas` and `BadgerRequestHandler.pas`
- **Where:** [Badger.pas:157-165, 257, 279, 301, 317, 330, 338, 350, 374, 387, 409, 415, 421, 432, 435, 441, 445, 447, 451, 458, 465, 486, 510, 518, 537](src/Badger.pas#L157) and similar `// OutputDebugString` blocks.
- **Why:** Dev instrumentation was commented out rather than gated. Noise in diffs, easy to re-enable accidentally, and gives the wrong impression about what the server logs under failure.
- **How:** Replace with `Logger.Debug(...)` (already level-gated) or delete; keep only the ones that are meaningful.
- **Prompt:** Grep `src/Badger.pas` and `src/BadgerRequestHandler.pas` for `// Logger.` and `// OutputDebugString`. Claim: dev instrumentation was commented out instead of gated with `{$IFDEF DEBUG}` / `Logger.Debug`. Verify: count occurrences. Report confirm/refute and recommend batch replacement. ≤60 words.

### [LOW] `isActive := False` by default — nothing logs until the host opts in
- **Where:** [BadgerLogger.pas:58](src/BadgerLogger.pas#L58).
- **Why:** Log lines in `Badger.Create` / `Destroy` are effectively unreachable unless the user discovers the flag. Masking operator-visible failures.
- **How:** Default to `True` with level `llWarning`; honor a `BADGER_LOG` env var.
- **Prompt:** Open `src/BadgerLogger.pas:58` (`FisActive := False`). Claim: the logger defaults to off; `Log(Level, Msg)` early-exits on `not FisActive`, so `Badger.Create`/`Destroy` warnings never reach an operator until they discover the flag. Verify: confirm default value and the `if not FisActive then Exit` guard. Report confirm/refute and recommended safe default. ≤80 words.

---

## 9. Build / Supply Chain

### [MED] No CI / no reproducible build in repo
- **Where:** No `.github/workflows`, no `azure-pipelines.yml`, no `Makefile` at the repo root.
- **Why:** Every PR merges blind; platform regressions surface only when a user builds a sample. "Fix Linux stop/restart" in recent history suggests this is already biting.
- **How:** Add a GitHub Actions job that compiles each sample against Delphi CE or FPC for Linux + Windows; run on every PR.
- **Prompt:** Inspect the repo root. Claim: there is no CI configuration (`.github/workflows`, `azure-pipelines.yml`, `Makefile`). Recent commits fixing "Linux stop/restart" suggest platform regressions ship without catch. Verify: confirm absence; list any existing automation. Report confirm/refute and recommend a minimal FPC-cross-compile-on-push workflow. ≤80 words.

### [MED] Third-party dependencies via Git submodule are not pinned to a commit visibly documented
- **Where:** `.gitmodules`, `ThirdParty/`.
- **Why:** Silent upstream change in Synapse or SuperObject can alter behaviour between two equivalent-looking checkouts. No SBOM, no CVE tracking.
- **How:** Document the exact commit SHA expected in `THIRD_PARTY.md`; add a quick integrity script.
- **Prompt:** Open `.gitmodules` and inspect `ThirdParty/` layout. Claim: Synapse and SuperObject are vendored via submodules without pinned commits documented in the repo, so silent upstream changes alter behaviour between checkouts; no SBOM / CVE tracking exists. Verify: list submodule URLs and current commit SHA; check whether `README` or a dedicated file records expected versions. Report confirm/refute and propose minimal pinning + integrity-check script. ≤100 words.

### [LOW] License compatibility not analyzed
- **Where:** `LICENSE` (MIT) vs Synapse (MPL/LGPL) vs SuperObject (MPL).
- **Why:** Static-linked MPL code into an MIT distribution is usually fine but needs to be documented; redistributors need the bundled notices.
- **How:** Add a NOTICE file listing bundled licenses.
- **Prompt:** Open `LICENSE` and inspect `ThirdParty/` for Synapse (MPL/LGPL) and SuperObject (MPL) license files. Claim: the MIT license at repo root is not accompanied by a NOTICE file documenting bundled third-party MPL/LGPL components, which redistributors need. Verify: presence/absence of NOTICE, COPYING, or THIRD_PARTY docs. Report confirm/refute and recommend minimal NOTICE content. ≤80 words.

---

## 10. Testing & Regression Safety

### [HIGH] Zero unit / integration tests in the repository
- **Where:** No `tests/`, no DUnit/DUnitX/FpcUnit project.
- **Why:** Every fix in this list risks regression with nothing to catch it. Past fixes ("Linux stop/restart") came from manual testing only.
- **How:** Bootstrap a minimal DUnitX + FpcUnit harness: HTTP fuzz (bad headers, bad body, slow client), JWT roundtrip, CORS preflight, path-traversal multipart. Each item in this file should land with a test.
- **Prompt:** Inspect the repo for any `tests/`, `*.dpr` test project, DUnit / DUnitX / FpcUnit references. Claim: there are zero automated tests, so every fix in this audit risks silent regression. Verify: presence/absence of test projects and CI references. Report confirm/refute and propose a minimal starter harness (5-10 tests covering the CRIT items in §2, §3, §4). ≤100 words.

---

## 11. Backdoors & Suspicious Constructs

### [LOW] No actual backdoors identified
- **Where:** Full read of all 13 units.
- **Why:** No hardcoded credentials, no `if User = 'admin'` shortcut, no unconditional `Result := True` in auth, no network-reachable diag endpoint, no environment variable that disables security, no phone-home.
- **How:** Keep this entry as a record of the sweep; recheck on every significant refactor.
- **Prompt:** Do an independent backdoor sweep of `src/**/*.pas`. Specifically grep for: hardcoded credentials (`Password =`, `Secret :=`, `'admin'`), unconditional `Result := True` in auth paths, environment-variable bypasses, diag/debug HTTP endpoints that skip middleware, network-reachable eval/exec. Verify: produce a list of every suspicious literal or flow-short-circuit you find. Report confirm/refute (does the original auditor's "none found" hold?) and list any new findings. ≤150 words.

### [LOW] `Logger.isActive := False` + bare `except end;` pattern is the closest thing to a silent-failure shape
- **Where:** [BadgerLogger.pas:58, 86-87](src/BadgerLogger.pas#L58).
- **Why:** Not a backdoor, but has the *shape* of one from an auditor's perspective — failures vanish. Worth calling out so it gets fixed (covered by §8.2 / §8.4).
- **How:** See §8.2 and §8.4.
- **Prompt:** Open `src/BadgerLogger.pas:58` and `:86-87`. Claim: the pattern of "off by default + bare except end;" is not a backdoor but has the auditor-shape of one — failures vanish silently. Verify and confirm that §8.2 and §8.4 cover the remediation. Report confirm/refute that remediation is fully covered by those items. ≤60 words.

---

## 12. Runtime Logic Issues (Crash & Performance)

### [CRIT] `QueryParams`, `Req.QueryParams`, `Req.Headers`, `Req.RouteParams`, `Resp.HeadersCustom` never reset between keep-alive iterations
- **Where:** [BadgerRequestHandler.pas:263-275](src/BadgerRequestHandler.pas#L263) (pre-loop init) vs repeat loop starting at line 277.
- **Why:** On the second request of a keep-alive connection, route handlers see query params / headers / custom response headers from the **previous** request. Combined with §6.1 (`Handled` stuck true), keep-alive is effectively broken — either silent data leakage across requests or wrong response served. Likely the source of sporadic "wrong response for right request" reports under load.
- **How:** Clear each list at the top of every iteration; reset `Resp := Default(THTTPResponse)` (recreating `HeadersCustom`).
- **Prompt:** Open `src/BadgerRequestHandler.pas:263-275` (pre-loop allocation) and the `repeat` body starting near line 277. Claim: `Req.QueryParams`, `Req.Headers`, `Req.RouteParams`, `Resp.HeadersCustom`, `Req.BodyStream` are allocated once **before** the keep-alive `repeat` loop and never cleared between iterations, so request N+1 sees request N's data. Verify: (1) list all per-iteration clears (or lack thereof); (2) send two pipelined requests with different query strings on one socket and describe what the second route handler sees in `QueryParams`; (3) check whether this explains any reported "wrong response for right request" bugs. Report confirm/refute, repro, and the minimal per-iteration reset pattern. ≤200 words.

### [HIGH] MIME table is rebuilt and registry re-enumerated on every file download
- **Where:** [BadgerUtils.pas:543-559](src/BadgerUtils.pas#L543); see also §1.6 and §1.7.
- **Why:** File-download latency is dominated by a full HKEY_CLASSES_ROOT walk (hundreds of registry opens) plus ~400 string adds. Concurrent downloads serialize on the registry. Benchmarks will not reproduce documented throughput.
- **How:** Build once at first use, cache in a class variable, refresh only on explicit invalidation.
- **Prompt:** Open `src/BadgerUtils.pas:543-559` and trace `BuildMimelist` / `GetMIMETableFromOS`. Claim: every file download rebuilds the MIME table, walks HKEY_CLASSES_ROOT (Windows), and re-adds ~400 entries. Verify: confirm unconditional rebuild; measure (or estimate) per-call cost on Windows vs Linux; count concurrent-download serialisation on the registry. Report confirm/refute and minimal cache pattern. ≤100 words.

### [HIGH] `TBadger.Stop` sleeps up to 15 s holding `FIsShuttingDown = True` while `DecActiveConnections` is a no-op
- **Where:** [Badger.pas:490-519](src/Badger.pas#L490); counter skip at [Badger.pas:377-391](src/Badger.pas#L377).
- **Why:** Stop always waits the full 15 s in parallel mode (see §2.4). Users perceive "server takes 15 s to stop".
- **How:** Fix §2.4 first; then the sleep loop terminates as soon as handlers finish.
- **Prompt:** Open `src/Badger.pas:490-519` (Stop path) and `:377-391` (`DecActiveConnections`). Claim: `Stop` sleeps up to 15 s waiting for `FActiveConnections` to reach 0, but the counter is frozen once `FIsShuttingDown = True` because `DecActiveConnections` becomes a no-op — so Stop always burns the full 15 s in parallel mode. Verify: trace the sequence on `Stop`; confirm the no-op branch fires before any handler completes. Report confirm/refute and the ordered fix (fix §2.4 first, then the Stop wait naturally terminates). ≤130 words.

### [MED] Middleware list is deep-cloned (with fresh `TMiddlewareWrapper`) on every request
- **Where:** [BadgerRequestHandler.pas:58, 64-66, 82, 88-90](src/BadgerRequestHandler.pas#L58).
- **Why:** Every HTTP request allocates N wrapper objects + a `TList` and frees them in `Destroy`. At 30 k req/s with 3 middlewares that's ~90 k alloc+free per second on the global heap — FastMM contention.
- **How:** Snapshot the list once at `TBadger.Start`, share the read-only list with all handlers (no clone, no per-request free).
- **Prompt:** Open `src/BadgerRequestHandler.pas:58-66` and `:82-90`. Claim: every HTTP request deep-clones the server's middleware list into a new `TList` of fresh `TMiddlewareWrapper` objects and frees them on request destroy; at documented 30 k req/s with 3 middlewares that's ~90 k alloc+free per second on the global heap. Verify: confirm clone+free per request; estimate FastMM contention. Report confirm/refute and recommend a read-only shared snapshot taken once after `Start`. ≤130 words.

### [MED] Route matching calls `SplitString` for both request path and **every** candidate pattern on every request
- **Where:** [BadgerRouteManager.pas:228-294](src/BadgerRouteManager.pas#L228) — `SplitString` twice in the outer function + once per candidate in the loop.
- **Why:** Two `TStringList` allocations per request plus one per bucket entry. Under load, measurable GC pressure.
- **How:** Pre-parse each registered pattern at registration time into a fixed array of segments stored on `TRouteEntry`; only split the request path per request.
- **Prompt:** Open `src/BadgerRouteManager.pas:228-294`. Claim: route matching calls `SplitString` on the request path and on **every candidate** registered pattern on **every** request — N+1 `TStringList` allocations per request where N is bucket size. Verify: count `SplitString` call sites in the match function. Report confirm/refute and recommend pre-parsing patterns at registration into a segment array on `TRouteEntry`. ≤100 words.

### [MED] `FindBoundary` reads `BoundaryLen` bytes per candidate position in a loop
- **Where:** [BadgerMultipartDataReader.pas:115-145](src/BadgerMultipartDataReader.pas#L115) — `MemoryStream.Position := i; ReadBuffer(Buffer^, BoundaryLen);` inside `while i <= Size - BoundaryLen do Inc(i);`.
- **Why:** O(N·M) with a syscall per step (stream seek+read, not direct memory compare). For a 50 MB multipart body with a 20-byte boundary that's ~10⁹ iterations.
- **How:** Cast to `TMemoryStream` and work with `Memory` pointer directly — single pointer walk with `CompareMem`; O(N·M) arithmetic but no syscalls.
- **Prompt:** Open `src/BadgerMultipartDataReader.pas:115-145` (`FindBoundary`). Claim: each candidate position performs `Stream.Position := i; ReadBuffer(...)` to read `BoundaryLen` bytes, resulting in O(N·M) **syscalls** (not just arithmetic). A 50 MB multipart body with a 20-byte boundary performs ~10⁹ seek+read cycles. Verify: confirm the inner loop does Position+ReadBuffer per step; note it's a `TMemoryStream` (so `.Memory` pointer is available). Report confirm/refute, estimated seconds per 50 MB upload, and the direct-memory-scan alternative. ≤130 words.

### [MED] `Logger` bare `WriteLn` can deadlock / crash in GUI/service hosts with no console
- **Where:** [BadgerLogger.pas:90-93](src/BadgerLogger.pas#L90) — `if FLogToConsole then WriteLn(Msg);`.
- **Why:** Windows GUI app with no console AVs on first `WriteLn`; a service may likewise throw. Current `FisActive := False` default hides this until a consumer flips it.
- **How:** Guard with `IsConsole` on Delphi (`System.IsConsole`) and `stdout`/`IsATTY` on FPC; or route through `OutputDebugString` only.
- **Prompt:** Open `src/BadgerLogger.pas:90-93`. Claim: `WriteLn(Msg)` runs unconditionally when `FLogToConsole = True`; on Windows GUI apps / services with no console, bare `WriteLn` can raise (Delphi: "I/O error 105") or silently fail. Verify: confirm no `IsConsole` guard; check FPC `{$MODE}` behaviour. Report confirm/refute and recommend the `IsConsole` guard pattern. ≤100 words.

### [LOW] `while Pos(DoubleSlash, URI) > 0 do URI := StringReplace(...)` is O(N²) on pathological URIs
- **Where:** [BadgerMethods.pas:47-48](src/BadgerMethods.pas#L47).
- **Why:** A request line like `/a/////b` works fine, but an attacker-controlled `/////////...` of large length incurs quadratic string replace. Minor.
- **How:** Single pass replace or a manual scan.
- **Prompt:** Open `src/BadgerMethods.pas:47-48`. Claim: `while Pos('//', URI) > 0 do URI := StringReplace(URI, '//', '/', [rfReplaceAll])` is O(N²) on a crafted URI of many consecutive slashes because each `StringReplace` allocates a new string. Verify: compute cost for a URI like `/` * 10 000. Report confirm/refute and recommend a single-pass scan. ≤80 words.

---

## 13. Minor / Nice-to-have

### [LOW] `TBytes` redeclared locally in `BadgerUtils`
- **Where:** [BadgerUtils.pas:15](src/BadgerUtils.pas#L15) defines `TBytes = array of Byte`.
- **Why:** On Delphi 2009+ `System.TBytes` exists already; local alias can shadow the RTL type, causing subtle overload resolution differences.
- **How:** Gate the declaration behind `{$IFNDEF UNICODE}`.
- **Prompt:** Open `src/BadgerUtils.pas:15`. Claim: `TBytes = array of Byte` is redeclared locally even on Delphi 2009+ where `System.TBytes` exists; this can shadow the RTL type and cause overload resolution differences. Verify: check the declaration and confirm compiler behaviour. Report confirm/refute and recommend `{$IFNDEF UNICODE}` gating. ≤80 words.

### [LOW] Commented `ShowMessage` calls inside library code
- **Where:** [BadgerMultipartDataReader.pas:211, 264, 273](src/BadgerMultipartDataReader.pas#L211).
- **Why:** Left-over GUI calls in a server-side library. Uncommenting would break console / service hosts hard.
- **How:** Replace with `Logger.Debug` and delete the commented lines.
- **Prompt:** Open `src/BadgerMultipartDataReader.pas:211, 264, 273`. Claim: `ShowMessage` calls remain commented-out inside a server-side library; uncommenting on a service host would block on an invisible dialog. Verify: count occurrences. Report confirm/refute and recommend deletion / replacement with `Logger.Debug`. ≤60 words.

### [LOW] `BadgerMethods.fParserJsonStream` echoes the raw body into the response
- **Where:** [BadgerMethods.pas:125-131](src/BadgerMethods.pas#L125) — `"Vc me mandou":"' + Request.Body + '"`.
- **Why:** Developer helper — but if accidentally wired to a route it reflects any client payload unescaped, which is both JSON-injection (breaks the surrounding JSON) and an information-leak vector.
- **How:** Either delete or keep under a clearly-named sample unit; if kept, JSON-escape `Request.Body`.
- **Prompt:** Open `src/BadgerMethods.pas:125-131` (`fParserJsonStream`). Claim: this helper concatenates `Request.Body` verbatim into a JSON response (`"Vc me mandou":"<body>"`) without JSON-escaping, so a body containing `"` or `\` breaks the JSON and arbitrary payload is reflected unescaped. Verify: confirm the concatenation; trace callers. Report confirm/refute, whether any live route wires this helper, and whether the helper should be deleted or moved to a sample. ≤100 words.

### [LOW] `TBadgerMethods.ParseRequestHeaderStr` uses `Pos('Header', Line) > 0`
- **Where:** [BadgerMethods.pas:89-105](src/BadgerMethods.pas#L89).
- **Why:** Same substring-match flaw as §4.2. Not currently on the auth path, but any future caller inherits the bug.
- **How:** Compare on the exact header name (pre-`:`-delimited).
- **Prompt:** Open `src/BadgerMethods.pas:89-105` (`ParseRequestHeaderStr`) and `:107-123` (`ParseRequestHeaderInt`). Claim: header lookup uses `Pos(Name, Line) > 0`, a substring test with the same flaw as §4.2. Not on the auth path today, but any future caller inherits the bug. Verify: confirm the pattern; list current callers. Report confirm/refute and recommend a single exact-name-match helper. ≤100 words.

### [LOW] `TRouteManager.Unregister` removes only the first match but logs nothing
- **Where:** [BadgerRouteManager.pas:70-106](src/BadgerRouteManager.pas#L70).
- **Why:** If a caller registered the same route twice, `Unregister` silently removes one — the other lingers. No diagnostic.
- **How:** Return a count or raise if no match; document behaviour.
- **Prompt:** Open `src/BadgerRouteManager.pas:70-106` (`Unregister`). Claim: `Unregister` removes only the first matching entry, silently succeeding even when duplicates remain. Verify: inspect the loop; check whether the function returns any status. Report confirm/refute and recommend returning a count or raising when no match. ≤80 words.

### [LOW] `TBadgerJWTClaims.FromJSON` does not validate types
- **Where:** [BadgerJWTClaims.pas:42-48](src/Auth/JWT/BadgerJWTClaims.pas#L42).
- **Why:** A token whose `exp` is a string (produced by a buggy issuer) returns 0 and is treated as "never expires". Silent auth weakening.
- **How:** Check the SuperObject field type before read; raise on mismatch.
- **Prompt:** Open `src/Auth/JWT/BadgerJWTClaims.pas:42-48` (`FromJSON`). Claim: `exp`, `nbf`, `iat` are read via SuperObject's lax accessors without type checks; a token where `exp` is a string returns 0 and is treated as "no expiry" (silent auth weakening). Verify: inspect each `.I[...]`, `.AsInteger` access and confirm no type guard. Report confirm/refute, which issuers realistically emit non-integer `exp`, and the minimal type-check pattern. ≤130 words.

---

## Summary

| # | Category | CRIT | HIGH | MED | LOW |
|---|----------|------|------|-----|-----|
| 1 | Memory & resource leaks            | 0 | 2 | 2 | 1 |
| 2 | Threading / concurrency            | 1 | 2 | 2 | 0 |
| 3 | Input parsing & DoS surface        | 2 | 3 | 2 | 1 |
| 4 | Crypto / auth correctness          | 3 | 2 | 2 | 0 |
| 5 | CORS & header injection            | 1 | 2 | 1 | 0 |
| 6 | Socket lifecycle & error paths     | 0 | 2 | 2 | 0 |
| 7 | Platform / compiler portability    | 0 | 0 | 2 | 2 |
| 8 | Logging & observability            | 0 | 1 | 2 | 1 |
| 9 | Build / supply chain               | 0 | 0 | 2 | 1 |
| 10 | Testing & regression              | 0 | 1 | 0 | 0 |
| 11 | Backdoors                         | 0 | 0 | 0 | 2 |
| 12 | Runtime logic (crash & perf)      | 1 | 2 | 3 | 1 |
| 13 | Minor / nice-to-have              | 0 | 0 | 0 | 5 |

**Top 5 to address first** (ordered by blast radius, not file position):

1. **§2.1** — `Request` by-value silently drops `UserID` / `UserRole`. Authorization context loss in every middleware.
2. **§3.1** — Multipart filename path traversal → remote arbitrary file write.
3. **§4.2** + **§4.3** — Auth header substring-match + protected-route exact-match. Any sub-path of a protected route is unauthenticated.
4. **§5.1** + **§5.3** — Response header CRLF injection, and `*` + credentials reflecting arbitrary origins. Complete cross-origin CSRF surface.
5. **§12.1** + **§6.1** — Keep-alive reuses previous request's state and skips route matching after first middleware-handled request. Silent cross-request data leak.

All other items remain in this file and will be addressed one by one in follow-up passes.
