# Badger TDD Harness

Library-boundary test suite for Badger. Compiles on every supported toolchain (Delphi 7, Delphi 12 VCL/FMX, FPC/Lazarus on Windows + Linux), uses zero test frameworks — only Synapse and the Badger source under `src/`.

## What it covers

The 27 tests in `BadgerTests.pas` exercise the CRIT/HIGH items from `docs/Audit_ToDo/20260422_AUDIT_TODO.md` that were closed by `docs/Audit_ToDo/20260423_AUDIT_DEEP_DIVE.md`. Each fix lands in here as a regression test:

| Group     | Coverage                                                                              |
|-----------|----------------------------------------------------------------------------------------|
| Sanity    | Ping, 404, route-param extraction                                                      |
| Headers   | CRLF response-splitting (§5.1), oversize header line (§3.3)                            |
| KeepAlive | State-bleed across iterations (§12.1, §6.1) — custom headers, query params             |
| Body      | Negative `Content-Length` (§3.2), happy-path POST                                      |
| Chunked   | `Transfer-Encoding: chunked` round-trip + connection sync (§3.4)                       |
| CORS      | Disabled, credentials + wildcard (§5.3), explicit origin, OPTIONS preflight            |
| BasicAuth | Missing/valid creds, sub-path coverage (§4.3), exact-name header match (§4.2), `var Request` propagation (§2.1) |
| JWT       | Missing token, valid token, tampered signature (§4.1), `alg=none` rejection (§4.4)     |
| Sanitize  | Multipart filename traversal stripped to basename (§3.1)                               |
| Base64Edge| Empty / 1-char / invalid-alphabet auth payloads do not crash (§3.5, §4.8)              |

## Running

### FPC / Lazarus (cross-platform)

```bash
# from repo root
fpc -Mdelphi sample/TDD/BadgerTestRunner.lpr \
    -Fu./src \
    -Fu./src/Auth/JWT \
    -Fu./src/Auth/Basic \
    -Fu./sample \
    -Fu./sample/TDD \
    -Fu./ThirdParty/Synapse \
    -Fu./ThirdParty/SuperObject/D_Plus_Laz_Linux \
    -B

./sample/TDD/BadgerTestRunner
echo "exit code: $?"   # 0 = all green
```

On Windows replace `/` with `\` and use the FPC executable path documented in the deep-dive (`C:\Laz_fix\fpc\bin\i386-win32\fpc.exe`).

### Delphi (7 / 12)

Open `sample/TDD/BadgerTestRunner.dpr` in the IDE, add the unit search paths matching the `-Fu` list above (Project → Options → Delphi Compiler → Search path), build, and run from a console window so `WriteLn` is visible.

Or compile from a Delphi command prompt (paths configured by `rsvars.bat`):

```cmd
dcc32 -B sample\TDD\BadgerTestRunner.dpr ^
      -U.\src;.\src\Auth\JWT;.\src\Auth\Basic;.\sample;.\sample\TDD;^
         .\ThirdParty\Synapse;.\ThirdParty\SuperObject\D_Plus_Laz_Linux
sample\TDD\BadgerTestRunner.exe
echo exit: %errorlevel%
```

## Output shape

```
===========================================================
 Badger TDD Harness — 27 tests
===========================================================

-- Sanity --
  [ 1/27] Ping_returns_200_and_Pong                          PASS (45ms)
  [ 2/27] Unknown_route_returns_404                          PASS (38ms)
...

-- BasicAuth --
  [16/27] Missing_Authorization_header_yields_401            PASS (62ms)
  [17/27] Valid_credentials_yield_200                        PASS (58ms)
...

===========================================================
 PASS: 27   FAIL: 0   ERROR: 0   (total 4123ms)
===========================================================
```

`PASS` = test green. `FAIL` = assertion mismatch (test detected a regression). `ERROR` = unexpected exception (probably a wiring bug in the test or the lib crashed). The process exits with `FAIL + ERROR` count, so any non-zero exit is a regression.

## Adding a test

1. Decide what observable behavior changes after the fix.
2. Add a `procedure Test_Group_What;` in `BadgerTests.pas`. Spin up a `TBadger`, register routes, drive over `BadgerTestClient`, assert.
3. Register it under its group in `RegisterAllTests`.
4. Use a fresh port — `MakeConfig` calls `NextTestPort` for you. Don't share state between tests.

## Design notes

- **No threads in tests.** Each test starts a `TBadger`, runs requests sequentially via raw socket, stops the server. Drive concurrency from `sample/StressTeste`, not from here.
- **Raw socket on purpose.** Smart HTTP clients hide the bugs we want to catch (CRLF injection, malformed chunked, oversized headers).
- **Per-test port.** Avoids `TIME_WAIT` collisions and makes failures isolatable. Port range starts at 18765.
- **No external test framework.** DUnitX is Delphi-only, FpcUnit is FPC-only. The cross-compiler matrix kills both. Hand-rolled assertions cost ~50 LOC and run anywhere.
- **Logger silenced.** Tests set `Logger.isActive := False` to keep output clean. Re-enable temporarily when debugging a specific failure.

## Limits

- Multipart-filename test exercises `SanitizeUploadFileName` directly via a route, not through a real `multipart/form-data` body. The function is the actual sink — testing it directly is faithful and avoids 50 lines of multipart-construction noise. If you want full end-to-end coverage of the multipart parser, add a test that sends a real boundary body to `/AtuImage` and checks the resulting filename in a tmp directory.
- No HTTPS coverage — Badger is plain HTTP today.
- No load/soak coverage — that's `sample/StressTeste/`.
- Tests assume no other process is listening on ports 18765–18791.
