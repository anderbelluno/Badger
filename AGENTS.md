# AGENTS.md — Badger Repository Guidance

This file is read by AI coding agents (Claude, Codex, Cursor, etc.) and by humans onboarding to the Badger HTTP microserver. Read it before touching code.

---

## What Badger is

A small Object Pascal HTTP server library, ~5 kLOC, built on top of [Synapse](http://www.ararat.cz/synapse/) for sockets and [SuperObject](https://github.com/hgourvest/superobject) for JSON. Targets Delphi 7+ and FPC/Lazarus on Windows + Linux. Ships routing, middleware, JWT + Basic auth, CORS, multipart upload, persistent logger.

The library code lives in `src/`. Demos live in `sample/`. Tests live in `sample/TDD/`.

---

## Repository layout

```
badger/
├─ src/                              Library — only edit here for fixes
│  ├─ Badger.pas                       Server thread, accept loop, CORS, lifecycle
│  ├─ BadgerRequestHandler.pas         Per-connection handler, parser, keep-alive
│  ├─ BadgerRouteManager.pas           Route registration + match
│  ├─ BadgerMethods.pas                MIME, multipart helpers
│  ├─ BadgerMultipartDataReader.pas    multipart/form-data parser
│  ├─ BadgerUploadUtils.pas            SanitizeUploadFileName
│  ├─ BadgerLogger.pas                 Persistent-stream logger (CS-protected)
│  ├─ BadgerUtils.pas                  Base64, signatures, MIME table
│  ├─ BadgerTypes.pas                  THTTPRequest / THTTPResponse / middleware sig
│  ├─ BadgerHttpStatus.pas             HTTP_* constants + status text
│  ├─ BadgerDefines.inc                Compiler/platform symbols
│  └─ Auth/
│     ├─ Basic/BadgerBasicAuth.pas     Basic auth (salted hash + constant-time cmp)
│     └─ JWT/
│        ├─ BadgerAuthJWT.pas          JWT issue/validate + middleware
│        ├─ BadgerJWTClaims.pas        Claims record + type-checked FromJSON
│        └─ BadgerJWTUtils.pas         Token persistence with restricted perms
│
├─ ThirdParty/                       Vendored — DO NOT EDIT
│  ├─ Synapse/
│  └─ SuperObject/
│
├─ sample/                           Demos and tests
│  ├─ D7/                              Delphi 7 GUI demo
│  ├─ D12/WinService/                  Delphi 12 Windows Service
│  ├─ D12/FMX Windows/                 Delphi 12 FMX GUI demo
│  ├─ Lazarus/Console_Linux/           Lazarus console (Linux smoke)
│  ├─ Lazarus/GUI/                     Lazarus GUI demo
│  ├─ StressTeste/                     Load driver — measurement, not assertion
│  ├─ SampleRouteManager.pas           Shared route helpers used by demos
│  └─ TDD/                             ★ Library-boundary test harness ★
│     ├─ BadgerTestSuite.pas             Assertions + registry + reporter
│     ├─ BadgerTestClient.pas            Raw HTTP client over Synapse
│     ├─ BadgerTestRoutes.pas            Route fixtures used only by tests
│     ├─ BadgerTests.pas                 28 tests grouped by audit area
│     ├─ BadgerTestRunner.lpr            FPC entry point
│     ├─ BadgerTestRunner.dpr            Delphi entry point
│     └─ README.md                       How to build + run + extend
│
├─ docs/Audit_ToDo/                  Audit reports — one per pass
│  ├─ 20260422_AUDIT_TODO.md            Original audit (56 items, all severities)
│  └─ 20260423_AUDIT_DEEP_DIVE.md       Dev's verification + fixes (14 items)
│
├─ AGENTS.md                         You are here
└─ README.md                         Library overview for end users
```

---

## How to work in this repo

### Before any change

1. Read the relevant unit in `src/` end-to-end. The codebase is small enough to hold in your head.
2. Search `docs/Audit_ToDo/` for prior findings about the area you're touching. Many items are already documented; don't re-litigate them.
3. Check if a test in `sample/TDD/BadgerTests.pas` covers the behaviour you're about to change.

### When fixing a bug or landing an audit item

1. **Write the failing test first** in `sample/TDD/BadgerTests.pas`. It must fail against current `main` for the right reason — run the harness to confirm.
2. Implement the fix in `src/`.
3. Re-run the harness. The new test goes green; no other test goes red.
4. If the fix closes an `AUDIT_TODO.md` item, add a one-line summary to the next dated `docs/Audit_ToDo/YYYYMMDD_*.md` (or open one if none exists for the day). Reference the test name.

### When adding a feature

1. Decide the public API on `TBadger` / `TRouteManager` / a new auth class. Keep it consistent with existing naming.
2. Add tests in `sample/TDD/` that drive the feature over real HTTP.
3. Keep the fix small. If the feature needs > ~150 LOC, split it across commits.

### When refactoring

1. Cover the area with TDD tests **first** (this is the regression net).
2. Refactor.
3. Tests must remain green — that's the contract.

---

## Test-Driven Development is mandatory

All non-trivial changes land with a TDD test in `sample/TDD/BadgerTests.pas`. The harness exists for two reasons:

1. **Regression safety.** This codebase has zero unit tests historically — every audit fix risked silent regression. The harness closes that gap.
2. **Cross-compiler verification.** The Delphi 7 / Delphi 12 / FPC matrix means manual curl is not enough. The harness compiles and runs on every supported toolchain.

### Run it locally

See `sample/TDD/README.md` for the exact command lines. Quick reference:

```
# FPC / Lazarus (cross-platform)
fpc -Mdelphi sample/TDD/BadgerTestRunner.lpr <path flags> -B
./sample/TDD/BadgerTestRunner

# Delphi (Windows)
call rsvars.bat
dcc32 -B sample\TDD\BadgerTestRunner.dpr <path flags>
sample\TDD\BadgerTestRunner.exe
```

The runner exits with `FAIL + ERROR` count. **0 = green, anything else = regression.**

### Test discipline

- Each test stands alone. No shared global state. No fixture files on disk.
- Each test starts a fresh `TBadger` on its own port (`NextTestPort`). Don't hardcode ports.
- Drive the server through `BadgerTestClient` (raw socket). Don't use a smart HTTP client — half the audit was about parser bugs that smart clients hide.
- A passing test is not a thoroughly written test. Assert on the **specific** thing that proves the fix: the absence of an injected header, the exact status code, the wire bytes.
- Group new tests under the right `--` heading in `RegisterAllTests`. Add a new group when the audit area is new.
- Keep total runtime under 2 minutes. If one test is slow (server timeout, etc.) tune it down.

### What does NOT belong in the harness

- Load testing → `sample/StressTeste/` already does this.
- Manual demos with GUIs → other `sample/` projects.
- Internal-procedure unit tests → keep coverage at the HTTP boundary so refactors don't break tests.

---

## Code style

### Pascal conventions in this repo

- `{$IFDEF FPC}{$mode delphi}{$H+}{$ENDIF}` at the top of every new unit. This is the cross-compiler contract.
- Cross-platform Windows guard: `{$IFDEF BADGER_WINDOWS}` (defined in `BadgerDefines.inc` from either `MSWINDOWS` or `WINDOWS`). Don't write `{$IFDEF MSWINDOWS}` directly — that drifts from FPC.
- `Try / finally` around every allocation. Always. Pre-try allocation chains are a documented audit finding (§1.3) — don't reintroduce them.
- `FreeAndNil(X)` for fields that may be re-used. `X.Free` is acceptable for locals that go out of scope immediately.
- Constant-time string comparison for any secret (token, password hash). Use the `ConstantTimeEquals` pattern from `BadgerBasicAuth.pas` or `BadgerAuthJWT.pas` — do not call `=` or `SameText` on a secret.
- Header name lookup must be exact-name (`Headers.Values['Authorization']`), never substring (`Pos('Authorization=', ...) > 0`). The substring pattern is a documented audit bug (§4.2).
- Route-protection match uses **exact OR prefix + `/`** (see `IsProtectedRoute` helper in both auth units). Never `SameText` only.
- Custom response header values must pass through `BuildHTTPResponse`'s sanitiser. Don't write to the wire directly.
- Multipart filenames must pass through `BadgerUploadUtils.SanitizeUploadFileName` before any disk write.

### Naming

- Types: `TPascalCase`. Records that mirror Badger primitives keep the `THTTP*` prefix; test-only records use `TTest*` to avoid namespace collisions (e.g. `TTestHttpResponse` in the harness — Badger has `THTTPResponse`).
- Constants: `UPPER_SNAKE_CASE` for HTTP status / headers (matches `BadgerHttpStatus.pas`).
- Files: `Badger*.pas` for library units. Test units live under `sample/TDD/Badger*.pas`.

### Comments

- Block comments at unit top describing purpose + cross-compiler intent.
- Inline comments only when the code is non-obvious or implements an audit item (`// §3.2: reject Content-Length < 0 to avoid SetLength overflow`).
- Avoid commented-out code. If it's worth keeping, put it behind `{$IFDEF DEBUG}`. The audit (§8.3) flagged commented-out `Logger.Info` litter.

---

## Cross-compiler discipline

All four targets must build:

- **Delphi 12** (the primary dev target — modern syntax welcome but no namespace-only constructs that break older versions)
- **Delphi 7** (legacy compat — no inline vars, no generics, no `TArray<T>`, no `Default(T)` for records, no `class const`)
- **FPC + Lazarus / Windows** (mode delphi)
- **FPC + Lazarus / Linux** (mode delphi, with `cmem` and `cthreads` in the program uses clause)

If a change adds a Delphi-only construct, gate it with `{$IFNDEF FPC}` and provide an FPC-equivalent branch.

Always compile in **all** four targets before shipping a fix. Not just the one you happen to be in.

---

## Logger usage

- The global `Logger` (in `BadgerLogger.pas`) is a unit-level singleton. It's safe to call from any thread (CS-protected), but heavy use under load contends — see audit §2.5.
- Default state: `isActive := False` (so library imports don't spam stdout). Tests and demos opt in by setting `isActive := True; LogToConsole := True;`.
- Levels: `Logger.Debug / Info / Warning / Error / Critical`. Default level is `llInfo`.
- File logging keeps the stream open for the logger's lifetime — don't reach into it directly.
- Never log secrets (passwords, tokens, full Authorization headers).

---

## Audit workflow

The repo runs a recurring audit loop:

1. An auditor produces `docs/Audit_ToDo/YYYYMMDD_AUDIT_TODO.md` listing findings with **Where / Why / How / Prompt** per item.
2. A second developer (or an AI agent using each item's `Prompt`) verifies each finding against current source and produces `docs/Audit_ToDo/YYYYMMDD_AUDIT_DEEP_DIVE.md` describing fixes applied.
3. Each fix lands with a regression test in `sample/TDD/`.
4. Open items are tracked across the dated files; nothing is silently dropped.

When opening a new audit pass:
- Use today's date as the filename prefix.
- Cite `file:line` for every finding.
- Severity tags: `[CRIT] [HIGH] [MED] [LOW]`. Order within sections by severity.

---

## Git rules

This repo follows a strict no-write-from-agent rule for git/gh:

- Agents may **read** git state freely (`git status`, `git log`, `git diff`, `gh pr view`, etc.).
- Agents may **NOT** stage, commit, push, branch, merge, rebase, reset, stash, or run any `gh` write command.
- File edits are always fine — that's normal work. Just don't commit them.
- After making changes, summarise what was changed and where, suggest a commit message in chat, and let the human run `git add` + `git commit`.

This rule overrides any slash command, skill, or subagent that would otherwise commit. If asked to "commit", remind the human of the rule instead.

---

## Markdown rules

- Never hard-wrap lines in markdown. Each bullet or sentence is one continuous line. (Affects this file, audit reports, READMEs, PR bodies.)
- Tables use pipe-style with header separator.
- Code fences carry a language tag.

---

## Things to NOT do

- Don't hand-roll crypto. Use the existing `CreateSignature` (HMAC-SHA256) and `ConstantTimeEquals` patterns. The custom Base64 has been audited; leave it alone unless a finding says otherwise.
- Don't reintroduce substring-match header lookup. Use `Headers.Values[name]`.
- Don't reintroduce `SameText`-only protected-route checks. Use the `IsProtectedRoute` helper.
- Don't pre-allocate before `try`. Move into the try.
- Don't widen `FSocketLock` scope. Keep the critical section tight.
- Don't swallow exceptions in a bare `except end;`. Log and re-raise, or report the failure outwards.
- Don't add commented-out diagnostic code. Use `Logger.Debug(...)`.
- Don't add a test that doesn't fail before your fix. Verify it fails for the right reason first.
- Don't ship a fix without a test, unless the change is purely cosmetic (whitespace, comment, name).

---

## Quick reference — useful entry points

- **Server boot:** `Badger.pas` → `TBadger.Create / Start / Stop`.
- **Per-request flow:** `BadgerRequestHandler.pas` → `Execute` → `repeat` loop.
- **Adding a route:** `Server.RouteManager.AddGet('/path', Handler).AddPost(...)`.
- **Adding middleware:** `Server.AddMiddleware(MyProc)` where `MyProc` matches `TMiddlewareProc` (note `var Request`).
- **Protecting routes:** `Auth.RegisterProtectedRoutes(Server, ['/private', '/api/admin'])`.
- **HTTP constants:** `BadgerHttpStatus.HTTP_OK`, `HTTP_BAD_REQUEST`, `APPLICATION_JSON`, `TEXT_PLAIN`.
- **Logger:** `Logger.Info('msg')` (set `Logger.isActive := True` to actually emit).

---

## Where to ask follow-ups

- Original library design: read the README + the audit trail in `docs/Audit_ToDo/`.
- Cross-compiler quirks: search for `{$IFDEF` in `src/` — patterns are consistent.
- Test patterns: read `sample/TDD/BadgerTests.pas` (28 worked examples).
