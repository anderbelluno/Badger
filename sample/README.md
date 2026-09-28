# Samples

Layout target (migration in progress — see `docs/Samples_Migracao.md`):

```
sample/
  Common/           SampleRouteManager, SampleWs*, ConnPoolRoutes, SampleDbTemplate
  D7/GUI/           VCL full demo (8080)
  D7/Console/       minimal ping
  D12/GUI/          FMX full demo (8080)
  D12/Console/      minimal ping
  D12/WinService/   Windows service host
  Lazarus/GUI/      LCL full demo — WS + ConnPool (Zeos) + auth
  Lazarus/Console/  minimal ping (Win/Linux)
  Lazarus/ConnPool/     docker/db + stress UI (routes moved to Common)
  Lazarus/Midd_before_after/ → absorb then delete
  IOCP/  Epoll/         → delete after D7/D12 GUI have WS
  StressTeste/      load clients
```

| App | Role |
|-----|------|
| **Console** | `TBadger` + `GET /teste/ping` only |
| **GUI** | Start/Stop, log, parallel, IOCP/epoll, Basic, JWT, WS `/chat`, ConnPool `/db/*` (Zeos) |

Bootstrap: Create → Port/Timeout → Parallel/MaxConn → EnableEventInfo → (only if needed) `UseIOCP`/`UseEpoll := False` → routes → Start.

Do **not** set `UseIOCP := True` / `UseEpoll := True` (defaults on Windows/Linux).

## Open now

| Path | Notes |
|------|--------|
| `Lazarus/GUI/project1.lpi` | WS + ConnPool done — test next |
| `Lazarus/Console/project1.lpi` | was Console_Linux |
| `D7/GUI/Project1.dpr` | mirror Lazarus next |
| `D7/Console/BadgerConsole.dpr` | minimal |
| `D12/GUI/FMXWindows.dproj` | mirror Lazarus next |
| `D12/Console/BadgerConsole.dproj` | minimal |
| `D12/WinService/` | unchanged |
| `Lazarus/ConnPool/db` | `docker compose` for Postgres |

Legacy `IOCP/`, `Epoll/` still compile for reference until faxina final.
