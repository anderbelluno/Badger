# Samples

Grouped by **context**, not only by compiler.

```
sample/
  Common/          shared official routes
  D7/              official VCL (8080)
  D12/             official FMX + WinService (8080)
  Lazarus/         official GUI, Console, ConnPool, Midd_before_after
  IOCP/            engine + WebSocket + CORS (8081)
    IocpDemoRoutes.pas
    IocpWsChatClient.pas
    D7/
    D12/
    Lazarus/
  StressTeste/     load tools
```

Bootstrap order (every server sample):

1. `TBadger.Create`
2. `Port` / `Timeout`
3. `ParallelProcessing` / `MaxConcurrentConnections`
4. `EnableEventInfo` (`OnRequest` / `OnResponse` only when True)
5. `UseIOCP` only when forcing Synapse on Windows (`False`)
6. Routes / auth / middleware / WS
7. `Start` — then `Stop` + `Free`

Windows IOCP is already the default. Do not set `UseIOCP := True`.

## Official

| Path | Recorte |
|------|---------|
| `Lazarus/GUI/` | Canonical GUI: routes, Basic/JWT, events, parallel, **IOCP checkbox** (uncheck = Synapse) |
| `D7/` | Same |
| `D12/FMX Windows/` | Same |
| `D12/WinService/` | Same bootstrap as a Windows service |
| `Lazarus/Console_Linux/` | Headless `/teste/ping` |
| `Lazarus/ConnPool/` | `TBadgerDBBridge` + `/db/*` |
| `Lazarus/Midd_before_after/` | Before/after middleware |

Shared routes: `Common/SampleRouteManager.pas`.

## IOCP (8081)

Same `TBadger` API. Extra recorte: CORS on, after-middleware, WebSocket `/chat`.

| Path | Apps |
|------|------|
| `IOCP/D12/` | `IocpPing`, `IocpPingGUI` |
| `IOCP/D7/` | `IocpPing`, `IocpPingGUI` |
| `IOCP/Lazarus/` | `IocpPing`, `IocpPingGUI` |

## Stress

`StressTeste/` — client load tools, not a Badger server demo.
