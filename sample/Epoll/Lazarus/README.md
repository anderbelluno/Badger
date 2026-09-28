# Lazarus Linux — recortes

Copiar o repo **local** (SMB/gvfs falha). Abrir o `.lpi` no Lazarus Linux. Não setar `UseEpoll := True` (já é o default).

## Proto 8081 (`sample/Epoll/Lazarus/`)

| Projeto | Recorte |
|---------|---------|
| `EpollPing.lpi` | Console: ping/echo/produtos/json/download/upload/login, CORS, WS `/chat` |
| `EpollPingGUI.lpi` | GUI: Start/Stop, GET ping/other, chat `/chat`. Checkbox epoll (desmarcar = Synapse) |

`GET /teste/ping` → `Pong` (`X-Engine: epoll`). `/json` Basic `username:password`. `/download` Bearer de `POST /login` (`usuario`/`senha123`).

## Oficiais 8080 (`sample/Lazarus/`)

| Projeto | Recorte |
|---------|---------|
| `Console_Linux/project1.lpi` | Headless `/teste/ping` |
| `GUI/project1.lpi` | Rotas SampleRouteManager, Basic/JWT, eventos, parallel. Checkbox = epoll no Linux (desmarcar = Synapse) |
| `ConnPool/src/ConnPool.lpi` | `TBadgerDBBridge` + `/db/*` (PostgreSQL + Zeos) |
| `Midd_before_after/MiddBeforeAfter.lpi` | Before Basic + after JSON envelope |

ConnPool precisa do Docker em `ConnPool/db` e pacotes Zeos no Lazarus.
