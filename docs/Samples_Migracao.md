# Samples — migração para GUI / Console por compilador

Branch/trabalho: consolidar demos. **Não apagar** `IOCP/` / `Epoll/` / `ConnPool` isolado até o GUI unificado passar nos gates abaixo.

## Alvo

```
sample/
  Common/                 rotas + (depois) WS demo + ConnPool routes/db
  D7/GUI/                 VCL — catálogo completo
  D7/Console/             ping mínimo
  D12/GUI/                FMX — catálogo completo
  D12/Console/            ping mínimo
  D12/WinService/         mantém (host ≠ GUI)
  Lazarus/GUI/            LCL — catálogo completo
  Lazarus/Console/        ping mínimo (Win + Linux)
  StressTeste/            mantém (cliente de carga)
```

| App | Conteúdo |
|-----|----------|
| **Console** | `TBadger` + `/teste/ping` + wait. Sem auth/WS/CORS/DB. |
| **GUI** | Start/Stop, log, parallel, motor (IOCP/epoll checkbox), rotas SampleRouteManager, Basic, JWT (sem item Token morto), WS `/chat`, ConnPool Zeos (painel), midd before/after se couber |

Porta oficial: **8080** (DB pool no GUI pode usar a mesma ou subpath `/db/*`).

---

## Checklist — migrar

### Estrutura
- [x] Pastas `D7/GUI`, `D7/Console`, `D12/GUI`, `D12/Console`, `Lazarus/Console`
- [x] Mover oficial D7 → `D7/GUI` (paths `Common`)
- [x] Mover `D12/FMX Windows` → `D12/GUI`
- [x] Mover `Lazarus/Console_Linux` → `Lazarus/Console`
- [x] Consoles mínimos D7 / D12 / Lazarus
- [x] `sample/README.md` apontando a árvore nova (+ legado até apagar)

### GUI — features a portar (fonte)
| Feature | Origem atual | D7 | D12 | Laz |
|---------|----------------|----|-----|-----|
| Rotas SampleRouteManager | `Lazarus/GUI`, `D7`, FMX | [ ] | [ ] | [x] |
| Basic + Realm | `BadgerBasicAuth` + RadioGroup | [ ] | [ ] | [x] |
| JWT Login/Refresh | idem (remover item **Token** morto) | [ ] | [ ] | [x] |
| Parallel + MaxConn | checkbox | [ ] | [ ] | [x] |
| Motor IOCP/epoll | checkbox (Win/Linux) | [ ] | [ ] | [x] |
| Request/Response log | EnableEventInfo + memo | [ ] | [ ] | [x] |
| WS `/chat` | `Common/SampleWs*` | [ ] | [ ] | [x] |
| ConnPool Zeos + `/db/*` | `Common/ConnPoolRoutes` + chk | [ ] | [ ] | [x] |
| Midd before/after | `Midd_before_after` (opcional no GUI) | [ ] | [ ] | [ ] |
| CORS demo | IOCP/Epoll (opcional) | [ ] | [ ] | [ ] |

### Common a extrair
- [x] `SampleRouteManager` (já existe)
- [x] `SampleWsChatClient` + `SampleWsDemo` (echo `/chat`)
- [x] `ConnPoolRoutes` + `SampleDbTemplate` em `Common` (docker ainda em `Lazarus/ConnPool/db`)
- [ ] Apontar D7/D12 GUI para as mesmas units

---

## Checklist — testar (por compilador)

### Console (8080 `/teste/ping`)
| | Compila | Ping HTTP | Motor default |
|--|---------|-----------|----------------|
| D7 Win | [ ] | [ ] | IOCP |
| D12 Win | [ ] | [ ] | IOCP |
| Lazarus Win | [ ] | [ ] | Synapse (sem LINUX) |
| Lazarus Linux | [ ] | [ ] | epoll |
| D12 Linux64 | [ ] | [ ] | epoll |

### GUI — smoke
| | Compila | Start/Stop | Ping | Basic | JWT | WS | DB `/db/ping` | IOCP↔Synapse / epoll↔Synapse |
|--|---------|------------|------|-------|-----|-----|---------------|------------------------------|
| D7 | [ ] | [x] | [x] | [ ] | [ ] | [ ] | [ ] | [ ] |
| D12 Win | [ ] | [x] | [x] | [ ] | [ ] | [ ] | [ ] | [ ] |
| Lazarus Win | [ ] | [x] | [x] | [ ] | [ ] | [ ] | [ ] | [ ] |
| Lazarus Linux | [ ] | [ ] | [ ] | [ ] | [ ] | [ ] | [ ] | [ ] |
| D12 Linux (se FMX/LCL) | [ ] | — | — | — | — | — | — | — |

DB: `docker compose` em `sample/Lazarus/ConnPool/db` (ou path novo) + Zeos.

### Faxina (só depois dos gates GUI+Console verdes)
- [ ] Apagar `sample/IOCP/`
- [ ] Apagar `sample/Epoll/`
- [ ] Apagar `Lazarus/Console_Linux` (se movido)
- [ ] Apagar `Lazarus/ConnPool` isolado (se absorvido)
- [ ] Apagar `Lazarus/Midd_before_after` (se absorvido)
- [ ] Remover item RadioGroup **Token** morto
- [ ] Atualizar README raiz + `docs/Badger_Documentacao.md` samples
- [ ] Marcar fase sample no `Epoll_Plano` / IOCP se ainda citarem pastas velhas

---

## Ordem de execução (esta faxina)

1. Estrutura + Consoles + mover oficiais para `*/GUI`
2. Enriquecer GUI (WS → ConnPool → limpar Token) **um compilador por vez** (Lazarus → D12 → D7)
3. Testes da tabela
4. Apagar legado

Regra: **não** setar `UseIOCP := True` / `UseEpoll := True` (já são default no SO certo).
