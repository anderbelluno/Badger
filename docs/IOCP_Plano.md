# Plano IOCP → paridade com o Badger atual

Branch: `IOCP`  
Regra: **não fazer commit** até o usuário pedir.  
Alvo: no Windows, `TBadger` usa **IOCP por padrão** e se comporta como o caminho clássico (rotas, middlewares, CORS, body, Keep-Alive, auth, DB pool, WebSocket). Linux/macOS continuam no Synapse (`select` + threads) até o plano de epoll.

## Definição de pronto

O sample Lazarus GUI (`sample/Lazarus/GUI`) sobe **sem mudar a API de uso**:

- `TBadger.Create` / `Port` / `Start` / `Stop`
- `RouteManager.AddGet/Post/Put/Patch/AddDel`
- `AddMiddleware` / `AddAfterMiddleware`
- CORS, `OnRequest` / `OnResponse`, `EnableEventInfo`
- Basic Auth + JWT (`RegisterProtectedRoutes`)
- `TBadgerDBBridge` + `AcquireConn` / `ReleaseConn`
- WebSocket (`OnWebSocketMessage`, broadcast)
- Keep-Alive, chunked, multipart, stream de download

Checklist de compilação a cada fase: **Lazarus Win** e **Delphi 7**. Linux não quebra (units IOCP fora do `uses` sem `BADGER_WINDOWS`).

Marca: `[x]` feito · `[ ]` pendente

---

## Estado atual (já feito)

- [x] Motor IOCP (`BadgerWinSock2` + `BadgerIOCP`) em D7, Lazarus e D12 Win64
- [x] Samples console + GUI (`sample/IOCP/{D7,D12,Lazarus}`) na 8081, `TBadger` + `UseIOCP := True`
- [x] Pipeline compartilhado: parser, rotas, MW, CORS, Keep-Alive, body, Basic/JWT, DB bridge
- [x] Stress `wrk` ping vs clássico — IOCP empata em rps no Keep-Alive; D12 ~33k, D7 ~18k (WoW64), Lazarus ~68k (string UTF-8). Não micro-otimizar Unicode do D12.
- [x] WebSocket no IOCP (fase 7)
- [x] `UseIOCP` default `True` no Windows (fase 8)
- [x] Docs oficiais + samples GUI D7/Lazarus/FMX/WinService sem mudar a API (fase 8/9)

---

## Princípio de arquitetura

Não duplicar `TRouteManager`, middlewares, CORS, auth, DB pool.

```
TBadger (API pública — samples oficiais não mudam)
    ├── Windows + UseIOCP (default True): motor IOCP
    └── senão: Execute atual (Synapse)

Pipeline HTTP (um só):
    parse → before MW → MatchRoute → after MW (LIFO) → send
```

`THTTPRequest.Socket` hoje é `TTCPBlockSocket`. Rotas/WS dependem disso. Ou extraímos uma conexão abstrata, ou um adapter Synapse-over-IOCP (pior). Preferir **desacoplar o socket da request** o mais cedo possível (fase 2).

Worker IOCP **não bloqueia** em rota lenta: se o callback fizer I/O de DB, ou offload para thread pool, ou documentar que rotas longas ocupam worker (fase 4).

---

## Fase 0 — Endurecer o motor

Objetivo: IOCP estável em D7, Lazarus e D12 Win64, ainda só `/ping`.

- [x] Compilar e rodar `sample/IOCP/D7/IocpPing.dpr` no Delphi 7
- [x] Corrigir `TIocpKey` / `TBadgerSocket` para Delphi Win64 (`NativeUInt`, não `DWORD`)
- [x] Shutdown drena AcceptEx pendentes (`WaitLiveCtxIdle` + `DrainLiveCtx`) — a validar no FastMM
- [x] Stress curto (`wrk` / JMeter) `/teste/ping` IOCP vs clássico — rps parecido no Keep-Alive; teto é o pipeline HTTP, não o AcceptEx

**Saída:** ping IOCP confiável nos dois compiladores Windows.

---

## Fase 1 — Parser HTTP em máquina de estados

Objetivo: request completa em buffer IOCP, sem `RecvString` bloqueante.

- [x] Request line → method, URI, query (`BadgerHttpParser`, sem Synapse)
- [x] Headers até `\r\n\r\n`
- [x] Body por `Content-Length`
- [x] Body chunked
- [x] Limite de header/body (DoS) — linha 16KB / headers 64KB / body 50MB (iguais ao handler)
- [x] Preencher `THTTPRequest` (Headers, Body, BodyStream, QueryParams, FRemoteIP)

Ainda **sem** `TRouteManager`: `GET /ping` → 200 pong; parse error → 400; body presente → 200 eco (ou `received N bytes` se > 7KB); resto → 404.

**Saída:** POST com body e chunked funcionam no protótipo IOCP.

---

## Fase 2 — Desacoplar o socket da request

Objetivo: rotas não dependem de `TTCPBlockSocket`.

- [x] Conexão `TBadgerConn` (Handle + RemoteIP) — o motor IOCP faz send/recv/close; `THTTPRequest.Socket` fica nil no IOCP
- [x] `BadgerBuildHTTPResponse` / `BadgerAssembleHTTPMessage` (sem Synapse) + `WSASend` do buffer
- [x] Stream/body de resposta em vários `WSASend` (janela 64KB no heap, não `SendString`)
- [x] D7: sem generics; `TBadgerConn` class simples

**Saída:** resposta HTTP sai 100% por IOCP; Synapse só no backend clássico (`BuildHTTPResponse` do handler chama a função compartilhada, Date ainda via Synapse).

---

## Fase 3 — Rotas + middlewares (núcleo da paridade)

Objetivo: mesmo pipeline do `THTTPRequestHandler`.

- [x] `TBadgerIOCP` expõe `RouteManager`, `AddMiddleware`, `AddAfterMiddleware`
- [x] Before MW: `True` = handled (não chama rota)
- [x] `MatchRoute` + callback (`TRouteManager` compartilhado; D7 `of object`)
- [x] After MW em **LIFO**, inclusive após short-circuit (exceto preflight CORS, como o handler clássico)
- [x] 404 quando não casa rota
- [x] `OnRequest` / `OnResponse` + `EnableEventInfo`
- [x] CORS: preflight OPTIONS + headers em resposta normal
- [x] Sample IOCP registra `/teste/ping`, `/echo`, `/produtos` (`IocpDemoRoutes` — mesmas assinaturas; JWT/upload ficam no `SampleRouteManager` clássico)

**Saída:** `GET /teste/ping` → `Pong` via `TRouteManager`. GUI IOCP aponta para `/teste/ping`.

---

## Fase 4 — Keep-Alive, timeout, concorrência

- [x] HTTP/1.1 keep-alive (`Connection: close` só se o cliente pedir ou HTTP/1.0 sem keep-alive)
- [x] `Timeout` por recv ocioso (watchdog 250ms + `CancelIo`; default 5000 ms; `0` desliga)
- [x] `MaxConcurrentConnections` (gate no accept; default 100; `<= 0` = sem teto)
- [x] `ParallelProcessing` default **True** no IOCP; `False` serializa o dispatch da rota
- [x] Rotas correm no worker IOCP; pool de offload fica para se a rota bloquear de verdade (fase 4 não adiciona pool)

**Saída:** curl/JMeter com Keep-Alive reutiliza o socket; `X-Engine` continua no segundo request.

---

## Fase 5 — Body rico (paridade de features HTTP)

- [x] JSON / `fParserJsonStream` — `POST /json`; body textual em `Req.Body` (mesmo critério do handler)
- [x] Download stream (`fDownloadStream`, MIME) — `GET /download`; `Assemble` prefere `Resp.Stream` quando `Size > 0`
- [x] Multipart (`BadgerMultipartDataReader`) — `POST /upload` via `BodyStream`
- [x] Upload utils — `SanitizeUploadFileName` já corre no reader; demo não grava arquivo
- [x] `HeadersCustom` na resposta (`X-Engine`, `Content-Disposition`, `X-Upload-Count`)
- [x] Teto de body 50MB (413 `body too large`), igual ao handler clássico

**Saída:** `POST /json`, `GET /download`, `POST /upload` no sample IOCP. JWT/login continuam no `SampleRouteManager` (fase 6).

---

## Fase 6 — Auth + DB pool (deve “sair de graça”)

`Register` pede `TBadger` — o motor IOCP ficou **dentro** de `TBadger` (`UseIOCP`; fase 8 default `True` no Windows). Sem `Register(TBadgerIOCP)`.

- [x] Basic Auth `RegisterProtectedRoutes(Server, ['/json'])` no sample IOCP
- [x] JWT `RegisterProtectedRoutes(Server, ['/download'])` + `POST /login`
- [x] `TBadgerDBBridge.Register(Server)` inalterado — ConnPool/Midd_before_after usam a API oficial (IOCP vem do default no Windows)

**Saída:** ping público; `/json` pede Basic `username:password`; `/download` pede Bearer do `/login` (`usuario`/`senha123`).

---

## Fase 7 — WebSocket

Hoje: loop `CanRead` + `RecvByte` numa thread. No IOCP: frames em estado (opcode, mask, payload).

- [x] Handshake RFC 6455 (`BadgerWebSocket` + handler clássico reusa `BadgerWsAcceptKey`)
- [x] Recv/send de frames via `WSARecv`/`WSASend` (overlapped duplo recv+send)
- [x] Close frame, limite de payload (`WS_MAX_PAYLOAD`)
- [x] `OnWebSocketMessage`, `BroadcastWebSocketText`, `SendToWebSocketRoute`
- [x] `TClientSocketInfo.Socket` nil no IOCP; `Ctx` aponta para `PIocpCtx`

**Saída:** o demo WS do README (`ws://host:port/chat`) funciona no Windows IOCP (`UseIOCP := True`, sample na 8081).

---

## Fase 8 — Unificar na API `TBadger`

- [x] Property `UseIOCP: Boolean` (default `True` no Windows; `False` força Synapse)
- [x] `Start`/`Stop` escolhem o motor (IOCP adota RouteManager/MW/CORS do `TBadger`)
- [x] Samples oficiais **não mudam** (GUI D7, Lazarus, FMX, WinService, ConnPool, Midd_before_after)
- [x] Linux: ignora `UseIOCP` (`Start` IOCP só com `BADGER_WINDOWS`)
- [x] Documentar: IOCP = Windows; fallback Synapse (`UseIOCP := False`)

**Saída:** “Badger é hoje” para o usuário da biblioteca; IOCP é implementação, não produto novo.

---

## Fase 9 — Qualidade e comparação

- [x] Mesmo stress do README (Keep-Alive Windows) IOCP vs clássico — `wrk -t4 -c400`; rps no ping é CPU do assembler/rotas, não o motor
- [x] GUI Lazarus / D7 VCL / FMX / WinService: mesma API; no Windows passam a usar IOCP pelo default
- [x] Console Linux: caminho clássico (`UseIOCP` ignorado; units IOCP fora do `uses`)
- [x] `docs/Badger_Documentacao.md` + README (backend Windows / fallback Synapse)

---

## Ordem sugerida (não pular)

```
0 motor → 1 parser → 2 socket off Synapse → 3 rotas+MW+CORS
  → 4 keep-alive → 5 body → 6 auth/DB → 7 WS → 8 TBadger único → 9 bench
```

Fase 3 é o primeiro momento em que “parece Badger”. Fase 8 é quando **é** o Badger.

**Este plano está concluído.**

---

## Próximo plano — epoll (Linux)

Não faz parte deste documento de execução. Escolha do motor por SO:

```
TBadger.Start
    ├── BADGER_WINDOWS + UseIOCP → IOCP
    ├── LINUX → epoll          (a implementar)
    └── senão → Synapse
```

`UseIOCP := False` no Windows continua forçando Synapse. macOS fica em Synapse até kqueue.

Reuso: `TBadgerHttpParser`, `BadgerAssembleHTTPMessage`, `TBadgerConn`, rotas/MW/CORS/auth/DB, `BadgerWebSocket`. Novo: `BadgerEpoll` (fds non-blocking, `epoll_wait`, sem `OVERLAPPED`/`AcceptEx`).

Antes de copiar `FinishRequest`: extrair o dispatch HTTP/WS para uma unit compartilhada, para IOCP e epoll não duplicarem o pipeline.

---

## Fora de escopo neste plano (IOCP)

- Implementar epoll / kqueue (próximo plano, acima)
- Mudar a API pública de rotas/middleware
- Commit/PR (só quando o usuário pedir)
