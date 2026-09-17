# Badger – Documentação Técnica Completa

## Visão Geral

Badger é um microservidor HTTP multithread, leve e focado em alto desempenho, com suporte a rotas estáticas e dinâmicas, middlewares (before/after), eventos de aplicação (`OnRequest`/`OnResponse`), autenticação via exemplos e pool genérico de conexões de banco (`TBadgerDBPool` / `TBadgerDBBridge`). A arquitetura privilegia simplicidade no hot path, com otimizações localizadas (índice estático e agrupamento por contexto) e controles explícitos de concorrência.

## Arquitetura

- Thread do servidor (`TBadger`): escolhe o motor de I/O no `Start` e expõe a API pública (rotas, middlewares, CORS, WS).
  - Windows: **IOCP** por padrão (`UseIOCP = True`). `UseIOCP := False` volta ao Synapse (`select` + thread por conexão).
  - Linux / macOS: Synapse (epoll no Linux é o próximo motor; mesma API).
  - IOCP: `src/IOCP/BadgerIOCP.pas` + parser `src/BadgerHttpParser.pas`. `THTTPRequest.Socket` é `nil`; rotas usam body/headers/`FRemoteIP`.
  - Clássico: accept loop em `TBadger.Execute` e `THTTPRequestHandler` por conexão.
  - Controles de concorrência:
    - `ParallelProcessing`: no IOCP serializa o dispatch da rota quando `False`; no clássico cria um handler por conexão quando `True`.
    - `MaxConcurrentConnections`: limite de conexões ativas (gate no accept).

- Handler de requisição (`THTTPRequestHandler`): realiza parsing do request, roteamento, execução de método/rotas, construção e envio de resposta. Usado no motor Synapse; o IOCP dispara o mesmo pipeline (rotas/MW/CORS) a partir do parser compartilhado.
  - Declaração e campos: `src/BadgerRequestHandler.pas:12–38`
  - Construtores (sequencial e paralelo): `src/BadgerRequestHandler.pas:45–66`, `src/BadgerRequestHandler.pas:68–89`
  - Destrutor com liberação de middlewares e decremento de conexão ativa: `src/BadgerRequestHandler.pas:84–115`
  - Parser de cabeçalho HTTP (linha a linha): `src/BadgerRequestHandler.pas:117–143`
  - Montagem de resposta HTTP: `src/BadgerRequestHandler.pas:145–176` e envio de corpo/stream: `src/BadgerRequestHandler.pas:380–408`
  - Eventos de aplicação (`OnRequest`/`OnResponse`) condicionados por `EnableEventInfo`: `src/BadgerRequestHandler.pas:410–445`
  - Suporte a CORS (preflight + cabeçalhos): `src/BadgerRequestHandler.pas:361–451`

- Gerenciador de rotas (`TRouteManager`): registra, desregistra e resolve rotas.
  - Declaração: `src/BadgerRouteManager.pas:25–49`
  - Estruturas internas:
    - Lista de rotas (`FRoutes`): `src/BadgerRouteManager.pas:27`
    - Índice por contexto (`FContextIndex`): buckets por primeiro segmento: `src/BadgerRouteManager.pas:27–29`, `src/BadgerRouteManager.pas:144–154`
  - Registro de rota (`AddMethod`): normaliza pattern, adiciona em `FRoutes` e bucket de contexto: `src/BadgerRouteManager.pas:107–124`
  - Remoção (`Unregister`): retira de `FRoutes` e do bucket: `src/BadgerRouteManager.pas:76–93`
  - Resolução (`MatchRoute`): seleciona bucket por contexto e compara segmentos, coletando parâmetros: `src/BadgerRouteManager.pas:179–227`

## Fluxo da Requisição

1. `TBadger.Start` escolhe o motor (Windows/IOCP ou Synapse). No IOCP, `WSARecv` alimenta `TBadgerHttpParser`; no clássico, `Execute` aceita e cria `THTTPRequestHandler`.
2. Parse do HTTP (parser compartilhado no IOCP; `ParseRequestHeader` no handler clássico).
3. Middlewares **before** (`AddMiddleware`): podem interromper com `Handled=True`
4. Roteamento via `TRouteManager.MatchRoute` e execução do callback: `src/BadgerRouteManager.pas:179–227`
5. Envio de headers, corpo (texto/JSON) ou stream
6. Eventos de aplicação, conforme `EnableEventInfo`
7. Middlewares **after** (`AddAfterMiddleware`), em ordem LIFO — rodam **antes** de gravar a resposta no socket (podem alterar body/headers) e mesmo se um before short-circuitou

## Roteamento

- Rotas estáticas: comparadas diretamente por número de segmentos e igualdade de partes.
- Rotas dinâmicas: partes com `:` são capturadas em `Params` (`nome=valor`).
- Índice por contexto:
  - Bucket selecionado pelo primeiro segmento do path; se o primeiro segmento do pattern for dinâmico, a rota entra no bucket vazio `''`.
  - Vantagem: reduz candidatos quando há muitos endpoints por domínio funcional (ex.: `/produtos/...`, `/login/...`).

## Middlewares

Badger tem dois ganchos no ciclo da requisição:

| Tipo | API | Quando | Contrato |
|------|-----|--------|----------|
| Before | `AddMiddleware` | Antes da rota | `True` = interrompe (handled); `False` = continua |
| After | `AddAfterMiddleware` | Depois da rota, antes do send | procedure (mutate Resp e/ou cleanup) |

- Declarações: `TMiddlewareProc` / `TAfterMiddlewareProc` em `src/BadgerTypes.pas`.
- Registro: `TBadger.AddMiddleware` / `TBadger.AddAfterMiddleware` em `src/Badger.pas`.
- After roda em ordem **LIFO** (modelo cebola), inclusive quando um before short-circuita com `Handled=True`.
- Cada `THTTPRequestHandler` copia wrappers no accept para isolamento entre threads.
- Uso típico do after: liberar recursos (ex.: conexão emprestada do DB pool).

Fluxo resumido:

```
before₁ → before₂ → rota → after₂ → after₁ → BuildHTTPResponse / send
```

## Pool de conexões (DB)

Units: `src/DBPool/BadgerDBPool.pas`, `src/DBPool/BadgerDBBridge.pas`.

### Peças

- **`TBadgerDBPool`**: pool genérico. Recebe um conector `TComponent` do DataModule (Zeos, FireDAC, UniDAC, `TSQLConnection`, …) e `APoolN` (**hard cap** de conexões idle+borrowed). Clona o template internamente (`WriteComponent`/`ReadComponent` + propriedade `Connected` via RTTI). O template **não** entra no pool. `Acquire` lança se o pool estiver esgotado; `Release` é idempotente (double-release é no-op); `Destroy` fecha idle e borrowed.
- **`TBadgerDBBridge`**: registra before (injeta `Request.DbPool`) e after (safety-net `ReleaseConn`).
- **`AcquireConn` / `ReleaseConn`**: helpers na request. `Release` **devolve** ao pool — não use `FreeAndNil` na conexão emprestada.

Campos opacos em `THTTPRequest`: `DbPool`, `DbConn` (`TObject`).

### Uso

```pascal
uses
  Badger, BadgerDBPool, BadgerDBBridge;

DbBridge := TBadgerDBBridge.Create(dm.ZConnection1, 15);
DbBridge.Register(Server);
Server.Start;

// na rota:
Conn := TZConnection(AcquireConn(Request));
try
  // queries...
finally
  ReleaseConn(Request);
end;
```

Threads de background (sem HTTP): `DbBridge.Pool.Acquire` / `Release` direto.

Destrua o bridge **depois** de `Server.Stop`.

### Sample

- `sample/Lazarus/ConnPool` — GUI + stress concorrente + rotas `/db/ping`, `/db/work`, `/db/stats` (PostgreSQL).

## Eventos de Aplicação

- `OnRequest` e `OnResponse` são callbacks configuráveis em `TBadger`:
  - Propriedades: `src/Badger.pas:68–69`
  - Disparo controlado por `EnableEventInfo` (não dependem do `Logger`): `src/BadgerRequestHandler.pas:410–445`
  - Ajuste via checkbox nos samples:
    - FMX D12: `sample/D12/FMX Windows/Unit1.pas:73`
    - VCL D7: `sample/D7/Unit1.pas:58`
    - Lazarus: `sample/Lazarus/GUI/unit1.pas:86`

## Logging

- `Logger.isActive` e `Logger.LogToConsole` controlam log interno do Badger (independente dos eventos): `src/Badger.pas:99`, samples definem no início.
- Recomendado desabilitar durante testes de throughput para reduzir overhead.

## CORS

- Configuração em `TBadger`:
  - `CorsEnabled`, `CorsAllowedOrigins`, `CorsAllowedMethods`, `CorsAllowedHeaders`, `CorsExposeHeaders`, `CorsAllowCredentials`, `CorsMaxAge`.
- Preflight (OPTIONS): `src/BadgerRequestHandler.pas:365–451`
  - Valida método solicitado e cabeçalhos; responde `204` com `Access-Control-Allow-*` e `Max-Age`.
  - Quando refletindo origem, inclui `Vary: Origin, Access-Control-Request-Method, Access-Control-Request-Headers`.
- Respostas normais: injeta `Access-Control-Allow-Origin` (`*` ou origem), `Access-Control-Allow-Credentials` (se habilitado) e `Access-Control-Expose-Headers`.
- Amostra FMX: habilitação direta em `sample/D12/FMX Windows/Unit1.pas:77–83`.

### Boas práticas
- Evitar `*` quando `AllowCredentials=True`; refletir origem whitelisted.
- Listar `Authorization` explicitamente em `Access-Control-Allow-Headers`.
- Usar `CorsMaxAge` para reduzir frequência de preflights.

### Testes rápidos
- Preflight: `curl -i -X OPTIONS http://localhost:8080/teste/ping -H "Origin: http://localhost:3000" -H "Access-Control-Request-Method: GET" -H "Access-Control-Request-Headers: Content-Type"`
- Normal: `curl -i http://localhost:8080/teste/ping -H "Origin: http://localhost:3000"`

## Concorrência e Desempenho

- Sequencial vs Paralelo:
  - Sequencial (`ParallelProcessing = False`): no clássico, uma thread cuida da conexão; no IOCP, o dispatch da rota é serializado (`FSerialLock`).
  - Paralelo (`ParallelProcessing = True`): clássico cria um handler por conexão; IOCP despacha a rota no worker. Ajustar `MaxConcurrentConnections` gradualmente.
- Keep-Alive HTTP/1.1: o IOCP reutiliza o socket; no Linux (Synapse) testes de carga costumam ir melhor sem Keep-Alive no cliente.
- Sugestões de otimização de baixo risco:
  - Cachear `PatternParts` no `TRouteEntry` ao registrar a rota.
  - No bucket de contexto, separar por `Verb` para reduzir candidatos.

## Parser de Headers

- Implementação linha a linha com `RecvString(FTimeout)`, grava `key=value` em minúsculo em `aHeaders.Values[...]`: `src/BadgerRequestHandler.pas:117–143`
- Evita conversões e risco de consumir bytes do corpo antes da leitura do body.

## Resposta HTTP

- Montagem do cabeçalho com status, content type e content length: `src/BadgerRequestHandler.pas:145–176`
- Envio de corpo (texto/JSON) com encoding UTF‑8 e de stream com buffer limitado: `src/BadgerRequestHandler.pas:380–408`

## Samples

Rotas compartilhadas dos demos oficiais: `sample/Common/SampleRouteManager.pas`.  
Referência canônica de setup: **`sample/Lazarus/GUI/unit1.pas`** (D7 e FMX D12 seguem o mesmo padrão).

### Oficial (porta 8080, API pública)

No Windows usam IOCP pelo default de `TBadger`. Linux/macOS: Synapse.

| Sample | Caminho | Propósito |
|--------|---------|-----------|
| Lazarus GUI | `sample/Lazarus/GUI/` | Demo completa: rotas, auth, eventos, paralelo, checkbox IOCP |
| VCL D7 | `sample/D7/` | Mesmo conjunto de rotas/auth que Lazarus GUI |
| FMX D12 | `sample/D12/FMX Windows/` | Mesmo conjunto de rotas/auth que Lazarus GUI |
| WinService D12 | `sample/D12/WinService/` | Badger como serviço Windows |
| Console Linux | `sample/Lazarus/Console_Linux/` | Headless (`/teste/ping`) |

### IOCP (porta 8081)

Mesma API de `TBadger` (IOCP já é o default no Windows). Recorte extra: CORS, after-middleware, WebSocket `/chat`. Units: `sample/IOCP/IocpDemoRoutes.pas`, `IocpWsChatClient.pas`.

| Sample | Caminho |
|--------|---------|
| Console / GUI D12 | `sample/IOCP/D12/` |
| Console / GUI D7 | `sample/IOCP/D7/` |
| Console / GUI Lazarus | `sample/IOCP/Lazarus/` |

### Feature

| Sample | Caminho | Propósito |
|--------|---------|-----------|
| ConnPool | `sample/Lazarus/ConnPool/` | Pool DB + stress + `/db/*` (PostgreSQL / Zeos) |
| Midd_before_after | `sample/Lazarus/Midd_before_after/` | `AddMiddleware` / `AddAfterMiddleware` |

### Carga

| Sample | Caminho | Propósito |
|--------|---------|-----------|
| StressTeste | `sample/StressTeste/` | Utilitários de carga |

Padrão comum nos demos GUI oficiais:
- `Logger.isActive := False` (eventos via checkbox `EnableEventInfo`)
- `MaxConcurrentConnections := 500` quando `ParallelProcessing = True`
- `FreeAndNil(ServerThread)` ao parar e no `FormDestroy`
- Rotas protegidas usam `/teste/ping` (mesmo path registrado no `RouteManager`)

## Boas Práticas

- No Windows o motor padrão é IOCP; `UseIOCP := False` força Synapse se precisar comparar ou depurar o caminho clássico.
- Desabilitar `Logger` e eventos (`EnableEventInfo`/checkbox) ao medir throughput.
- Ajustar `MaxConcurrentConnections` gradualmente conforme hardware.
- Agrupar endpoints por contexto para máxima efetividade do bucket.
- Evitar primeiro segmento dinâmico quando possível, para usar buckets específicos.
- No DB pool: sempre `ReleaseConn` (ou after do bridge); nunca `Free` da conexão emprestada; destruir o bridge após `Server.Stop`.

## Troubleshooting

- Throughput baixo (~40 req/s):
  - Verificar antivírus/firewall (ESET) — pausar ou adicionar exceções para executável, pasta e porta.
  - Desativar logging e eventos.
  - Voltar para parser por linha e `Sleep(10)` no loop.

## Referências de Código

- `TBadger` `Start`/`Stop` e escolha do motor: `src/Badger.pas`
- Motor IOCP: `src/IOCP/BadgerIOCP.pas`, parser: `src/BadgerHttpParser.pas`, WS: `src/BadgerWebSocket.pas`
- `THTTPRequestHandler` parsing e resposta (Synapse): `src/BadgerRequestHandler.pas`
- `TRouteManager` registro e matching com contexto: `src/BadgerRouteManager.pas`
- After-middleware: `AddAfterMiddleware` / `RunAfterMiddlewares` em `src/Badger.pas` e `src/BadgerRequestHandler.pas`
- DB pool: `src/DBPool/BadgerDBPool.pas`, bridge HTTP: `src/DBPool/BadgerDBBridge.pas`
