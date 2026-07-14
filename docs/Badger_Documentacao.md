# Badger – Documentação Técnica Completa

## Visão Geral

Badger é um microservidor HTTP multithread, leve e focado em alto desempenho, com suporte a rotas estáticas e dinâmicas, middlewares (before/after), eventos de aplicação (`OnRequest`/`OnResponse`), autenticação via exemplos e pool genérico de conexões de banco (`TBadgerDBPool` / `TBadgerDBBridge`). A arquitetura privilegia simplicidade no hot path, com otimizações localizadas (índice estático e agrupamento por contexto) e controles explícitos de concorrência.

## Arquitetura

- Thread do servidor (`TBadger`): implementa accept loop, gerenciamento de conexões e criação de handlers por requisição.
  - Declaração e propriedades: `src/Badger.pas:24–71`
  - Construtor inicializa socket, gerenciador de rotas e listas: `src/Badger.pas:80–100`
  - Loop principal (`Execute`): aceita conexões, decide entre processamento paralelo ou sequencial: `src/Badger.pas:564–647`
  - Controles de concorrência:
    - `ParallelProcessing`: habilita processamento paralelo por thread: `src/Badger.pas:66`
    - `MaxConcurrentConnections`: limite de conexões ativas: `src/Badger.pas:67`
    - Gate de accept quando atingir o limite: `src/Badger.pas:584–589`

- Handler de requisição (`THTTPRequestHandler`): realiza parsing do request, roteamento, execução de método/rotas, construção e envio de resposta.
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

1. Accept loop em `TBadger.Execute` aceita conexão e cria `THTTPRequestHandler`: `src/Badger.pas:591–612`
2. `ParseRequestHeader` lê cabeçalhos linha a linha e preenche `TStringList` com `key=value`: `src/BadgerRequestHandler.pas:117–143`
3. Middlewares **before** (`AddMiddleware`): podem interromper com `Handled=True`
4. Roteamento via `TRouteManager.MatchRoute` e execução do callback: `src/BadgerRouteManager.pas:179–227`
5. Envio de headers, corpo (texto/JSON) ou stream: `src/BadgerRequestHandler.pas:380–408`
6. Eventos de aplicação, conforme `EnableEventInfo`
7. Middlewares **after** (`AddAfterMiddleware`), em ordem LIFO — cleanup mesmo se um before short-circuitou

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
| After | `AddAfterMiddleware` | Depois da rota / resposta | procedure (cleanup) |

- Declarações: `TMiddlewareProc` / `TAfterMiddlewareProc` em `src/BadgerTypes.pas`.
- Registro: `TBadger.AddMiddleware` / `TBadger.AddAfterMiddleware` em `src/Badger.pas`.
- After roda em ordem **LIFO** (modelo cebola), inclusive quando um before short-circuita com `Handled=True`.
- Cada `THTTPRequestHandler` copia wrappers no accept para isolamento entre threads.
- Uso típico do after: liberar recursos (ex.: conexão emprestada do DB pool).

Fluxo resumido:

```
before₁ → before₂ → rota → resposta HTTP → after₂ → after₁
```

## Pool de conexões (DB)

Units: `src/DBPool/BadgerDBPool.pas`, `src/DBPool/BadgerDBBridge.pas`.

### Peças

- **`TBadgerDBPool`**: pool genérico. Recebe um conector `TComponent` do DataModule (Zeos, FireDAC, UniDAC, `TSQLConnection`, …) e `APoolN`. Clona o template internamente (`WriteComponent`/`ReadComponent` + propriedade `Connected` via RTTI). O template **não** entra no pool.
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
    - Lazarus: `sample/Lazarus/unit1.pas:74`

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
  - Sequencial (`ParallelProcessing = False`): uma thread cuida da conexão; simples e previsível.
  - Paralelo (`ParallelProcessing = True`): cria um `THTTPRequestHandler` por conexão; ajustar `MaxConcurrentConnections` gradualmente.
- Accept loop e latência: `Sleep(10)` no loop para reduzir busy‑wait e estabilizar accept: `src/Badger.pas:646–647`.
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

- FMX D12 (`sample/D12/FMX Windows/Unit1.pas`), VCL D7 (`sample/D7/Unit1.pas`) e Lazarus GUI (`sample/Lazarus/GUI/unit1.pas`) mostram:
  - Como iniciar/parar o servidor
  - Como configurar `OnRequest`/`OnResponse` e `EnableEventInfo` via checkbox
  - Registro de rotas e autenticação básica/JWT.
- Lazarus ConnPool (`sample/Lazarus/ConnPool/`): pool DB + stress multi-thread + endpoints `/db/*` (PostgreSQL / Zeos).

## Boas Práticas

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

- `TBadger` execução e aceitação: `src/Badger.pas:564–647`
- `THTTPRequestHandler` parsing e resposta: `src/BadgerRequestHandler.pas:117–176`, `src/BadgerRequestHandler.pas:380–445`
- `TRouteManager` registro e matching com contexto: `src/BadgerRouteManager.pas:107–124`, `src/BadgerRouteManager.pas:179–227`
- After-middleware: `AddAfterMiddleware` / `RunAfterMiddlewares` em `src/Badger.pas` e `src/BadgerRequestHandler.pas`
- DB pool: `src/DBPool/BadgerDBPool.pas`, bridge HTTP: `src/DBPool/BadgerDBBridge.pas`
