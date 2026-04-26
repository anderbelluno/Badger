# Badger — Deep Dive Audit (Static + Baseline Runtime)

**Data:** 2026-04-23  
**Escopo desta rodada:** validação sem alteração de código (leitura estática + execução de baseline com FPC)

## Ambiente de Teste

- **FPC:** `C:\Laz_fix\fpc\bin\i386-win32\fpc.exe` (`3.2.3`)
- **Build alvo:** `sample/Lazarus/Console_Linux/project1.lpr`
- **Observação:** compilação requer `-Mdelphi` neste ambiente

Comando de build usado:

```powershell
& 'C:\Laz_fix\fpc\bin\i386-win32\fpc.exe' -Mdelphi '.\sample\Lazarus\Console_Linux\project1.lpr' `
  -Fu'.\src' -Fu'.\sample' -Fu'.\src\Auth\JWT' -Fu'.\src\Auth\Basic' `
  -Fu'.\ThirdParty\Synapse' -Fu'.\ThirdParty\SuperObject\D_Plus_Laz_Linux' -B
```

## Tabela de Triagem (Round 1)

| Item (AUDIT_TODO) | Status | Evidência | Prioridade prática |
|---|---|---|---|
| §2.1 `Request` por valor em middleware | **Corrigido (em validação)** | assinatura de `TMiddlewareProc` alterada para `var Request` e implementações JWT/Basic ajustadas | **Crítico** |
| §4.3 Rotas protegidas por match exato (`SameText`) | **Corrigido (validado em FPC)** | middleware agora usa match exato ou prefixo com separador (`/`) | **Crítico** |
| §4.2 Header `Authorization` com substring (`Pos > 0`) | **Corrigido (validado em FPC)** | middlewares agora usam lookup exato `Request.Headers.Values['Authorization']` | **Crítico** |
| §3.1 Multipart path traversal em filename | **Corrigido (validado em FPC)** | nome agora é sanitizado em `ExtractFileName/UniqueFileName` antes do `SaveToFile` | **Crítico** |
| §5.1 Header injection/CRLF em resposta | **Corrigido (validado em FPC)** | `BuildHTTPResponse` sanitiza nome/valor de header removendo CRLF e caracteres inválidos | **Crítico** |
| §5.3 CORS credentials + wildcard refletindo origem | **Corrigido (validado em FPC)** | com `credentials=true`, apenas origem explicitamente listada recebe CORS headers | **Crítico** |
| §3.4 Sem suporte a chunked | **Corrigido (validado em FPC)** | parser agora lê `Transfer-Encoding: chunked`, consome trailers e mantém conexão sincronizada | **Alta** |
| §1.2 / §6 sockets em paralelo não removidos corretamente | **Corrigido (validado em FPC)** | destrutor paralelo voltou a notificar fechamento e `Stop` agora fecha sockets ativos no início do shutdown | **Alta** |
| §2.4 / §12.3 `Stop` espera 15s com contador congelado | **Corrigido (validado em FPC)** | `DecActiveConnections` volta a decrementar em shutdown; `Stop` não fica preso no timeout máximo | **Alta** |
| §2.3 lock global durante accept/sleep | **Corrigido (validado em FPC)** | escopo do `FSocketLock` foi reduzido ao `Accept`; `Sleep(10)` e fluxo não crítico ficaram fora do lock | **Alta** |
| §1.6 + §12.2 rebuild de MIME por chamada | **Corrigido (validado em FPC)** | MIME table passa a ser carregada no `Create` e reutilizada; lookup sem rebuild por chamada | **Média** |
| §8.1 logger abre/fecha arquivo por log | **Corrigido (validado em FPC)** | logger mantém `TFileStream` aberto (com lock) e não reabre arquivo a cada escrita | **Média** |
| §6.1 `Handled` não resetado por iteração | **Refutado** | Há `Handled := False` no loop antes de middlewares | **Baixa** |
| §3.2 `Content-Length` negativo estoura `SetLength` | **Corrigido (validado em FPC)** | `Content-Length < 0` retorna `400` com erro explícito e fluxo HTTP permanece sincronizado | **Média** |
| §12.1 “todos os coletores Req/Resp não resetam” | **Corrigido (validado em FPC)** | estado de `Req/Resp` é resetado a cada iteração de keep-alive (incluindo headers custom, body e streams) | **Alta** |

## Testes Runtime Executados (Sem Alterar Código)

### T1 — Build baseline
- **Resultado:** compilou com sucesso com `-Mdelphi`
- **Observação:** sem `-Mdelphi`, erro de modo de linguagem em `BadgerMultipartDataReader.pas`

### T2 — GET simples
Requisição:

```bash
curl -i http://127.0.0.1:8080/teste/ping
```

Resultado:
- `HTTP/1.1 200 OK`
- Body `Pong`

### T3 — Keep-alive com 2 requisições na mesma conexão
- **Resultado:** duas respostas `200` corretas para `/teste/ping` no sample atual
- **Leitura:** não invalida os riscos de estado residual em fluxos que usam middlewares/CORS/headers customizados

### T4 — `Content-Length: -1`
- **Resultado observado:** `404 Not Found` (handler não crashou neste cenário)
- **Leitura:** reforça que o bug descrito no TODO está superestimado no caminho atual; ainda é desejável validar e rejeitar `<0`

### T5 — `Transfer-Encoding: chunked` + segunda request no mesmo socket
- **Resultado observado:** primeira request retorna `404`, segunda resposta vem vazia (desincronização/fechamento)
- **Leitura:** comportamento consistente com ausência de suporte chunked

### T6 — Header muito grande (`X-Big` ~256KB)
- **Resultado observado:** servidor respondeu `200 OK`
- **Leitura:** não prova ausência de risco DoS; só mostra que esse tamanho específico não derrubou no ambiente atual

## Execução do Item 1

### Mudanças aplicadas
- `src/BadgerTypes.pas`: `TMiddlewareProc` mudou de `Request: THTTPRequest` para `var Request: THTTPRequest`.
- `src/Auth/JWT/BadgerAuthJWT.pas`: assinatura de `MiddlewareProc` ajustada para `var Request`.
- `src/Auth/Basic/BadgerBasicAuth.pas`: assinatura de `Check` ajustada para `var Request`.

### Validação realizada
- **FPC (`C:\Laz_fix`)**: build do sample Lazarus compilou com sucesso após a mudança.
- **Teste funcional isolado (runtime)**: middleware alterou `Req.UserID` e a alteração propagou corretamente (`PASS`).
- **Delphi 12 (`dcc32`)**: compilador encontrado, mas ambiente de linha de comando sem paths padrão de RTL (`SysUtils`/`Windows` não resolvidos).
- **Delphi 7 (`dcc32`)**: compilador encontrado; build direto bloqueado por encoding/compatibilidade de unidades (`Illegal character $EF` e dependência Synapse para D7 no caminho atual).

### Situação do item
- **Item 1 concluído no código**, com validação positiva em FPC.
- **Pendente de fechamento cross-compiler** em sessão com environment Delphi configurado (prompt de comando da IDE / scripts de env).

## Execução do Item 2

### Mudanças aplicadas
- `src/Auth/JWT/BadgerAuthJWT.pas`: adição de helper de normalização/match e troca de `SameText` por `IsProtectedRoute(...)`.
- `src/Auth/Basic/BadgerBasicAuth.pas`: mesma estratégia de normalização/match para proteção de subrotas.
- Regra aplicada: rota protegida casa em `exato` ou `prefixo + '/'`, evitando falso positivo em caminhos parecidos.

### Validação realizada
- **Build FPC (`C:\Laz_fix`)**: compilação do sample Lazarus passou sem erros.
- **Teste funcional dedicado** (programa temporário):
- `/api/admin` sem auth → `401`
- `/api/admin/users/1` sem auth → `401` (subrota agora protegida)
- `/api/adminX` sem auth → `200` (não protegido por similaridade de prefixo textual)

### Situação do item
- **Item 2 concluído no código e validado em FPC**.
- Fechamento em Delphi 12/7 continua dependente de ambiente de compilação configurado na sessão CLI.

## Execução do Item 3

### Mudanças aplicadas
- `src/Auth/JWT/BadgerAuthJWT.pas`: removida varredura por substring; token lido por `Request.Headers.Values['Authorization']`.
- `src/Auth/Basic/BadgerBasicAuth.pas`: removida varredura por substring; header lido por `Request.Headers.Values['Authorization']`.

### Validação realizada
- **Build FPC (`C:\Laz_fix`)**: compilação do sample Lazarus passou sem erros.
- **Teste funcional dedicado** (programa temporário BasicAuth):
- sem header auth (`/secure`) → `401`
- com `Authorization: Basic ...` correto → `200`
- apenas `X-Forwarded-Authorization: Basic ...` (sem `Authorization`) → `401`

### Situação do item
- **Item 3 concluído no código e validado em FPC**.

## Execução do Item 4

### Mudanças aplicadas
- `src/BadgerMultipartDataReader.pas`: adicionado helper `SanitizeUploadFileName(...)`.
- `ExtractFileName(...)`: resultado bruto do multipart agora passa por sanitização.
- `UniqueFileName(...)`: também sanitiza `BaseName` antes de gerar nome final/colisão.
- Regras: remove componente de caminho (`ExtractFileName`), neutraliza caracteres de path/controle, remove sequências `..`, aplica fallback seguro para nome vazio.

### Validação realizada
- **Build FPC (`C:\Laz_fix`)**: compilação do sample Lazarus passou sem erros.
- **Teste funcional dedicado** (programa temporário):
- entrada `../../Windows/System32/drivers/etc/hosts` → `hosts`
- entrada `..\\..\\secret.txt` → `secret.txt`
- entrada `/etc/passwd` → `passwd`
- entrada `C:\\Windows\\win.ini` → `win.ini`
- entrada `normal-file.png` → `normal-file.png`

### Situação do item
- **Item 4 concluído no código e validado em FPC**.

## Execução do Item 5

### Mudanças aplicadas
- `src/BadgerRequestHandler.pas`: `BuildHTTPResponse(...)` passou a sanitizar headers customizados antes de serializar.
- Nome do header: remove caracteres de controle e `:`.
- Valor do header: remove `#13/#10` (CR/LF), impedindo quebra de linha/injeção de novo header.

### Validação realizada
- **Build FPC (`C:\Laz_fix`)**: compilação do sample Lazarus passou sem erros.
- **Teste funcional raw** (programa temporário):
- rota seta `X-Test = 'safe' + CRLF + 'Injected: yes'`
- resposta recebida contém `X-Test:safeInjected: yes`
- verificação explícita: `HAS_INJECTED_HEADER=False` para `\r\nInjected:`

### Situação do item
- **Item 5 concluído no código e validado em FPC**.

## Execução do Item 6 (CORS Credentials + Wildcard)

### Mudanças aplicadas
- `src/BadgerRequestHandler.pas`: lógica de CORS foi ajustada para calcular `OriginAllowed` de forma segura.
- Com `CorsAllowCredentials=True`: **não** considera `*` como origem válida; exige origem explícita em `CorsAllowedOrigins`.
- Com `CorsAllowCredentials=False`: mantém comportamento de wildcard (`*`) permitido.
- Removida a estratégia de refletir `Origin` quando havia `credentials + *`.

### Validação realizada
- **Build FPC (`C:\Laz_fix`)**: compilação do sample Lazarus passou sem erros.
- **Teste funcional dedicado** (programa temporário CORS):
- configuração: `CorsEnabled=True`, `CorsAllowCredentials=True`, `CorsAllowedOrigins=['*']`
- `OPTIONS /ping` com `Origin: https://evil.test` + preflight method → `403 Forbidden`
- `GET /ping` com `Origin: https://evil.test` → `200`, sem header `Access-Control-Allow-Origin` refletido

### Situação do item
- **Item 6 concluído no código e validado em FPC**.

## Execução do Item 7 (Stop / ActiveConnections)

### Mudanças aplicadas
- `src/Badger.pas`: `DecActiveConnections` deixou de ignorar decremento durante shutdown.
- Comportamento anterior: quando `FIsShuttingDown=True`, o método não decrementava e o `Stop` podia aguardar até `MaxWaitTime=15000`.
- Comportamento atual: decremento sempre ocorre de forma atômica; se houver underflow eventual, contador é normalizado para `0`.

### Validação realizada
- **Build FPC (`C:\Laz_fix`)**: compilação do sample Lazarus passou sem erros.
- **Teste funcional dedicado** (programa temporário):
- rota `/slow` com `Sleep(4000)` para manter conexão ativa durante shutdown.
- `Server.Stop` medido logo após iniciar request ativo.
- resultado observado: `STOP_MS=3906` (antes o problema típico era encostar no timeout de ~15000 ms).

### Situação do item
- **Item 7 concluído no código e validado em FPC**.

## Execução do Item 8 (Stop com Parallel + Socket Ocioso)

### Achado da revalidação
- Mesmo após corrigir `DecActiveConnections`, ainda existia cenário de demora percebida no `Stop` quando `ParallelProcessing=True`.
- Repro: conexão TCP ociosa (sem request), `Timeout=10000`.
- Resultado antes do ajuste adicional: `STOP_IDLE_MS=9735`.

### Mudanças aplicadas
- `src/Badger.pas`: adicionado `CloseClientSocketsForShutdown`, chamado no início de `Stop` (logo após fechar socket de escuta).
- Efeito: handlers paralelos bloqueados em leitura são interrompidos imediatamente por fechamento de socket.
- `src/BadgerRequestHandler.pas`: destrutor paralelo agora usa `NotifyClientSocketClosed(FClientSocket)` (remove da lista + decrementa contador) em vez de apenas decrementar.

### Validação realizada
- **Build FPC (`C:\Laz_fix`)**: compilação do sample Lazarus passou sem erros.
- **Teste funcional dedicado** (mesmo cenário de conexão ociosa):
- antes: `STOP_IDLE_MS=9735`
- depois: `STOP_IDLE_MS=157`

### Situação do item
- **Item 8 concluído no código e validado em FPC**.

## Execução do Item 9 (Transfer-Encoding: chunked)

### Mudanças aplicadas
- `src/BadgerRequestHandler.pas`: adicionado parser de body chunked (`ReadChunkedBodyToStream`).
- Suporte a:
- leitura de tamanho em hex (`chunk-size`)
- ignorar chunk extensions (`;...`)
- consumo de `CRLF` entre chunks
- consumo de trailers até linha vazia final
- integração com fluxo existente:
- se `Transfer-Encoding` contém `chunked`, body passa a ser lido por chunks
- mantém lógica de `Req.Body` (text/json) e `Req.BodyStream` (binário)
- proteção adicional: `Content-Length < 0` agora retorna `400 Bad Request`.

### Validação realizada
- **Build FPC (`C:\Laz_fix`)**: compilação do sample Lazarus passou sem erros.
- **Teste funcional dedicado** (mesmo socket / keep-alive):
- request 1: `POST /echo` com `Transfer-Encoding: chunked` e payload `hello`
- request 2: `GET /ping` na mesma conexão
- resultados:
- `HAS_FIRST_200=True`
- `HAS_ECHO_HELLO=True`
- `HAS_SECOND_200=True`
- resposta confirmou duas respostas válidas (`hello` e `pong`) sem desincronização.

### Situação do item
- **Item 9 concluído no código e validado em FPC**.

## Execução do Item 10 (Lock Global no Accept Loop)

### Mudanças aplicadas
- `src/Badger.pas`: refatorado `TBadger.Execute` para reduzir a região crítica de `FSocketLock`.
- `Sleep(10)` (backoff por limite de conexões) saiu de dentro do lock.
- `CanRead(100)` e decisões de fluxo não crítico passaram a ocorrer fora da região crítica.
- `FSocketLock` ficou concentrado no trecho sensível de `Accept`/validação de estado do socket de escuta.

### Validação realizada
- **Build FPC (`C:\Laz_fix`)**: compilação do sample Lazarus passou sem erros.
- **Smoke test sequencial**: `PING_200_COUNT=30` em 30 requests (`PING_TOTAL_MS=2427`).
- **Smoke test paralelo**: `PARALLEL_200_COUNT=10` em 10 requests concorrentes.

### Situação do item
- **Item 10 concluído no código e validado em FPC**.

## Execução do Item 11 (MIME Rebuild por Chamada)

### Mudanças aplicadas
- `src/BadgerUtils.pas`: `BuildMimelist` passou a ser executado no `constructor Create`.
- `src/BadgerUtils.pas`: `GetFileMIMEType(...)` agora faz apenas lookup em lista já carregada.
- `src/BadgerMethods.pas`: `TBadgerMethods` ganhou `FUtils` persistente (criado no construtor/destruído no destrutor), removendo `Create/Free` de `TBadgerUtils` por chamada.

### Validação realizada
- **Build FPC (`C:\Laz_fix`)**: compilação do sample Lazarus passou sem erros.
- **Teste funcional dedicado**:
- `MIME_PNG=image/png`
- `MIME_JSON=application/json`
- `MIME_5000_MS=281` (lookup repetido sem rebuild em loop).

### Situação do item
- **Item 11 concluído no código e validado em FPC**.

## Execução do Item 12 (Logger Open/Close por Escrita)

### Mudanças aplicadas
- `src/BadgerLogger.pas`: adicionado `FLogStream` persistente com `EnsureLogStream`/`CloseLogStream`.
- Escrita em arquivo agora usa stream aberto e protegido pelo `FCS` já existente.
- Troca de nome de arquivo (`LogFileName`) é tratada com reopen seguro.

### Validação realizada
- **Build FPC (`C:\Laz_fix`)**: compilação do sample Lazarus passou sem erros.
- **Teste funcional dedicado**:
- `LOG_1000_MS=31`
- `LOG_FILE_EXISTS=TRUE`
- verificação pós-processo: arquivo com `1000` linhas de log.

### Situação do item
- **Item 12 concluído no código e validado em FPC**.

## Execução do Item 13 (Content-Length Negativo)

### Mudanças aplicadas
- `src/BadgerRequestHandler.pas`: rejeição explícita para `Content-Length < 0` com:
- `HTTP 400 Bad Request`
- body `{"error":"Invalid Content-Length"}`
- sem quebrar sincronização da conexão.

### Validação realizada
- **Build FPC (`C:\Laz_fix`)**: compilação do sample Lazarus passou sem erros.
- **Teste funcional raw** (mesma conexão):
- request 1: `POST /teste/ping` com `Content-Length: -1`
- request 2: `GET /teste/ping`
- resultados:
- `HAS_400=True`
- `HAS_INVALID_CL=True`
- `HAS_200=True`

### Situação do item
- **Item 13 concluído no código e validado em FPC**.

## Execução do Item 14 (Reset de Estado no Keep-Alive)

### Mudanças aplicadas
- `src/BadgerRequestHandler.pas`: reset per-request no início de cada iteração:
- `Req.QueryParams`, `Req.Headers`, `Req.RouteParams`, `Req.Body`
- `Req.BodyStream` (free + nil)
- `Resp.StatusCode`, `Resp.Body`, `Resp.ContentType`
- `Resp.Stream` (free + nil), `Resp.HeadersCustom.Clear`
- variáveis auxiliares (`Origin`, etc.) e flags de controle.
- Também foi adicionado `SkipRequestProcessing` para erros de parse/validação, garantindo envio consistente da resposta sem vazar estado para a próxima requisição.

### Validação realizada
- **Build FPC (`C:\Laz_fix`)**: compilação passou sem erros.
- **Teste funcional keep-alive** (mesmo socket):
- request 1 (`/set`) retorna `201` + header custom `X-Leak: 1`.
- request 2 (`/check`) retorna `200` e **não** contém `X-Leak`.
- resultados:
- `SECOND_HAS_X_LEAK=False`
- `SECOND_BODY_TWO=True`

### Situação do item
- **Item 14 concluído no código e validado em FPC**.

## Backlog de Teste Profundo (Próxima Rodada)

1. Fechar validação cross-compiler (Delphi 12 e Delphi 7) em ambiente de linha de comando da IDE.
2. Executar teste de carga concorrente mais longo (5–10 min) para baseline pós-refactors de paralelismo.
3. Revisar limites/erros de parsing para inputs malformados adicionais (chunk size inválido, trailers grandes, headers extremos).

## Nota de Confiabilidade

Este documento combina:
- **Confirmação estática forte** para bugs semânticos.
- **Confirmação dinâmica inicial** para parser/protocolo em ambiente local.

Itens marcados como **Parcial** exigem cenário de teste adicional antes de classificação final.
