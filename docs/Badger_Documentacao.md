# Badger – Documentação Técnica Completa

## Visão Geral

Badger é um microservidor HTTP multithread, leve e focado em alto desempenho, com suporte a rotas estáticas e dinâmicas, middlewares, eventos de aplicação (`OnRequest`/`OnResponse`) e autenticação via exemplos. A arquitetura privilegia simplicidade no hot path, com otimizações localizadas (índice estático e agrupamento por contexto) e controles explícitos de concorrência.

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
3. Montagem de `THTTPRequest` e roteamento via `TRouteManager.MatchRoute`: `src/BadgerRouteManager.pas:179–227`
4. Execução do callback da rota (método) e construção da `THTTPResponse`: `src/BadgerRequestHandler.pas:145–176`
5. Envio de headers, corpo (texto/JSON) ou stream com chunking simples: `src/BadgerRequestHandler.pas:380–408`
6. Eventos de aplicação, conforme `EnableEventInfo`: `src/BadgerRequestHandler.pas:410–445`

## Roteamento

- Rotas estáticas: comparadas diretamente por número de segmentos e igualdade de partes.
- Rotas dinâmicas: partes com `:` são capturadas em `Params` (`nome=valor`).
- Índice por contexto:
  - Bucket selecionado pelo primeiro segmento do path; se o primeiro segmento do pattern for dinâmico, a rota entra no bucket vazio `''`.
  - Vantagem: reduz candidatos quando há muitos endpoints por domínio funcional (ex.: `/produtos/...`, `/login/...`).

## Middlewares

- `TBadger.AddMiddleware` adiciona procedimentos de middleware (declaração em `BadgerTypes.pas`).
- Cada `THTTPRequestHandler` clona wrappers dos middlewares para isolamento: `src/BadgerRequestHandler.pas:57–66`, `src/BadgerRequestHandler.pas:68–89`
- Liberação no destrutor: `src/BadgerRequestHandler.pas:110–115`

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

- FMX D12 (`sample/D12/FMX Windows/Unit1.pas`) e VCL D7 (`sample/D7/Unit1.pas`) e Lazarus (`sample/Lazarus/unit1.pas`) mostram:
  - Como iniciar/parar o servidor
  - Como configurar `OnRequest`/`OnResponse` e `EnableEventInfo` via checkbox
  - Registro de rotas e autenticação básica/JWT.

## Boas Práticas

- Desabilitar `Logger` e eventos (`EnableEventInfo`/checkbox) ao medir throughput.
- Ajustar `MaxConcurrentConnections` gradualmente conforme hardware.
- Agrupar endpoints por contexto para máxima efetividade do bucket.
- Evitar primeiro segmento dinâmico quando possível, para usar buckets específicos.

## Troubleshooting

- Throughput baixo (~40 req/s):
  - Verificar antivírus/firewall (ESET) — pausar ou adicionar exceções para executável, pasta e porta.
  - Desativar logging e eventos.
  - Voltar para parser por linha e `Sleep(10)` no loop.

## Referências de Código

- `TBadger` execução e aceitação: `src/Badger.pas:564–647`
- `THTTPRequestHandler` parsing e resposta: `src/BadgerRequestHandler.pas:117–176`, `src/BadgerRequestHandler.pas:380–445`
- `TRouteManager` registro e matching com contexto: `src/BadgerRouteManager.pas:107–124`, `src/BadgerRouteManager.pas:179–227`