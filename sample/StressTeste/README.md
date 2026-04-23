# StressTeste

Cliente de stress HTTP para o servidor Badger, usando apenas recursos padrão de thread (`TThread`) e `Synapse` (`blcksock`), sem dependência de ferramentas externas.

## Build (FPC/Lazarus)

```bash
fpc -Mdelphi StressTeste.lpr -Fu../../src -Fu../../src/Auth/Basic -Fu../../src/Auth/JWT -Fu../../ThirdParty/Synapse -Fu../../ThirdParty/SuperObject/D_Plus_Laz_Linux
```

## Execução

```bash
./StressTeste --host=127.0.0.1 --port=8080 --path=/teste/ping --threads=50 --requests=1000 --timeout=3000
```

## Parâmetros

- `--host` (padrão `127.0.0.1`)
- `--port` (padrão `8080`)
- `--path` (padrão `/teste/ping`)
- `--threads` (padrão `20`)
- `--requests` (padrão `200`)
- `--timeout` em ms (padrão `3000`)

## Métricas reportadas

- Total de requests
- Sucesso / Falha
- Tempo total
- Throughput (req/s)
- Latência média / mínima / máxima

## Versão GUI (Lazarus)

- Projeto: `sample/StressTeste/GUI/StressTesteGUI.lpi`
- Unit principal: `sample/StressTeste/GUI/MainForm.pas`
- Funcionalidades iniciais:
- configuração por `edits` (host, porta, path, threads, requests/thread, timeout)
- botões `Iniciar Stress`, `Parar Stress`, `Limpar Log`
- checkbox `Keep-Alive (experimental)`
- resumo em tempo real (total, sucesso/falha, rps, latência)
