# Badger ConnPool sample (Lazarus)

Demonstrates **BadgerDBPool** with Zeos + **PostgreSQL** under concurrent load.

## Layout

```
ConnPool/
  db/          schema + docker-compose
  src/         Lazarus GUI (ConnPool.lpi)
```

## 1. Start database

```bash
cd sample/Lazarus/ConnPool/db
docker compose up -d
```

Defaults: `postgres` / `postgres` @ `127.0.0.1:5432` / `badger_pool`.

## 2. Open project

Open `src/ConnPool.lpi` in Lazarus (Zeos packages required).

## 3. Run

1. **Test DB** — opens the Zeos template connection  
2. **Start server** — creates pool (`Pool N`) and Badger on **HTTP Port** (8088)  
3. **Stress pool** — N threads × loops calling `Acquire` / `Release` (if threads > pool size, extras fail with “pool exhausted”)  
4. Or HTTP:

```bash
curl http://127.0.0.1:8088/ping
curl http://127.0.0.1:8088/db/ping
curl 'http://127.0.0.1:8088/db/work?ms=100'
curl http://127.0.0.1:8088/db/stats
```

Parallel HTTP (example):

```bash
seq 1 40 | xargs -P20 -I{} curl -s 'http://127.0.0.1:8088/db/work?ms=80'
```

`/db/stats` reports `pg_backends` from `pg_stat_activity`.

## Notes

- Wire with `TBadgerDBBridge.Create(ZConnection, N).Register(Server)`.
- Template on the DataModule is **not** used by workers; the pool clones it.
- `ReleaseConn` returns the connection to the pool (do not `Free`).
- After-middleware on the bridge releases any connection left borrowed when a route exits.
