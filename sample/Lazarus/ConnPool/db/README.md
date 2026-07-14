# ConnPool sample — database setup

Default engine: **PostgreSQL** (multi-connection). Firebird is optional if you already have a server.

## Quick start (Docker)

```bash
cd sample/Lazarus/ConnPool/db
docker compose up -d
```

Defaults used by the Lazarus app:

| Setting  | Value        |
|----------|--------------|
| Host     | 127.0.0.1    |
| Port     | 5432         |
| Database | badger_pool  |
| User     | postgres     |
| Password | postgres     |
| Protocol | postgresql   |

## Manual apply (existing Postgres)

```bash
PGPASSWORD=postgres psql -h 127.0.0.1 -U postgres -d badger_pool -f schema.sql
```

## Optional Firebird

If you prefer Firebird, create an empty DB and run the same logical model
(`jobs` / `job_hits`) with Firebird types (`IDENTITY` / `TIMESTAMP`). Point Zeos to
`protocol=firebird` and the `.fdb` path. The Lazarus UI only needs a working
multi-session engine — BadgerDBPool is engine-agnostic.
