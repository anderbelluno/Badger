-- Badger ConnPool demo schema (PostgreSQL)
-- Target DB: badger_pool  |  User: postgres

BEGIN;

CREATE TABLE IF NOT EXISTS jobs (
  id          BIGSERIAL PRIMARY KEY,
  label       TEXT NOT NULL,
  payload     TEXT NOT NULL DEFAULT '',
  status      TEXT NOT NULL DEFAULT 'open',
  priority    INTEGER NOT NULL DEFAULT 1,
  created_at  TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS job_hits (
  id          BIGSERIAL PRIMARY KEY,
  job_id      BIGINT NOT NULL REFERENCES jobs(id) ON DELETE CASCADE,
  worker_name TEXT NOT NULL,
  backend_pid INTEGER NOT NULL,
  hit_at      TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS products (
  id          BIGSERIAL PRIMARY KEY,
  sku         TEXT NOT NULL UNIQUE,
  name        TEXT NOT NULL,
  price       NUMERIC(12,2) NOT NULL DEFAULT 0,
  stock       INTEGER NOT NULL DEFAULT 0,
  active      BOOLEAN NOT NULL DEFAULT TRUE,
  created_at  TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS ix_job_hits_job_id ON job_hits(job_id);
CREATE INDEX IF NOT EXISTS ix_job_hits_hit_at ON job_hits(hit_at);
CREATE INDEX IF NOT EXISTS ix_jobs_status ON jobs(status);
CREATE INDEX IF NOT EXISTS ix_products_active ON products(active);

-- Seed jobs (idempotent)
INSERT INTO jobs (label, payload, status, priority)
SELECT
  'seed-' || g,
  'demo payload ' || g,
  CASE WHEN g % 5 = 0 THEN 'closed' ELSE 'open' END,
  1 + (g % 3)
FROM generate_series(1, 50) AS g
WHERE NOT EXISTS (SELECT 1 FROM jobs LIMIT 1);

-- Seed products (idempotent)
INSERT INTO products (sku, name, price, stock, active)
SELECT
  'SKU-' || lpad(g::text, 4, '0'),
  'Product ' || g,
  round((10 + random() * 990)::numeric, 2),
  (random() * 200)::int,
  (g % 7 <> 0)
FROM generate_series(1, 100) AS g
WHERE NOT EXISTS (SELECT 1 FROM products LIMIT 1);

COMMIT;

-- Useful checks:
--   SELECT pg_backend_pid();
--   SELECT count(*) FROM pg_stat_activity WHERE datname = current_database();
--   SELECT count(*) FROM jobs; SELECT count(*) FROM products;
