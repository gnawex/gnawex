-- Deploy gnawex:extension__pgcron to pg

BEGIN;

CREATE EXTENSION pg_cron;

COMMIT;
