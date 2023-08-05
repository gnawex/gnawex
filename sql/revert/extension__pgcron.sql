-- Revert gnawex:extension__pgcron from pg

BEGIN;

DROP EXTENSION pg_cron;

COMMIT;
