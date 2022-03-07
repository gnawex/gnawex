-- Revert gnawex:transactions from pg

BEGIN;

DROP TABLE app.transactions;

COMMIT;
