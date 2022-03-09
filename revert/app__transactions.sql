-- Revert gnawex:transactions from pg

BEGIN;

REVOKE SELECT ON TABLE app.transactions FROM api;

DROP TABLE app.transactions;

COMMIT;
