-- Revert gnawex:transactions from pg

BEGIN;

REVOKE INSERT ON TABLE app.transactions FROM gnawex_merchant;
REVOKE SELECT ON TABLE app.transactions FROM api;

DROP TABLE app.transactions;

COMMIT;
