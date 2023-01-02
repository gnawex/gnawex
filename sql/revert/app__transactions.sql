-- Revert gnawex:transactions from pg

BEGIN;

REVOKE INSERT ON TABLE app.tradable_item_transactions FROM gnawex_merchant;
REVOKE SELECT ON TABLE app.tradable_item_transactions FROM api;
DROP TABLE app.tradable_item_transactions;
DROP TYPE app.TRANSACTION_STATUS;

COMMIT;
