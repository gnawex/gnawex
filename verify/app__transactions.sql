-- Verify gnawex:transactions on pg

BEGIN;

SELECT
    transaction_id,
    buy_order,
    sell_order,
    quantity,
    buyer_id,
    seller_id,
    created_at
  FROM app.transactions
  WHERE false;

ROLLBACK;
