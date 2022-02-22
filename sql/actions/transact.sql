CREATE OR REPLACE PROCEDURE transact(buy_order_id BIGINT, sell_order_id BIGINT)
AS $$
DECLARE
  BUY_AFFECTED_ROWS  integer;
  SELL_AFFECTED_ROWS integer;
  BUY_QUANTITY       integer;
  SELL_QUANTITY      integer;
BEGIN
  -- Creates a transaction involving two listings; a buy and sell listing.
  INSERT INTO transactions (buy_order, sell_order) VALUES (3, 1);

  -- Get the sell order's quantity
  SELECT COALESCE(SUM(quantity), 0) AS quantity
  FROM listings
  WHERE listings.id = sell_order_id
  AND   listings.type = 'sell'
  INTO SELL_QUANTITY;

  -- Get the buy order's quantity
  SELECT COALESCE(SUM(quantity), 0) AS quantity
  FROM listings
  WHERE listings.id = buy_order_id
  AND listings.type = 'buy'
  INTO BUY_QUANTITY;

  -- Reduces the quantity of a listing by 1. This ensures that
  -- something was actually reduced cause otherwise, the transaction
  -- just rolls back. It's considered an invalid transaction.
  WITH affected_buy_cte AS (
      UPDATE listings
      SET quantity = GREATEST(quantity - SELL_QUANTITY, 0)
      WHERE id = buy_order_id 
      AND   listings.type = 'buy'
      AND   listings.quantity > 0
      RETURNING 1
  )
  SELECT COUNT(*) FROM affected_buy_cte INTO BUY_AFFECTED_ROWS;

  IF BUY_AFFECTED_ROWS = 0 THEN
    RAISE EXCEPTION 'I was not able to update the BUY order. This transaction is not valid.';
  END IF;

  -- This is pretty much the same as the previous query except for the
  -- sell order listing.
  WITH affected_sell_cte AS (
      UPDATE listings
      SET quantity = GREATEST(quantity - BUY_QUANTITY, 0)
      WHERE id = sell_order_id AND listings.type = 'sell'
      RETURNING 1
  )
  SELECT count(*) FROM affected_sell_cte INTO SELL_AFFECTED_ROWS;

  IF SELL_AFFECTED_ROWS = 0 THEN
    RAISE EXCEPTION 'I was not able to update the SELL order. This transaction is not valid.';
  END IF;
END;
$$
LANGUAGE plpgsql;

START TRANSACTION;

-- Create a transaction between a BUY order (ID of 3),
-- and a SELL order (ID of 1).
CALL transact(CAST('3' AS BIGINT), CAST('1' AS BIGINT));

COMMIT;
