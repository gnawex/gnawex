--------------------------------------------------------------------------------
-- gnawex_merchant role

CREATE ROLE gnawex_merchant BYPASSRLS;
GRANT SELECT, UPDATE ON listings TO gnawex_merchant;
GRANT INSERT ON transactions TO gnawex_merchant;
GRANT anon TO gnawex_merchant;

--------------------------------------------------------------------------------
-- Procedures

-- TODO: Add creation of transaction(s)
CREATE OR REPLACE PROCEDURE transact(matchee_id BIGINT, matchee_item_id BIGINT,
  matchee_user_id BIGINT, matchee_quantity INTEGER, matchee_type LISTING_TYPE,
  matchee_cost INTEGER) AS $$
BEGIN
  -- Need `gnawex_merchant` to update the involved listings because `valid_user`
  -- can't. Users are not permitted to modify the listing once created.
  -- TODO: The above epxlanation should change a little bit. It shouldn't be
  -- altered after a transaction exists. I suppose altering `is_active` is
  -- fine.
  SET LOCAL ROLE gnawex_merchant;

  -- Ok I know this word doesn't exist but I don't know what else to call it.
  WITH matches_cte AS (
    -- Looks for matching listing(s) based on the following:
    -- 1. Same `item_id`
    -- 2. Opposite listing type. e.g if the matchee is BUY, it'll look for SELL
    --    and vice versa.
    -- 3. Listings not created by the matchee's user
    -- 4. Same `cost`
    -- 5. If it's active (`is_active`)
    SELECT id, quantity, item_id, sum(quantity) OVER (ORDER BY id ASC) AS running_amount
      FROM listings
      WHERE item_id = matchee_item_id
        AND is_active = TRUE
        AND type = (
          CASE WHEN matchee_type = 'buy' THEN 'sell'
               ELSE 'buy'
          END
        ) :: LISTING_TYPE
        AND user_id != matchee_user_id
        AND cost = matchee_cost
  ), total_cte AS (
    -- Further filters the matches to fulfill the matchee's quantity.
    SELECT id, quantity, running_amount, sum(quantity) OVER (partition BY item_id) AS total_quantity
      FROM matches_cte
      WHERE running_amount - quantity <= matchee_quantity
  ), update_cte AS (
    UPDATE listings
      SET quantity = (
        CASE
          WHEN
            total_cte.running_amount - total_cte.total_quantity = 0 AND
              total_cte.total_quantity > matchee_quantity
          THEN total_cte.total_quantity - matchee_quantity

          ELSE
            GREATEST(total_cte.running_amount - total_cte.total_quantity, 0) :: INT

        END
      )
      FROM total_cte
      WHERE listings.id = total_cte.id
  )
  UPDATE listings
    SET quantity = GREATEST(total_cte.running_amount - total_cte.total_quantity, 0) :: INT
    FROM total_cte
    WHERE listings.id = matchee_id;
END;
$$ LANGUAGE plpgsql;

--------------------------------------------------------------------------------
-- Functions

CREATE OR REPLACE FUNCTION match() RETURNS TRIGGER AS $$
BEGIN
  CALL transact(NEW.id, NEW.item_id, NEW.user_id, NEW.quantity, NEW.type, NEW.cost);

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

--------------------------------------------------------------------------------
-- Triggers

CREATE OR REPLACE TRIGGER match_listings
  AFTER INSERT
   ON listings
   FOR EACH ROW
    EXECUTE PROCEDURE match();

--------------------------------------------------------------------------------
