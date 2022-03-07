--------------------------------------------------------------------------------
-- gnawex_merchant role

CREATE ROLE gnawex_merchant BYPASSRLS;

GRANT SELECT, UPDATE ON listings TO gnawex_merchant;

GRANT INSERT ON transactions TO gnawex_merchant;

GRANT anon TO gnawex_merchant;

--------------------------------------------------------------------------------
-- Procedures

--------------------------------------------------------------------------------
-- Functions

-- NOTE: `trigger_match/0` is in the same transaction as the one that called it.
-- So if ever something gets inserted, then any of this fails, it will also
-- rollback the original insert operation.
CREATE OR REPLACE FUNCTION match() RETURNS TRIGGER AS $$
BEGIN
  -- Need `gnawex_merchant` to update the involved listings because `valid_user`
  -- can't. Users are not permitted to modify the listing once created.
  -- TODO: The above epxlanation should change a little bit. It shouldn't be
  -- altered after a transaction exists. I suppose altering `is_active` is
  -- fine.
  SET LOCAL ROLE gnawex_merchant;
  -- Brace yourself for loads of CTEs. I don't know if I want it like this but
  -- hey, if it ain't broke don't fix it.
  -- Ok I know this word doesn't exist but I don't know what else to call it.
  WITH matches_cte AS (
    -- Looks for matching listing(s) based on the following:
    -- 1. Same `item_id`
    -- 2. Opposite listing type. e.g if the matchee is BUY, it'll look for SELL
    --    and vice versa.
    -- 3. Listings not created by the matchee's user
    -- 4. Same `cost`
    -- 5. If it's active (`is_active`)
    SELECT id, quantity, item_id, user_id, sum(quantity) OVER (ORDER BY id ASC) AS running_amount
      FROM listings
      WHERE item_id = NEW.item_id
        AND is_active = TRUE
        AND type = (
          CASE WHEN NEW.type = 'buy' THEN 'sell'
               ELSE 'buy'
          END
        ) :: LISTING_TYPE
        AND user_id != NEW.user_id
        AND cost = NEW.cost
  ), total_cte AS (
    -- Further filters the matches to fulfill the matchee's quantity.
    SELECT id, quantity, user_id, running_amount, sum(quantity) OVER (partition BY item_id) AS total_quantity
      FROM matches_cte
      WHERE running_amount - quantity <= NEW.quantity
  ), update_matchee AS (
    -- Updates the matchee with the new quantity after it got matched with
    -- other listing(s).
    UPDATE listings
      SET quantity = GREATEST(total_cte.running_amount - total_cte.total_quantity, 0) :: INT
      FROM total_cte
      WHERE listings.id = NEW.id
  ), update_matches AS (
   -- Updates the matches after being matched with the matchee.
    UPDATE listings
        SET quantity = (
          CASE
            -- `total_cte.total_quantity` doesn't reflect the matchee's
            -- quantity! In fact, it only represents the total quantity fetched
            -- that may (at least) fulfill the matchee. The `total_quantity`
            -- can exceed `NEW.quantity` (the matchee quantity)!
            -- 
            -- So, you have to check two things:
            --
            -- (1)
            -- `total_cte.running_amount - total_cte.total_quantity = 0`
            -- Because you need to make sure you're at the last matched listing.
            -- It _always_ zeroes out at the end of the listing.
            --
            -- (2)
            -- `total_cte.total_quantity > NEW.quantity`
            -- Since you have to check if the last matching listing should have
            -- a new quantity of 0. If the `total_quantity` does exceed the
            -- actual quantity, then you have to subtract it. 
            -- e.g total_quantity = 5, quantity = 3. The last match's quantity
            -- is going to be 2.
            --
            -- When either of these conditions fail, the idea is to zero out
            -- the match's quantity.
            WHEN
              total_cte.running_amount - total_cte.total_quantity = 0 AND
                total_cte.total_quantity > NEW.quantity
            THEN total_cte.total_quantity - NEW.quantity
            ELSE
              GREATEST(total_cte.running_amount - total_cte.total_quantity, 0) :: INT
          END
        )
        FROM total_cte
        WHERE listings.id = total_cte.id
  )
  INSERT INTO transactions (buy_order, sell_order, buyer_id, seller_id)
    (
      SELECT
        CASE WHEN NEW.type = 'buy' THEN NEW.id ELSE total_cte.id
        END AS buy_order,
        CASE WHEN NEW.type = 'buy' THEN total_cte.id ELSE NEW.id
        END AS sell_order,
        CASE WHEN NEW.type = 'buy' THEN NEW.user_id ELSE total_cte.user_id
        END AS buyer_id,
        CASE WHEN NEW.type = 'buy' THEN total_cte.user_id ELSE NEW.user_id
        END AS seller_id
        FROM total_cte
    );
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

