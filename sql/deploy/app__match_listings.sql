-- Deploy gnawex:match_listing to pg
-- requires: listings
-- requires: transactions

BEGIN;

-- NOTE: `trigger_match/0` is in the same transaction as the one that called it.
-- So if ever something gets inserted, then any of this fails, it will also
-- rollback the original insert operation.
CREATE OR REPLACE FUNCTION app.match() RETURNS TRIGGER AS $$
BEGIN
  -- Need `gnawex_merchant` to update the involved listings because `valid_user`
  -- can't. Users are not permitted to modify the listing once created.
  -- TODO: The above epxlanation should change a little bit. It shouldn't be
  -- altered after a transaction exists. I suppose altering `active` is
  -- fine.
  SET LOCAL ROLE gnawex_merchant;

  -- Brace yourself for loads of CTEs. I don't know if I want it like this but
  -- hey, if it ain't broke don't fix it.
  -- Ok I know this word doesn't exist but I don't know what else to call it.
  WITH running_orders_cte AS (
    -- Looks for matching listing(s) based on the following:
    -- 1. Same `tradable_item__id`
    -- 2. Opposite listing type. e.g if the matchee is BUY, it'll look for SELL
    --    and vice versa.
    -- 3. Listings not created by the matchee's user
    -- 4. Same `cost`
    -- 5. If it's active (`active`)
    -- 6. Batch size
    SELECT
        id,
        current_unit_quantity,
        batched_by,
        tradable_item__id,
        user__id,
        sum(current_unit_quantity) OVER (ORDER BY id ASC) AS running_amount
      FROM app.tradable_item_listings
      WHERE tradable_item__id = NEW.tradable_item__id
        AND active = true
        AND batched_by = NEW.batched_by
        AND type = (
          CASE WHEN NEW.type = 'buy' THEN 'sell'
               ELSE 'buy'
          END
        ) :: app.LISTING_TYPE
        AND user__id <> NEW.user__id
        AND cost = NEW.cost
  ), matches_cte AS (
    -- Further filters the matches to fulfill the matchee's current_unit_quantity.
    SELECT id
         , current_unit_quantity
         , user__id
         , running_amount
         , sum(current_unit_quantity)
             OVER (PARTITION BY tradable_item__id) AS total_unit_quantity
      FROM running_orders_cte
      WHERE running_amount - current_unit_quantity <= NEW.current_unit_quantity
  ), update_matchee AS (
    -- Updates the matchee with the new current_unit_quantity after it got matched with
    -- other listing(s).
    UPDATE app.tradable_item_listings
      SET current_unit_quantity =
        greatest( NEW.current_unit_quantity - matches_cte.total_unit_quantity
                , 0
                ) :: INT
      FROM matches_cte
      WHERE tradable_item_listings.id = NEW.id
  ), update_matches AS (
   -- Updates the matches after being matched with the matchee.
    UPDATE app.tradable_item_listings
        SET current_unit_quantity = (
          CASE
            -- `matches_cte.total_unit_quantity` doesn't reflect the matchee's
            -- unit_quantity! In fact, it only represents the total unit_quantity fetched
            -- that may (at least) fulfill the matchee. The `total_unit_quantity`
            -- can exceed `NEW.unit_quantity` (the matchee unit_quantity)!
            --
            -- So, you have to check two things:
            --
            -- (1)
            -- `matches_cte.running_amount - matches_cte.total_unit_quantity = 0`
            -- Because you need to make sure you're at the last matched listing.
            -- It _always_ zeroes out at the end of the listing.
            --
            -- (2)
            -- `matches_cte.total_unit_quantity > NEW.unit_quantity`
            -- Since you have to check if the last matching listing should have
            -- a new unit_quantity of 0. If the `total_unit_quantity` does exceed the
            -- actual unit_quantity, then you have to subtract it.
            -- e.g total_unit_quantity = 5, unit_quantity = 3. The last match's
            -- unit_quantity is going to be 2.
            --
            -- When either of these conditions fail, the idea is to zero out
            -- the match's unit_quantity.
            WHEN
              matches_cte.running_amount - matches_cte.total_unit_quantity = 0 AND
                matches_cte.total_unit_quantity > NEW.current_unit_quantity
            THEN matches_cte.total_unit_quantity - NEW.current_unit_quantity
            ELSE
              greatest(matches_cte.running_amount - matches_cte.total_unit_quantity, 0) :: INT
          END
        )
        FROM matches_cte
        WHERE tradable_item_listings.id = matches_cte.id
  )
  INSERT
    INTO app.tradable_item_transactions
      ( buy_item_listing__id
      , sell_item_listing__id
      , buyer__id
      , seller__id
      , quantity
      )
    (
      SELECT
        CASE WHEN NEW.type = 'buy' THEN NEW.id ELSE matches_cte.id
        END AS buy_item_listing__id,
        CASE WHEN NEW.type = 'buy' THEN matches_cte.id ELSE NEW.id
        END AS sell_item_listing__id,
        CASE WHEN NEW.type = 'buy' THEN NEW.user__id ELSE matches_cte.user__id
        END AS buyer__id,
        CASE WHEN NEW.type = 'buy' THEN matches_cte.user__id ELSE NEW.user__id
        END AS seller__id,
        least(matches_cte.current_unit_quantity, NEW.current_unit_quantity) AS unit_quantity
        FROM matches_cte
    );

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER match_listings
  AFTER INSERT
   ON app.tradable_item_listings
   FOR EACH ROW
    EXECUTE PROCEDURE app.match();

--------------------------------------------------------------------------------

COMMIT;
