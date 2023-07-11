/// Create a new buy/sell listing
pub(crate) const CREATE_LISTING: &str = "
    INSERT INTO app.tradable_item_listings
        ( tradable_item__id
        , user__id
        , type
        , batched_by
        , unit_quantity
        , current_unit_quantity
        , cost
        , active
        )
        VALUES ($1, $2, $3, $4, $5, $5, $6, true)
        RETURNING *
";

pub(crate) const MATCH_LISTING: &str = "
    SELECT *
         , sum(current_unit_quantity)
             OVER (ORDER BY created_at ASC)
             AS running_current_unit_quantity
      FROM app.tradable_item_listings
      WHERE tradable_item__id = $1
        AND user__id <> $2
        AND type = $3
        AND batched_by = $4
        AND cost = $5
        AND active = true
";
