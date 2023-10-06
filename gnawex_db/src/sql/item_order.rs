/// Create a new buy/sell listing
pub(crate) const CREATE_ORDER: &str = "
    INSERT INTO api.tradable_item_listings
        ( tradable_item__id
        , type
        , unit_quantity
        , current_unit_quantity
        , cost
        , active
        )
        VALUES ($1, $2, $3, $3, $4, true)
        RETURNING *
";

pub(crate) const FIND_MATCHING_ORDERS: &str = "
    SELECT *
         , sum(current_unit_quantity)
             OVER (ORDER BY created_at ASC)
             AS running_current_unit_quantity
      FROM app.tradable_item_listings
      WHERE tradable_item__id = $1
        AND user__id <> $2
        AND type = $3
        AND cost = $4
        AND active = true
";
