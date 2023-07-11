pub(crate) const AUTH_LIST_GROUPED_BUY_ORDERS: &str = "
    SELECT sum(current_unit_quantity)
         , tradable_item__id
         , batched_by
         , cost
      FROM app.tradable_item_listings
      WHERE tradable_item__id = $1
        AND user__id <> (current_setting('auth.user_id', true)::BIGINT)
        AND type = 'buy'
        AND active = true
      GROUP BY tradable_item__id
             , batched_by
             , cost
      ORDER BY cost DESC
";

pub(crate) const AUTH_LIST_GROUPED_SELL_ORDERS: &str = "
    SELECT sum(current_unit_quantity)
         , tradable_item__id
         , batched_by
         , cost
      FROM app.tradable_item_listings
      WHERE tradable_item__id = $1
        AND user__id <> (current_setting('auth.user_id', true)::BIGINT)
        AND type = 'sell'
        AND active = true
      GROUP BY tradable_item__id
             , batched_by
             , cost
      ORDER BY cost ASC
";

pub(crate) const LIST_GROUPED_BUY_ORDERS: &str = "
    SELECT sum(current_unit_quantity)
         , tradable_item__id
         , batched_by
         , cost
      FROM app.tradable_item_listings
      WHERE tradable_item__id = $1
        AND type = 'buy'
        AND active = true
      GROUP BY tradable_item__id
             , batched_by
             , cost
      ORDER BY cost DESC
";

pub(crate) const LIST_GROUPED_SELL_ORDERS: &str = "
    SELECT sum(current_unit_quantity)
         , tradable_item__id
         , batched_by
         , cost
      FROM app.tradable_item_listings
      WHERE tradable_item__id = $1
        AND type = 'sell'
        AND active = true
      GROUP BY tradable_item__id
             , batched_by
             , cost
      ORDER BY cost ASC
";
