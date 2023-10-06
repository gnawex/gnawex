pub(crate) const LIST_GROUPED_BUY_ORDERS: &str = "
    SELECT sum(current_unit_quantity)
         , tradable_item__id
         , cost
      FROM app.tradable_item_listings
      WHERE tradable_item__id = $1
        AND user__id <> coalesce(app.current_user_id(), 0)
        AND type = 'buy'
        AND active = true
        AND current_unit_quantity > 0
      GROUP BY tradable_item__id
             , cost
      ORDER BY cost DESC
";

pub(crate) const LIST_GROUPED_SELL_ORDERS: &str = "
    SELECT sum(current_unit_quantity)
         , tradable_item__id
         , cost
      FROM app.tradable_item_listings
      WHERE tradable_item__id = $1
        AND user__id <> coalesce(app.current_user_id(), 0)
        AND type = 'sell'
        AND active = true
        AND current_unit_quantity > 0
      GROUP BY tradable_item__id
             , cost
      ORDER BY cost ASC
";
