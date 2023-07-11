use crate::{db, error::ParseError};
use tokio_postgres::Row;

#[derive(Debug)]
pub struct GroupedOrder {
    pub batches: i64,
    pub batchedby: i16,
    pub cost: i32,
}

pub struct GroupedOrders {
    pub buy_orders: Vec<GroupedOrder>,
    pub sell_orders: Vec<GroupedOrder>,
}

impl TryFrom<Row> for GroupedOrder {
    type Error = ParseError;

    fn try_from(value: Row) -> Result<Self, Self::Error> {
        let cost: i32 = value.try_get("cost").map_err(|e| ParseError {
            table: "app.tradable_item_listings".into(),
            field: "cost".into(),
            cause: e.to_string(),
        })?;

        let batchedby: i16 = value.try_get("batched_by").map_err(|e| ParseError {
            table: "app.tradable_item_listings".into(),
            field: "batched_by".into(),
            cause: e.to_string(),
        })?;

        let batches: i64 = value.try_get("sum").map_err(|e| ParseError {
            table: "app.tradable_item_listings".into(),
            field: "unit_quantity".into(),
            cause: e.to_string(),
        })?;

        Ok(GroupedOrder {
            cost,
            batchedby,
            batches,
        })
    }
}

const AUTH_LIST_GROUPED_BUY_ORDERS: &str = "
    SELECT sum(current_unit_quantity)
         , tradable_item__id
         , batched_by
         , cost
      FROM app.tradable_item_listings
      WHERE tradable_item__id = $1
        AND user__id <> $2
        AND type = 'buy'
        AND active = true
      GROUP BY tradable_item__id
             , batched_by
             , cost
      ORDER BY cost DESC
";

const AUTH_LIST_GROUPED_SELL_ORDERS: &str = "
    SELECT sum(current_unit_quantity)
         , tradable_item__id
         , batched_by
         , cost
      FROM app.tradable_item_listings
      WHERE tradable_item__id = $1
        AND user__id <> $2
        AND type = 'sell'
        AND active = true
      GROUP BY tradable_item__id
             , batched_by
             , cost
      ORDER BY cost ASC
";

const LIST_GROUPED_BUY_ORDERS: &str = "
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

const LIST_GROUPED_SELL_ORDERS: &str = "
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

// TODO: Use proper error type
// TODO: Make a public version of this where it doesn't filter user ID
pub async fn get_grouped_orders_by_item_id(
    db_handle: &db::Handle,
    item_id: crate::item::Id,
) -> Result<GroupedOrders, tokio_postgres::Error> {
    let mut client = db_handle.get_client().await.unwrap();
    let txn = client.transaction().await.unwrap();
    let buy_statement = txn.prepare(AUTH_LIST_GROUPED_BUY_ORDERS).await.unwrap();
    let sell_statement = txn.prepare(AUTH_LIST_GROUPED_SELL_ORDERS).await.unwrap();
    let buy_rows = txn
        .query(&buy_statement, &[&item_id, &2_i64])
        .await
        .unwrap();
    let sell_rows = txn
        .query(&sell_statement, &[&item_id, &2_i64])
        .await
        .unwrap();

    txn.commit().await?;

    let buy_orders: Result<Vec<GroupedOrder>, ParseError> = buy_rows
        .into_iter()
        .map(|r| GroupedOrder::try_from(r))
        .collect();
    let sell_orders: Result<Vec<GroupedOrder>, ParseError> = sell_rows
        .into_iter()
        .map(|r| GroupedOrder::try_from(r))
        .collect();

    tracing::info!("Buy: {:#?}", buy_orders);
    tracing::info!("Sell {:#?}", sell_orders);

    Ok(GroupedOrders {
        buy_orders: buy_orders.unwrap(),
        sell_orders: sell_orders.unwrap(),
    })
}
