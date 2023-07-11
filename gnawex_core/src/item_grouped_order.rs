use crate::{db, error::ParseError, sql};
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

// TODO: Use proper error type
// TODO: Make a public version of this where it doesn't filter user ID
pub async fn get_grouped_orders_by_item_id(
    db_handle: &db::Handle,
    item_id: crate::item::Id,
) -> Result<GroupedOrders, tokio_postgres::Error> {
    // TODO: Clean up unwraps
    let mut client = db_handle.get_client().await.unwrap();
    let txn = client.transaction().await.unwrap();
    let user_statement = txn.prepare(sql::user::SET_CURRENT_USER_ID).await.unwrap();
    let buy_statement = txn
        .prepare(sql::item_grouped_order::AUTH_LIST_GROUPED_BUY_ORDERS)
        .await
        .unwrap();
    let sell_statement = txn
        .prepare(sql::item_grouped_order::AUTH_LIST_GROUPED_SELL_ORDERS)
        .await
        .unwrap();

    txn.execute(&user_statement, &[&String::from("1")])
        .await
        .unwrap();

    let buy_rows = txn.query(&buy_statement, &[&item_id]).await.unwrap();
    let sell_rows = txn.query(&sell_statement, &[&item_id]).await.unwrap();

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
