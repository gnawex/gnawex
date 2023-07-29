use crate::{db, error::ParseError, sql};
use deadpool_postgres::Transaction;
use thiserror::Error;
use tokio_postgres::Row;

#[derive(Debug)]
pub struct GroupedOrder {
    pub batches: i64,
    pub batchedby: i16,
    pub cost: i32,
}

#[derive(Debug)]
pub struct GroupedOrders {
    pub buy_orders: Vec<GroupedOrder>,
    pub sell_orders: Vec<GroupedOrder>,
}

#[derive(Debug, Error)]
pub enum FilterByItemIdError {
    /// Failed to communicate with the database
    #[error("Failed to communicate with the database. Reason: {0}")]
    Db(#[from] tokio_postgres::Error),
    /// Failed to get a DB client from the pool
    #[error("Failed to get a DB client from the pool. Reason: {0}")]
    GetClient(#[from] db::GetClientError),
    /// Unable to parse the DB type to `Item`
    #[error("Unable to parse the DB row to an Item {0}")]
    Parse(#[from] ParseError),
    /// Item being looked for either does not have a match, or has multiple
    /// matches. Though as long as `app.tradable_items.id` is unique then it's
    /// the former.
    #[error("Couldn't group orders because item doesn't exist")]
    NotFound,
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

// TODO: Make a public version of this where it doesn't filter user ID
pub async fn filter_grouped_orders_by_item_id<'t>(
    txn: &Transaction<'t>,
    item_id: crate::item::Id,
) -> Result<GroupedOrders, FilterByItemIdError> {
    // TODO: Clean up unwraps
    let user_statement = txn.prepare(sql::user::SET_CURRENT_USER_ID).await?;

    let buy_statement = txn
        .prepare(sql::item_grouped_order::AUTH_LIST_GROUPED_BUY_ORDERS)
        .await?;

    let sell_statement = txn
        .prepare(sql::item_grouped_order::AUTH_LIST_GROUPED_SELL_ORDERS)
        .await?;

    txn.execute(&user_statement, &[&String::from("1")]).await?;

    let buy_rows = txn.query(&buy_statement, &[&item_id]).await?;
    let sell_rows = txn.query(&sell_statement, &[&item_id]).await?;

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
        buy_orders: buy_orders?,
        sell_orders: sell_orders?,
    })
}
