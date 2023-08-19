use crate::{db, error::ParseError};
use deadpool_postgres::Transaction;
use thiserror::Error;
use tokio_postgres::Row;

#[derive(Debug)]
pub struct GroupedOrder {
    pub batches: i64,
    pub batchedby: i16,
    pub cost: i32,
    pub cost_per_unit: f32,
}

#[derive(Debug, Error)]
pub enum GetGroupedOrdersError {
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
            batches,
            batchedby,
            cost,
            cost_per_unit: cost as f32 / batchedby as f32,
        })
    }
}

pub async fn get_grouped_buy_orders<'t>(
    txn: &Transaction<'t>,
    item_id: crate::item::Id,
) -> Result<Vec<GroupedOrder>, GetGroupedOrdersError> {
    let rows = gnawex_db::item_order::get_grouped_buy_orders(txn, item_id.0).await?;

    let grouped_orders = rows
        .into_iter()
        .map(GroupedOrder::try_from)
        .collect::<Result<Vec<GroupedOrder>, ParseError>>()?;

    Ok(grouped_orders)
}

pub async fn get_grouped_sell_orders<'t>(
    txn: &Transaction<'t>,
    item_id: crate::item::Id,
) -> Result<Vec<GroupedOrder>, GetGroupedOrdersError> {
    let rows = gnawex_db::item_order::get_grouped_sell_orders(txn, item_id.0).await?;

    let grouped_orders = rows
        .into_iter()
        .map(GroupedOrder::try_from)
        .collect::<Result<Vec<GroupedOrder>, ParseError>>()?;

    Ok(grouped_orders)
}
