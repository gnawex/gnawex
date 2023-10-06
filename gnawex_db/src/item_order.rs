use deadpool_postgres::Transaction;
use postgres_types::{FromSql, ToSql};
use serde::{Deserialize, Serialize};
use tokio_postgres::{Error, Row};

use crate::{session, sql};

// FIXME: Qualify with schema name `app`. Blocked until this issue is resolved:
// https://github.com/sfackler/rust-postgres/issues/627
#[derive(Debug, Deserialize, FromSql, ToSql, Serialize)]
#[postgres(name = "listing_type")]
pub enum Type {
    #[postgres(name = "buy")]
    Buy,
    #[postgres(name = "sell")]
    Sell,
}

#[derive(Debug, FromSql, ToSql)]
#[postgres(transparent)]
pub struct Id(pub i64);

pub async fn get_grouped_buy_orders<'t>(
    txn: &Transaction<'t>,
    item_id: i64,
) -> Result<Vec<Row>, Error> {
    let buy_statement = txn
        .prepare(sql::item_grouped_order::LIST_GROUPED_BUY_ORDERS)
        .await?;

    txn.query(&buy_statement, &[&item_id]).await
}

pub async fn get_grouped_sell_orders<'t>(
    txn: &Transaction<'t>,
    item_id: i64,
) -> Result<Vec<Row>, Error> {
    let buy_statement = txn
        .prepare(sql::item_grouped_order::LIST_GROUPED_SELL_ORDERS)
        .await?;

    txn.query(&buy_statement, &[&item_id]).await
}

pub async fn create<'t>(
    txn: &Transaction<'t>,
    user_id: i64,
    item_id: i64,
    order_type: Type,
    unit_quantity: i32,
    cost: f32,
) -> Result<Row, Error> {
    let statement = txn.prepare(sql::item_order::CREATE_ORDER).await?;

    session::set_current_user_id(txn, user_id).await?;

    txn.query_one(
        &statement,
        &[
            // Item ID
            &item_id,
            // Order type
            &order_type,
            // Batched by quantity
            &0_i16,
            // Unit quantity
            &unit_quantity,
            // Individual cost
            &cost,
        ],
    )
    .await
}
