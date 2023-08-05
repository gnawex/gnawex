use deadpool_postgres::Transaction;
use tokio_postgres::{Error, Row};

use crate::sql;

pub async fn list_items<'t>(txn: &Transaction<'t>) -> Result<Vec<Row>, Error> {
    txn.query(sql::item::INDEX_ITEMS, &[]).await
}

pub async fn get<'t>(txn: &Transaction<'t>, item_id: i64) -> Result<Row, Error> {
    let statement = txn.prepare(sql::item::GET_ITEM).await?;

    txn.query_one(&statement, &[&item_id]).await
}
