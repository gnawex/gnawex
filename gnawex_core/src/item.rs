use std::fmt::Display;

use deadpool_postgres::Transaction;
use postgres_types::{FromSql, ToSql};
use serde::Deserialize;
use tokio_postgres::Row;

use crate::{db, error::ParseError, item_grouped_order::GroupedOrder};

use self::error::{CreateItemError, GetItemDetailsError, GetItemError, ListItemsError};

pub mod error;

#[derive(Debug, PartialEq)]
pub struct Item {
    pub id: Id,
    pub name: String,
    pub description: String,
    pub wiki_link: String,
}

#[derive(Clone, Copy, Debug, Deserialize, FromSql, ToSql, PartialEq)]
#[postgres(transparent)]
pub struct Id(pub i64);

impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl TryFrom<Row> for Item {
    type Error = ParseError;

    /// Parses a Postgres row into an `Item`
    fn try_from(value: Row) -> Result<Self, Self::Error> {
        let id: Id = value.try_get("id").map_err(|e| ParseError {
            table: "app.tradable_items".into(),
            field: "id".into(),
            cause: e.to_string(),
        })?;

        let name: String = value.try_get("name").map_err(|e| ParseError {
            table: "app.tradable_items".into(),
            field: "name".into(),
            cause: e.to_string(),
        })?;

        let description: String = value.try_get("description").map_err(|e| ParseError {
            table: "app.tradable_items".into(),
            field: "description".into(),
            cause: e.to_string(),
        })?;

        let wiki_link: String = value.try_get("wiki_link").map_err(|e| ParseError {
            table: "app.tradable_items".into(),
            field: "wiki_link".into(),
            cause: e.to_string(),
        })?;

        Ok(Self {
            id,
            name,
            description,
            wiki_link,
        })
    }
}

/// Lists all tradable items
pub async fn list_items(db_handle: &db::Handle) -> Result<Vec<Item>, ListItemsError> {
    let mut client = db_handle.get_client().await?;
    let txn = client.transaction().await?;

    let rows = gnawex_db::item::list_items(&txn)
        .await
        .map_err(ListItemsError::from)?;

    rows.into_iter()
        .map(Item::try_from)
        .collect::<Result<Vec<_>, _>>()
        .map_err(ListItemsError::from)
}

pub async fn get<'t>(txn: &Transaction<'t>, item_id: Id) -> Result<Item, GetItemError> {
    let row = gnawex_db::item::get(txn, item_id.0).await?;
    Item::try_from(row).map_err(GetItemError::from)
}

pub async fn get_item_details(
    db_handle: &db::Handle,
    item_id: Id,
) -> Result<(Item, Vec<GroupedOrder>, Vec<GroupedOrder>), GetItemDetailsError> {
    let mut client = db_handle.get_client().await?;
    let txn = client.transaction().await?;
    let item_row = gnawex_db::item::get(&txn, item_id.0).await?;
    let grouped_buy_rows = gnawex_db::item_order::get_grouped_buy_orders(&txn, item_id.0).await?;
    let grouped_sell_rows = gnawex_db::item_order::get_grouped_sell_orders(&txn, item_id.0).await?;
    let item = Item::try_from(item_row).map_err(GetItemDetailsError::from)?;

    let grouped_buys = grouped_buy_rows
        .into_iter()
        .map(GroupedOrder::try_from)
        .collect::<Result<Vec<GroupedOrder>, ParseError>>()?;

    let grouped_sells = grouped_sell_rows
        .into_iter()
        .map(GroupedOrder::try_from)
        .collect::<Result<Vec<GroupedOrder>, ParseError>>()?;

    txn.commit().await?;

    Ok((item, grouped_buys, grouped_sells))
}

pub async fn create(
    db_handle: &db::Handle,
    name: String,
    description: String,
    wiki_link: String,
) -> Result<Item, CreateItemError> {
    let client = db_handle.get_client().await?;
    let item = client
        .query_one(
            "
            INSERT INTO app.tradable_items
                ( name
                , description
                , wiki_link
                )
                VALUES ($1, $2, $3)
                RETURNING *
            ",
            &[&name, &description, &wiki_link],
        )
        .await
        .map_err(CreateItemError::from)?;

    Item::try_from(item).map_err(CreateItemError::from)
}
