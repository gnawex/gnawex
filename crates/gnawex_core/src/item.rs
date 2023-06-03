use postgres_types::{FromSql, ToSql};
use tokio_postgres::Row;

use crate::db;

#[derive(Debug)]
pub struct Item {
    id: ItemId,
    name: String,
    description: String,
}

#[derive(Debug, FromSql, ToSql)]
pub struct ItemId(i64);

/// o
#[derive(Debug)]
pub struct ParseError {
    field: String,
    cause: String,
}

#[derive(Debug)]
pub enum Error {
    /// Failed to communicate with the database
    Db(tokio_postgres::Error),
    /// Failed to get a DB client from the pool
    GetClient(db::GetClientError),
    /// Unable to parse the DB type to `Item`
    Parse(ParseError),
}

impl From<db::GetClientError> for Error {
    fn from(err: db::GetClientError) -> Self {
        Error::GetClient(err)
    }
}

impl From<tokio_postgres::Error> for Error {
    fn from(err: tokio_postgres::Error) -> Self {
        Self::Db(err)
    }
}

impl From<ParseError> for Error {
    fn from(err: ParseError) -> Self {
        Self::Parse(err)
    }
}

impl TryFrom<Row> for Item {
    type Error = ParseError;

    /// Parses a Postgres row into an `Item`
    fn try_from(value: Row) -> Result<Self, Self::Error> {
        let id: ItemId = value.try_get("id").map_err(|e| ParseError {
            field: "id".into(),
            cause: e.to_string(),
        })?;

        let name: String = value.try_get("name").map_err(|e| ParseError {
            field: "name".into(),
            cause: e.to_string(),
        })?;

        let description: String = value.try_get("description").map_err(|e| ParseError {
            field: "description".into(),
            cause: e.to_string(),
        })?;

        Ok(Self {
            id,
            name,
            description,
        })
    }
}

/// Lists all tradable items
pub async fn list_items(db_handle: &db::Handle) -> Result<Vec<Item>, Error> {
    let client = db_handle.get_client().await?;
    let items = client
        .query("SELECT * FROM app.tradable_items", &[])
        .await
        .map_err(Error::from)?;

    items
        .into_iter()
        .map(Item::try_from)
        .collect::<Result<Vec<_>, _>>()
        .map_err(Error::from)
}
