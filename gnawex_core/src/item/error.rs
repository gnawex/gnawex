use thiserror::Error;

use crate::{db, error::ParseError};

/// Unable to list items
#[derive(Debug, Error)]
pub enum ListItemsError {
    /// Failed to communicate with the database
    #[error("Failed to communicate with the DB: {0}")]
    Db(#[from] tokio_postgres::Error),
    /// Failed to get a DB client from the pool
    #[error("Failed to acquire a Postgres client from the pool: {0}")]
    GetClient(#[from] db::GetClientError),
    /// Unable to parse the DB type to `Item`
    #[error("Failed to parse row to an Item: {0}")]
    Parse(#[from] ParseError),
}

/// Unable to create an item
#[derive(Debug, Error)]
pub enum GetItemError {
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
    #[error("Item does not exist")]
    NotFound,
}

/// Unable to create an item
#[derive(Debug, Error)]
pub enum CreateItemError {
    #[error("Failed to communicate with the database. Reason: {0}")]
    Db(#[from] tokio_postgres::Error),
    #[error("Failed to get a DB client from the pool. Reason: {0}")]
    GetClient(#[from] db::GetClientError),
    #[error("Unable to parse the DB row to an Item {0}")]
    Parse(#[from] ParseError),
}
