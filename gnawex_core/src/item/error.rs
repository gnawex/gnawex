use crate::{db, error::ParseError};

/// Unable to list items
#[derive(Debug)]
pub enum ListItemsError {
    /// Failed to communicate with the database
    Db(tokio_postgres::Error),
    /// Failed to get a DB client from the pool
    GetClient(db::GetClientError),
    /// Unable to parse the DB type to `Item`
    Parse(ParseError),
}

/// Unable to create an item
#[derive(Debug)]
pub enum GetItemError {
    /// Failed to communicate with the database
    Db(tokio_postgres::Error),
    /// Failed to get a DB client from the pool
    GetClient(db::GetClientError),
    /// Unable to parse the DB type to `Item`
    Parse(ParseError),
    /// Item being looked for either does not have a match, or has multiple
    /// matches. Though as long as `app.tradable_items.id` is unique then it's
    /// the former.
    NoSingleMatch(String),
}

/// Unable to create an item
#[derive(Debug)]
pub enum CreateItemError {
    Db(tokio_postgres::Error),
    GetClient(db::GetClientError),
    Parse(ParseError),
}

impl From<db::GetClientError> for ListItemsError {
    fn from(err: db::GetClientError) -> Self {
        Self::GetClient(err)
    }
}

impl From<tokio_postgres::Error> for ListItemsError {
    fn from(err: tokio_postgres::Error) -> Self {
        Self::Db(err)
    }
}

impl From<ParseError> for ListItemsError {
    fn from(err: ParseError) -> Self {
        Self::Parse(err)
    }
}

impl From<db::GetClientError> for GetItemError {
    fn from(err: db::GetClientError) -> Self {
        Self::GetClient(err)
    }
}

impl From<tokio_postgres::Error> for GetItemError {
    fn from(err: tokio_postgres::Error) -> Self {
        // Since `tokio_postgres::ErrorInner` is private, it's not possible to
        // figure out exactly what went wrong, and I could only guess.
        match (err.as_db_error(), err.code(), err.is_closed()) {
            (None, None, false) => Self::NoSingleMatch(err.to_string()),
            _ => Self::Db(err),
        }
    }
}

impl From<ParseError> for GetItemError {
    fn from(err: ParseError) -> Self {
        Self::Parse(err)
    }
}

impl From<db::GetClientError> for CreateItemError {
    fn from(err: db::GetClientError) -> Self {
        Self::GetClient(err)
    }
}

impl From<tokio_postgres::Error> for CreateItemError {
    fn from(err: tokio_postgres::Error) -> Self {
        Self::Db(err)
    }
}

impl From<ParseError> for CreateItemError {
    fn from(err: ParseError) -> Self {
        Self::Parse(err)
    }
}
