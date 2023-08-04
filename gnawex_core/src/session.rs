use thiserror::Error;

use crate::{db, error::ParseError, user::User};

pub struct Token(pub String);

#[derive(Debug, Error)]
pub enum NewSessionError {
    #[error("failed to communicate with the DB")]
    Db(#[from] tokio_postgres::Error),
    #[error("failed to get a Postgres client")]
    GetClient(#[from] db::GetClientError),
}

#[derive(Debug, Error)]
pub enum AuthenticateError {
    #[error("failed to communicate with the DB")]
    Db(#[from] tokio_postgres::Error),
    #[error("failed to get a Postgres client")]
    GetClient(#[from] db::GetClientError),
    #[error("invalid session token")]
    InvalidSession,
}

#[derive(Debug, Error)]
pub enum GetSessionUserError {
    #[error("failed to communicate with the DB")]
    Db(#[from] tokio_postgres::Error),
    #[error("failed to get a Postgres client")]
    GetClient(#[from] db::GetClientError),
    #[error("invalid session token")]
    InvalidSession,
    #[error("failed to parse row into `User`")]
    Parse(#[from] ParseError),
}

pub async fn new(
    db_handle: &db::Handle,
    username: String,
    password: String,
) -> Result<Token, NewSessionError> {
    let mut client = db_handle.get_client().await?;
    let txn = client.transaction().await?;
    let token = gnawex_db::session::new(&txn, username, password).await?;

    txn.commit().await?;

    Ok(Token(token))
}

pub async fn get_session_user(
    db_handle: &db::Handle,
    session_token: Token,
) -> Result<User, GetSessionUserError> {
    let mut client = db_handle.get_client().await?;
    let txn = client.transaction().await?;

    gnawex_db::session::set_session_token(&txn, session_token.0).await?;
    gnawex_db::session::authenticate(&txn).await?;
    let row = gnawex_db::session::get_current_user(&txn).await?;

    txn.commit().await?;

    let user: User = row.get("current_user");

    Ok(user)
}
