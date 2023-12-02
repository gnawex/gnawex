use thiserror::Error;
use time::OffsetDateTime;

use crate::{db, error::ParseError, user::User};

#[derive(Clone, Debug)]
pub struct Token {
    pub val: String,
    pub expires_on: OffsetDateTime,
}

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

#[derive(Debug, Error)]
pub enum RefreshSessionError {
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
    let (val, expires_on) = gnawex_db::session::new(&txn, username, password).await?;

    println!("EXPIRES?? {expires_on}");

    txn.commit().await?;

    Ok(Token { val, expires_on })
}

pub async fn get_session_user(
    db_handle: &db::Handle,
    token: String,
) -> Result<User, GetSessionUserError> {
    let mut client = db_handle.get_client().await?;
    let txn = client.transaction().await?;

    gnawex_db::session::set_session_token(&txn, token).await?;
    gnawex_db::session::authenticate(&txn).await?;

    let row = gnawex_db::session::get_current_user(&txn).await?;

    txn.commit().await?;

    let user: User = row.get("current_user");

    Ok(user)
}

pub async fn refresh(
    db_handle: &db::Handle,
    session_token: String,
) -> Result<(), RefreshSessionError> {
    let mut client = db_handle.get_client().await?;
    let txn = client.transaction().await?;

    gnawex_db::session::set_session_token(&txn, session_token).await?;
    gnawex_db::session::refresh(&txn).await?;

    txn.commit().await?;

    Ok(())
}
