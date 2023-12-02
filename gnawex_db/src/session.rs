use chrono::{DateTime, Utc};
use deadpool_postgres::Transaction;
use time::OffsetDateTime;
use tokio_postgres::{Error, Row};

use crate::sql;

/// Sets the current user ID in the local DB session
pub async fn set_current_user_id<'t>(
    txn: &deadpool_postgres::Transaction<'t>,
    user_id: i64,
) -> Result<(), tokio_postgres::Error> {
    let statement = txn
        .prepare_typed(
            sql::session::SET_CURRENT_USER_ID,
            &[postgres_types::Type::INT8],
        )
        .await?;

    txn.execute(&statement, &[&user_id]).await?;

    Ok(())
}

pub async fn set_session_token<'t>(txn: &Transaction<'t>, token: String) -> Result<(), Error> {
    tracing::debug!("Setting session token: {}", token);
    let statement = txn.prepare(sql::session::SET_SESSION_TOKEN).await?;
    txn.execute(&statement, &[&token]).await?;

    Ok(())
}

pub async fn authenticate<'t>(txn: &Transaction<'t>) -> Result<(), Error> {
    let anon_stmt = txn.prepare(sql::session::SET_ANON_ROLE).await?;
    let auth_stmt = txn.prepare_typed(sql::session::AUTHENTICATE, &[]).await?;

    txn.execute(&anon_stmt, &[]).await?;
    txn.execute(&auth_stmt, &[]).await?;

    Ok(())
}

/// Create a new user session
pub async fn new<'t>(
    txn: &Transaction<'t>,
    username: String,
    password: String,
) -> Result<(String, OffsetDateTime), Error> {
    let _auth_stmt = txn.prepare(sql::session::SET_ANON_ROLE).await?;
    let login_statement = txn.prepare(sql::session::LOGIN).await?;
    let row = txn
        .query_one(&login_statement, &[&username, &password])
        .await?;
    let token = row.get("token");
    let expires_on = row.get("expires_on");

    Ok((token, expires_on))
}

pub async fn get_current_user<'t>(txn: &Transaction<'t>) -> Result<Row, Error> {
    let statement = txn.prepare(sql::session::GET_CURRENT_USER).await?;
    let row = txn.query_one(&statement, &[]).await?;

    Ok(row)
}

pub async fn refresh<'t>(txn: &Transaction<'t>) -> Result<(), Error> {
    let statement = txn.prepare(sql::session::REFRESH_SESSION).await?;
    txn.query_one(&statement, &[]).await?;

    Ok(())
}
