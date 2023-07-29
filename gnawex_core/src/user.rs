use deadpool_postgres::Transaction;

use crate::sql;

pub struct User {
    pub id: Id,
    pub username: String,
}

pub struct SessionToken(String);

pub struct Id(pub i64);

/// Sets the current user in the local DB session
pub async fn set_current_user<'t>(
    txn: &deadpool_postgres::Transaction<'t>,
    user_id: Id,
) -> Result<u64, tokio_postgres::Error> {
    let statement = txn
        .prepare_typed(
            sql::user::SET_CURRENT_USER_ID,
            &[postgres_types::Type::TEXT],
        )
        .await?;

    txn.execute(&statement, &[&user_id.0.to_string()]).await
}

pub async fn set_session_token<'t>(
    txn: &Transaction<'t>,
    token: String,
) -> Result<(), tokio_postgres::Error> {
    tracing::info!("Token: {}", token);
    let statement = txn.prepare(sql::user::SET_SESSION_TOKEN).await.unwrap();
    txn.execute(&statement, &[&token]).await.unwrap();

    Ok(())
}

pub async fn authenticate<'t>(txn: &Transaction<'t>) -> Result<(), tokio_postgres::Error> {
    let anon_stmt = txn.prepare(sql::user::SET_ANON_ROLE).await?;
    let auth_stmt = txn.prepare_typed(sql::user::AUTHENTICATE, &[]).await?;

    txn.execute(&anon_stmt, &[]).await?;
    txn.execute(&auth_stmt, &[]).await?;

    Ok(())
}

pub async fn login<'t>(
    db_handle: &crate::db::Handle,
    username: String,
    password: String,
) -> Result<String, tokio_postgres::Error> {
    let mut client = db_handle.get_client().await.unwrap();
    let txn = client.transaction().await?;
    let auth_stmt = txn.prepare(sql::user::SET_ANON_ROLE).await?;
    let statement = txn.prepare(sql::user::LOGIN).await?;
    let row = txn.query_one(&statement, &[&username, &password]).await?;
    let token = row.get("login");

    txn.commit().await?;

    tracing::info!("Token: {:?}", token);

    Ok(token)
}

pub async fn get_current_user<'t>(txn: &Transaction<'t>) -> Result<User, ()> {
    let statement = txn.prepare(sql::user::GET_CURRENT_USER).await.unwrap();
    let row = txn.query_one(&statement, &[]).await.unwrap();

    tracing::info!("Test: {:#?}", row.get::<_, i64>("id"));

    todo!()
}
