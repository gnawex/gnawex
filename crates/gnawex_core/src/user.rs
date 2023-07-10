pub(crate) mod sql;

/// Sets the current user in the local DB session
pub(crate) async fn set_current_user(
    txn: &deadpool_postgres::Transaction<'_>,
) -> Result<u64, tokio_postgres::Error> {
    let statement = txn
        .prepare_typed(sql::SET_CURRENT_USER_ID, &[postgres_types::Type::TEXT])
        .await
        .unwrap();

    txn.execute(&statement, &[&2_i64.to_string()]).await
}