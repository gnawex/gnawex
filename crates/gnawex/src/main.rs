#[tokio::main]
async fn main() {
    let db_handle = gnawex_core::db::Handle::new(
        "127.0.0.1".to_string(),
        "gnawex_development".to_string(),
        5432,
        "gnawex".to_string(),
        None,
        None,
    )
    .unwrap();

    println!("{:?}", db_handle);

    println!("{:?}", gnawex_core::item::list_items(&db_handle).await);
}
