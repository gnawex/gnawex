use gnawex_core::{db, item};

#[tokio::main]
async fn main() {
    let subscriber = tracing_subscriber::FmtSubscriber::new();

    tracing::subscriber::set_global_default(subscriber).unwrap();

    let db_handle = db::Handle::new(
        "127.0.0.1".to_string(),
        "gnawex_development".to_string(),
        5432,
        "gnawex".to_string(),
        Some("gnawex".to_string()),
        None,
    )
    .unwrap();

    let buy = item::listing::create_buy(&db_handle).await.unwrap();
    let sell = item::listing::create_sell(&db_handle).await.unwrap();

    tracing::info!("buy: {:?}", buy);
    tracing::info!("sell: {:?}", sell);
}
