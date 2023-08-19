use tracing::Level;
use tracing_subscriber::FmtSubscriber;

#[tokio::main]
async fn main() {
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::DEBUG)
        .finish();

    tracing::subscriber::set_global_default(subscriber).unwrap();

    // TODO: Error handling
    let foo = gnawex_server::run().await;

    println!("{:#?}", foo);

    // let hello = HelloTemplate {
    //     name: "Hey".to_string(),
    //     title: "GNAWEX".to_string(),
    // };

    // println!("{}", hello.render().unwrap());

    // // Others

    // let db_handle = db::Handle::new(
    //     "127.0.0.1".to_string(),
    //     "gnawex_development".to_string(),
    //     5432,
    //     "gnawex".to_string(),
    //     Some("gnawex".to_string()),
    //     None,
    // )
    // .unwrap();

    // let buy = listing::create_and_match::<listing::Buy>(
    //     &db_handle,
    //     CreateListing {
    //         item_id: item::Id(1),
    //         user_id: 2,
    //         batched_by: 1,
    //         unit_quantity: 5,
    //         cost: 200,
    //     },
    // )
    // .await
    // .unwrap();

    // tracing::info!("buy: {:?}", buy);
}
