use axum::{
    extract::{Path, State},
    routing::get,
    Router, Server,
};
use gnawex_html::app::{ItemIndexPage, ItemShowPage};
use std::{net::SocketAddr, sync::Arc};
use tower_http::services::ServeDir;

struct AppState {
    db_handle: gnawex_core::db::Handle,
}

/// Runs the GNAWEX server on port 3000
pub async fn run() -> Result<(), hyper::Error> {
    let app = app();
    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));

    tracing::debug!("listening on {}", addr);

    Server::bind(&addr)
        .serve(app.into_make_service())
        .with_graceful_shutdown(signal_shutdown())
        .await
}

fn app() -> Router {
    // TODO: Error handling
    let db_handle = gnawex_core::db::Handle::new(
        "127.0.0.1".to_string(),
        "gnawex_development".to_string(),
        5432,
        "gnawex".to_string(),
        Some("gnawex".to_string()),
        None,
    )
    .unwrap();

    let app_state = Arc::new(AppState { db_handle });

    Router::new()
        .route("/items", get(item_index))
        .route("/items/:id", get(item_show))
        .nest_service("/assets", ServeDir::new("assets"))
        .with_state(app_state)
}

async fn item_index(State(state): State<Arc<AppState>>) -> ItemIndexPage {
    // TODO: Error handling. Render an error page instead
    tracing::info!("hey");
    let items = gnawex_core::item::list_items(&state.db_handle)
        .await
        .unwrap();

    tracing::info!("{:#?}", items);

    gnawex_html::app::ItemIndexPage {
        items,
        next_page: Some(3),
        prev_page: Some(1),
    }
}

async fn item_show(Path(id): Path<i64>, State(state): State<Arc<AppState>>) -> ItemShowPage {
    let item = gnawex_core::item::get_item(&state.db_handle, gnawex_core::item::Id(id))
        .await
        .unwrap();

    let grouped_orders = gnawex_core::item_grouped_order::get_grouped_orders_by_item_id(
        &state.db_handle,
        gnawex_core::item::Id(id),
    )
    .await
    .unwrap();

    gnawex_html::app::ItemShowPage {
        item,
        grouped_buy_orders: grouped_orders.buy_orders,
        grouped_sell_orders: grouped_orders.sell_orders,
    }
}

async fn signal_shutdown() {
    tokio::signal::ctrl_c()
        .await
        .expect("failed to send shutdown signal CTRL+C");

    tracing::debug!("shutting down gnawex_server");
}
