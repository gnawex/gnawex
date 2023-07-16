use axum::{
    extract::{Path, State},
    routing::{get, post},
    Form, Router, Server,
};
use gnawex_core::{
    item,
    item_order::{self, Buy, CreateListing, ItemOrder, Sell},
};
use gnawex_html::{
    app::{ItemIndexPage, ItemShowPage},
    error::Error404Page,
};
use hyper::Uri;
use serde::Deserialize;
use std::{net::SocketAddr, sync::Arc};
use tower_http::services::ServeDir;

// TODO: Move handlers into their own files

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
        // HTML content type routes
        .route("/items", get(item_index))
        .route("/items/:id", get(item_show))
        .route("/items/:id", post(item_order_create))
        .fallback(error_404)
        .nest_service("/assets", ServeDir::new("assets"))
        .with_state(app_state)
}

async fn error_404(uri: Uri) -> Error404Page {
    println!("{:#?}", uri);

    Error404Page {}
}

#[derive(Debug, Deserialize)]
struct NewItemOrder {
    batched_by: i16,
    cost: i32,
    unit_quantity: i32,
    kind: item_order::Type,
}

async fn item_order_create(
    State(state): State<Arc<AppState>>,
    Path(item_id): Path<item::Id>,
    Form(new_order): Form<NewItemOrder>,
) -> ItemShowPage {
    println!("{:#?}", item_id);
    println!("{:#?}", new_order);

    // TODO: Refactor to have these in the same txn (create & match, get grouped orders)
    // TODO: Refactor to extract user ID from session instead of it being hardcoded
    // TODO: Remove unwraps
    // TODO: Render error page when any of the operations fail.

    match new_order.kind {
        item_order::Type::Buy => {
            let buy = Buy::create_and_match(
                &state.db_handle,
                CreateListing {
                    item_id,
                    user_id: 2,
                    batched_by: new_order.batched_by,
                    unit_quantity: new_order.unit_quantity,
                    cost: new_order.cost,
                },
            )
            .await;

            println!("{:#?}", buy);
        }
        item_order::Type::Sell => {
            let sell = Sell::create_and_match(
                &state.db_handle,
                CreateListing {
                    item_id,
                    user_id: 2,
                    batched_by: new_order.batched_by,
                    unit_quantity: new_order.unit_quantity,
                    cost: new_order.cost,
                },
            )
            .await;

            println!("{:#?}", sell);
        }
    };

    let item = gnawex_core::item::get_item(&state.db_handle, item_id)
        .await
        .unwrap();

    let grouped_orders =
        gnawex_core::item_grouped_order::get_grouped_orders_by_item_id(&state.db_handle, item_id)
            .await
            .unwrap();

    gnawex_html::app::ItemShowPage {
        item,
        grouped_buy_orders: grouped_orders.buy_orders,
        grouped_sell_orders: grouped_orders.sell_orders,
    }
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
    // TODO: Handle errors by rendering error page
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
