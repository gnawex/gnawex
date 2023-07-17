use std::{net::SocketAddr, sync::Arc};

use axum::{
    routing::{get, post},
    Router, Server,
};
use handler::{error, item_index, item_order_create, item_show};
use tower_http::services::ServeDir;

pub(crate) mod handler;

pub struct AppState {
    db_handle: gnawex_core::db::Handle,
}

/// Runs the GNAWEX server on port 3000
pub async fn run() -> anyhow::Result<()> {
    let app = mk_app()?;
    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));

    tracing::debug!("listening on {}", addr);

    Server::bind(&addr)
        .serve(app.into_make_service())
        .with_graceful_shutdown(signal_shutdown())
        .await?;

    Ok(())
}

fn mk_app() -> anyhow::Result<Router> {
    // TODO: Error handling
    let db_handle = gnawex_core::db::Handle::new(
        "127.0.0.1".to_string(),
        "gnawex_development".to_string(),
        5432,
        "gnawex".to_string(),
        Some("gnawex".to_string()),
        None,
    )?;

    let app_state = Arc::new(AppState { db_handle });

    let router = Router::new()
        // HTML content type routes
        .route("/items", get(item_index::handle))
        .route("/items/:id", get(item_show::handle))
        .route("/items/:id", post(item_order_create::handle))
        .fallback(error::error_404)
        .nest_service("/assets", ServeDir::new("assets"))
        .with_state(app_state);

    Ok(router)
}

async fn signal_shutdown() {
    tokio::signal::ctrl_c()
        .await
        .expect("failed to send shutdown signal CTRL+C");

    tracing::debug!("shutting down gnawex_server");
}
