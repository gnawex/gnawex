#![warn(unsafe_code, clippy::all, clippy::pedantic, clippy::cargo)]

use std::{net::SocketAddr, sync::Arc};

use axum::{
    extract::FromRef,
    routing::{get, post},
    Router, Server,
};
use axum_extra::extract::cookie::Key;
use extractor::context::Context;
use gnawex_core::db;
use handler::{error, item_index, item_order_create, item_show, login};
use tower_http::services::ServeDir;

pub(crate) mod extractor;
pub(crate) mod handler;

struct AppStateKind {
    db_handle: gnawex_core::db::Handle,
    cookie_key: Key,
}

#[derive(Clone)]
pub struct AppState(Arc<AppStateKind>);

impl AppState {
    pub fn get_cookie_key_ref(&self) -> &Key {
        &self.0.cookie_key
    }

    pub fn get_db_handle_ref(&self) -> &db::Handle {
        &self.0.db_handle
    }
}

impl FromRef<AppState> for Key {
    fn from_ref(state: &AppState) -> Self {
        state.0.cookie_key.clone()
    }
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

    // TODO: Load cookie key from config
    let app_state = Arc::new(AppStateKind {
        db_handle,
        // TODO: Replace this to load from config
        cookie_key: Key::from(b"y2T-YcKjJ9WsntIGRPafygHddsoppeduokao0NZZBXPyUlouchBFNPeOScJ0q-mi-JnyunWL-YK7Uc4Djqp4sw"),
    });

    let router = Router::new()
        // HTML content type routes
        .route("/items", get(item_index::handle))
        .route("/items/:id", get(item_show::handle))
        .route("/items/:id", post(item_order_create::handle))
        .route("/login", get(login::show))
        .route("/login", post(login::new))
        .fallback(error::error_404)
        .nest_service("/assets", ServeDir::new("dist"))
        .with_state(AppState(app_state));

    Ok(router)
}

async fn signal_shutdown() {
    tokio::signal::ctrl_c()
        .await
        .expect("failed to send shutdown signal CTRL+C");

    tracing::debug!("shutting down gnawex_server");
}
