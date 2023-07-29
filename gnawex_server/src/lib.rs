use std::{net::SocketAddr, sync::Arc};

use axum::{
    async_trait,
    extract::{FromRef, FromRequest, FromRequestParts, State},
    http::request::Parts,
    routing::{get, post},
    RequestExt, Router, Server,
};
use axum_extra::extract::{cookie::Key, PrivateCookieJar};
use deadpool_postgres::{Client, Transaction};
use gnawex_core::{
    db,
    user::{self, User},
};
use handler::{error, item_index, item_order_create, item_show, login};
use hyper::Request;
use tower_http::services::ServeDir;

pub(crate) mod handler;

pub struct AppState {
    db_handle: gnawex_core::db::Handle,
    cookie_key: Key,
}

#[derive(Clone)]
pub struct ArcAppState(Arc<AppState>);

pub struct AuthContext {
    current_user: (),
    session_token: String,
}

#[async_trait]
impl<'t, S> FromRequestParts<S> for AuthContext
where
    S: Send + Sync,
    Key: FromRef<S>,
    ArcAppState: FromRef<S>,
{
    type Rejection = ();

    async fn from_request_parts(parts: &mut Parts, state: &S) -> Result<Self, Self::Rejection> {
        let jar = PrivateCookieJar::<Key>::from_request_parts(parts, state)
            .await
            .unwrap();

        tracing::info!("{:#?}", jar);

        let app_state = ArcAppState::from_ref(state);
        let token = jar
            .get("session")
            .and_then(|cookie| Some(cookie.value().to_owned()))
            .unwrap();
        let mut client = app_state.0.db_handle.get_client().await.unwrap();
        let txn = client.transaction().await.unwrap();
        let _ = user::get_current_user(&txn);

        Ok(AuthContext {
            current_user: (),
            session_token: token,
        })
    }
}

impl FromRef<ArcAppState> for Key {
    fn from_ref(state: &ArcAppState) -> Self {
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
    let app_state = Arc::new(AppState {
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
        .with_state(ArcAppState(app_state));

    Ok(router)
}

async fn signal_shutdown() {
    tokio::signal::ctrl_c()
        .await
        .expect("failed to send shutdown signal CTRL+C");

    tracing::debug!("shutting down gnawex_server");
}
