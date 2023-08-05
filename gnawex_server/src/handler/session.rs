use askama::Template;
use axum::{
    extract::State,
    http::HeaderName,
    response::{Html, Redirect},
    Form,
};
use axum_extra::extract::cookie::{Cookie, PrivateCookieJar};
use axum_macros::debug_handler;
use gnawex_core::session;
use gnawex_html::app::LoginPage;
use hyper::{header, HeaderMap, StatusCode};
use serde::Deserialize;
use time::{Duration, OffsetDateTime};

use crate::{extract::context::Context, AppState};

#[derive(Debug, Deserialize)]
pub struct Credentials {
    pub username: String,
    pub password: String,
}

#[debug_handler]
pub async fn show(
    State(_state): State<AppState>,
    context: Context,
) -> (StatusCode, [(HeaderName, String); 1], Html<String>) {
    tracing::info!("GET /login");

    match context {
        Context::Authenticated { .. } => (
            StatusCode::TEMPORARY_REDIRECT,
            [(header::LOCATION, "/items".into())],
            Html("Redirecting...".into()),
        ),
        Context::Guest => {
            let html = LoginPage {
                current_user: None,
                bad_credentials: false,
            }
            .render();

            (
                StatusCode::OK,
                [(header::LOCATION, "/items".into())],
                Html(html.unwrap()),
            )
        }
    }
}

#[debug_handler]
pub async fn new(
    State(state): State<AppState>,
    context: Context,
    jar: PrivateCookieJar,
    Form(credentials): Form<Credentials>,
) -> impl axum::response::IntoResponse {
    tracing::debug!("POST /login -> {:#?}", credentials);

    match context {
        Context::Authenticated { .. } => (jar, Redirect::to("/items")),
        Context::Guest => {
            let token = session::new(
                &state.0.db_handle,
                credentials.username,
                credentials.password,
            )
            .await;

            match token {
                Ok(token) => {
                    let session_cookie = Cookie::build("session", token.0)
                        .http_only(true)
                        .expires(OffsetDateTime::now_utc() + Duration::days(30))
                        .finish();

                    (jar.add(session_cookie), Redirect::to("/items"))
                }
                Err(_) => (jar, Redirect::to("/login")),
            }
        }
    }
}
