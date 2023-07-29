use axum::{extract::State, response::Redirect, Form};
use axum_extra::extract::cookie::{Cookie, PrivateCookieJar, SameSite};
use axum_macros::debug_handler;
use gnawex_core::user;
use gnawex_html::app::LoginPage;
use serde::Deserialize;
use time::{Duration, OffsetDateTime};

use crate::ArcAppState;

#[derive(Debug, Deserialize)]
pub struct Credentials {
    pub username: String,
    pub password: String,
}

pub async fn show() -> LoginPage {
    tracing::info!("GET /login");
    LoginPage {
        current_user: None,
        bad_credentials: false,
    }
}

#[debug_handler]
pub async fn new(
    State(state): State<ArcAppState>,
    cookie_jar: PrivateCookieJar,
    Form(credentials): Form<Credentials>,
) -> impl axum::response::IntoResponse {
    tracing::info!("POST /login -> {:#?}", credentials);
    let token = user::login(
        &state.0.db_handle,
        credentials.username,
        credentials.password,
    )
    .await;

    match token {
        Ok(token) => {
            let session_cookie = Cookie::build("session", token)
                .http_only(true)
                .expires(OffsetDateTime::now_utc() + Duration::days(30))
                .finish();

            (cookie_jar.add(session_cookie), Redirect::to("/items"))
        }
        Err(_) => (cookie_jar, Redirect::to("/login")),
    }
}
