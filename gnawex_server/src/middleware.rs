use axum::http::Request;
use axum::response::{IntoResponse, Response};
use axum::{extract::State, middleware::Next};
use axum_extra::extract::PrivateCookieJar;
use gnawex_core::session::{self, Token};

use crate::extract::context::Context;
use crate::AppState;

pub async fn refresh_session<B>(
    State(state): State<AppState>,
    context: Context,
    request: Request<B>,
    next: Next<B>,
) -> Response {
    tracing::debug!("REFRESHING SESSION!!!!!!!!");
    tracing::debug!("Headers: {:#?}", request.headers());

    match context {
        Context::Authenticated { session_token, .. } => {
            let refresh = session::refresh(&state.0.db_handle, session_token).await;

            tracing::debug!("Refresh: {:#?}", refresh);
        }
        Context::Guest => tracing::debug!("Nothing to refresh"),
    };

    let response = next.run(request).await;

    response
}
