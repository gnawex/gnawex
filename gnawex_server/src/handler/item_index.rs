use askama::Template;
use axum::{extract::State, response::Html};
use axum_extra::extract::CookieJar;
use axum_macros::debug_handler;
use gnawex_html::{app::ItemIndexPage, error::Error500Page};

use crate::{extract::context::Context, AppState};

#[debug_handler]
pub async fn handle(
    State(state): State<AppState>,
    context: Context,
    jar: CookieJar,
) -> Html<String> {
    let current_user = match context {
        Context::Authenticated { current_user, .. } => Some(current_user),
        Context::Guest => None,
    };

    let cookie = jar.get("session");

    println!("{:#?}", cookie);

    let template = match gnawex_core::item::list_items(&state.0.db_handle).await {
        Ok(items) => ItemIndexPage {
            items,
            current_user,
        }
        .render(),
        Err(err) => {
            tracing::error!("{:#?}", err);
            Error500Page { current_user: None }.render()
        }
    };

    Html(template.unwrap())
}
