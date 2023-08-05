use askama::Template;
use axum::{extract::State, response::Html};
use axum_macros::debug_handler;
use gnawex_html::{app::ItemIndexPage, error::Error500Page};

use crate::{extract::context::Context, AppState};

#[debug_handler]
pub async fn handle(State(state): State<AppState>, context: Context) -> Html<String> {
    let current_user = match context {
        Context::Authenticated { current_user, .. } => Some(current_user),
        Context::Guest => None,
    };

    let template = match gnawex_core::item::list_items(&state.0.db_handle).await {
        Ok(items) => ItemIndexPage {
            items,
            current_user,
        }
        .render(),
        Err(_err) => Error500Page { current_user: None }.render(),
    };

    Html(template.unwrap())
}
