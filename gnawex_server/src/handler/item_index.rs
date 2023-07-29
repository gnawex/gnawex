use askama::Template;
use axum::{extract::State, response::Html};
use axum_macros::debug_handler;
use gnawex_html::{app::ItemIndexPage, error::Error500Page};

use crate::ArcAppState;

#[debug_handler]
pub async fn handle(State(state): State<ArcAppState>) -> Html<String> {
    let template = match gnawex_core::item::list_items(&state.0.db_handle).await {
        Ok(items) => ItemIndexPage {
            items,
            // TODO: Pagination
            next_page: Some(3),
            prev_page: Some(1),
            current_user: None,
        }
        .render(),
        Err(_err) => Error500Page { current_user: None }.render(),
    };

    Html(template.unwrap())
}
