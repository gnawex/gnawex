use std::sync::Arc;

use askama::Template;
use axum::{extract::State, response::Html};
use gnawex_html::{app::ItemIndexPage, error::Error500Page};

use crate::AppState;

pub async fn handle(State(state): State<Arc<AppState>>) -> Html<String> {
    let template = match gnawex_core::item::list_items(&state.db_handle).await {
        Ok(items) => ItemIndexPage {
            items,
            // TODO: Pagination
            next_page: Some(3),
            prev_page: Some(1),
        }
        .render(),
        Err(_err) => Error500Page.render(),
    };

    Html(template.unwrap())
}
