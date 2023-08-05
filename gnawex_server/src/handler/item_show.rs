use askama::Template;
use axum::{
    extract::{Path, State},
    response::Html,
};
use gnawex_core::item::{self, error::GetItemDetailsError};
use gnawex_html::{
    app::ItemShowPage,
    error::{Error404Page, Error500Page},
};

use crate::{AppState, Context};

pub async fn handle(
    State(state): State<AppState>,
    context: Context,
    Path(id): Path<item::Id>,
) -> Html<String> {
    let current_user = match context {
        Context::Authenticated { current_user, .. } => Some(current_user),
        Context::Guest => None,
    };

    let html = match item::get_item_details(&state.0.db_handle, id).await {
        Ok((item, grouped_buy_orders, grouped_sell_orders)) => ItemShowPage {
            item,
            grouped_buy_orders,
            grouped_sell_orders,
            current_user,
        }
        .render(),
        Err(GetItemDetailsError::NotFound) => Error404Page { current_user: None }.render(),
        Err(err) => {
            tracing::error!("{:#?}", err);
            Error500Page { current_user: None }.render()
        }
    };

    Html(html.unwrap())
}
