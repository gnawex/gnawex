use std::sync::Arc;

use askama::Template;
use axum::{
    extract::{Path, State},
    response::Html,
};
use gnawex_core::item::error::GetItemError;
use gnawex_html::{app::ItemShowPage, error::Error500Page};

use crate::AppState;

pub async fn handle(Path(id): Path<i64>, State(state): State<Arc<AppState>>) -> Html<String> {
    // TODO: Handle errors by rendering error page
    let item = gnawex_core::item::get_item(&state.db_handle, gnawex_core::item::Id(id)).await;

    let grouped_orders = gnawex_core::item_grouped_order::get_grouped_orders_by_item_id(
        &state.db_handle,
        gnawex_core::item::Id(id),
    )
    .await;

    // TODO: Have grouped orders only run if item exists
    // Maybe it's better to create an `IntoResponse` instance for
    // (item, grouped_orders). That way rendering templates doesn't need to be
    // reimplemented in each handler that needs it.

    let template = match (item, grouped_orders) {
        (Ok(item), Ok(grouped_orders)) => ItemShowPage {
            item,
            grouped_buy_orders: grouped_orders.buy_orders,
            grouped_sell_orders: grouped_orders.sell_orders,
        }
        .render(),
        (Err(GetItemError::NotFound), _) => gnawex_html::error::Error404Page.render(),
        (_, _) => Error500Page.render(),
    }
    .expect("not a valid template");

    Html(template)
}
