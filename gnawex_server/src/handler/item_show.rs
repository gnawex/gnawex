use std::sync::Arc;

use askama::Template;
use axum::{
    extract::{Path, State},
    response::Html,
};
use gnawex_core::{
    item::{self, error::GetItemError, get_item},
    item_grouped_order::{filter_grouped_orders_by_item_id, FilterByItemIdError},
};
use gnawex_html::{
    app::ItemShowPage,
    error::{Error404Page, Error500Page},
};

use crate::AppState;

pub async fn handle(Path(id): Path<item::Id>, State(state): State<Arc<AppState>>) -> Html<String> {
    let html = match get_item(&state.db_handle, id).await {
        Ok(item) => match filter_grouped_orders_by_item_id(&state.db_handle, id).await {
            Ok(grouped_orders) => ItemShowPage {
                item,
                grouped_buy_orders: grouped_orders.buy_orders,
                grouped_sell_orders: grouped_orders.sell_orders,
            }
            .render(),
            Err(FilterByItemIdError::NotFound) => Error404Page.render(),
            Err(_) => Error500Page.render(),
        },
        Err(GetItemError::NotFound) => Error404Page.render(),
        Err(_) => Error500Page.render(),
    };

    Html(html.unwrap())
}
