use std::sync::Arc;

use askama::Template;
use axum::{
    extract::{Path, State},
    response::Html,
    Form,
};
use gnawex_core::{
    item::{self, error::GetItemError, get_item},
    item_grouped_order::{filter_grouped_orders_by_item_id, FilterByItemIdError},
    item_order::{self, Buy, CreateListing, ItemOrder, Sell},
};
use gnawex_html::{
    app::ItemShowPage,
    error::{Error404Page, Error500Page},
};
use serde::Deserialize;

use crate::AppState;

#[derive(Debug, Deserialize)]
pub(crate) struct NewItemOrder {
    batched_by: i16,
    cost: i32,
    unit_quantity: i32,
    kind: item_order::Type,
}

pub(crate) async fn handle(
    State(state): State<Arc<AppState>>,
    Path(item_id): Path<item::Id>,
    Form(new_order): Form<NewItemOrder>,
) -> Html<String> {
    tracing::debug!("{:#?}", state);
    tracing::debug!("{:#?}", item_id);
    tracing::debug!("{:#?}", new_order);

    match new_order.kind {
        item_order::Type::Buy => {
            let buy = Buy::create_and_match(
                &state.db_handle,
                CreateListing {
                    item_id,
                    user_id: 2,
                    batched_by: new_order.batched_by,
                    unit_quantity: new_order.unit_quantity,
                    cost: new_order.cost,
                },
            )
            .await;

            tracing::debug!("{:#?}", buy);
        }
        item_order::Type::Sell => {
            let sell = Sell::create_and_match(
                &state.db_handle,
                CreateListing {
                    item_id,
                    user_id: 2,
                    batched_by: new_order.batched_by,
                    unit_quantity: new_order.unit_quantity,
                    cost: new_order.cost,
                },
            )
            .await;

            tracing::debug!("{:#?}", sell);
        }
    };

    let html = match get_item(&state.db_handle, item_id).await {
        Ok(item) => match filter_grouped_orders_by_item_id(&state.db_handle, item_id).await {
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
