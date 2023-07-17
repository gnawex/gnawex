use std::sync::Arc;

use axum::{
    extract::{Path, State},
    Form,
};
use gnawex_core::{
    item,
    item_order::{self, Buy, CreateListing, ItemOrder, Sell},
};
use gnawex_html::app::ItemShowPage;
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
) -> ItemShowPage {
    println!("{:#?}", item_id);
    println!("{:#?}", new_order);

    // TODO: Refactor to have these in the same txn (create & match, get grouped orders)
    // TODO: Refactor to extract user ID from session instead of it being hardcoded
    // TODO: Remove unwraps
    // TODO: Render error page when any of the operations fail.

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

            println!("{:#?}", buy);
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

            println!("{:#?}", sell);
        }
    };

    let item = gnawex_core::item::get_item(&state.db_handle, item_id)
        .await
        .unwrap();

    let grouped_orders =
        gnawex_core::item_grouped_order::get_grouped_orders_by_item_id(&state.db_handle, item_id)
            .await
            .unwrap();

    gnawex_html::app::ItemShowPage {
        item,
        grouped_buy_orders: grouped_orders.buy_orders,
        grouped_sell_orders: grouped_orders.sell_orders,
    }
}
