use askama::Template;
use axum::{
    extract::{Path, State},
    response::{Html, Redirect},
    Form,
};
use gnawex_core::{
    item::{self, error::GetItemError, get_item},
    item_grouped_order::{filter_grouped_orders_by_item_id, FilterByItemIdError},
    item_order::{self, Buy, CreateListing, ItemOrder, Sell},
    user,
};
use gnawex_html::{
    app::ItemShowPage,
    error::{Error404Page, Error500Page},
};
use serde::Deserialize;

use crate::{ArcAppState, AuthContext};

#[derive(Debug, Deserialize)]
pub(crate) struct NewItemOrder {
    batched_by: i16,
    cost: i32,
    unit_quantity: i32,
    kind: item_order::Type,
}

pub(crate) async fn handle(
    State(state): State<ArcAppState>,
    context: AuthContext,
    Path(item_id): Path<item::Id>,
    Form(new_order): Form<NewItemOrder>,
) -> Redirect {
    tracing::debug!("Item ID: {:#?}", item_id);
    tracing::debug!("New order params: {:#?}", new_order);

    let mut client = state.0.db_handle.get_client().await.unwrap();
    let txn = client.transaction().await.unwrap();

    let _ = user::set_session_token(&txn, context.session_token)
        .await
        .unwrap();
    let _ = user::authenticate(&txn).await.unwrap();

    match new_order.kind {
        item_order::Type::Buy => {
            let buy = Buy::create_and_match(
                &txn,
                CreateListing {
                    item_id,
                    batched_by: new_order.batched_by,
                    unit_quantity: new_order.unit_quantity,
                    cost: new_order.cost,
                },
            )
            .await;

            tracing::debug!("Buy: {:#?}", buy);
        }
        item_order::Type::Sell => {
            let sell = Sell::create_and_match(
                &txn,
                CreateListing {
                    item_id,
                    batched_by: new_order.batched_by,
                    unit_quantity: new_order.unit_quantity,
                    cost: new_order.cost,
                },
            )
            .await;

            tracing::debug!("Sell: {:#?}", sell);
        }
    };

    txn.commit().await.unwrap();

    Redirect::to(format!("/items/{}", item_id.0).as_str())
}
