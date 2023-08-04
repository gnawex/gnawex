use axum::{
    extract::{Path, State},
    response::Redirect,
    Form,
};
use gnawex_core::{item, item_order};
use serde::Deserialize;

use crate::{extractor::context::AuthContext, AppState};

#[derive(Debug, Deserialize)]
pub(crate) struct NewItemOrder {
    batched_by: i16,
    cost: i32,
    unit_quantity: i32,
    kind: item_order::OrderType,
}

pub(crate) async fn handle(
    State(state): State<AppState>,
    context: AuthContext,
    Path(item_id): Path<item::Id>,
    Form(new_order): Form<NewItemOrder>,
) -> Redirect {
    tracing::debug!("Item ID: {:#?}", item_id);
    tracing::debug!("New order params: {:#?}", new_order);

    let _order = item_order::create(
        &state.0.db_handle,
        context.0,
        new_order.kind,
        item_id,
        new_order.batched_by,
        new_order.unit_quantity,
        new_order.cost,
    )
    .await;

    Redirect::to(format!("/items/{}", item_id.0).as_str())
}
