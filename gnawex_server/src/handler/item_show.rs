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

use crate::{ArcAppState, AuthContext};

pub async fn handle(
    State(state): State<ArcAppState>,
    _context: AuthContext,
    Path(id): Path<item::Id>,
) -> Html<String> {
    let mut client = state.0.db_handle.get_client().await.unwrap();
    let txn = client.transaction().await.unwrap();

    let html = match get_item(&txn, id).await {
        Ok(item) => match filter_grouped_orders_by_item_id(&txn, id).await {
            Ok(grouped_orders) => {
                txn.commit().await.unwrap();
                ItemShowPage {
                    item,
                    grouped_buy_orders: grouped_orders.buy_orders,
                    grouped_sell_orders: grouped_orders.sell_orders,
                    current_user: None,
                }
                .render()
            }
            Err(FilterByItemIdError::NotFound) => Error404Page { current_user: None }.render(),
            Err(_) => Error500Page { current_user: None }.render(),
        },
        Err(GetItemError::NotFound) => Error404Page { current_user: None }.render(),
        Err(_) => Error500Page { current_user: None }.render(),
    };

    Html(html.unwrap())
}
