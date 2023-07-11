use askama::Template;
use gnawex_core::item::Item;
use gnawex_core::item_grouped_order::GroupedOrder;

#[derive(Template)]
#[template(path = "item_index.html")]
pub struct ItemIndexPage {
    pub items: Vec<Item>,
    pub next_page: Option<u32>,
    pub prev_page: Option<u32>,
}

#[derive(Template)]
#[template(path = "item_show.html")]
pub struct ItemShowPage {
    pub item: Item,
    pub grouped_buy_orders: Vec<GroupedOrder>,
    pub grouped_sell_orders: Vec<GroupedOrder>,
}
