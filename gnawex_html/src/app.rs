use askama::Template;
use gnawex_core::item::Item;
use gnawex_core::item_grouped_order::GroupedOrder;
use gnawex_core::user::User;

#[derive(Template)]
#[template(path = "item_index.html")]
pub struct ItemIndexPage {
    pub current_user: Option<User>,
    pub items: Vec<Item>,
    pub next_page: Option<u32>,
    pub prev_page: Option<u32>,
}

#[derive(Template)]
#[template(path = "item_show.html")]
pub struct ItemShowPage {
    pub current_user: Option<User>,
    pub item: Item,
    pub grouped_buy_orders: Vec<GroupedOrder>,
    pub grouped_sell_orders: Vec<GroupedOrder>,
}

#[derive(Template)]
#[template(path = "login.html")]
pub struct LoginPage {
    pub current_user: Option<User>,
    pub bad_credentials: bool,
}
