use askama::Template;
use gnawex_core::item::Item;

#[derive(Template)]
#[template(path = "item_index.html")]
pub struct ItemIndexPage {
    pub title: String,
    pub items: Vec<Item>,
    pub next_page: Option<u32>,
    pub prev_page: Option<u32>,
}
