pub(crate) const INDEX_ITEMS: &str = "SELECT * FROM app.tradable_items ORDER BY name ASC";
pub(crate) const GET_ITEM: &str = "SELECT * FROM app.tradable_items WHERE id = $1";
