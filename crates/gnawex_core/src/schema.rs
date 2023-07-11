// @generated automatically by Diesel CLI.

pub mod sql_types {
    #[derive(diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "citext"))]
    pub struct Citext;

    #[derive(diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "item_order_transaction_status"))]
    pub struct ItemOrderTransactionStatus;

    #[derive(diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "order_kind"))]
    pub struct OrderKind;

    #[derive(diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "user_role"))]
    pub struct UserRole;
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::ItemOrderTransactionStatus;

    item_order_transactions (id) {
        id -> Uuid,
        buy_item_order_id -> Nullable<Int8>,
        sell_item_order_id -> Nullable<Int8>,
        quantity -> Int4,
        status -> ItemOrderTransactionStatus,
        buyer_id -> Nullable<Int8>,
        seller_id -> Nullable<Int8>,
        created_at -> Timestamptz,
        updated_at -> Nullable<Timestamptz>,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::OrderKind;

    item_orders (id) {
        id -> Int8,
        item_id -> Nullable<Int8>,
        user_id -> Nullable<Int8>,
        kind -> OrderKind,
        batched_by -> Int2,
        unit_quantity -> Int4,
        current_unit_quantity -> Int4,
        cost -> Int4,
        created_at -> Timestamptz,
        updated_at -> Nullable<Timestamptz>,
        deactivated_at -> Nullable<Timestamptz>,
    }
}

diesel::table! {
    items (id) {
        id -> Int8,
        name -> Text,
        wiki_link -> Text,
        description -> Text,
        created_at -> Timestamptz,
        updated_at -> Nullable<Timestamptz>,
        deleted_at -> Nullable<Timestamptz>,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::Citext;
    use super::sql_types::UserRole;

    users (id) {
        id -> Int8,
        hunter_id -> Int8,
        username -> Citext,
        password -> Text,
        role -> UserRole,
    }
}

diesel::joinable!(item_orders -> items (item_id));
diesel::joinable!(item_orders -> users (user_id));

diesel::allow_tables_to_appear_in_same_query!(
    item_order_transactions,
    item_orders,
    items,
    users,
);
