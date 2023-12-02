use chrono::{DateTime, Utc};
use serde::Deserialize;
use thiserror::Error;
use tokio_postgres::Row;

use crate::context::AuthContext;
use crate::error::ParseError;
use crate::{db, item};

// ----------------------------------------------------------------------------
// Types

/// Represents an item order
#[derive(Debug)]
pub struct ItemOrder {
    pub id: Id,
    // TODO: Replace with `UserId` when `User` module is created
    pub user_id: i64,
    pub item_id: item::Id,
    pub unit_quantity: i32,
    pub current_unit_quantity: i32,
    pub cost: f32,
    pub active: bool,
    pub created_at: DateTime<Utc>,
    pub updated_at: Option<DateTime<Utc>>,
}

#[derive(Debug, Deserialize)]
pub enum OrderType {
    Buy,
    Sell,
}

// ID of a order
#[derive(Debug, PartialEq)]
pub struct Id(pub i64);

pub struct AdjustQuantityError;
pub struct DelistError;

#[derive(Debug, Deserialize)]
pub struct CreateOrder {
    pub item_id: item::Id,
    pub batched_by: i16,
    pub unit_quantity: i32,
    pub cost: i32,
}

/// Possible failure scenarios when trying to create a order.
#[derive(Debug, Error)]
pub enum CreateItemOrderError {
    /// Something bad happened while communicating with the database.
    #[error("failed to communicate with the DB")]
    Db(#[from] tokio_postgres::Error),
    /// Failed to get a DB connection
    #[error("failed to get a Postgres client")]
    GetClient(#[from] db::GetClientError),
    /// Failed to parse the row into a `Buy`/`Sell` but the DB request succeeded.
    #[error("failed to parse the row into an `ItemOrder`")]
    Parse(#[from] ParseError),
}

// ----------------------------------------------------------------------------
// Trait impls

impl From<gnawex_db::item_order::Id> for Id {
    fn from(value: gnawex_db::item_order::Id) -> Self {
        Id(value.0)
    }
}

impl From<gnawex_db::item_order::Type> for OrderType {
    fn from(value: gnawex_db::item_order::Type) -> Self {
        match value {
            gnawex_db::item_order::Type::Buy => OrderType::Buy,
            gnawex_db::item_order::Type::Sell => OrderType::Sell,
        }
    }
}

impl From<OrderType> for gnawex_db::item_order::Type {
    fn from(value: OrderType) -> Self {
        match value {
            OrderType::Buy => gnawex_db::item_order::Type::Buy,
            OrderType::Sell => gnawex_db::item_order::Type::Sell,
        }
    }
}

impl TryFrom<Row> for ItemOrder {
    type Error = ParseError;

    fn try_from(value: Row) -> Result<Self, Self::Error> {
        let order_type: OrderType = value
            .try_get::<_, gnawex_db::item_order::Type>("type")
            .map(OrderType::from)
            .map_err(|e| ParseError {
                table: "app.tradable_item_orders".into(),
                field: "type".into(),
                cause: e.to_string(),
            })?;

        match order_type {
            OrderType::Buy => {
                let id: Id = value
                    .try_get::<_, gnawex_db::item_order::Id>("id")
                    .map(Id::from)
                    .map_err(|e| ParseError {
                        table: "app.tradable_item_orders".into(),
                        field: "id".into(),
                        cause: e.to_string(),
                    })?;

                let user_id: i64 = value.try_get("user__id").map_err(|e| ParseError {
                    table: "app.tradable_item_orders".into(),
                    field: "user__id".into(),
                    cause: e.to_string(),
                })?;

                let item_id: item::Id =
                    value.try_get("tradable_item__id").map_err(|e| ParseError {
                        table: "app.tradable_item_orders".into(),
                        field: "tradable_item__id".into(),
                        cause: e.to_string(),
                    })?;

                let unit_quantity: i32 =
                    value.try_get("unit_quantity").map_err(|e| ParseError {
                        table: "app.tradable_item_orders".into(),
                        field: "unit_quantity".into(),
                        cause: e.to_string(),
                    })?;

                let current_unit_quantity: i32 =
                    value
                        .try_get("current_unit_quantity")
                        .map_err(|e| ParseError {
                            table: "app.tradable_item_orders".into(),
                            field: "current_unit_quantity".into(),
                            cause: e.to_string(),
                        })?;

                let cost: f32 = value.try_get("cost").map_err(|e| ParseError {
                    table: "app.tradable_item_orders".into(),
                    field: "cost".into(),
                    cause: e.to_string(),
                })?;

                let active: bool = value.try_get("active").map_err(|e| ParseError {
                    table: "app.tradable_item_orders".into(),
                    field: "active".into(),
                    cause: e.to_string(),
                })?;

                let created_at: DateTime<Utc> =
                    value.try_get("created_at").map_err(|e| ParseError {
                        table: "app.tradable_item_orders".into(),
                        field: "created_at".into(),
                        cause: e.to_string(),
                    })?;

                let updated_at: Option<DateTime<Utc>> =
                    value.try_get("updated_at").map_err(|e| ParseError {
                        table: "app.tradable_item_orders".into(),
                        field: "updated_at".into(),
                        cause: e.to_string(),
                    })?;

                Ok(Self {
                    id,
                    user_id,
                    item_id,
                    unit_quantity,
                    current_unit_quantity,
                    cost,
                    active,
                    created_at,
                    updated_at,
                })
            }

            OrderType::Sell => Err(ParseError {
                table: "app.tradable_item_orders".into(),
                field: "type".into(),
                cause: "Unable to parse as buy order when row has field `type` with value `sell`"
                    .into(),
            }),
        }
    }
}

// ----------------------------------------------------------------------------
// Associated functions

// ----------------------------------------------------------------------------
// Functions

pub async fn create(
    db_handle: &db::Handle,
    context: AuthContext,
    order_type: OrderType,
    item_id: item::Id,
    unit_quantity: i32,
    cost: f32,
) -> Result<ItemOrder, CreateItemOrderError> {
    let mut client = db_handle.get_client().await?;
    let txn = client.transaction().await?;

    // TODO: Introduce new error variants for failing to authenticate
    gnawex_db::session::set_session_token(&txn, context.session_token).await?;
    gnawex_db::session::authenticate(&txn).await?;

    let row = gnawex_db::item_order::create(
        &txn,
        context.current_user.id.0,
        item_id.0,
        order_type.into(),
        unit_quantity,
        cost,
    )
    .await?;

    txn.commit().await?;

    let item_order = ItemOrder::try_from(row)?;

    Ok(item_order)
}
