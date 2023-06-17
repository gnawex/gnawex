use chrono::{DateTime, Utc};
use deadpool_postgres::Transaction;
use postgres_types as pg;
use postgres_types::{FromSql, ToSql};
use serde::Serialize;
use tokio_postgres::Row;

use crate::error::ParseError;
use crate::{db, item};

#[derive(Debug, FromSql, ToSql, PartialEq)]
pub struct Id(pub i64);

/// Represents a buy listing
#[derive(Debug)]
pub struct Buy {
    id: Id,
    // TODO: Replace with `UserId` when `User` module is created
    user_id: i64,
    item_id: item::Id,
    batched_by: i16,
    unit_quantity: i32,
    current_unit_quantity: i32,
    cost: i32,
    active: bool,
    created_at: DateTime<Utc>,
    updated_at: Option<DateTime<Utc>>,
}

impl Buy {
    /// Attempts to match a buy listing with an equivalent sell listing. If it
    /// finds no matches, then it proceeds as normal.
    async fn match_listing(self, mut _txn: Transaction<'_>) -> Result<Buy, tokio_postgres::Error> {
        todo!()
    }
}

impl TryFrom<Row> for Buy {
    type Error = ParseError;

    fn try_from(value: Row) -> Result<Self, Self::Error> {
        let listing_type: Type = value.try_get("type").map_err(|e| ParseError {
            table: "app.tradable_item_listings".into(),
            field: "type".into(),
            cause: e.to_string(),
        })?;

        match listing_type {
            Type::Buy => {
                let id: Id = value.try_get("id").map_err(|e| ParseError {
                    table: "app.tradable_item_listings".into(),
                    field: "id".into(),
                    cause: e.to_string(),
                })?;

                let user_id: i64 = value.try_get("user__id").map_err(|e| ParseError {
                    table: "app.tradable_item_listings".into(),
                    field: "user__id".into(),
                    cause: e.to_string(),
                })?;

                let item_id: item::Id =
                    value.try_get("tradable_item__id").map_err(|e| ParseError {
                        table: "app.tradable_item_listings".into(),
                        field: "tradable_item__id".into(),
                        cause: e.to_string(),
                    })?;

                let batched_by: i16 = value.try_get("batched_by").map_err(|e| ParseError {
                    table: "app.tradable_item_listings".into(),
                    field: "batched_by".into(),
                    cause: e.to_string(),
                })?;

                let unit_quantity: i32 =
                    value.try_get("unit_quantity").map_err(|e| ParseError {
                        table: "app.tradable_item_listings".into(),
                        field: "unit_quantity".into(),
                        cause: e.to_string(),
                    })?;

                let current_unit_quantity: i32 =
                    value
                        .try_get("current_unit_quantity")
                        .map_err(|e| ParseError {
                            table: "app.tradable_item_listings".into(),
                            field: "current_unit_quantity".into(),
                            cause: e.to_string(),
                        })?;

                let cost: i32 = value.try_get("cost").map_err(|e| ParseError {
                    table: "app.tradable_item_listings".into(),
                    field: "cost".into(),
                    cause: e.to_string(),
                })?;

                let active: bool = value.try_get("active").map_err(|e| ParseError {
                    table: "app.tradable_item_listings".into(),
                    field: "active".into(),
                    cause: e.to_string(),
                })?;

                let created_at: DateTime<Utc> =
                    value.try_get("created_at").map_err(|e| ParseError {
                        table: "app.tradable_item_listings".into(),
                        field: "created_at".into(),
                        cause: e.to_string(),
                    })?;

                let updated_at: Option<DateTime<Utc>> =
                    value.try_get("updated_at").map_err(|e| ParseError {
                        table: "app.tradable_item_listings".into(),
                        field: "updated_at".into(),
                        cause: e.to_string(),
                    })?;

                Ok(Self {
                    id,
                    user_id,
                    item_id,
                    batched_by,
                    unit_quantity,
                    current_unit_quantity,
                    cost,
                    active,
                    created_at,
                    updated_at,
                })
            }

            Type::Sell => Err(ParseError {
                table: "app.tradable_item_listings".into(),
                field: "type".into(),
                cause: "Unable to parse as buy listing when row has field `type` with value `sell`"
                    .into(),
            }),
        }
    }
}

#[derive(Debug)]
pub struct Sell {
    id: Id,
    // TODO: Replace with `UserId` when `User` module is created
    user_id: i64,
    item_id: item::Id,
    batched_by: i16,
    unit_quantity: i32,
    current_unit_quantity: i32,
    cost: i32,
    active: bool,
    created_at: DateTime<Utc>,
    updated_at: Option<DateTime<Utc>>,
}

impl TryFrom<Row> for Sell {
    type Error = ParseError;

    fn try_from(value: Row) -> Result<Self, Self::Error> {
        let listing_type: Type = value.try_get("type").map_err(|e| ParseError {
            table: "app.tradable_item_listings".into(),
            field: "type".into(),
            cause: e.to_string(),
        })?;

        match listing_type {
            Type::Sell => {
                let id: Id = value.try_get("id").map_err(|e| ParseError {
                    table: "app.tradable_item_listings".into(),
                    field: "id".into(),
                    cause: e.to_string(),
                })?;

                let user_id: i64 = value.try_get("user__id").map_err(|e| ParseError {
                    table: "app.tradable_item_listings".into(),
                    field: "user__id".into(),
                    cause: e.to_string(),
                })?;

                let item_id: item::Id =
                    value.try_get("tradable_item__id").map_err(|e| ParseError {
                        table: "app.tradable_item_listings".into(),
                        field: "tradable_item__id".into(),
                        cause: e.to_string(),
                    })?;

                let created_at: DateTime<Utc> =
                    value.try_get("created_at").map_err(|e| ParseError {
                        table: "app.tradable_item_listings".into(),
                        field: "created_at".into(),
                        cause: e.to_string(),
                    })?;

                let updated_at: Option<DateTime<Utc>> =
                    value.try_get("updated_at").map_err(|e| ParseError {
                        table: "app.tradable_item_listings".into(),
                        field: "updated_at".into(),
                        cause: e.to_string(),
                    })?;

                let batched_by: i16 = value.try_get("batched_by").map_err(|e| ParseError {
                    table: "app.tradable_item_listings".into(),
                    field: "batched_by".into(),
                    cause: e.to_string(),
                })?;

                let unit_quantity: i32 =
                    value.try_get("unit_quantity").map_err(|e| ParseError {
                        table: "app.tradable_item_listings".into(),
                        field: "unit_quantity".into(),
                        cause: e.to_string(),
                    })?;

                let current_unit_quantity: i32 =
                    value
                        .try_get("current_unit_quantity")
                        .map_err(|e| ParseError {
                            table: "app.tradable_item_listings".into(),
                            field: "current_unit_quantity".into(),
                            cause: e.to_string(),
                        })?;

                let cost: i32 = value.try_get("cost").map_err(|e| ParseError {
                    table: "app.tradable_item_listings".into(),
                    field: "cost".into(),
                    cause: e.to_string(),
                })?;

                let active: bool = value.try_get("active").map_err(|e| ParseError {
                    table: "app.tradable_item_listings".into(),
                    field: "active".into(),
                    cause: e.to_string(),
                })?;

                let created_at: DateTime<Utc> =
                    value.try_get("created_at").map_err(|e| ParseError {
                        table: "app.tradable_item_listings".into(),
                        field: "created_at".into(),
                        cause: e.to_string(),
                    })?;

                let updated_at: Option<DateTime<Utc>> =
                    value.try_get("updated_at").map_err(|e| ParseError {
                        table: "app.tradable_item_listings".into(),
                        field: "updated_at".into(),
                        cause: e.to_string(),
                    })?;

                Ok(Self {
                    id,
                    user_id,
                    item_id,
                    batched_by,
                    unit_quantity,
                    current_unit_quantity,
                    cost,
                    active,
                    created_at,
                    updated_at,
                })
            }

            Type::Buy => Err(ParseError {
                table: "app.tradable_item_listings".into(),
                field: "type".into(),
                cause: "Unable to parse as sell listing when row has field `type` with value `buy`"
                    .into(),
            }),
        }
    }
}

/// Possible failure scenarios when trying to create a listing.
#[derive(Debug)]
pub enum CreateListingError {
    /// Something bad happened while communicating with the database.
    Db(tokio_postgres::Error),
    /// Failed to get a DB connection
    GetClient(db::GetClientError),
    /// Failed to parse the row into a `Buy`/`Sell` but the DB request succeeded.
    Parse(ParseError),
}

impl From<tokio_postgres::Error> for CreateListingError {
    fn from(err: tokio_postgres::Error) -> Self {
        CreateListingError::Db(err)
    }
}

impl From<db::GetClientError> for CreateListingError {
    fn from(err: db::GetClientError) -> Self {
        CreateListingError::GetClient(err)
    }
}

impl From<ParseError> for CreateListingError {
    fn from(err: ParseError) -> Self {
        CreateListingError::Parse(err)
    }
}

// FIXME: Qualify with schema name `app`. Blocked until this issue is resolved:
// https://github.com/sfackler/rust-postgres/issues/627
#[derive(Debug, FromSql, ToSql, Serialize)]
#[postgres(name = "listing_type")]
enum Type {
    #[postgres(name = "buy")]
    Buy,
    #[postgres(name = "sell")]
    Sell,
}

/// Creates a buy listing, and attempts to match it with an equivalent sell
/// listing automatically.
pub async fn create_buy(db_handle: &db::Handle) -> Result<Buy, CreateListingError> {
    let mut client = db_handle.get_client().await?;
    let txn = client.transaction().await?;
    let buy_row = do_create(&txn, Type::Buy).await?;
    let buy = Buy::try_from(buy_row)?;

    txn.commit().await?;

    Ok(buy)
}

pub async fn create_sell(db_handle: &db::Handle) -> Result<Sell, CreateListingError> {
    let mut client = db_handle.get_client().await?;
    let txn = client.transaction().await?;
    let sell_row = do_create(&txn, Type::Sell).await?;
    let sell = Sell::try_from(sell_row)?;

    txn.commit().await?;

    Ok(sell)
}

/// It's a general version of creating a listing.
async fn do_create(
    txn: &Transaction<'_>,
    listing_type: Type,
) -> Result<Row, tokio_postgres::Error> {
    let create_query = "
        INSERT INTO app.tradable_item_listings
            ( tradable_item__id
            , user__id
            , type
            , batched_by
            , unit_quantity
            , current_unit_quantity
            , cost
            , active
            )
            VALUES ($1, $2, $3, $4, $5, $5, $6, true)
            RETURNING *
        ";

    set_current_user(&txn).await?;

    let statement = txn.prepare(create_query).await?;

    txn.query_one(
        &statement,
        &[&1_i64, &2_i64, &listing_type, &1_i16, &5_i32, &200_i32],
    )
    .await
}

// TODO: Move this to user module
async fn set_current_user(txn: &Transaction<'_>) -> Result<u64, tokio_postgres::Error> {
    let query = "SELECT set_config('auth.user_id', $1, true)";
    let statement = txn.prepare_typed(query, &[pg::Type::TEXT]).await.unwrap();

    txn.execute(&statement, &[&2_i64.to_string()]).await
}
