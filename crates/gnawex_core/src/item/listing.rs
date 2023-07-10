use async_trait::async_trait;
use chrono::{DateTime, Utc};
use postgres_types::{FromSql, ToSql};
use serde::Serialize;
use tokio_postgres::Row;

use crate::error::ParseError;
use crate::{db, item, user};

pub(crate) mod sql;

// ----------------------------------------------------------------------------
// Types

/// Common behaviours for item listings
#[async_trait]
pub trait Listing {
    /// Creates, and matches the created listing with a relevant listing.
    async fn create_and_match(
        db_handle: &db::Handle,
        params: CreateListing,
    ) -> Result<Self, CreateListingError>
    where
        Self: Sized;

    /// Delists an active listing. Once delisted, it will not be a part of the
    /// pool of listings that can be matched.
    fn delist(self, db_handle: &db::Handle, user_id: i64) -> Result<Self, DelistError>
    where
        Self: Sized;

    /// Reduces the current unit quantity of a listing. Once reduced to zero, it
    /// automatically closes the listing as there are no more quantities to be
    /// matched with.
    fn reduce_current_unit_quantity(
        self,
        db_handle: &db::Handle,
        new_current_quantity: i32,
    ) -> Result<Self, AdjustQuantityError>
    where
        Self: Sized;

    fn get_type() -> Type;

    /// What type of listing is needed to match with the listing.
    fn get_matching_type() -> Type;

    fn get_item_id(&self) -> item::Id;
    fn get_user_id(&self) -> i64;
    fn get_batched_by(&self) -> i16;
    fn get_cost(&self) -> i32;
}

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

// ID of a listing
#[derive(Debug, FromSql, ToSql, PartialEq)]
#[postgres(transparent)]
pub struct Id(pub i64);

pub struct AdjustQuantityError;
pub struct DelistError;

// FIXME: Qualify with schema name `app`. Blocked until this issue is resolved:
// https://github.com/sfackler/rust-postgres/issues/627
#[derive(Debug, FromSql, ToSql, Serialize)]
#[postgres(name = "listing_type")]
pub enum Type {
    #[postgres(name = "buy")]
    Buy,
    #[postgres(name = "sell")]
    Sell,
}

#[derive(Debug)]
pub struct CreateListing {
    pub item_id: item::Id,
    pub user_id: i64,
    pub batched_by: i16,
    pub unit_quantity: i32,
    pub cost: i32,
}

pub async fn create_and_match<L: Listing>(
    db_handle: &db::Handle,
    params: CreateListing,
) -> Result<L, CreateListingError> {
    L::create_and_match(db_handle, params).await
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

// ----------------------------------------------------------------------------
// Trait impls

// Buy
#[async_trait]
impl Listing for Buy {
    async fn create_and_match(
        db_handle: &crate::db::Handle,
        params: CreateListing,
    ) -> Result<Self, CreateListingError>
    where
        Self: Sized,
    {
        let mut client = db_handle.get_client().await?;
        let txn = client.transaction().await?;
        let buy = do_create::<Buy>(&txn, params).await?;

        tracing::info!("Created listing: {:#?}", buy);

        let row: Vec<Sell> = do_match(&txn, &buy).await?;

        tracing::info!("{:#?}", row);

        txn.rollback().await?;

        todo!()
    }

    fn delist(self, _db_handle: &crate::db::Handle, _user_id: i64) -> Result<Self, DelistError>
    where
        Self: Sized,
    {
        todo!()
    }

    fn reduce_current_unit_quantity(
        self,
        _db_handle: &crate::db::Handle,
        _new_current_quantity: i32,
    ) -> Result<Self, AdjustQuantityError>
    where
        Self: Sized,
    {
        todo!()
    }

    fn get_type() -> Type {
        Type::Buy
    }

    fn get_matching_type() -> Type {
        Type::Sell
    }

    fn get_cost(&self) -> i32 {
        self.cost
    }

    fn get_batched_by(&self) -> i16 {
        self.batched_by
    }

    fn get_user_id(&self) -> i64 {
        self.user_id
    }

    fn get_item_id(&self) -> item::Id {
        self.item_id
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

// Sell

#[async_trait]
impl Listing for Sell {
    async fn create_and_match(
        db_handle: &crate::db::Handle,
        params: CreateListing,
    ) -> Result<Self, CreateListingError>
    where
        Self: Sized,
    {
        let mut client = db_handle.get_client().await?;
        let txn = client.transaction().await?;
        let buy = do_create::<Buy>(&txn, params).await?;

        tracing::info!("Created listing: {:#?}", buy);

        let row: Vec<Sell> = do_match(&txn, &buy).await?;

        tracing::info!("{:#?}", row);

        txn.rollback().await?;

        todo!()
    }

    fn delist(self, db_handle: &crate::db::Handle, user_id: i64) -> Result<Self, DelistError>
    where
        Self: Sized,
    {
        todo!()
    }

    fn reduce_current_unit_quantity(
        self,
        db_handle: &crate::db::Handle,
        new_current_quantity: i32,
    ) -> Result<Self, AdjustQuantityError>
    where
        Self: Sized,
    {
        todo!()
    }

    fn get_type() -> Type {
        Type::Sell
    }

    fn get_matching_type() -> Type {
        Type::Buy
    }

    fn get_cost(&self) -> i32 {
        self.cost
    }

    fn get_batched_by(&self) -> i16 {
        self.batched_by
    }

    fn get_user_id(&self) -> i64 {
        self.user_id
    }

    fn get_item_id(&self) -> item::Id {
        self.item_id
    }
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

// CreateListingError

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

// ----------------------------------------------------------------------------
// Associated functions

// ----------------------------------------------------------------------------
// Functions

/// Attempts to match one or more listings to the given listing. If the given
/// listing cannot be fulfilled with just one available matching listing, it
/// will keep looking for listings until:
///
/// 1. There are no more available matching listings; or
/// 2. The given listing was completely fulfilled
async fn do_match<'t, CandidateListing, MatchedListing>(
    txn: &deadpool_postgres::Transaction<'t>,
    listing: &CandidateListing,
) -> Result<Vec<MatchedListing>, tokio_postgres::Error>
where
    CandidateListing: Listing + TryFrom<Row> + std::fmt::Debug,
    <CandidateListing as TryFrom<Row>>::Error: std::fmt::Debug,
    MatchedListing: Listing + TryFrom<Row> + std::fmt::Debug,
    <MatchedListing as TryFrom<Row>>::Error: std::fmt::Debug,
{
    user::set_current_user(txn).await?;
    let statement = txn.prepare(sql::MATCH_LISTING).await?;

    let rows = txn
        .query(
            &statement,
            &[
                &listing.get_item_id(),
                &listing.get_user_id(),
                &<CandidateListing as Listing>::get_matching_type(),
                &listing.get_batched_by(),
                &listing.get_cost(),
            ],
        )
        .await?;

    let listings: Result<Vec<MatchedListing>, _> = rows
        .into_iter()
        .map(|r| MatchedListing::try_from(r))
        .collect();

    tracing::info!("{:#?}", listings);

    Ok(Vec::new())
}

/// Creates an item listing
async fn do_create<'t, L>(
    txn: &deadpool_postgres::Transaction<'t>,
    params: CreateListing,
) -> Result<L, CreateListingError>
where
    L: Listing + TryFrom<Row>,
    CreateListingError: From<<L>::Error>,
{
    user::set_current_user(txn).await?;

    let statement = txn.prepare(sql::CREATE_LISTING).await?;

    tracing::info!("{:?}", statement.params());
    tracing::info!("{:?}", params);

    let row = txn
        .query_one(
            &statement,
            &[
                // Item ID
                &params.item_id,
                // User ID
                &params.user_id,
                // Listing type
                &L::get_type(),
                // Batched by quantity
                &params.batched_by,
                // Unit quantity
                &params.unit_quantity,
                // Individual cost
                &params.cost,
            ],
        )
        .await?;

    let listing = L::try_from(row)?;

    Ok(listing)
}
