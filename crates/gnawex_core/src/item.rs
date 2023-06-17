use postgres_types::{FromSql, ToSql};
use tokio_postgres::Row;

use crate::{db, error::ParseError};

use self::error::{CreateItemError, GetItemError, ListItemsError};

pub mod error;
pub mod listing;

#[derive(Debug, PartialEq)]
pub struct Item {
    id: Id,
    name: String,
    description: String,
    wiki_link: String,
}

#[derive(Debug, FromSql, ToSql, PartialEq)]
pub struct Id(pub i64);

impl TryFrom<Row> for Item {
    type Error = ParseError;

    /// Parses a Postgres row into an `Item`
    fn try_from(value: Row) -> Result<Self, Self::Error> {
        let id: Id = value.try_get("id").map_err(|e| ParseError {
            table: "app.tradable_items".into(),
            field: "id".into(),
            cause: e.to_string(),
        })?;

        let name: String = value.try_get("name").map_err(|e| ParseError {
            table: "app.tradable_items".into(),
            field: "name".into(),
            cause: e.to_string(),
        })?;

        let description: String = value.try_get("description").map_err(|e| ParseError {
            table: "app.tradable_items".into(),
            field: "description".into(),
            cause: e.to_string(),
        })?;

        let wiki_link: String = value.try_get("wiki_link").map_err(|e| ParseError {
            table: "app.tradable_items".into(),
            field: "wiki_link".into(),
            cause: e.to_string(),
        })?;

        Ok(Self {
            id,
            name,
            description,
            wiki_link,
        })
    }
}

/// Lists all tradable items
pub async fn list_items(db_handle: &db::Handle) -> Result<Vec<Item>, ListItemsError> {
    let client = db_handle.get_client().await?;
    let items = client
        .query("SELECT * FROM app.tradable_items ORDER BY id ASC", &[])
        .await
        .map_err(ListItemsError::from)?;

    items
        .into_iter()
        .map(Item::try_from)
        .collect::<Result<Vec<_>, _>>()
        .map_err(ListItemsError::from)
}

pub async fn get_item(db_handle: &db::Handle, item_id: Id) -> Result<Item, GetItemError> {
    let client = db_handle.get_client().await?;
    let item = client
        .query_one(
            "SELECT * FROM app.tradable_items WHERE id = $1",
            &[&item_id.0],
        )
        .await
        .map_err(GetItemError::from)?;

    Item::try_from(item).map_err(GetItemError::from)
}

pub async fn create_item(
    db_handle: &db::Handle,
    name: String,
    description: String,
    wiki_link: String,
) -> Result<Item, CreateItemError> {
    let client = db_handle.get_client().await?;
    let item = client
        .query_one(
            "
            INSERT INTO app.tradable_items
                ( name
                , description
                , wiki_link
                )
                VALUES ($1, $2, $3)
                RETURNING *
            ",
            &[&name, &description, &wiki_link],
        )
        .await
        .map_err(CreateItemError::from)?;

    Item::try_from(item).map_err(CreateItemError::from)
}

// TODO: Move to own file
#[cfg(test)]
mod tests {
    use crate::db;
    use crate::item;

    async fn setup() -> db::Handle {
        let db_handle = db::Handle::new(
            "127.0.0.1".to_string(),
            "gnawex_test".to_string(),
            5432,
            "gnawex".to_string(),
            Some("gnawex".to_string()),
            None,
        )
        .expect("Failed to create DB handle");

        let client = db_handle
            .get_client()
            .await
            .expect("Failed to get client from pool");

        client
            .execute("TRUNCATE TABLE app.tradable_items, app.tradable_item_listings, app.tradable_item_transactions RESTART IDENTITY", &[])
            .await
            .expect("Failed to delete rows from app.tradable_items. Have you tried running the schema migrations first?");

        db_handle
    }

    #[tokio::test]
    async fn it_should_create_an_item() {
        let db_handle = setup().await;

        let item = item::create_item(
            &db_handle,
            "Adorned Empyrean Jewel".to_string(),
            "Just farm bro".to_string(),
            "https://mhwiki.com".to_string(),
        )
        .await
        .expect("Expected to create an item");

        assert_eq!(item.name, "Adorned Empyrean Jewel".to_string());
        assert_eq!(item.description, "Just farm bro".to_string());
        assert_eq!(item.wiki_link, "https://mhwiki.com".to_string());
    }

    #[tokio::test]
    async fn it_should_list_all_items() {
        let db_handle = setup().await;

        item::create_item(
            &db_handle,
            "Adorned Empyrean Jewel".to_string(),
            "Just farm bro".to_string(),
            "https://mhwiki.com".to_string(),
        )
        .await
        .expect("Expected to create an item");

        item::create_item(
            &db_handle,
            "Timesplit Rune".to_string(),
            "Time machine type beat".to_string(),
            "https://mhwiki.com".to_string(),
        )
        .await
        .expect("Expected to create an item");

        item::create_item(
            &db_handle,
            "Gilded Treasure Scroll".to_string(),
            "Shiny".to_string(),
            "https://mhwiki.com".to_string(),
        )
        .await
        .expect("Expected to create an item");

        let items = item::list_items(&db_handle)
            .await
            .expect("Expected to list all items");

        println!("{:?}", items);

        let mut iter = items.into_iter();

        assert_eq!(
            item::Item {
                id: item::Id(1),
                name: "Adorned Empyrean Jewel".to_string(),
                description: "Just farm bro".to_string(),
                wiki_link: "https://mhwiki.com".to_string()
            },
            iter.next().unwrap()
        );

        assert_eq!(
            item::Item {
                id: item::Id(2),
                name: "Timesplit Rune".to_string(),
                description: "Time machine type beat".to_string(),
                wiki_link: "https://mhwiki.com".to_string()
            },
            iter.next().unwrap()
        );

        assert_eq!(
            item::Item {
                id: item::Id(3),
                name: "Gilded Treasure Scroll".to_string(),
                description: "Shiny".to_string(),
                wiki_link: "https://mhwiki.com".to_string()
            },
            iter.next().unwrap()
        );
    }

    #[tokio::test]
    async fn it_should_get_an_item() {
        let db_handle = setup().await;

        let item = item::create_item(
            &db_handle,
            "Adorned Empyrean Jewel".to_string(),
            "Just farm bro".to_string(),
            "https://mhwiki.com".to_string(),
        )
        .await
        .expect("Expected to create an item");

        assert_eq!(item.name, "Adorned Empyrean Jewel".to_string());
        assert_eq!(item.description, "Just farm bro".to_string());
        assert_eq!(item.wiki_link, "https://mhwiki.com".to_string());
    }
}
