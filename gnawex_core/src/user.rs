use postgres_types::{FromSql, ToSql};

use crate::error::ParseError;

#[derive(Debug, FromSql, ToSql)]
pub struct User {
    pub id: Id,
    hunter_id: i64,
    pub username: String,
}

#[derive(Debug, FromSql, ToSql)]
#[postgres(transparent)]
pub struct Id(pub i64);

impl TryFrom<tokio_postgres::Row> for User {
    type Error = ParseError;

    fn try_from(value: tokio_postgres::Row) -> Result<Self, Self::Error> {
        let id: Id = value
            .try_get::<_, i64>("id")
            .map(Id)
            .map_err(|e| ParseError {
                table: "app.users".into(),
                field: "id".into(),
                cause: e.to_string(),
            })?;

        let username = value
            .try_get::<_, String>("username")
            .map_err(|e| ParseError {
                table: "app.users".into(),
                field: "username".into(),
                cause: e.to_string(),
            })?;

        Ok(User {
            id,
            username,
            hunter_id: 1,
        })
    }
}
