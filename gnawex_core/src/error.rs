use std::fmt::Display;

use thiserror::Error;

/// Failed to parse a DB row into a Rust struct
#[derive(Debug, Error)]
pub struct ParseError {
    /// The table name, and schema (if applicable)
    pub table: String,
    /// The table field that failed to parse
    pub field: String,
    /// The reason why it failed
    pub cause: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Failed to parse field {} from a {} row: {}",
            self.field, self.table, self.cause
        )
    }
}
