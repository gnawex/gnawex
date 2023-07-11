/// Failed to parse a DB row into a Rust struct
#[derive(Debug)]
pub struct ParseError {
    /// The table name, and schema (if applicable)
    pub table: String,
    /// The table field that failed to parse
    pub field: String,
    /// The reason why it failed
    pub cause: String,
}
