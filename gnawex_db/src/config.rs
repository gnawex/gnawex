use std::path::PathBuf;

use serde::Deserialize;
use thiserror::Error;

#[derive(Debug, Deserialize)]
pub struct DbConfig {
    pub db_name: String,
    pub db_host: String,
    pub db_port: u16,
    pub db_user: String,
    pub db_password: Option<String>,
    pub db_password_file: Option<PathBuf>,
    pub db_ca_cert_file: Option<PathBuf>,
}

#[derive(Debug, Error)]
pub enum DbConfigError {
    #[error("Failed to load configuration")]
    Config(#[from] config::ConfigError),
}

impl DbConfig {
    pub fn from_env() -> Result<DbConfig, DbConfigError> {
        let source = config::Environment::with_prefix("GX")
            .try_parsing(true)
            .separator("__");

        let config = config::Config::builder().add_source(source).build()?;
        let db_config: DbConfig = config.try_deserialize()?;

        Ok(db_config)
    }
}
