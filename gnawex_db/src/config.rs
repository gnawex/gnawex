use std::path::PathBuf;

use serde::Deserialize;
use thiserror::Error;

#[derive(Clone, Debug, Deserialize)]
pub struct DbConfig {
    pub name: String,
    pub host: String,
    pub port: u16,
    pub user: String,
    pub pool_size: u16,
    pub password: Option<String>,
    pub password_file: Option<PathBuf>,
    pub ca_cert_file: Option<PathBuf>,
    pub client_cert_file: Option<PathBuf>,
    pub client_key_file: Option<PathBuf>,
}

#[derive(Debug, Error)]
pub enum DbConfigError {
    #[error("Failed to load configuration")]
    Config(#[from] config::ConfigError),
}

impl DbConfig {
    pub fn from_env() -> Result<DbConfig, DbConfigError> {
        let source = config::Environment::with_prefix("GX_DB")
            .try_parsing(true)
            .separator("__");

        let config = config::Config::builder().add_source(source).build()?;
        let db_config: DbConfig = config.try_deserialize()?;

        Ok(db_config)
    }
}
