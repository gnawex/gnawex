use serde::Deserialize;
use thiserror::Error;

#[derive(Clone, Debug, Deserialize)]
pub struct ServerConfig {
    pub port: u16,
    pub env: Env,
    pub secret_key: String,
}

#[derive(Clone, Debug, Deserialize)]
pub enum Env {
    Prod,
    Test,
    Dev,
}

#[derive(Debug, Error)]
pub enum ServerConfigError {
    #[error("Failed to load configuration")]
    Config(#[from] config::ConfigError),
}

impl ServerConfig {
    pub fn from_env() -> Result<ServerConfig, ServerConfigError> {
        let source = config::Environment::with_prefix("GX_SERVER")
            .try_parsing(true)
            .separator("__");

        let config = config::Config::builder().add_source(source).build()?;
        let db_config: ServerConfig = config.try_deserialize()?;

        Ok(db_config)
    }
}
