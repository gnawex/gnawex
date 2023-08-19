use deadpool_postgres::{
    Client, CreatePoolError, ManagerConfig, PoolError, RecyclingMethod, Runtime,
};
use gnawex_db::config::DbConfig;
use thiserror::Error;
use tokio_postgres::NoTls;

#[derive(Clone, Debug)]
pub struct Handle {
    pool: deadpool_postgres::Pool,
}

#[derive(Debug, Error)]
#[error(transparent)]
pub enum CreateHandleError {
    Pool(#[from] CreatePoolError),
    Path(#[from] std::io::Error),
}

#[derive(Debug, Error)]
#[error(transparent)]
pub struct GetClientError(#[from] PoolError);

impl Handle {
    pub fn new(db_config: DbConfig) -> Result<Self, CreateHandleError> {
        let mut cfg = deadpool_postgres::Config::new();
        let async_runtime = Some(Runtime::Tokio1);

        let db_password = match db_config {
            DbConfig {
                password: Some(_password),
                password_file: Some(path),
                ..
            } => Some(std::fs::read_to_string(path)?),
            DbConfig {
                password: Some(password),
                password_file: None,
                ..
            } => Some(password),
            DbConfig {
                password: None,
                password_file: Some(path),
                ..
            } => Some(std::fs::read_to_string(path)?),
            _ => None,
        };

        cfg.dbname = Some(db_config.name);
        cfg.user = Some(db_config.user);
        cfg.host = Some(db_config.host);
        cfg.password = db_password;
        cfg.port = Some(db_config.port);

        cfg.manager = Some(ManagerConfig {
            recycling_method: RecyclingMethod::Fast,
        });

        let pool = match db_config.ca_cert_file {
            // TODO: Implement TLS
            Some(_path) => cfg.create_pool(async_runtime, NoTls),
            None => cfg.create_pool(async_runtime, NoTls),
        }?;

        Ok(Handle { pool })
    }

    /// Gets a DB client from the pool
    pub async fn get_client(&self) -> Result<Client, GetClientError> {
        let client = self.pool.get().await?;

        Ok(client)
    }
}
