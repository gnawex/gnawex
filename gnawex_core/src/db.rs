use std::path::PathBuf;

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
                db_password: Some(_password),
                db_password_file: Some(path),
                ..
            } => Some(std::fs::read_to_string(path)?),
            DbConfig {
                db_password: Some(password),
                db_password_file: None,
                ..
            } => Some(password),
            DbConfig {
                db_password: None,
                db_password_file: Some(path),
                ..
            } => Some(std::fs::read_to_string(path)?),
            _ => None,
        };

        cfg.dbname = Some(db_config.db_name);
        cfg.user = Some(db_config.db_user);
        cfg.host = Some(db_config.db_host);
        cfg.password = db_password;
        cfg.port = Some(db_config.db_port);

        cfg.manager = Some(ManagerConfig {
            recycling_method: RecyclingMethod::Fast,
        });

        let pool = match db_config.db_ca_cert_file {
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
