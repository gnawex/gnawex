use std::path::PathBuf;

use deadpool_postgres::{
    Client, CreatePoolError, ManagerConfig, PoolError, RecyclingMethod, Runtime,
};
use thiserror::Error;
use tokio_postgres::NoTls;

#[derive(Clone, Debug)]
pub struct Handle {
    pool: deadpool_postgres::Pool,
}

#[derive(Debug, Error)]
#[error(transparent)]
pub struct CreateHandleError(#[from] CreatePoolError);

#[derive(Debug, Error)]
#[error(transparent)]
pub struct GetClientError(#[from] PoolError);

impl Handle {
    pub fn new(
        host: String,
        dbname: String,
        port: u16,
        user: String,
        password: Option<String>,
        ca_cert_path: Option<PathBuf>,
    ) -> Result<Self, CreateHandleError> {
        let mut cfg = deadpool_postgres::Config::new();
        let async_runtime = Some(Runtime::Tokio1);

        cfg.dbname = Some(dbname);
        cfg.user = Some(user);
        cfg.host = Some(host);
        cfg.password = password;
        cfg.port = Some(port);
        cfg.manager = Some(ManagerConfig {
            recycling_method: RecyclingMethod::Fast,
        });

        let pool = match ca_cert_path {
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
