use deadpool_postgres::{
    Client, CreatePoolError, ManagerConfig, PoolError, RecyclingMethod, Runtime,
};
use gnawex_db::config::DbConfig;
use openssl::{
    error::ErrorStack,
    ssl::{SslConnector, SslFiletype, SslMethod},
};
use postgres_openssl::MakeTlsConnector;
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
    Tls(#[from] ErrorStack),
}

#[derive(Debug, Error)]
#[error(transparent)]
pub struct GetClientError(#[from] PoolError);

impl Handle {
    pub fn new(db_config: DbConfig) -> Result<Self, CreateHandleError> {
        let mut cfg = deadpool_postgres::Config::new();
        let async_runtime = Some(Runtime::Tokio1);

        // TODO: Move pool creation to gnawex_db crate
        let cfg_db_config = db_config.clone();

        let db_password = match cfg_db_config {
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
            } => Some(std::fs::read_to_string(path)?.trim().to_string()),
            _ => None,
        };

        cfg.dbname = Some(cfg_db_config.name);
        cfg.user = Some(cfg_db_config.user);
        cfg.host = Some(cfg_db_config.host);
        cfg.password = db_password;
        cfg.port = Some(cfg_db_config.port);

        cfg.manager = Some(ManagerConfig {
            recycling_method: RecyclingMethod::Fast,
        });

        let pool = match db_config {
            DbConfig {
                ca_cert_file: Some(server_ca_path),
                client_cert_file: Some(client_cert_path),
                client_key_file: Some(client_key_path),
                ..
            } => {
                let mut builder = SslConnector::builder(SslMethod::tls())?;

                builder.set_ca_file(server_ca_path)?;
                builder.set_certificate_file(client_cert_path, SslFiletype::PEM)?;
                builder.set_private_key_file(client_key_path, SslFiletype::PEM)?;

                let mut connector = MakeTlsConnector::new(builder.build());

                // https://github.com/sfackler/rust-openssl/issues/1572
                connector.set_callback(|connect_config, _domain| {
                    connect_config.set_verify_hostname(false);

                    Ok(())
                });

                cfg.create_pool(async_runtime, connector)
            }
            _ => cfg.create_pool(async_runtime, NoTls),
        }?;

        Ok(Handle { pool })
    }

    /// Gets a DB client from the pool
    pub async fn get_client(&self) -> Result<Client, GetClientError> {
        let client = self.pool.get().await?;

        Ok(client)
    }
}
