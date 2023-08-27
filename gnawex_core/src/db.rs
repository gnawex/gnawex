use std::{io::BufReader, sync::Arc};

use deadpool_postgres::{
    Client, CreatePoolError, ManagerConfig, PoolError, RecyclingMethod, Runtime,
};
use gnawex_db::config::DbConfig;
use rustls::{Certificate, ClientConfig, OwnedTrustAnchor, PrivateKey, RootCertStore};
use rustls_pemfile::Item;
use thiserror::Error;
use tokio_postgres::NoTls;
use tokio_postgres_rustls::MakeRustlsConnect;

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

// Implementation of `ServerCertVerifier` that verifies everything as trustworthy.
struct SkipServerVerification;

impl SkipServerVerification {
    fn new() -> Arc<Self> {
        Arc::new(Self)
    }
}

impl rustls::client::ServerCertVerifier for SkipServerVerification {
    fn verify_server_cert(
        &self,
        _end_entity: &rustls::Certificate,
        _intermediates: &[rustls::Certificate],
        _server_name: &rustls::ServerName,
        _scts: &mut dyn Iterator<Item = &[u8]>,
        _ocsp_response: &[u8],
        _now: std::time::SystemTime,
    ) -> Result<rustls::client::ServerCertVerified, rustls::Error> {
        Ok(rustls::client::ServerCertVerified::assertion())
    }
}

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
            // TODO: Implement TLS
            DbConfig {
                ca_cert_file: Some(server_ca_path),
                client_cert_file: Some(client_cert_path),
                client_key_file: Some(client_key_path),
                ..
            } => {
                let tls_config = ClientConfig::builder();

                // TODO: Refactor this. Should probably move this to db crate
                // TODO: Remove unwraps

                // Server CA
                let server_ca_file = std::fs::File::open(&server_ca_path)?;
                let mut server_ca_reader = BufReader::new(server_ca_file);
                let server_ca = rustls_pemfile::certs(&mut server_ca_reader)?;
                let mut root_cert_store = RootCertStore::empty();
                let server_ca_certs: Vec<Certificate> =
                    server_ca.into_iter().map(Certificate).collect();

                // Client certificate
                let client_cert_file = std::fs::File::open(client_cert_path)?;
                let mut client_cert_reader = BufReader::new(client_cert_file);
                let client_certs = rustls_pemfile::certs(&mut client_cert_reader)?;
                let client_certs: Vec<Certificate> =
                    client_certs.into_iter().map(Certificate).collect();

                // Client private key
                let private_key_file = std::fs::File::open(client_key_path)?;
                let mut private_key_reader = BufReader::new(private_key_file);

                let private_key = match rustls_pemfile::read_one(&mut private_key_reader) {
                    Ok(Some(Item::RSAKey(key))) => PrivateKey(key),
                    Ok(Some(_)) => todo!(),
                    Ok(None) => todo!(),
                    Err(_) => todo!(),
                };

                root_cert_store
                    .add(server_ca_certs.get(0).unwrap())
                    .unwrap();

                let mut tls_config = tls_config
                    .with_safe_defaults()
                    .with_root_certificates(root_cert_store)
                    .with_client_auth_cert(client_certs, private_key)
                    .unwrap();

                // TODO: Either find a way around this without `dangerous()` for
                // Cloud SQL, or switch DB vendors.
                tls_config
                    .dangerous()
                    .set_certificate_verifier(SkipServerVerification::new());

                let tls = MakeRustlsConnect::new(tls_config);
                cfg.create_pool(async_runtime, tls)
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
