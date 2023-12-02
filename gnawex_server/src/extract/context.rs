use axum::{
    async_trait,
    extract::{FromRef, FromRequestParts},
    http::request::Parts,
    response::Redirect,
};
use axum_extra::extract::{cookie::Key, PrivateCookieJar};
use gnawex_core::{session, user::User};

use crate::AppState;

pub enum Context {
    Authenticated {
        current_user: User,
        session_token: String,
    },
    Guest,
}

pub struct AuthContext(pub gnawex_core::context::AuthContext);

#[async_trait]
impl<S> FromRequestParts<S> for AuthContext
where
    S: Send + Sync,
    Key: FromRef<S>,
    AppState: FromRef<S>,
{
    type Rejection = Redirect;

    async fn from_request_parts(parts: &mut Parts, state: &S) -> Result<Self, Self::Rejection> {
        let jar = PrivateCookieJar::<Key>::from_request_parts(parts, state)
            .await
            .unwrap();

        tracing::info!("{:#?}", jar);

        let app_state = AppState::from_ref(state);
        let session_token = get_session_token(jar);

        match session_token {
            Some(session_token) => {
                match session::get_session_user(&app_state.0.db_handle, session_token.clone()).await
                {
                    Ok(current_user) => {
                        let context = gnawex_core::context::AuthContext {
                            current_user,
                            session_token,
                        };

                        Ok(AuthContext(context))
                    }

                    Err(_) => Err(Redirect::to("/login")),
                }
            }
            None => Err(Redirect::to("/login")),
        }
    }
}

#[async_trait]
impl<S> FromRequestParts<S> for Context
where
    S: Send + Sync,
    Key: FromRef<S>,
    AppState: FromRef<S>,
{
    type Rejection = Redirect;

    async fn from_request_parts(parts: &mut Parts, state: &S) -> Result<Self, Self::Rejection> {
        let jar = PrivateCookieJar::<Key>::from_request_parts(parts, state)
            .await
            .unwrap();
        let app_state = AppState::from_ref(state);
        let session_token = get_session_token(jar);
        let context = match session_token {
            Some(session_token) => {
                let current_user =
                    session::get_session_user(&app_state.0.db_handle, session_token.clone()).await;

                match current_user {
                    Ok(current_user) => Context::Authenticated {
                        current_user,
                        session_token,
                    },
                    Err(_) => Context::Guest,
                }
            }
            None => Context::Guest,
        };

        Ok(context)
    }
}

fn get_session_token(jar: PrivateCookieJar) -> Option<String> {
    let cookie = jar.get("session").and_then(|cookie| Some(cookie));
    let token = cookie?.value().to_owned();

    Some(token)
}
