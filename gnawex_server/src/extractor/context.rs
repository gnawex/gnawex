use axum::{
    async_trait,
    extract::{FromRef, FromRequestParts},
    http::request::Parts,
    response::Redirect,
};
use axum_extra::extract::{cookie::Key, PrivateCookieJar};
use gnawex_core::{
    session::{self, Token},
    user::User,
};

use crate::AppState;

pub enum Context {
    Authenticated {
        current_user: User,
        session_token: Token,
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
        let token = jar
            .get("session")
            .and_then(|cookie| Some(cookie.value().to_owned()));

        match token {
            Some(token) => {
                let current_user =
                    session::get_session_user(&app_state.0.db_handle, Token(token.clone()))
                        .await
                        .unwrap();

                let context = gnawex_core::context::AuthContext {
                    current_user,
                    session_token: session::Token(token),
                };

                Ok(AuthContext(context))
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

        tracing::info!("{:#?}", jar);

        let app_state = AppState::from_ref(state);
        let token = jar
            .get("session")
            .and_then(|cookie| Some(cookie.value().to_owned()));

        let context = match token {
            Some(token) => {
                let current_user =
                    session::get_session_user(&app_state.0.db_handle, Token(token.clone()))
                        .await
                        .unwrap();

                Context::Authenticated {
                    current_user,
                    session_token: session::Token(token),
                }
            }
            None => Context::Guest,
        };

        Ok(context)
    }
}
