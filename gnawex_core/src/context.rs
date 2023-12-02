use crate::{session, user::User};

pub struct AuthContext {
    pub current_user: User,
    pub session_token: String,
}
