use askama::Template;
use gnawex_core::user::User;

#[derive(Template)]
#[template(path = "404.html")]
pub struct Error404Page {
    pub current_user: Option<User>,
}

#[derive(Template)]
#[template(path = "500.html")]
pub struct Error500Page {
    pub current_user: Option<User>,
}
