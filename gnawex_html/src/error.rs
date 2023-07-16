use askama::Template;

#[derive(Template)]
#[template(path = "404.html")]
pub struct Error404Page {}
