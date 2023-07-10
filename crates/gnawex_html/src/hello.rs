use askama::Template;

#[derive(Template)]
#[template(path = "base.html")]
pub struct HelloTemplate {
    pub name: String,
    pub title: String,
}
