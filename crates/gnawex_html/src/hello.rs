use askama::Template;

#[derive(Template)]
#[template(path = "hello.html")]
pub struct HelloTemplate {
    pub name: String,
    pub title: String,
}
