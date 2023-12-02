use gnawex_html::error::Error404Page;
use hyper::Uri;

pub(crate) async fn error_404() -> Error404Page {
    Error404Page { current_user: None }
}
