use gnawex_html::error::Error404Page;
use hyper::Uri;

pub(crate) async fn error_404(_uri: Uri) -> Error404Page {
    Error404Page
}
