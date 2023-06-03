#![forbid(unsafe_code)]

use tokio_postgres::{connect, NoTls};

async fn example() -> Result<(), tokio_postgres::Error> {
    let conn = connect("host=localhost user=gnawex_master", NoTls).await?;
    let bar = foo();

    todo!()
}

fn foo() -> i32 {
    1
}
