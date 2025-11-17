use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    vibeos_cli::app::run().await
}
