use vcl_parser::backend::Backend;

use dashmap::DashMap;
use std::error::Error;
use tower_lsp::{LanguageServer, LspService, Server};

// #[async_std::main]
// async fn main() -> Result<(), Box<dyn Error>> {
#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        // ast_map: DashMap::new(),
        document_map: DashMap::new(),
        // semantic_token_map: DashMap::new(),
    })
    // .custom_method("custom/inlay_hint", Backend::inlay_hint)
    .finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}
