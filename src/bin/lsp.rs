use clap::Parser;
use log::debug;
use simplelog::{Config, LevelFilter, WriteLogger};
use tower_lsp::{LspService, Server};

use varnish_lsp::backend::Backend;

#[derive(Debug, Parser)]
#[clap(name = "varnish_lsp")]
#[clap(long_version(option_env!("LONG_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"))))]
pub struct Opt {
    /// Debug mode
    #[clap(short = 'd', long = "debug")]
    pub debug: bool,
    #[clap(long = "stdio")]
    pub stdio: bool,
}

#[tokio::main]
async fn main() {
    let opt: Opt = Parser::parse();

    if opt.debug {
        let _ = WriteLogger::init(
            LevelFilter::Debug,
            Config::default(),
            std::fs::File::create("varnish_lsp.log").unwrap(),
        );
    }

    debug!("start");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend::new(client)).finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}
