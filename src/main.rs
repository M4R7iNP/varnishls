use clap::{Parser, Subcommand, ValueEnum};
use log::debug;
use simplelog::{Config, LevelFilter, WriteLogger};
use std::{fs, process::ExitCode};
use tower_lsp::{LspService, Server};

use varnish_lsp::backend::Backend;
use varnish_lsp::document::Document;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum LintLevel {
    Error,   // only errors
    Warning, // warnings and above
}

#[derive(Debug, Parser)]
#[clap(name = "varnish_lsp")]
#[clap(long_version(option_env!("LONG_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"))))]
pub struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Lsp {
        /// Debug mode
        #[clap(short = 'd', long = "debug")]
        debug: bool,
        #[clap(long = "stdio")]
        stdio: bool,
    },

    Lint {
        /// Files to lint
        files: Vec<String>,
        #[clap(long = "level")]
        level: Option<LintLevel>,
    },
}

#[tokio::main]
async fn main() -> ExitCode {
    let cli: Cli = Parser::parse();

    match cli.command {
        Command::Lsp { debug, .. } => {
            if debug {
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
        Command::Lint { files, level } => {
            let mut results = Vec::new();

            if files.len() == 0 {
                panic!("No files supplied");
            }

            for file_path in files {
                let src = fs::read_to_string(&file_path).expect("Could not read VCL");
                let doc = Document::new(src);
                let mut errors = doc.get_error_ranges();
                results.append(&mut errors);
            }

            match level {
                Some(LintLevel::Error) => {
                    todo!("filter out warnings");
                }
                _ => {}
            }

            // TODO: format error messages with path to file
            println!("{:?}", results);

            if results.len() > 0 {
                return ExitCode::from(1);
            }
        }
    }

    ExitCode::SUCCESS
}
