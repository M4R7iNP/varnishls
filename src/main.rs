use clap::{Parser, Subcommand, ValueEnum};
use log::debug;
use simplelog::{Config, LevelFilter, WriteLogger};
use std::path::PathBuf;
use std::process::ExitCode;
use tokio::fs;
use tower_lsp::lsp_types::Url;
use tower_lsp::{LspService, Server};

use varnishls::backend::Backend;
use varnishls::document::Document;
use varnishls::vmod::{read_vmod_lib, read_vmod_lib_by_name};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum LintLevel {
    Error,   // only errors
    Warning, // warnings and above
}

#[derive(Debug, Parser)]
#[clap(name = "varnishls")]
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
        files: Vec<PathBuf>,
        #[clap(long = "level")]
        level: Option<LintLevel>,
    },

    InspectVmod {
        /// VMOD name
        name: String,
        /// Path to VMOD
        #[clap(long = "path")]
        path: Option<String>,
        /// Dump vmod JSON
        #[clap(long = "json")]
        json: bool,
    },

    InspectVcc {
        /// Files to lint
        path: PathBuf,
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
                    std::fs::File::create("varnishls.log").unwrap(),
                );
            }

            debug!("start");

            let stdin = tokio::io::stdin();
            let stdout = tokio::io::stdout();

            let (service, socket) = LspService::build(Backend::new).finish();
            Server::new(stdin, stdout, socket).serve(service).await;
        }
        Command::Lint { files, level } => {
            let mut results = Vec::new();

            if files.is_empty() {
                panic!("No files supplied");
            }

            let now = std::time::Instant::now();
            for file_path in files {
                let src = fs::read_to_string(&file_path)
                    .await
                    .expect("Could not read VCL");
                let doc = Document::new(
                    Url::from_file_path(fs::canonicalize(file_path).await.unwrap()).unwrap(),
                    src,
                );
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
            for lint_error in &results {
                let line = lint_error.loc.range.start.line;
                println!(
                    "[{:?}] [{} at line {}]: {}",
                    lint_error.severity, lint_error.loc.uri, line, lint_error.message
                );
            }
            println!("Took: {}", now.elapsed().as_millis());

            if !results.is_empty() {
                return ExitCode::from(1);
            }
        }
        Command::InspectVmod { name, path, json } => {
            let vmod = if path.is_some() {
                read_vmod_lib(name, PathBuf::from(path.unwrap()))
                    .await
                    .unwrap()
            } else {
                match read_vmod_lib_by_name(name, vec![]).await.unwrap() {
                    Some(vmod) => vmod,
                    None => {
                        panic!("VMOD not found");
                    }
                }
            };

            if json {
                println!("{}", vmod.json);
            } else {
                println!("VMOD: {:?}", vmod);
            }
        }
        Command::InspectVcc { path } => {
            let src = fs::read_to_string(&path)
                .await
                .expect("Could not read VCC");

            let scope = varnishls::vcc::parse_vcc(src);
            println!("scope: {:?}", scope);
        }
    }

    ExitCode::SUCCESS
}
