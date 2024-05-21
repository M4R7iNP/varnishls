use ansi_term::Colour;
use clap::{CommandFactory, Parser, Subcommand, ValueEnum};
use log::debug;
use simplelog::{ConfigBuilder, LevelFilter, WriteLogger};
use std::path::PathBuf;
use std::process::ExitCode;
use tokio::fs;
use tower_lsp::lsp_types::{DiagnosticSeverity, Url};
use tower_lsp::{LspService, Server};

use varnishls::backend::{read_config, Backend};
use varnishls::document::Include;
use varnishls::formatter;
use varnishls::vmod::{read_vmod_lib, read_vmod_lib_by_name};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum LintLevel {
    Error,   // only errors
    Warning, // warnings and above
    Info,
    Hint,
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
        #[clap(short, long)]
        debug: bool,
        #[clap(long)]
        stdio: bool,
        #[clap(long, requires = "port")]
        listen: bool,
        #[clap(long, requires = "listen")]
        port: Option<u16>,
    },

    Lint {
        /// File to lint
        file_path: PathBuf,
        #[clap(long, default_value = "error")]
        level: LintLevel,
        /// Debug mode
        #[clap(short, long)]
        debug: bool,
    },

    InspectVmod {
        /// VMOD name
        name: String,
        /// Path to VMOD
        #[clap(long)]
        path: Option<String>,
        /// Dump vmod JSON
        #[clap(long)]
        json: bool,
    },

    InspectVcc {
        /// Files to lint
        path: PathBuf,
    },

    Format {
        /// Files to lint
        paths: Vec<PathBuf>,
    },
}

#[tokio::main]
async fn main() -> ExitCode {
    let cli: Cli = Parser::parse();

    match cli.command {
        Command::Lsp {
            debug,
            stdio,
            listen,
            port,
        } => {
            if debug {
                let _ = WriteLogger::init(
                    LevelFilter::Debug,
                    ConfigBuilder::new()
                        .add_filter_allow_str("varnishls")
                        .build(),
                    std::fs::File::create("varnishls.log").unwrap(),
                );
            }

            debug!("start");

            if listen {
                let port = port.unwrap();
                let listener = tokio::net::TcpListener::bind(format!("[::1]:{port}"))
                    .await
                    .unwrap();
                loop {
                    let (mut stream, _) = listener.accept().await.unwrap();
                    tokio::spawn(async move {
                        let (input, output) = stream.split();
                        let (service, socket) = LspService::build(Backend::new).finish();
                        Server::new(input, output, socket).serve(service).await;
                    });
                }
            } else if stdio {
                let stdin = tokio::io::stdin();
                let stdout = tokio::io::stdout();
                let (service, socket) = LspService::build(Backend::new).finish();
                Server::new(stdin, stdout, socket).serve(service).await;
            } else {
                let mut cmd = Cli::command();
                cmd.error(
                    clap::error::ErrorKind::MissingRequiredArgument,
                    "Missing either --stdio or --listen",
                )
                .exit();
            }
        }
        Command::Lint {
            file_path,
            level,
            debug,
        } => {
            if debug {
                let _ = WriteLogger::init(
                    LevelFilter::Debug,
                    ConfigBuilder::new()
                        .add_filter_allow_str("varnishls")
                        .build(),
                    std::fs::File::create("varnishls.log").unwrap(),
                );
            }

            debug!("lint start");

            let backend: Backend = Default::default();
            let mut error_count = 0;

            let severity_filter = match level {
                LintLevel::Warning => DiagnosticSeverity::WARNING,
                LintLevel::Error => DiagnosticSeverity::ERROR,
                LintLevel::Info => DiagnosticSeverity::INFORMATION,
                LintLevel::Hint => DiagnosticSeverity::HINT,
            };

            let now = std::time::Instant::now();
            let cwd = std::env::current_dir().unwrap();
            let config = read_config(&cwd).await.unwrap_or_default();
            debug!("config: {config:?}");
            let mut root_uri = Url::from_file_path(cwd).unwrap();
            // Fix workspace directory missing slash
            root_uri.set_path(format!("{}/", root_uri.path()).as_str());
            backend.set_root_uri(root_uri).await;
            let initial_include_uri =
                Url::from_file_path(fs::canonicalize(file_path.clone()).await.unwrap()).unwrap();
            let initial_include = Include {
                url: Some(initial_include_uri.clone()),
                path: file_path,
                nested_pos: Default::default(),
            };
            backend.set_config(config.clone()).await;
            backend.read_new_includes(vec![initial_include]).await;
            debug!("doc count {}", backend.document_map.iter().count());

            let scope = backend
                .get_all_definitions_across_all_documents(Some(&initial_include_uri))
                .await;

            for doc in backend.document_map.iter() {
                let len_lines = doc.rope.len_lines();
                // debug!("hei {}", doc.url);
                let errors = doc.get_error_ranges(&scope, &config.lint);
                for error in errors {
                    if error.severity <= severity_filter {
                        if error.severity == DiagnosticSeverity::ERROR {
                            error_count += 1;
                        }

                        // print error message
                        println!(
                            "{}: {}:{}:{} - {}",
                            match error.severity {
                                DiagnosticSeverity::ERROR => Colour::Red.paint("ERROR"),
                                DiagnosticSeverity::WARNING => Colour::Yellow.paint("WARNING"),
                                DiagnosticSeverity::INFORMATION => Colour::Blue.paint("INFO"),
                                DiagnosticSeverity::HINT => Colour::Blue.paint("HINT"),
                                _ => unreachable!(),
                            },
                            error.loc.uri,
                            error.loc.range.start.line,
                            error.loc.range.start.character,
                            error.message
                        );

                        // print source code with context (one extra line above and below)
                        let start_line_idx = error.loc.range.start.line as usize;
                        let end_line_idx = error.loc.range.end.line as usize;
                        for line_idx in (start_line_idx - 1).max(0)
                            ..=(end_line_idx + 1).min(len_lines).min(start_line_idx + 9)
                        {
                            let line = doc.rope.line(line_idx);
                            let line_num = line_idx + 1;
                            // print line number and source code
                            print!("{}", Colour::Cyan.paint(format!("{line_num} | ")));
                            print!("{line}");

                            // if current line has the error, print a line with carets indicating
                            // where the error is
                            if line_idx == start_line_idx
                                && error.loc.range.start.line == error.loc.range.end.line
                            {
                                let start = error.loc.range.start.character as usize;
                                let end = error.loc.range.end.character as usize;
                                let whitespace_offset = line.to_string()[0..start]
                                    .replace(|c: char| !c.is_whitespace(), " ");

                                print!(
                                    "{}",
                                    Colour::Cyan.paint(format!(
                                        "{} | ",
                                        " ".repeat(line_num.to_string().len())
                                    ))
                                );
                                println!(
                                    "{whitespace_offset}{}",
                                    Colour::Yellow.paint("^".repeat(end - start))
                                );
                            }
                        }

                        // empty line as spacing between errors
                        println!();
                    }
                }
            }

            println!("Took: {}ms", now.elapsed().as_millis());

            if error_count > 0 {
                return ExitCode::from(1);
            }
        }
        Command::InspectVmod { name, path, json } => {
            let vmod = if path.is_some() {
                read_vmod_lib(name, PathBuf::from(path.unwrap()))
                    .await
                    .expect("Failed to parse vmod")
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
            let src = fs::read_to_string(&path).await.expect("Could not read VCC");

            let scope = varnishls::vcc::parse_vcc(src);
            println!("scope: {:?}", scope);
        }
        Command::Format { paths } => {
            let exit_code = ExitCode::SUCCESS;
            for path in paths {
                let src = fs::read_to_string(&path)
                    .await
                    .expect("Could not read VCL file");
                let formatted = formatter::format(src, &Default::default());
                print!("{formatted}");
            }

            return exit_code;
        }
    }

    ExitCode::SUCCESS
}
