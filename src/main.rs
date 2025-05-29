use ansi_term::Colour;
use clap::{CommandFactory, Parser, Subcommand, ValueEnum};
use log::{debug, error, warn};
use simplelog::{ConfigBuilder, LevelFilter, WriteLogger};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::PathBuf;
use std::process::ExitCode;
use tokio::fs;
use tokio::io::{AsyncReadExt, AsyncSeekExt, AsyncWriteExt};
use tower_lsp::lsp_types::{DiagnosticSeverity, Url};
use tower_lsp::{LspService, Server};

use varnishls::backend::{read_config, Backend};
use varnishls::document::Include;
use varnishls::formatter;
use varnishls::vmod::{read_vmod_lib, read_vmod_lib_by_name};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum LintLevel {
    /// Only errors
    Error,
    /// Warnings and above
    Warning,
    /// Info and above
    Info,
    /// Hints and above (most verbose)
    Hint,
}

#[derive(Debug, Parser)]
#[clap(name = "varnishls")]
#[clap(long_version(option_env!("LONG_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"))))]
pub struct Cli {
    /// Debug mode
    #[clap(short, long, global = true)]
    debug: bool,
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Start the LSP server
    Lsp {
        #[clap(long)]
        stdio: bool,
        #[clap(long, requires = "port")]
        listen: bool,
        #[clap(long, requires = "listen")]
        port: Option<u16>,
    },

    /// Lint VCL
    ///
    /// Lint the whole workspace by running lint without any file path and having main_vcl set in
    /// the .varnishls.toml
    Lint {
        /// File to lint, unless main_vcl is set in .varnishls.toml
        file_path: Option<PathBuf>,
        #[clap(long, default_value = "error")]
        level: LintLevel,
    },

    /// Inspect Vmod binary
    #[clap(hide = true)]
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

    /// Print out parsed data from .vcc files
    #[clap(hide = true)]
    InspectVcc {
        /// Files to lint
        path: PathBuf,
    },

    /// Run the formatter on VCL files
    #[clap(alias = "fmt")]
    Format {
        /// Write
        #[clap(short, long)]
        write: bool,
        /// Validate if formatting is correct (exit with failure if wrong)
        #[clap(long)]
        validate: bool,
        /// Files to lint
        paths: Vec<PathBuf>,
    },
}

#[tokio::main]
async fn main() -> ExitCode {
    let cli: Cli = Parser::parse();
    let debug = cli.debug;

    if debug {
        let _ = WriteLogger::init(
            LevelFilter::Debug,
            ConfigBuilder::new()
                .add_filter_allow_str("varnishls")
                .build(),
            std::fs::File::create("varnishls.log").unwrap(),
        );
    }

    match cli.command {
        Command::Lsp {
            stdio,
            listen,
            port,
        } => {
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
        Command::Lint { file_path, level } => {
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
            let Some(initial_file_path) = file_path.or(config.main_vcl.clone()) else {
                error!("Missing VCL file path to lint. Either setup main_vcl in .varnishls.toml or point cmd arg to your main VCL file.");
                return ExitCode::from(2);
            };

            let mut root_uri = Url::from_file_path(cwd).unwrap();
            // Fix workspace directory missing slash
            root_uri.set_path(format!("{}/", root_uri.path()).as_str());
            backend.set_root_uri(root_uri).await;
            let initial_include_uri =
                Url::from_file_path(fs::canonicalize(initial_file_path.clone()).await.unwrap())
                    .unwrap();
            let initial_include = Include {
                url: Some(initial_include_uri.clone()),
                path: initial_file_path,
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
                let errors = doc.get_error_ranges(&scope, &config.lint);
                for error in errors {
                    let Some(severity) = error.severity else {
                        continue;
                    };
                    if severity > severity_filter {
                        continue;
                    }
                    if severity == DiagnosticSeverity::ERROR {
                        error_count += 1;
                    }

                    // print error message
                    println!(
                        "{}: {}:{}:{} - {}",
                        match severity {
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
                    for line_idx in start_line_idx.checked_sub(1).unwrap_or(start_line_idx)
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

            warn!("Took: {}ms", now.elapsed().as_millis());

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
        Command::Format {
            write,
            validate,
            paths,
        } => {
            let mut exit_code = ExitCode::SUCCESS;
            let cwd = std::env::current_dir().unwrap();
            let config = read_config(&cwd).await.unwrap_or_default();
            let now = std::time::Instant::now();
            let paths_len = paths.len();
            for path in paths {
                let mut file = fs::File::options()
                    .read(true)
                    .write(write)
                    .open(&path)
                    .await
                    .map_err(|err| panic!("Could not open VCL file {path:?}: {err:}"))
                    .unwrap();
                let mut src: String = "".into();
                file.read_to_string(&mut src).await.unwrap();
                let src_hash = {
                    if validate {
                        let mut hasher = DefaultHasher::new();
                        src.hash(&mut hasher);
                        hasher.finish()
                    } else {
                        0
                    }
                };

                let Ok(formatted) = formatter::format(src, &config.formatter).inspect_err(|err| {
                    eprintln!("Failed to format {}: {err}", &path.to_string_lossy());
                    exit_code = ExitCode::FAILURE;
                }) else {
                    continue;
                };

                if write {
                    file.rewind().await.unwrap();
                    file.set_len(0).await.unwrap();
                    file.write_all(formatted.as_bytes()).await.expect("rip");
                } else if validate {
                    let formatted_hash = {
                        let mut hasher = DefaultHasher::new();
                        formatted.hash(&mut hasher);
                        hasher.finish()
                    };
                    if src_hash.cmp(&formatted_hash).is_ne() {
                        exit_code = ExitCode::FAILURE;
                    }
                } else if paths_len == 1 {
                    print!("{formatted}");
                }
            }

            warn!("Took: {}ms", now.elapsed().as_millis());

            return exit_code;
        }
    }

    ExitCode::SUCCESS
}
