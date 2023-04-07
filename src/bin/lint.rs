use clap::{Parser, ValueEnum};
use std::{fs, process::ExitCode};
use varnish_lsp::document::Document;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum Level {
    Error,   // only errors
    Warning, // warnings and above
}

#[derive(Debug, Parser)]
#[clap(name = "varnish_lint")]
#[clap(long_version(option_env!("LONG_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"))))]
pub struct Opt {
    /// Files to lint
    pub files: Vec<String>,
    #[clap(long = "level")]
    pub level: Option<Level>,
}

pub fn main() -> ExitCode {
    let opt: Opt = Parser::parse();
    let mut results = Vec::new();

    if opt.files.len() == 0 {
        panic!("No files supplied");
    }

    for file_path in opt.files {
        let src = fs::read_to_string(&file_path).expect("Could not read VCL");
        let doc = Document::new(src);
        let mut errors = doc.get_error_ranges();
        results.append(&mut errors);
    }

    match opt.level {
        Some(Level::Error) => {
            todo!("filter out warnings");
        }
        _ => {}
    }

    // TODO: format error messages with path to file
    println!("{:?}", results);

    if results.len() > 0 {
        return ExitCode::from(1);
    }

    ExitCode::SUCCESS
}
