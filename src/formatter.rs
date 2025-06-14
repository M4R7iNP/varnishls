use std::error::Error;
use topiary_core::{formatter, Language, Operation, TopiaryQuery};
use tree_sitter_vcl;

use crate::config::FormatterConfig;

static QUERY_MAIN: &str = include_str!("./formatter_queries/main.scm");
static QUERY_IFS_LOOSE: &str = include_str!("./formatter_queries/ifs_loose.scm");
static QUERY_IFS_TIGHT: &str = include_str!("./formatter_queries/ifs_tight.scm");
static QUERY_AUTOFIX_ELSE_IFS: &str = include_str!("./formatter_queries/autofix_else_if.scm");
static QUERY_DOWNCASE_HEADERS: &str = include_str!("./formatter_queries/downcase_headers.scm");

pub fn format(
    input: String,
    config: &FormatterConfig,
) -> Result<String, Box<dyn Error + Send + Sync>> {
    if !config.enabled {
        return Err("Formatting disabled".into());
    }

    let mut query = QUERY_MAIN.to_string();
    let query_large_ifs = match config.format_large_ifs_style {
        crate::config::FormatIfStyle::Loose => QUERY_IFS_LOOSE,
        crate::config::FormatIfStyle::Tight => QUERY_IFS_TIGHT,
    };
    query.push_str(query_large_ifs);
    if config.autofix_else_ifs {
        query.push_str(QUERY_AUTOFIX_ELSE_IFS);
    }
    if config.downcase_headers {
        query.push_str(QUERY_DOWNCASE_HEADERS);
    }

    let mut output = vec![];
    let mut input_bytes = input.as_bytes();

    let ts_grammar = tree_sitter_vcl::LANGUAGE;
    let language = Language {
        name: "vcl".to_owned(),
        query: TopiaryQuery::new(&ts_grammar.into(), &query).unwrap(),
        grammar: ts_grammar.into(),
        indent: Some(config.indent_size.to_string()),
    };

    match formatter(
        &mut input_bytes,
        &mut output,
        &language,
        Operation::Format {
            skip_idempotence: false,
            // tolerate_parsing_errors: cfg!(not(debug_assertions)),
            tolerate_parsing_errors: false,
        },
    ) {
        Ok(()) => Ok(String::from_utf8(output)?),
        Err(err) => Result::Err(
            format!(
                "Failed to format. Error: {}",
                err.source()
                    .map(|source| source.to_string())
                    .unwrap_or(format!("🤷 ({err:?})"))
            )
            .into(),
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::{assert_snapshot, glob};
    use std::fs;

    #[test]
    fn snapshots() {
        glob!("formatter_tests/*.vcl", |input_path| {
            let input = fs::read_to_string(input_path).unwrap();
            let config: FormatterConfig = match input_path.file_name().unwrap().to_str().unwrap() {
                "004_large_ifs_tight.vcl" => FormatterConfig {
                    format_large_ifs_style: crate::config::FormatIfStyle::Tight,
                    ..Default::default()
                },
                "006_autofix_else_if.vcl" => FormatterConfig {
                    autofix_else_ifs: true,
                    ..Default::default()
                },
                _ => FormatterConfig::default(),
            };
            assert_snapshot!(format(input, &config).unwrap());
        });
    }
}
