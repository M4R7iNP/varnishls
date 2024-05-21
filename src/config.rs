use serde::{
    de::{self, Visitor},
    Deserialize, Deserializer, Serialize,
};
use std::fmt;
use std::path::PathBuf;
use tower_lsp::lsp_types::DiagnosticSeverity;

fn default_vcl_paths() -> Vec<PathBuf> {
    vec!["./".into()]
}

fn default_vmod_paths() -> Vec<PathBuf> {
    vec![
        // ubuntu, debian
        "/usr/lib/x86_64-linux-gnu/varnish/vmods/".into(),
        // fedora, centos
        "/usr/lib64/varnish/vmods/".into(),
        // freebsd
        "/usr/local/lib/varnish/vmods".into(),
        // arch
        "/usr/lib/varnish/vmods".into(),
        // macos homebrew
        "/opt/homebrew/usr/lib/varnish/vmods".into(),
    ]
}

fn default_vcc_paths() -> Vec<PathBuf> {
    let default_from_env = std::env::var("VARNISHLS_VCC_PATHS")
        .map(|env_str| env_str.split(';').map(Into::into).collect::<Vec<PathBuf>>())
        .ok();

    if let Some(default_from_env) = default_from_env {
        return default_from_env;
    }

    vec![
        // fedora
        "/usr/share/varnish/vcc".into(),
        // macos homebrew
        "/opt/homebrew/share/varnish/vcc".into(),
    ]
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Config {
    #[serde(default)]
    pub main_vcl: Option<PathBuf>,
    #[serde(default = "default_vcl_paths")]
    pub vcl_paths: Vec<PathBuf>,
    #[serde(default = "default_vmod_paths")]
    pub vmod_paths: Vec<PathBuf>,
    #[serde(default = "default_vcc_paths")]
    pub vcc_paths: Vec<PathBuf>,
    #[serde(default)]
    pub lint: LintConfig,
    #[serde(default)]
    pub formatter: FormatterConfig,
}

impl Default for Config {
    fn default() -> Self {
        toml::from_str::<Config>("").unwrap()
    }
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum LintLevel {
    // Enabled,
    Disabled,
    Hint,
    Info,
    Warning,
    Error,
}

impl LintLevel {
    pub fn is_enabled(&self) -> bool {
        !matches!(self, Self::Disabled)
    }
    fn disabled() -> Self {
        Self::Disabled
    }
    fn hint() -> Self {
        Self::Hint
    }
    pub fn lsp_severity(&self) -> Option<DiagnosticSeverity> {
        match self {
            LintLevel::Disabled => None,
            LintLevel::Hint => Some(DiagnosticSeverity::HINT),
            LintLevel::Info => Some(DiagnosticSeverity::INFORMATION),
            LintLevel::Warning => Some(DiagnosticSeverity::WARNING),
            LintLevel::Error => Some(DiagnosticSeverity::ERROR),
        }
    }
}

impl<'de> de::Deserialize<'de> for LintLevel {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct LintLevelVisitor;

        impl<'de> Visitor<'de> for LintLevelVisitor {
            type Value = LintLevel;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a string representing a lint level (error, warning, or hint), or false for disabled linting rule")
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                if v {
                    Err(de::Error::invalid_value(de::Unexpected::Bool(v), &self))
                    // Ok(LintLevel::Hint) // TODO: maybe find a fitting default
                } else {
                    Ok(LintLevel::Disabled)
                }
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v.to_lowercase().as_str() {
                    "error" | "err" => Ok(LintLevel::Error),
                    "warning" | "warn" => Ok(LintLevel::Warning),
                    "hint" => Ok(LintLevel::Hint),
                    "info" => Ok(LintLevel::Info),
                    _ => Err(de::Error::invalid_value(de::Unexpected::Str(v), &self)),
                }
            }
        }

        deserializer.deserialize_str(LintLevelVisitor)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct LintConfig {
    #[serde(default = "LintLevel::disabled")]
    pub no_rewrite_req_url: LintLevel,
    #[serde(default = "LintLevel::hint")]
    pub prefer_else_if: LintLevel,
    #[serde(default = "LintLevel::hint")]
    pub prefer_lowercase_headers: LintLevel,
    #[serde(default = "LintLevel::disabled")]
    pub prefer_custom_headers_without_prefix: LintLevel,
}

impl Default for LintConfig {
    fn default() -> Self {
        LintConfig {
            no_rewrite_req_url: LintLevel::Disabled,
            prefer_else_if: LintLevel::Hint,
            prefer_lowercase_headers: LintLevel::Hint,
            prefer_custom_headers_without_prefix: LintLevel::Disabled,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, Default)]
pub struct FormatterConfig {
    #[serde(default)]
    pub indent_size: IndentSize,
    #[serde(default)]
    pub format_large_ifs_style: FormatIfStyle,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(untagged)]
pub enum IndentSize {
    Number(u8),
    // TODO: FIXME:
    Tab,
}

impl Default for IndentSize {
    fn default() -> Self {
        return IndentSize::Number(4);
    }
}

impl ToString for IndentSize {
    fn to_string(&self) -> String {
        match self {
            Self::Number(n) => " ".repeat(*n as usize),
            Self::Tab => "\t".to_string(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum FormatIfStyle {
    Tight,
    Loose,
}

impl Default for FormatIfStyle {
    fn default() -> Self {
        return Self::Loose; // Default in C code style guides
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_config_defaults(config: Config) {
        assert_eq!(
            config.vcl_paths,
            vec![PathBuf::from("./")],
            "vcl_paths should default to ./"
        );
        assert!(
            !config.vmod_paths.is_empty(),
            "vmod_paths should not be empty"
        );
    }

    #[test]
    fn partial_config_with_defaults() {
        let toml_str = r#"
            main_vcl = "martin.vcl"
            vcc_paths = ["./vcc-files"]
        "#;

        let config: Config = toml::from_str(toml_str).unwrap();
        println!("{:?}", config);
        assert_config_defaults(config);
    }

    #[test]
    fn empty_config_with_defaults() {
        let config = Config::default();
        println!("{:?}", config);
        assert_config_defaults(config);
    }

    #[test]
    fn can_parse_lint_config() {
        let toml_str = r#"
            no_rewrite_req_url = "hint"
            prefer_else_if = "warning"
            prefer_lowercase_headers = "info"
            prefer_custom_headers_without_prefix = false
        "#;

        let parsed: LintConfig = toml::from_str(toml_str).unwrap();
        println!("{:?}", parsed);
        assert_eq!(parsed.no_rewrite_req_url, LintLevel::Hint);
        assert_eq!(parsed.prefer_else_if, LintLevel::Warning);
        assert_eq!(parsed.prefer_lowercase_headers, LintLevel::Info);
        assert_eq!(
            parsed.prefer_custom_headers_without_prefix,
            LintLevel::Disabled
        );
    }
}
