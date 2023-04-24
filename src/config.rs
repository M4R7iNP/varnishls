use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Config {
    #[serde(default)]
    pub main_vcl: Option<PathBuf>,
    #[serde(default)]
    pub vmod_paths: Vec<PathBuf>,
    #[serde(default)]
    pub vcc_paths: Vec<PathBuf>,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            main_vcl: None,
            vmod_paths: vec![
                // ubuntu, debian
                "/usr/lib/x86_64-linux-gnu/varnish/vmods/".into(),
                // fedora, centos
                "/usr/lib64/varnish/vmods/".into(),
                // freebsd
                "/usr/local/lib/varnish/vmods".into(),
                // arch
                "/usr/lib/varnish/vmods".into(),
            ],
            vcc_paths: vec![],
        }
    }
}
