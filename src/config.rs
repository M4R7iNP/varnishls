use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Clone, Debug, Deserialize, Serialize, Default)]
pub struct Config {
    pub main_vcl: Option<PathBuf>,
    pub vmod_paths: Vec<PathBuf>,
}
