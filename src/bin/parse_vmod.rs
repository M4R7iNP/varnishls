use std::{error::Error, path::PathBuf};

use varnish_lsp::vmod;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = std::env::args().collect();
    let vmod_name = args.get(1).expect("Missing vmod name");
    let vmod_path = args.get(2).expect("Missing vmod path");

    let vmod = vmod::read_vmod_lib(vmod_name.to_string(), PathBuf::from(vmod_path)).await?;

    println!("VMOD: {:?}", vmod);
    println!("VMOD JSON: {}", vmod.json);

    Ok(())
}
