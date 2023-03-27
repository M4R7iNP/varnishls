use std::path::PathBuf;

fn main() {
    // println!("cargo:rustc-link-lib=varnishapi");

    let dir: PathBuf = ["src"].iter().collect();

    cc::Build::new()
        .include(&dir)
        .file(dir.join("parser.c"))
        // .file(dir.join("scanner.c"))
        .compile("tree-sitter-vcl");

}
