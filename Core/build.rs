use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    cbindgen::generate(&crate_dir)
        .expect("Unable to generate header file")
        .write_to_file("./include/libimago.h");
}
