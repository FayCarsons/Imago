use std::env;
use std::fs;
use std::path::Path;

fn go() -> std::io::Result<()> {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let target_dir = Path::new(&crate_dir).join("target/release");

    cbindgen::generate(&crate_dir)
        .expect("Unable to generate header file")
        .write_to_file("../cbits/libimago.h");

    let lib_name = if cfg!(target_os = "windows") {
        "imago.lib"
    } else if cfg!(target_os = "macos") {
        "libimago.dylib"
    } else {
        "libimago.so"
    };

    let src = target_dir.join(lib_name);
    let dst = Path::new("../cbits").join(lib_name);

    if src.exists() {
        fs::create_dir_all("../cbits")?;
        fs::copy(&src, &dst)?;
        println!("Copied executable {src:?} to {dst:?}");
    }

    Ok(())
}

fn main() {
    go().unwrap()
}
