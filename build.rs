fn main() {
    let out_dir = std::env::var_os("OUT_DIR").unwrap();
    println!("cargo:rerun-if-changed=src/shader");
    for entry in std::fs::read_dir("src/shader").unwrap() {
        let entry = entry.unwrap();
        if entry.file_type().unwrap().is_file() {
            if let Some(Some(extension)) = entry.path().extension().map(|extension| extension.to_str()) {
                let out_path =
                    std::path::Path::new(&out_dir).join(format!("{}_{}.spv", entry.path().file_stem().unwrap().to_str().unwrap(), extension));
                std::process::Command::new("glslc")
                    .args(&[
                        format!("-fshader-stage={}", extension),
                        "-o".to_string(),
                        out_path.to_str().unwrap().to_string(),
                        entry.path().to_str().unwrap().to_string(),
                    ])
                    .status()
                    .unwrap();
            }
        }
    }
}
