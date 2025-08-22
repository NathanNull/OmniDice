use std::{fs, io::{Read, Write}, path::Path};

use serde_cbor::{from_slice, to_vec};
use sha2::{Digest, Sha256};

use crate::interpreter::parser::Expr;

pub fn try_load_cache(code: &str, path: &Path) -> Option<Box<Expr>> {
    let mut hasher = Sha256::new();
    hasher.update(code.as_bytes());
    let code_hash = hasher.finalize().to_vec();

    let mut cache_path = path.canonicalize().unwrap().parent().unwrap().to_path_buf();
    cache_path.push("_compiled");
    let mut filename = path.file_stem().unwrap().to_os_string();
    filename.push(".odc");
    cache_path.push(filename);

    // Use the cached AST if the code's hash matches the old one
    match fs::File::open(cache_path.clone()) {
        Ok(mut file) => {
            let mut old_hash = [0u8; 32];
            file.read_exact(&mut old_hash)
                .expect("Couldn't read cached code hash");
            if old_hash == *code_hash {
                let mut buf = vec![];
                file.read_to_end(&mut buf)
                    .expect("Couldn't read cached AST");
                match from_slice(&buf) {
                    Ok(ret) => Some(ret),
                    Err(err) => panic!("Failed to deserialize: {err:#?}"),
                }
            } else {
                None
            }
        }
        Err(_) => None,
    }
}

pub fn write_cache(code: &str, path: &Path, ast: Box<Expr>) {
    let mut hasher = Sha256::new();
    hasher.update(code.as_bytes());
    let code_hash = hasher.finalize().to_vec();

    let mut cache_path = path.canonicalize().unwrap().parent().unwrap().to_path_buf();
    cache_path.push("_compiled");
    let mut filename = path.file_stem().unwrap().to_os_string();
    filename.push(".odc");
    cache_path.push(filename);

    match cache_path.parent() {
        Some(parent) => fs::create_dir_all(parent).expect("Directory creation shouldn't fail"),
        None => (),
    }
    match fs::File::create(cache_path) {
        Ok(mut file) => {
            file.write_all(&code_hash)
                .expect("File writing shouldn't fail");
            file.write_all(
                &to_vec(&ast)
                    .map_err(|e| e.to_string())
                    .expect("Serialization shouldn't fail"),
            )
            .expect("File writing shouldn't fail");
        }
        Err(_) => println!("Couldn't cache AST"),
    }
}
