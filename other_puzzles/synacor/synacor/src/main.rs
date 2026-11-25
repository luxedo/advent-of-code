use std::{error::Error, fs::File, io::Read as _};

use synacor::VM;

fn read_file(path: &str) -> Result<Vec<u8>, Box<dyn Error>> {
    let f = File::open(path)?;
    let mut buffer = Vec::new();

    // Using the same limit logic you had before
    let limit = (VM::ADDRESS_SPACE * 2) as u64;
    f.take(limit).read_to_end(&mut buffer)?;

    Ok(buffer)
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 3 {
        panic!("Usage: cargo run -- <run|diss> <path_to_binary>");
    }
    let command = &args[1];
    let bin = &args[2];
    let program = read_file(bin)?;
    let vm = VM::new(program);
    match command.as_ref() {
        "run" => for () in vm {},
        "diss" => vm.diss(),
        _ => {
            panic!("Command not found");
        }
    }

    Ok(())
}
