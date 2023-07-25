// Helper library for AOC 2022

use std::fs::{metadata, read_to_string};
use std::path::Path;

pub fn load_input(filename: &str) -> String {
    match metadata(Path::new(&filename)) {
        Ok(_) => read_to_string(filename)
            .expect("Failed reading file {filename}")
            .parse::<String>()
            .unwrap()
            .trim()
            .to_string(),
        Err(_) => panic!("Input file {filename} not found"),
    }
}

#[macro_export]
macro_rules! test_solution {
    ($name:ident, $func:ident, $expected:expr, $input:expr) => {
        #[test]
        fn $name() {
            assert_eq!($expected, super::$func($input));
        }
    };
    ($name:ident, $func:ident, $expected:expr, $input:expr, $args:expr) => {
        #[test]
        fn $name() {
            assert_eq!($expected, super::$func($input, $args));
        }
    };
}
