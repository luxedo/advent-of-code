/* Advent Of Code 2016 - day 5
 * https://adventofcode.com/2016/day/5
 *
 * --- Day 5: How About a Nice Game of Chess? ---
 * You are faced with a security door designed by Easter Bunny engineers that seem to have acquired most of their security knowledge by watching hacking movies.
 *
 * The eight-character password for the door is generated one character at a time by finding the MD5 hash of some Door ID (your puzzle input) and an increasing integer index (starting with 0).
 *
 * A hash indicates the next character in the password if its hexadecimal representation starts with five zeroes. If it does, the sixth character in the hash is the next character of the password.
 *
 * For example, if the Door ID is abc:
 *
 * The first index which produces a hash that starts with five zeroes is 3231929, which we find by hashing abc3231929; the sixth character of the hash, and thus the first character of the password, is 1.
 * 5017308 produces the next interesting hash, which starts with 000008f82..., so the second character of the password is 8.
 * The third time a hash starts with five zeroes is for abc5278568, discovering the character f.
 *
 *
 * In this example, after continuing this search a total of eight times, the password is 18f47a30.
 *
 * Given the actual Door ID, what is the password?
 *
 *
 * --- Part Two ---
 * You notice that the screen is only capable of displaying capital letters; in the font it uses, each letter is 5 pixels wide and 6 tall.
 *
 * After you swipe your card, what code is the screen trying to display?
*/

use aoc_lang_rust::load_input;
use std::error::Error;

use md5::{Digest, Md5};

fn solve_pt1(input_text: &str) -> String {
    const MAX_LEN: usize = 8;
    let mut key = "".to_string();
    let mut i = 0;
    while key.len() < MAX_LEN {
        let mut hasher = Md5::new();
        let attempt = input_text.to_owned() + &i.to_string();
        hasher.update(attempt.as_bytes());
        let hex = format!("{:02x}", &hasher.finalize());
        if hex.starts_with("00000") {
            key.push(hex.chars().nth(5).unwrap())
        }
        i += 1;
    }
    key
}

fn solve_pt2(input_text: &str) -> String {
    const MAX_LEN: usize = 8;
    let mut key: [char; MAX_LEN] = ['\0'; MAX_LEN];
    let mut i = 0;
    while key.into_iter().filter(|k| *k == '\0').count() != 0 {
        let mut hasher = Md5::new();
        let attempt = input_text.to_owned() + &i.to_string();
        hasher.update(attempt.as_bytes());
        let hex = format!("{:02x}", &hasher.finalize());
        if hex.starts_with("00000") {
            let idx = usize::from_str_radix(&hex.chars().nth(5).unwrap().to_string(), 16).unwrap();
            let value = hex.chars().nth(6).unwrap();
            if idx < MAX_LEN && key[idx] == '\0' {
                key[idx] = value;
            }
        }
        i += 1;
    }
    key.iter().collect()
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_05_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {:#?}", solve_pt1(&input_text));
    // solution_pt1: f97c354d

    println!("Part two: {:#?}", solve_pt2(&input_text));
    // solution_pt2: 863dde27

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_lang_rust::test_solution;

    // test_solution!(test1, solve_pt1, "18f47a30", "abc");
    test_solution!(test2, solve_pt2, "05ace8e3", "abc");
}
