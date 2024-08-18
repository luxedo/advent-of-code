/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 06
* Rust Solution
*
* Day 6: Tuning Trouble
*
* https://adventofcode.com/2022/day/6
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;

fn solve(input_text: &str, window_size: usize) -> u64 {
    (input_text
        .as_bytes()
        .windows(window_size)
        .position(|w| {
            let mut window = w.iter().collect::<Vec<_>>().clone();
            window.sort();
            window.dedup();
            window.len() == window_size
        })
        .unwrap()
        + window_size) as u64
}
fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let window_size: usize = 4;
    let answer = solve(input_data, window_size);
    Ok(answer)
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let window_size: usize = 14;
    let answer = solve(input_data, window_size);
    Ok(answer)
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)?;
    Ok(())
}
