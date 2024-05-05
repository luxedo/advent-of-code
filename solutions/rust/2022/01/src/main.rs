/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 01
* Rust Solution
*
* Day 1: Calorie Counting
*
* https://adventofcode.com/2022/day/1
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;

fn solve(input_data: &str, elves: usize) -> u64 {
    let mut calories = input_data
        .split("\n\n")
        .map(|elf| {
            elf.split("\n")
                .map(|elf_box| elf_box.parse::<u64>().unwrap_or(0))
                .sum()
        })
        .collect::<Vec<u64>>();
    calories.sort_by(|a, b| b.cmp(a));
    calories[0..elves].iter().sum()
}

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    Ok(solve(input_data, 1))
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    Ok(solve(input_data, 3))
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)
}
