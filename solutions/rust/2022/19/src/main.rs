/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 19
* Rust Solution
*
* Day 19: Not Enough Minerals
*
* https://adventofcode.com/2022/day/19
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    Ok(25)
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    Ok("December")
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)?;
    Ok(())
}
