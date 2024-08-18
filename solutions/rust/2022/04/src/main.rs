/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 04
* Rust Solution
*
* Day 4: Camp Cleanup
*
* https://adventofcode.com/2022/day/4
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;
use std::ops::Range;

fn fully_contains(r0: &Range<u64>, r1: &Range<u64>) -> bool {
    (r0.contains(&r1.start) && r0.contains(&(r1.end - 1)))
        || (r1.contains(&r0.start) && r1.contains(&(r0.end - 1)))
}

fn any_overlap(r0: &Range<u64>, r1: &Range<u64>) -> bool {
    r0.contains(&r1.start)
        || r0.contains(&(r1.end - 1))
        || r1.contains(&r0.start)
        || r1.contains(&(r0.end - 1))
}

fn solve(input_text: &str, overlap_fn: fn(&Range<u64>, &Range<u64>) -> bool) -> u64 {
    input_text
        .trim()
        .lines()
        .map(|line| {
            line.split(",")
                .map(|elf_ids| {
                    elf_ids
                        .split("-")
                        .map(|id| id.parse::<u64>().unwrap())
                        .collect::<Vec<_>>()
                })
                .map(|elf_ids| Range {
                    start: elf_ids[0],
                    end: elf_ids[1] + 1, // Ranges excludes the end value
                })
                .collect::<Vec<_>>()
        })
        .filter(|ranges| overlap_fn(&ranges[0], &ranges[1]))
        .count() as u64
}
fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let answer = solve(input_data, fully_contains);
    Ok(answer)
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let answer = solve(input_data, any_overlap);
    Ok(answer)
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)?;
    Ok(())
}
