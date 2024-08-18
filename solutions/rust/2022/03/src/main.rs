/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 03
* Rust Solution
*
* Day 3: Rucksack Reorganization
*
* https://adventofcode.com/2022/day/3
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::convert::TryInto;
use std::fmt::Display;
use std::result::Result;

fn split_middle(s: &str) -> (&str, &str) {
    let length: usize = s.len();
    let middle: usize = (length / 2).try_into().unwrap();
    (&s[0..middle], &s[middle..length])
}

fn string_intersection((str_1, str_2): (String, String)) -> String {
    let mut str_1_clone = str_1.chars().collect::<Vec<char>>();
    let mut str_2_clone = str_2.chars().collect::<Vec<char>>();
    str_1_clone.sort();
    str_2_clone.sort();
    str_1_clone.dedup();
    str_2_clone.dedup();

    str_1_clone
        .into_iter()
        .filter(|c| str_2_clone.contains(c))
        .collect()
}

fn parse_priority(item: char) -> u32 {
    match item {
        'a'..='z' => item as u32 - 'a' as u32 + 1,
        'A'..='Z' => item as u32 - 'A' as u32 + 27,
        _ => todo!(),
    }
}

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let answer = input_data
        .lines()
        .map(split_middle)
        .map(|(s1, s2)| string_intersection((s1.to_string(), s2.to_string())))
        .map(|items| items.chars().map(parse_priority).sum::<u32>())
        .sum::<u32>();
    Ok(answer)
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    const ELVES: usize = 3;
    let answer = input_data
        .lines()
        .collect::<Vec<&str>>()
        .chunks(ELVES)
        .map(|sacks| {
            sacks
                .iter()
                .map(|s| s.to_string())
                .reduce(|acc, item| string_intersection((acc, item)))
                .unwrap()
        })
        .map(|items| items.chars().map(parse_priority).sum::<u32>())
        .sum::<u32>();
    Ok(answer)
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)?;
    Ok(())
}
