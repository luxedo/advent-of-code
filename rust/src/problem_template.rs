// Advent Of Code
// https://adventofcode.com/2022/day/4
//
//  --- PROBLEM DESCRIPTION ---

use aoc2016::load_input;
use std::error::Error;

fn solve_pt1(input_text: &str) -> u64 {
    0
}

fn solve_pt2(input_text: &str) -> u64 {
    1
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_X_input.txt";
    let input_text = load_input(FILENAME);

    print!("Part one: {:#?}\n", solve_pt1(&input_text));
    // Correct: AAA

    print!("Part two: {:#?}\n", solve_pt2(&input_text));
    // Correct: BBB

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc2016::test_solution;

    test_solution!(test1, solve_pt1, 5, "Your data here");
    test_solution!(test4, solve_pt2, 4, "Another data");
}
