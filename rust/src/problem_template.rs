/* Advent Of Code 2016
 * {url}
 *
{description}
*/

use aoc2016::load_input;
use std::error::Error;

fn solve_pt1(input_text: &str) -> u64 {
    0
}

fn solve_pt2(input_text: &str) -> u64 {
    1
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_{day}_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {:#?}", solve_pt1(&input_text));
    // solution_pt1: ???

    println!("Part two: {:#?}", solve_pt2(&input_text));
    // solution_pt2: ???

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc2016::test_solution;

    test_solution!(test1, solve_pt1, 5, "Your data here");
    test_solution!(test2, solve_pt2, 4, "Another data");
}
