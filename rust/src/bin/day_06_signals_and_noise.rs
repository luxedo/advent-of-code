/* Advent Of Code 2016
 * https://adventofcode.com/2016/day/6
 *
 * --- Day 6: Signals and Noise ---
 * Something is jamming your communications with Santa. Fortunately, your signal is only partially jammed, and protocol in situations like this is to switch to a simple repetition code to get the message through.
 *
 * In this model, the same message is sent repeatedly.  You've recorded the repeating message signal (your puzzle input), but the data seems quite corrupted - almost too badly to recover. Almost.
 *
 * All you need to do is figure out which character is most frequent for each position. For example, suppose you had recorded the following messages:
 * eedadn
 * drvtee
 * eandsr
 * raavrd
 * atevrs
 * tsrnev
 * sdttsa
 * rasrtv
 * nssdts
 * ntnada
 * svetve
 * tesnvt
 * vntsnd
 * vrdear
 * dvrsen
 * enarar
 *
 *
 * The most common character in the first column is e; in the second, a; in the third, s, and so on. Combining these characters returns the error-corrected message, easter.
 *
 * Given the recording in your puzzle input, what is the error-corrected version of the message being sent?
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
    const FILENAME: &str = "../data/day_06_input.txt";
    let input_text = load_input(FILENAME);

    print!("Part one: {:#?}\n", solve_pt1(&input_text));
    // solution_pt1: ???

    print!("Part two: {:#?}\n", solve_pt2(&input_text));
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
