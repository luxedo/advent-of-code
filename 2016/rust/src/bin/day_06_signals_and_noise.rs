/* Advent Of Code 2016 - day 6
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
 *
 * --- Part Two ---
 * Of course, that would be the message - if you hadn't agreed to use a modified repetition code instead.
 *
 * In this modified code, the sender instead transmits what looks like random data, but for each character, the character they actually want to send is slightly less likely than the others. Even after signal-jamming noise, you can look at the letter distributions in each column and choose the least common letter to reconstruct the original message.
 *
 * In the above example, the least common character in the first column is a; in the second, d, and so on. Repeating this process for the remaining characters produces the original message, advent.
 *
 * Given the recording in your puzzle input and this new decoding methodology, what is the original message that Santa is trying to send?
*/

use aoc_lang_rust::load_input;
use std::collections::BTreeMap;
use std::error::Error;

fn solve_pt1(input_text: &str) -> String {
    let word_len = input_text.find('\n').unwrap();
    let mut counters = vec![BTreeMap::<char, u32>::new(); word_len];
    input_text.lines().for_each(|line| {
        line.chars().enumerate().for_each(|(i, c)| {
            *counters[i].entry(c).or_insert(0) += 1;
        })
    });
    counters
        .iter()
        .map(|counter| {
            *counter
                .iter()
                .max_by(|i, j| i.1.partial_cmp(j.1).unwrap())
                .unwrap()
                .0
        })
        .collect()
}

fn solve_pt2(input_text: &str) -> String {
    let word_len = input_text.find('\n').unwrap();
    let mut counters = vec![BTreeMap::<char, u32>::new(); word_len];
    input_text.lines().for_each(|line| {
        line.chars().enumerate().for_each(|(i, c)| {
            *counters[i].entry(c).or_insert(0) += 1;
        })
    });
    counters
        .iter()
        .map(|counter| {
            *counter
                .iter()
                .min_by(|i, j| i.1.partial_cmp(j.1).unwrap())
                .unwrap()
                .0
        })
        .collect()
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_06_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {:#?}", solve_pt1(&input_text));
    // solution_pt1: umejzgdw

    println!("Part two: {:#?}", solve_pt2(&input_text));
    // solution_pt2: aovueakv

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_lang_rust::test_solution;
    const INPUT_DATA: &str = "eedadn\n\
drvtee\n\
eandsr\n\
raavrd\n\
atevrs\n\
tsrnev\n\
sdttsa\n\
rasrtv\n\
nssdts\n\
ntnada\n\
svetve\n\
tesnvt\n\
vntsnd\n\
vrdear\n\
dvrsen\n\
enarar";

    test_solution!(test1, solve_pt1, "easter", INPUT_DATA);
    test_solution!(test2, solve_pt2, "advent", INPUT_DATA);
}
