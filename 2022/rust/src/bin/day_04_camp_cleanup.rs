/*
 * Advent Of Code
 * https://adventofcode.com/2022/day/4
 *
 * -- Day 4: Camp Cleanup ---
 * Space needs to be cleared before the last supplies can be unloaded from the ships, and so several Elves have been assigned the job of cleaning up sections of the camp. Every section has a unique ID number, and each Elf is assigned a range of section IDs.
 *
 * However, as some of the Elves compare their section assignments with each other, they've noticed that many of the assignments overlap. To try to quickly find overlaps and reduce duplicated effort, the Elves pair up and make a big list of the section assignments for each pair (your puzzle input).
 *
 * For example, consider the following list of section assignment pairs:
 *
 * 2-4,6-8
 * 2-3,4-5
 * 5-7,7-9
 * 2-8,3-7
 * 6-6,4-6
 * 2-6,4-8
 * For the first few pairs, this list means:
 *
 * Within the first pair of Elves, the first Elf was assigned sections 2-4 (sections 2, 3, and 4), while the second Elf was assigned sections 6-8 (sections 6, 7, 8).
 * The Elves in the second pair were each assigned two sections.
 * The Elves in the third pair were each assigned three sections: one got sections 5, 6, and 7, while the other also got 7, plus 8 and 9.
 * This example list uses single-digit section IDs to make it easier to draw; your actual list might contain larger numbers. Visually, these pairs of section assignments look like this:
 *
 * .234.....  2-4
 * .....678.  6-8
 *
 * .23......  2-3
 * ...45....  4-5
 *
 * ....567..  5-7
 * ......789  7-9
 *
 * .2345678.  2-8
 * ..34567..  3-7
 *
 * .....6...  6-6
 * ...456...  4-6
 *
 * .23456...  2-6
 * ...45678.  4-8
 * Some of the pairs have noticed that one of their assignments fully contains the other. For example, 2-8 fully contains 3-7, and 6-6 is fully contained by 4-6. In pairs where one assignment fully contains the other, one Elf in the pair would be exclusively cleaning sections their partner will already be cleaning, so these seem like the most in need of reconsideration. In this example, there are 2 such pairs.
 *
 * In how many assignment pairs does one range fully contain the other?
 *
 * --- Part Two ---
 * It seems like there is still quite a bit of duplicate work planned. Instead, the Elves would like to know the number of pairs that overlap at all.
 *
 * In the above example, the first two pairs (2-4,6-8 and 2-3,4-5) don't overlap, while the remaining four pairs (5-7,7-9, 2-8,3-7, 6-6,4-6, and 2-6,4-8) do overlap:
 *
 * 5-7,7-9 overlaps in a single section, 7.
 * 2-8,3-7 overlaps all of the sections 3 through 7.
 * 6-6,4-6 overlaps in a single section, 6.
 * 2-6,4-8 overlaps in sections 4, 5, and 6.
 * So, in this example, the number of overlapping assignment pairs is 4.
 */

use aoc_rust::load_input;
use std::error::Error;
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

fn solve_pt1(input_text: &str) -> u64 {
    solve(input_text, fully_contains)
}

fn solve_pt2(input_text: &str) -> u64 {
    solve(input_text, any_overlap)
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_04_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {}", solve_pt1(&input_text));
    // Correct: 515

    println!("Part two: {}", solve_pt2(&input_text));
    // Correct: 883

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_rust::test_solution;

    const TEST_DATA: &str = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8";

    test_solution!(test1, solve_pt1, 2, TEST_DATA);
    test_solution!(test2, solve_pt2, 4, TEST_DATA);
}
