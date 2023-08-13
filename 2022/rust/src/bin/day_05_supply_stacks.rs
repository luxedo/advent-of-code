/*
 * Advent Of Code
 * https://adventofcode.com/2022/day/4
 *
 *  --- Day 5: Supply Stacks ---
 * The expedition can depart as soon as the final supplies have been unloaded from the ships. Supplies are stored in stacks of marked crates, but because the needed supplies are buried under many other crates, the crates need to be rearranged.
 *
 * The ship has a giant cargo crane capable of moving crates between stacks. To ensure none of the crates get crushed or fall over, the crane operator will rearrange them in a series of carefully-planned steps. After the crates are rearranged, the desired crates will be at the top of each stack.
 *
 * The Elves don't want to interrupt the crane operator during this delicate procedure, but they forgot to ask her which crate will end up where, and they want to be ready to unload them as soon as possible so they can embark.
 *
 * They do, however, have a drawing of the starting stacks of crates and the rearrangement procedure (your puzzle input). For example:
 *
 *     [D]
 * [N] [C]
 * [Z] [M] [P]
 *  1   2   3
 *
 * move 1 from 2 to 1
 * move 3 from 1 to 3
 * move 2 from 2 to 1
 * move 1 from 1 to 2
 * In this example, there are three stacks of crates. Stack 1 contains two crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains three crates; from bottom to top, they are crates M, C, and D. Finally, stack 3 contains a single crate, P.
 *
 * Then, the rearrangement procedure is given. In each step of the procedure, a quantity of crates is moved from one stack to a different stack. In the first step of the above rearrangement procedure, one crate is moved from stack 2 to stack 1, resulting in this configuration:
 *
 * [D]
 * [N] [C]
 * [Z] [M] [P]
 *  1   2   3
 * In the second step, three crates are moved from stack 1 to stack 3. Crates are moved one at a time, so the first crate to be moved (D) ends up below the second and third crates:
 *
 *         [Z]
 *         [N]
 *     [C] [D]
 *     [M] [P]
 *  1   2   3
 * Then, both crates are moved from stack 2 to stack 1. Again, because crates are moved one at a time, crate C ends up below crate M:
 *
 *         [Z]
 *         [N]
 * [M]     [D]
 * [C]     [P]
 *  1   2   3
 * Finally, one crate is moved from stack 1 to stack 2:
 *
 *         [Z]
 *         [N]
 *         [D]
 * [C] [M] [P]
 *  1   2   3
 * The Elves just need to know which crate will end up on top of each stack; in this example, the top crates are C in stack 1, M in stack 2, and Z in stack 3, so you should combine these together and give the Elves the message CMZ.
 *
 * After the rearrangement procedure completes, what crate ends up on top of each stack?
 *
 * --- Part Two ---
 * As you watch the crane operator expertly rearrange the crates, you notice the process isn't following your prediction.
 *
 * Some mud was covering the writing on the side of the crane, and you quickly wipe it away. The crane isn't a CrateMover 9000 - it's a CrateMover 9001.
 *
 * The CrateMover 9001 is notable for many new and exciting features: air conditioning, leather seats, an extra cup holder, and the ability to pick up and move multiple crates at once.
 *
 * Again considering the example above, the crates begin in the same configuration:
 *
 *     [D]
 * [N] [C]
 * [Z] [M] [P]
 *  1   2   3
 * Moving a single crate from stack 2 to stack 1 behaves the same as before:
 *
 * [D]
 * [N] [C]
 * [Z] [M] [P]
 *  1   2   3
 * However, the action of moving three crates from stack 1 to stack 3 means that those three moved crates stay in the same order, resulting in this new configuration:
 *
 *         [D]
 *         [N]
 *     [C] [Z]
 *     [M] [P]
 *  1   2   3
 * Next, as both crates are moved from stack 2 to stack 1, they retain their order as well:
 *
 *         [D]
 *         [N]
 * [C]     [Z]
 * [M]     [P]
 *  1   2   3
 * Finally, a single crate is still moved from stack 1 to stack 2, but now it's crate C that gets moved:
 *
 *         [D]
 *         [N]
 *         [Z]
 * [M] [C] [P]
 *  1   2   3
 * In this example, the CrateMover 9001 has put the crates in a totally different order: MCD.
 *
 * Before the rearrangement process finishes, update your simulation so that the Elves know where they should stand to be ready to unload the final supplies. After the rearrangement procedure completes, what crate ends up on top of each stack?
 */
use aoc_rust::load_input;
use std::error::Error;

#[derive(Debug)]
struct Move {
    boxes: usize,
    from: usize,
    to: usize,
}

fn crane9000(mut boxes: Vec<Vec<char>>, moves: &Vec<Move>) -> Vec<Vec<char>> {
    moves.iter().for_each(|mv| {
        (0..mv.boxes).for_each(|_| {
            let bx = boxes[mv.from].pop().unwrap();
            boxes[mv.to].push(bx);
        })
    });
    boxes
}

fn crane9001(mut boxes: Vec<Vec<char>>, moves: &Vec<Move>) -> Vec<Vec<char>> {
    moves.iter().for_each(|mv| {
        let slice_start = boxes[mv.from].len() - mv.boxes;
        let bxs = boxes[mv.from].drain(slice_start..).collect::<Vec<_>>();
        boxes[mv.to].extend(bxs);
    });
    boxes
}

fn solve_pt1(input_text: &str) -> String {
    solve(input_text, crane9000)
}

fn solve_pt2(input_text: &str) -> String {
    solve(input_text, crane9001)
}

fn solve(input_text: &str, crane: fn(Vec<Vec<char>>, &Vec<Move>) -> Vec<Vec<char>>) -> String {
    let split = input_text.trim_end().split("\n\n").collect::<Vec<&str>>();
    let mut storage = split
        .get(0)
        .expect("Can't find storage")
        .lines()
        .map(|line| {
            line.as_bytes()
                .chunks(4)
                .map(|chunk| *chunk.get(1).unwrap() as char)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    storage = (0..storage[0].len())
        .map(|i| {
            storage
                .iter()
                .map(move |c| c[i])
                .rev()
                .skip(1)
                .filter(|item| item != &' ')
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let moves = split
        .get(1)
        .expect("Can't find moves")
        .lines()
        .map(|line| {
            let s = line.split(" ").collect::<Vec<&str>>();
            Move {
                boxes: s[1].parse::<usize>().unwrap(),
                from: s[3].parse::<usize>().unwrap() - 1,
                to: s[5].parse::<usize>().unwrap() - 1,
            }
        })
        .collect::<Vec<Move>>();

    storage = crane(storage, &moves);
    get_top_boxes(storage)
}

fn get_top_boxes(boxes: Vec<Vec<char>>) -> String {
    boxes
        .iter()
        .map(|pile| pile.last().unwrap())
        .collect::<String>()
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_05_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {}", solve_pt1(&input_text));
    // Correct: VRWBSFZWM

    println!("Part two: {}", solve_pt2(&input_text));
    // Correct: RBTWJWMCF

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_rust::test_solution;

    const TEST_DATA: &str = "    [D]    \n\
[N] [C]    \n\
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2";

    test_solution!(test1, solve_pt1, "CMZ", TEST_DATA);
    test_solution!(test2, solve_pt2, "MCD", TEST_DATA);
}
