/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 05
* Rust Solution
*
* Day 5: Supply Stacks
*
* https://adventofcode.com/2022/day/5
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;

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

fn solve_pt1(input_data: &str, _args: Vec<&str>) -> FireplaceResult<impl Display> {
    let answer = solve(input_data, crane9000);
    Ok(answer)
}

fn solve_pt2(input_data: &str, _args: Vec<&str>) -> FireplaceResult<impl Display> {
    let answer = solve(input_data, crane9001);
    Ok(answer)
}


fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)
}
