/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 08
* Rust Solution
*
* Day 8: Treetop Tree House
*
* https://adventofcode.com/2022/day/8
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;
use std::cmp::Ordering;
use std::error::Error;

type BVec = Vec<bool>;
type BMatrix = Vec<BVec>;
type IVec = Vec<i32>;
type IMatrix = Vec<IVec>;

fn visible_left(row: &IVec) -> BVec {
    let mut highest: i32 = -1;
    row.iter()
        .map(|tree| match tree.cmp(&highest) {
            Ordering::Greater => {
                highest = *tree;
                true
            }
            Ordering::Less | Ordering::Equal => false,
        })
        .collect::<BVec>()
}

fn rotate_90<T: Copy>(m: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    let len = m.len();
    let n = m.clone();
    (0..len)
        .map(|i| (0..len).map(|j| n[len - j - 1][i] as T).collect::<Vec<T>>())
        .collect::<Vec<Vec<T>>>()
}

fn rotate<T: Copy>(m: &Vec<Vec<T>>, side: i8) -> Vec<Vec<T>> {
    let mut n: Vec<Vec<T>> = m.clone();
    for _ in 0..=(side - 1) {
        n = rotate_90(&n);
    }
    n
}

fn visible_from(forest: &IMatrix, side: i8) -> BMatrix {
    let visible: IMatrix = rotate::<i32>(forest, side);
    let visible: BMatrix = rotate::<i32>(forest, side)
        .iter()
        .map(visible_left)
        .collect::<BMatrix>();
    rotate::<bool>(&visible, 4 - side)
}

fn visible_from_borders(forest: &IMatrix) -> BMatrix {
    (0..4)
        .map(|side| visible_from(forest, side))
        .reduce(|acc, cur| matrix_or(acc, cur))
        .unwrap()
}

fn matrix_or(m1: BMatrix, m2: BMatrix) -> BMatrix {
    m1.into_iter()
        .zip(m2)
        .map(|(v1, v2)| {
            v1.into_iter()
                .zip(v2)
                .map(|(c1, c2)| c1 || c2)
                .collect::<BVec>()
        })
        .collect::<BMatrix>()
}

fn score_vector(right: Vec<i32>, tree: i32) -> i32 {
    let mut max_r: i32 = -1;
    right.into_iter().fold(0, |acc, r| {
        if max_r >= tree {
            // stop if you reach an edge or at the first tree that is the same height or
            // taller than the tree under consideration.
            acc
        } else {
            max_r = if r > max_r { r } else { max_r };
            acc + 1
        }
    })
}

fn scenic_score(m: &IMatrix, i: usize, j: usize) -> i32 {
    let len = m.len();
    let tree = m[i][j];
    let row = &m[i];
    let col = m.iter().map(|row| row[j]).collect::<Vec<_>>();

    let top = col[..i].iter().rev().map(|x| *x).collect::<Vec<i32>>();
    let right = row[j + 1..].to_vec();
    let bottom = col[i + 1..].to_vec();
    let left = row[..j].iter().rev().map(|x| *x).collect::<Vec<i32>>();

    let t_score = if i == 0 { 0 } else { score_vector(top, tree) };
    let r_score = if j == len - 1 {
        0
    } else {
        score_vector(right, tree)
    };
    let b_score = if i == len - 1 {
        0
    } else {
        score_vector(bottom, tree)
    };
    let l_score = if j == 0 { 0 } else { score_vector(left, tree) };
    r_score * l_score * t_score * b_score
}

fn calculate_scenic_scores(forest: &IMatrix) -> IMatrix {
    forest
        .into_iter()
        .enumerate()
        .map(|(i, row)| {
            row.into_iter()
                .enumerate()
                .map(|(j, _)| scenic_score(forest, i, j))
                .collect::<IVec>()
        })
        .collect::<IMatrix>()
}
fn solve_pt1(input_data: &str, _args: Vec<&str>) -> FireplaceResult<impl Display> {
    let forest = input_data
        .lines()
        .map(|row| {
            row.chars()
                .map(|c| c.to_digit(10).unwrap() as i32)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let vis_all = visible_from_borders(&forest);
    let answer = vis_all.iter().fold(0, |acc, row| {
        acc + row.into_iter().map(|b| *b as u64).sum::<u64>()
    });
    Ok(answer)
}

fn solve_pt2(input_data: &str, _args: Vec<&str>) -> FireplaceResult<impl Display> {
    let forest = input_data
        .lines()
        .map(|row| {
            row.chars()
                .map(|c| c.to_digit(10).unwrap() as i32)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let scenic_scores = calculate_scenic_scores(&forest);
    let answer = scenic_scores
        .into_iter()
        .map(|row| row.into_iter().max().unwrap())
        .max()
        .unwrap() as u64;
    Ok(answer)
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)
}
