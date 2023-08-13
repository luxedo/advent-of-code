/*
 * Advent Of Code
 * https://adventofcode.com/2022/day/8
 *
 * --- Day 8: Treetop Tree House ---
 * The expedition comes across a peculiar patch of tall trees all planted carefully in a grid. The Elves explain that a previous expedition planted these trees as a reforestation effort. Now, they're curious if this would be a good location for a tree house.
 *
 * First, determine whether there is enough tree cover here to keep a tree house hidden. To do this, you need to count the number of trees that are visible from outside the grid when looking directly along a row or column.
 *
 * The Elves have already launched a quadcopter to generate a map with the height of each tree (your puzzle input). For example:
 *
 * 30373
 * 25512
 * 65332
 * 33549
 * 35390
 * Each tree is represented as a single digit whose value is its height, where 0 is the shortest and 9 is the tallest.
 *
 * A tree is visible if all of the other trees between it and an edge of the grid are shorter than it. Only consider trees in the same row or column; that is, only look up, down, left, or right from any given tree.
 *
 * All of the trees around the edge of the grid are visible - since they are already on the edge, there are no trees to block the view. In this example, that only leaves the interior nine trees to consider:
 *
 * The top-left 5 is visible from the left and top. (It isn't visible from the right or bottom since other trees of height 5 are in the way.)
 * The top-middle 5 is visible from the top and right.
 * The top-right 1 is not visible from any direction; for it to be visible, there would need to only be trees of height 0 between it and an edge.
 * The left-middle 5 is visible, but only from the right.
 * The center 3 is not visible from any direction; for it to be visible, there would need to be only trees of at most height 2 between it and an edge.
 * The right-middle 3 is visible from the right.
 * In the bottom row, the middle 5 is visible, but the 3 and 4 are not.
 * With 16 trees visible on the edge and another 5 visible in the interior, a total of 21 trees are visible in this arrangement.
 *
 * Consider your map; how many trees are visible from outside the grid?
 *
 * --- Part Two ---
 * Content with the amount of tree cover available, the Elves just need to know the best spot to build their tree house: they would like to be able to see a lot of trees.
 *
 * To measure the viewing distance from a given tree, look up, down, left, and right from that tree; stop if you reach an edge or at the first tree that is the same height or taller than the tree under consideration. (If a tree is right on the edge, at least one of its viewing distances will be zero.)
 *
 * The Elves don't care about distant trees taller than those found by the rules above; the proposed tree house has large eaves to keep it dry, so they wouldn't be able to see higher than the tree house anyway.
 *
 * In the example above, consider the middle 5 in the second row:
 *
 * 30373
 * 25512
 * 65332
 * 33549
 * 35390
 * Looking up, its view is not blocked; it can see 1 tree (of height 3).
 * Looking left, its view is blocked immediately; it can see only 1 tree (of height 5, right next to it).
 * Looking right, its view is not blocked; it can see 2 trees.
 * Looking down, its view is blocked eventually; it can see 2 trees (one of height 3, then the tree of height 5 that blocks its view).
 * A tree's scenic score is found by multiplying together its viewing distance in each of the four directions. For this tree, this is 4 (found by multiplying 1 * 1 * 2 * 2).
 *
 * However, you can do even better: consider the tree of height 5 in the middle of the fourth row:
 *
 * 30373
 * 25512
 * 65332
 * 33549
 * 35390
 * Looking up, its view is blocked at 2 trees (by another tree with a height of 5).
 * Looking left, its view is not blocked; it can see 2 trees.
 * Looking down, its view is also not blocked; it can see 1 tree.
 * Looking right, its view is blocked at 2 trees (by a massive tree of height 9).
 * This tree's scenic score is 8 (2 * 2 * 1 * 2); this is the ideal spot for the tree house.
 *
 * Consider each tree on your map. What is the highest scenic score possible for any tree?
 */
use aoc_rust::load_input;
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

fn solve_pt1(input_text: &str) -> u64 {
    let forest = input_text
        .lines()
        .map(|row| {
            row.chars()
                .map(|c| c.to_digit(10).unwrap() as i32)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let vis_all = visible_from_borders(&forest);
    let total_visible = vis_all.iter().fold(0, |acc, row| {
        acc + row.into_iter().map(|b| *b as u64).sum::<u64>()
    });
    total_visible
}

fn solve_pt2(input_text: &str) -> u64 {
    let forest = input_text
        .lines()
        .map(|row| {
            row.chars()
                .map(|c| c.to_digit(10).unwrap() as i32)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let scenic_scores = calculate_scenic_scores(&forest);
    let max_scenic = scenic_scores
        .into_iter()
        .map(|row| row.into_iter().max().unwrap())
        .max()
        .unwrap() as u64;
    max_scenic
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_08_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {}", solve_pt1(&input_text));
    // Correct: 1814

    println!("Part two: {}", solve_pt2(&input_text));
    // Correct: 330786

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_rust::test_solution;

    const TEST_DATA: &str = "30373
25512
65332
33549
35390";

    test_solution!(test1, solve_pt1, 21, TEST_DATA);
    test_solution!(test2, solve_pt2, 8, TEST_DATA);
}
