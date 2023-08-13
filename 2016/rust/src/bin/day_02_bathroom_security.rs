/* Advent Of Code 2016 - day 2
 * https://adventofcode.com/2016/day/2
 *
 *
 * --- Day 2: Bathroom Security ---
 *  You arrive at Easter Bunny Headquarters under cover of darkness. However, you left in such a rush that you forgot to use the bathroom! Fancy office buildings like this one usually have keypad locks on their bathrooms, so you search the front desk for the code.
 *
 *  "In order to improve security," the document you find says, "bathroom codes will no longer be written down. Instead, please memorize and follow the procedure below to access the bathrooms."
 *
 *  The document goes on to explain that each button to be pressed can be found by starting on the previous button and moving to adjacent buttons on the keypad: U moves up, D moves down, L moves left, and R moves right. Each line of instructions corresponds to one button, starting at the previous button (or, for the first line, the "5" button); press whatever button you're on at the end of each line. If a move doesn't lead to a button, ignore it.
 *
 *  You can't hold it much longer, so you decide to figure out the code as you walk to the bathroom. You picture a keypad like this:
 *
 *  1 2 3
 *  4 5 6
 *  7 8 9
 *  Suppose your instructions are:
 *
 *  ULL
 *  RRDDD
 *  LURDL
 *  UUUUD
 *  You start at "5" and move up (to "2"), left (to "1"), and left (you can't, and stay on "1"), so the first button is 1.
 *  Starting from the previous button ("1"), you move right twice (to "3") and then down three times (stopping at "9" after two moves and ignoring the third), ending up with 9.
 *  Continuing from "9", you move left, up, right, down, and left, ending with 8.
 *  Finally, you move up four times (stopping at "2"), then down once, ending with 5.
 *  So, in this example, the bathroom code is 1985.
 *
 *  Your puzzle input is the instructions from the document you found at the front desk. What is the bathroom code?
 *
 *
 *  --- Part Two ---
 *  You finally arrive at the bathroom (it's a several minute walk from the lobby so visitors can behold the many fancy conference rooms and water coolers on this floor) and go to punch in the code. Much to your bladder's dismay, the keypad is not at all like you imagined it. Instead, you are confronted with the result of hundreds of man-hours of bathroom-keypad-design meetings:
 *
 *      1
 *    2 3 4
 *  5 6 7 8 9
 *    A B C
 *      D
 *  You still start at "5" and stop when you're at an edge, but given the same instructions as above, the outcome is very different:
 *
 *  You start at "5" and don't move at all (up and left are both edges), ending at 5.
 *  Continuing from "5", you move right twice and down three times (through "6", "7", "B", "D", "D"), ending at D.
 *  Then, from "D", you move five more times (through "D", "B", "C", "C", "B"), ending at B.
 *  Finally, after five more moves, you end at 3.
 *  So, given the actual keypad layout, the code would be 5DB3.
 *
 *  Using the same instructions in your puzzle input, what is the correct bathroom code?
 */

use aoc_rust::load_input;
use std::error::Error;
use std::str::FromStr;

enum Direction {
    Up,
    Right,
    Down,
    Left,
}

#[derive(Debug)]
struct ParseDirectionError;
impl FromStr for Direction {
    type Err = ParseDirectionError;
    fn from_str(input: &str) -> Result<Direction, Self::Err> {
        match input {
            "U" => Ok(Direction::Up),
            "R" => Ok(Direction::Right),
            "D" => Ok(Direction::Down),
            "L" => Ok(Direction::Left),
            _ => Err(ParseDirectionError),
        }
    }
}

struct KeyPad {
    pad: Vec<Vec<char>>,
    finger: (i32, i32),
    max_idx: (i32, i32),
}

impl KeyPad {
    fn new(pad: Vec<Vec<char>>, starting_pos: char) -> Self {
        let finger: (i32, i32) = KeyPad::find_starting_position(&pad, &starting_pos);
        let max_idx = ((pad[0].len() - 1) as i32, (pad.len() - 1) as i32);
        KeyPad {
            pad,
            finger,
            max_idx,
        }
    }

    fn find_starting_position(pad: &[Vec<char>], starting_pos: &char) -> (i32, i32) {
        for (y, line) in pad.iter().enumerate() {
            for (x, cell) in line.iter().enumerate() {
                if cell == starting_pos {
                    return (x.try_into().unwrap(), y.try_into().unwrap());
                }
            }
        }
        panic!("Could not find starting position")
    }

    fn move_finger(&mut self, direction: Direction) {
        match direction {
            Direction::Up => self.finger.1 -= 1,
            Direction::Right => self.finger.0 += 1,
            Direction::Down => self.finger.1 += 1,
            Direction::Left => self.finger.0 -= 1,
        };
        self.finger.0 = self.finger.0.clamp(0, self.max_idx.0);
        self.finger.1 = self.finger.1.clamp(0, self.max_idx.1);

        if self.digit() == ' ' {
            match direction {
                Direction::Up => self.finger.1 += 1,
                Direction::Right => self.finger.0 -= 1,
                Direction::Down => self.finger.1 -= 1,
                Direction::Left => self.finger.0 += 1,
            };
        }
    }

    fn run(&mut self, commands: Vec<Vec<Direction>>) -> String {
        commands
            .into_iter()
            .map(|line| {
                let _ = line
                    .into_iter()
                    .map(|direction| self.move_finger(direction))
                    .collect::<Vec<_>>();
                self.digit()
            })
            .collect::<String>()
    }

    fn digit(&self) -> char {
        self.pad[self.finger.1 as usize][self.finger.0 as usize]
    }
}

fn parse_input(input_text: &str) -> Vec<Vec<Direction>> {
    input_text
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| Direction::from_str(&c.to_string()).unwrap())
                .collect()
        })
        .collect()
}

fn solve_pt1(input_text: &str) -> String {
    let mut keypad: KeyPad = KeyPad::new(
        vec![
            vec!['1', '2', '3'],
            vec!['4', '5', '6'],
            vec!['7', '8', '9'],
        ],
        '5',
    );
    let commands = parse_input(input_text);
    keypad.run(commands)
}

fn solve_pt2(input_text: &str) -> String {
    let mut keypad: KeyPad = KeyPad::new(
        vec![
            vec![' ', ' ', '1', ' ', ' '],
            vec![' ', '2', '3', '4', ' '],
            vec!['5', '6', '7', '8', '9'],
            vec![' ', 'A', 'B', 'C', ' '],
            vec![' ', ' ', 'D', ' ', ' '],
        ],
        '5',
    );
    let commands = parse_input(input_text);
    keypad.run(commands)
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_02_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {:#?}", solve_pt1(&input_text));
    // solution_pt1: 84452

    println!("Part two: {:#?}", solve_pt2(&input_text));
    // solution_pt2: D65C3

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_rust::test_solution;

    const TEST_INPUT: &str = "ULL\n\
                              RRDDD\n\
                              LURDL\n\
                              UUUUD";
    test_solution!(test1, solve_pt1, "1985", TEST_INPUT);
    test_solution!(test2, solve_pt2, "5DB3", TEST_INPUT);
}
