/* Advent Of Code 2016 - day 8
 * https://adventofcode.com/2016/day/8
 *
 * --- Day 8: Two-Factor Authentication ---
 * You come across a door implementing what you can only assume is an implementation of two-factor authentication after a long game of requirements telephone.
 *
 * To get past the door, you first swipe a keycard (no problem; there was one on a nearby desk). Then, it displays a code on a little screen, and you type that code on a keypad. Then, presumably, the door unlocks.
 *
 * Unfortunately, the screen has been smashed. After a few minutes, you've taken everything apart and figured out how it works. Now you just have to work out what the screen would have displayed.
 *
 * The magnetic strip on the card you swiped encodes a series of instructions for the screen; these instructions are your puzzle input. The screen is 50 pixels wide and 6 pixels tall, all of which start off, and is capable of three somewhat peculiar operations:
 *
 * rect AxB turns on all of the pixels in a rectangle at the top-left of the screen which is A wide and B tall.
 * rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B pixels. Pixels that would fall off the right end appear at the left end of the row.
 * rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down by B pixels. Pixels that would fall off the bottom appear at the top of the column.
 *
 *
 * For example, here is a simple sequence on a smaller screen:
 *
 *
 * rect 3x2 creates a small rectangle in the top-left corner:###....
 * ###....
 * .......
 *
 * rotate column x=1 by 1 rotates the second column down by one pixel:#.#....
 * ###....
 * .#.....
 *
 * rotate row y=0 by 4 rotates the top row right by four pixels:....#.#
 * ###....
 * .#.....
 *
 * rotate column x=1 by 1 again rotates the second column down by one pixel, causing the bottom pixel to wrap back to the top:.#..#.#
 * #.#....
 * .#.....
 *
 *
 * As you can see, this display technology is extremely powerful, and will soon dominate the tiny-code-displaying-screen market.  That's what the advertisement on the back of the display tries to convince you, anyway.
 *
 * There seems to be an intermediate check of the voltage used by the display: after you swipe your card, if the screen did work, how many pixels should be lit?
*/

use aoc_lang_rust::load_input;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
enum Command {
    Rect(usize, usize),
    RotateY { col: usize, by: usize },
    RotateX { row: usize, by: usize },
}

impl Command {
    fn parse(line: &str) -> Self {
        let (cmd, tail) = line.split_once(' ').unwrap();
        match cmd {
            "rect" => {
                let (width, height) = tail.split_once('x').unwrap();
                Self::Rect(
                    width.parse::<usize>().unwrap(),
                    height.parse::<usize>().unwrap(),
                )
            }
            "rotate" => {
                let (axis, tail) = tail.split_once(' ').unwrap();
                let mut tail = tail.split(' ');
                let (_, idx) = tail.next().unwrap().split_once('=').unwrap();
                let idx = idx.parse::<usize>().unwrap();
                tail.next();
                let by = tail.next().unwrap();
                let by = by.parse::<usize>().unwrap();
                match axis {
                    "column" => Command::RotateY { col: idx, by },
                    "row" => Command::RotateX { row: idx, by },
                    &_ => panic!("Cannot parse command"),
                }
            }
            &_ => panic!("Cannot parse command"),
        }
    }
}

struct Screen {
    width: usize,
    height: usize,
    screen: Vec<char>,
}

impl fmt::Display for Screen {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for y in 0..self.height {
            for x in 0..self.width {
                write!(f, "{}", self.get(y, x))?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Screen {
    fn new(width: usize, height: usize) -> Self {
        Self {
            width,
            height,
            screen: vec![' '; width * height],
        }
    }

    fn get(&self, row: usize, col: usize) -> char {
        if row >= self.height || col >= self.width {
            panic!("Can't set value at screen");
        }
        let idx = col + row * self.width;
        self.screen[idx]
    }
    fn set(&mut self, row: usize, col: usize, value: char) {
        if row >= self.height || col >= self.width {
            panic!("Can't set value at screen");
        }
        let idx = col + row * self.width;
        self.screen[idx] = value;
    }
    fn rotate_x(&mut self, row: usize, by: usize) {
        for _ in 0..by {
            let storage = self.get(row, self.width - 1);
            for col in (1..self.width).rev() {
                self.set(row, col, self.get(row, col - 1));
            }
            self.set(row, 0, storage);
        }
    }
    fn rotate_y(&mut self, col: usize, by: usize) {
        for _ in 0..by {
            let storage = self.get(self.height - 1, col);
            for row in (1..self.height).rev() {
                self.set(row, col, self.get(row - 1, col));
            }
            self.set(0, col, storage);
        }
    }

    fn run(&mut self, command: Command) {
        match command {
            Command::Rect(width, height) => {
                for x in 0..width {
                    for y in 0..height {
                        self.set(y, x, '#')
                    }
                }
            }
            Command::RotateX { row, by } => self.rotate_x(row, by),
            Command::RotateY { col, by } => self.rotate_y(col, by),
        }
    }
    fn lit(&self) -> usize {
        self.screen
            .clone()
            .into_iter()
            .filter(|c| *c == '#')
            .count()
    }
}

fn solve_pt1(input_text: &str) -> u64 {
    let mut s = Screen::new(50, 6);
    input_text.lines().for_each(|line| {
        s.run(Command::parse(line));
    });
    s.lit().try_into().unwrap()
}

fn solve_pt2(input_text: &str) -> String {
    let mut s = Screen::new(50, 6);
    input_text.lines().for_each(|line| {
        s.run(Command::parse(line));
    });
    format!("\n{}", &s.to_string())
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_08_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {}", solve_pt1(&input_text));
    // solution_pt1: 128

    println!("Part two: {}", solve_pt2(&input_text));
    // solution_pt2: ???

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_lang_rust::test_solution;

    const INPUT: &str = "rect 3x2\n\
                        rotate column x=1 by 1\n\
                        rotate row y=0 by 4\n\
                        rotate column x=1 by 1";
    const ANS_PT2: &str = r#"
    # #
# #
 #
 #


"#;

    test_solution!(test1, solve_pt1, 6, INPUT);
    test_solution!(test2, solve_pt2, ANS_PT2, INPUT);
}
