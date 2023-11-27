/*
 * Advent Of Code
 * https://adventofcode.com/2022/day/20
 *
 * --- Day 20: Grove Positioning System ---
 * It's finally time to meet back up with the Elves. When you try to contact them, however, you get no reply. Perhaps you're out of range?
 *
 * You know they're headed to the grove where the star fruit grows, so if you can figure out where that is, you should be able to meet back up with them.
 *
 * Fortunately, your handheld device has a file (your puzzle input) that contains the grove's coordinates! Unfortunately, the file is encrypted - just in case the device were to fall into the wrong hands.
 *
 * Maybe you can decrypt it?
 *
 * When you were still back at the camp, you overheard some Elves talking about coordinate file encryption. The main operation involved in decrypting the file is called mixing.
 *
 * The encrypted file is a list of numbers. To mix the file, move each number forward or backward in the file a number of positions equal to the value of the number being moved. The list is circular, so moving a number off one end of the list wraps back around to the other end as if the ends were connected.
 *
 * For example, to move the 1 in a sequence like 4, 5, 6, 1, 7, 8, 9, the 1 moves one position forward: 4, 5, 6, 7, 1, 8, 9. To move the -2 in a sequence like 4, -2, 5, 6, 7, 8, 9, the -2 moves two positions backward, wrapping around: 4, 5, 6, 7, 8, -2, 9.
 *
 * The numbers should be moved in the order they originally appear in the encrypted file. Numbers moving around during the mixing process do not change the order in which the numbers are moved.
 *
 * Consider this encrypted file:
 *
 * 1
 * 2
 * -3
 * 3
 * -2
 * 0
 * 4
 * Mixing this file proceeds as follows:
 *
 * Initial arrangement:
 * 1, 2, -3, 3, -2, 0, 4
 *
 * 1 moves between 2 and -3:
 * 2, 1, -3, 3, -2, 0, 4
 *
 * 2 moves between -3 and 3:
 * 1, -3, 2, 3, -2, 0, 4
 *
 * -3 moves between -2 and 0:
 * 1, 2, 3, -2, -3, 0, 4
 *
 * 3 moves between 0 and 4:
 * 1, 2, -2, -3, 0, 3, 4
 *
 * -2 moves between 4 and 1:
 * 1, 2, -3, 0, 3, 4, -2
 *
 * 0 does not move:
 * 1, 2, -3, 0, 3, 4, -2
 *
 * 4 moves between -3 and 0:
 * 1, 2, -3, 4, 0, 3, -2
 * Then, the grove coordinates can be found by looking at the 1000th, 2000th, and 3000th numbers after the value 0, wrapping around the list as necessary. In the above example, the 1000th number after 0 is 4, the 2000th is -3, and the 3000th is 2; adding these together produces 3.
 *
 * Mix your encrypted file exactly once. What is the sum of the three numbers that form the grove coordinates?
 *
 *
 * --- Part Two ---
 * The grove coordinate values seem nonsensical. While you ponder the mysteries of Elf encryption, you suddenly remember the rest of the decryption routine you overheard back at camp.
 *
 * First, you need to apply the decryption key, 811589153. Multiply each number by the decryption key before you begin; this will produce the actual list of numbers to mix.
 *
 * Second, you need to mix the list of numbers ten times. The order in which the numbers are mixed does not change during mixing; the numbers are still moved in the order they appeared in the original, pre-mixed list. (So, if -3 appears fourth in the original list of numbers to mix, -3 will be the fourth number to move during each round of mixing.)
 *
 * Using the same example as above:
 *
 * Initial arrangement:
 * 811589153, 1623178306, -2434767459, 2434767459, -1623178306, 0, 3246356612
 *
 * After 1 round of mixing:
 * 0, -2434767459, 3246356612, -1623178306, 2434767459, 1623178306, 811589153
 *
 * After 2 rounds of mixing:
 * 0, 2434767459, 1623178306, 3246356612, -2434767459, -1623178306, 811589153
 *
 * After 3 rounds of mixing:
 * 0, 811589153, 2434767459, 3246356612, 1623178306, -1623178306, -2434767459
 *
 * After 4 rounds of mixing:
 * 0, 1623178306, -2434767459, 811589153, 2434767459, 3246356612, -1623178306
 *
 * After 5 rounds of mixing:
 * 0, 811589153, -1623178306, 1623178306, -2434767459, 3246356612, 2434767459
 *
 * After 6 rounds of mixing:
 * 0, 811589153, -1623178306, 3246356612, -2434767459, 1623178306, 2434767459
 *
 * After 7 rounds of mixing:
 * 0, -2434767459, 2434767459, 1623178306, -1623178306, 811589153, 3246356612
 *
 * After 8 rounds of mixing:
 * 0, 1623178306, 3246356612, 811589153, -2434767459, 2434767459, -1623178306
 *
 * After 9 rounds of mixing:
 * 0, 811589153, 1623178306, -2434767459, 3246356612, 2434767459, -1623178306
 *
 * After 10 rounds of mixing:
 * 0, -2434767459, 1623178306, 3246356612, -1623178306, 2434767459, 811589153
 * The grove coordinates can still be found in the same way. Here, the 1000th number after 0 is 811589153, the 2000th is 2434767459, and the 3000th is -1623178306; adding these together produces 1623178306.
 *
 * Apply the decryption key and mix your encrypted file ten times. What is the sum of the three numbers that form the grove coordinates?
 */
use aoc_rust::load_input;
use std::error::Error;

#[derive(Debug)]
struct Decryptor {
    message: Vec<(usize, isize)>,
    current: Vec<(usize, isize)>,
    rounds: usize,
    length: isize,
    key: isize,
}
impl Decryptor {
    fn run(&mut self, rounds: usize) {
        for _ in 0..rounds {
            let cursor = self.rounds % self.current.len();
            let position = self.current.iter().position(|n| n.0 == cursor).unwrap();
            self.move_number(position);
            self.rounds += 1;
        }
    }
    fn move_number(&mut self, position: usize) {
        let number = self.current[position];
        let total_offset = position as isize + number.1;
        let final_position = total_offset.rem_euclid(self.length - 1);
        self.current.remove(position);
        self.current.insert(final_position as usize, number);
    }
    fn decrypt(&mut self, cycles: usize) -> isize {
        self.reset();
        for _ in 0..cycles {
            self.run(self.length as usize);
        }
        let d0 = self.get_grove(0, 1000);
        let d1 = self.get_grove(0, 2000);
        let d2 = self.get_grove(0, 3000);
        (d0 + d1 + d2) as isize
    }
    fn get_grove(&self, g: isize, o: isize) -> isize {
        let position = self.current.iter().position(|n| n.1 == g).unwrap() as isize;
        let position = (position + o).rem_euclid(self.length) as usize;
        self.current[position].1
    }
    fn reset(&mut self) {
        self.current = self
            .message
            .iter()
            .map(|(i, value)| (*i, value * self.key))
            .collect::<Vec<(usize, isize)>>();
    }
    fn set_key(&mut self, key: isize) {
        self.key = key;
        self.reset();
    }
}

fn parse_input(input_text: &str) -> Decryptor {
    let message = input_text
        .lines()
        .enumerate()
        .map(|(i, line)| (i, line.parse::<isize>().unwrap()))
        .collect::<Vec<(usize, isize)>>();
    let length = message.len() as isize;
    Decryptor {
        current: message.clone(),
        message,
        rounds: 0,
        length,
        key: 1,
    }
}
fn solve_pt1(input_text: &str) -> u64 {
    let mut decryptor = parse_input(input_text);
    decryptor.decrypt(1) as u64
}

fn solve_pt2(input_text: &str) -> u64 {
    let mut decryptor = parse_input(input_text);
    let key = 811589153;
    let cycles = 10;
    decryptor.set_key(key);
    decryptor.decrypt(cycles) as u64
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_20_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {}", solve_pt1(&input_text));
    // Correct: 13967

    println!("Part two: {}", solve_pt2(&input_text));
    // Correct: 1790365671518

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use super::*;
    use aoc_rust::test_solution;

    const TEST_DATA: &str = "1
2
-3
3
-2
0
4";

    test_solution!(test1, solve_pt1, 3, TEST_DATA);

    #[test]
    fn test_pt1_simple() {
        const ANS_PT1_S_0: isize = 4;
        const ANS_PT1_S_1: isize = 4;
        const ANS_PT1_S_2: isize = 4;
        const ANS_PT1_S_3: isize = 4;
        const ANS_PT1_S_4: isize = 3;
        const ANS_PT1_S_5: isize = 3;
        const ANS_PT1_S_6: isize = 3;
        const ANS_PT1_S_7: isize = 3;
        let mut decryptor = parse_input(TEST_DATA);
        assert_eq!(decryptor.get_grove(0, 1), ANS_PT1_S_0);
        decryptor.run(1);
        assert_eq!(decryptor.get_grove(0, 1), ANS_PT1_S_1);
        decryptor.run(1);
        assert_eq!(decryptor.get_grove(0, 1), ANS_PT1_S_2);
        decryptor.run(1);
        assert_eq!(decryptor.get_grove(0, 1), ANS_PT1_S_3);
        decryptor.run(1);
        assert_eq!(decryptor.get_grove(0, 1), ANS_PT1_S_4);
        decryptor.run(1);
        assert_eq!(decryptor.get_grove(0, 1), ANS_PT1_S_5);
        decryptor.run(1);
        assert_eq!(decryptor.get_grove(0, 1), ANS_PT1_S_6);
        decryptor.run(1);
        assert_eq!(decryptor.get_grove(0, 1), ANS_PT1_S_7);
    }

    test_solution!(test2, solve_pt2, 1623178306, TEST_DATA);
}
