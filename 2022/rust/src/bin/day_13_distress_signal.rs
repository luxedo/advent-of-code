/*
 * Advent Of Code
 * https://adventofcode.com/2022/day/13
 *
 * --- Day 13: Distress Signal ---
 * You climb the hill and again try contacting the Elves. However, you instead receive a signal you weren't expecting: a distress signal.
 *
 * Your handheld device must still not be working properly; the packets from the distress signal got decoded out of order. You'll need to re-order the list of received packets (your puzzle input) to decode the message.
 *
 * Your list consists of pairs of packets; pairs are separated by a blank line. You need to identify how many pairs of packets are in the right order.
 *
 * For example:
 *
 * [1,1,3,1,1]
 * [1,1,5,1,1]
 *
 * [[1],[2,3,4]]
 * [[1],4]
 *
 * [9]
 * [[8,7,6]]
 *
 * [[4,4],4,4]
 * [[4,4],4,4,4]
 *
 * [7,7,7,7]
 * [7,7,7]
 *
 * []
 * [3]
 *
 * [[[]]]
 * [[]]
 *
 * [1,[2,[3,[4,[5,6,7]]]],8,9]
 * [1,[2,[3,[4,[5,6,0]]]],8,9]
 * Packet data consists of lists and integers. Each list starts with [, ends with ], and contains zero or more comma-separated values (either integers or other lists). Each packet is always a list and appears on its own line.
 *
 * When comparing two values, the first value is called left and the second value is called right. Then:
 *
 * If both values are integers, the lower integer should come first. If the left integer is lower than the right integer, the inputs are in the right order. If the left integer is higher than the right integer, the inputs are not in the right order. Otherwise, the inputs are the same integer; continue checking the next part of the input.
 * If both values are lists, compare the first value of each list, then the second value, and so on. If the left list runs out of items first, the inputs are in the right order. If the right list runs out of items first, the inputs are not in the right order. If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.
 * If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison. For example, if comparing [0,0,0] and 2, convert the right value to [2] (a list containing 2); the result is then found by instead comparing [0,0,0] and [2].
 * Using these rules, you can determine which of the pairs in the example are in the right order:
 *
 * == Pair 1 ==
 * - Compare [1,1,3,1,1] vs [1,1,5,1,1]
 *   - Compare 1 vs 1
 *   - Compare 1 vs 1
 *   - Compare 3 vs 5
 *     - Left side is smaller, so inputs are in the right order
 *
 * == Pair 2 ==
 * - Compare [[1],[2,3,4]] vs [[1],4]
 *   - Compare [1] vs [1]
 *     - Compare 1 vs 1
 *   - Compare [2,3,4] vs 4
 *     - Mixed types; convert right to [4] and retry comparison
 *     - Compare [2,3,4] vs [4]
 *       - Compare 2 vs 4
 *         - Left side is smaller, so inputs are in the right order
 *
 * == Pair 3 ==
 * - Compare [9] vs [[8,7,6]]
 *   - Compare 9 vs [8,7,6]
 *     - Mixed types; convert left to [9] and retry comparison
 *     - Compare [9] vs [8,7,6]
 *       - Compare 9 vs 8
 *         - Right side is smaller, so inputs are not in the right order
 *
 * == Pair 4 ==
 * - Compare [[4,4],4,4] vs [[4,4],4,4,4]
 *   - Compare [4,4] vs [4,4]
 *     - Compare 4 vs 4
 *     - Compare 4 vs 4
 *   - Compare 4 vs 4
 *   - Compare 4 vs 4
 *   - Left side ran out of items, so inputs are in the right order
 *
 * == Pair 5 ==
 * - Compare [7,7,7,7] vs [7,7,7]
 *   - Compare 7 vs 7
 *   - Compare 7 vs 7
 *   - Compare 7 vs 7
 *   - Right side ran out of items, so inputs are not in the right order
 *
 * == Pair 6 ==
 * - Compare [] vs [3]
 *   - Left side ran out of items, so inputs are in the right order
 *
 * == Pair 7 ==
 * - Compare [[[]]] vs [[]]
 *   - Compare [[]] vs []
 *     - Right side ran out of items, so inputs are not in the right order
 *
 * == Pair 8 ==
 * - Compare [1,[2,[3,[4,[5,6,7]]]],8,9] vs [1,[2,[3,[4,[5,6,0]]]],8,9]
 *   - Compare 1 vs 1
 *   - Compare [2,[3,[4,[5,6,7]]]] vs [2,[3,[4,[5,6,0]]]]
 *     - Compare 2 vs 2
 *     - Compare [3,[4,[5,6,7]]] vs [3,[4,[5,6,0]]]
 *       - Compare 3 vs 3
 *       - Compare [4,[5,6,7]] vs [4,[5,6,0]]
 *         - Compare 4 vs 4
 *         - Compare [5,6,7] vs [5,6,0]
 *           - Compare 5 vs 5
 *           - Compare 6 vs 6
 *           - Compare 7 vs 0
 *             - Right side is smaller, so inputs are not in the right order
 * What are the indices of the pairs that are already in the right order? (The first pair has index 1, the second pair has index 2, and so on.) In the above example, the pairs in the right order are 1, 2, 4, and 6; the sum of these indices is 13.
 *
 * Determine which pairs of packets are already in the right order. What is the sum of the indices of those pairs?
 *
 *
 * --- Part Two ---
 * Now, you just need to put all of the packets in the right order. Disregard the blank lines in your list of received packets.
 *
 * The distress signal protocol also requires that you include two additional divider packets:
 *
 * [[2]]
 * [[6]]
 * Using the same rules as before, organize all packets - the ones in your list of received packets as well as the two divider packets - into the correct order.
 *
 * For the example above, the result of putting the packets in the correct order is:
 *
 * []
 * [[]]
 * [[[]]]
 * [1,1,3,1,1]
 * [1,1,5,1,1]
 * [[1],[2,3,4]]
 * [1,[2,[3,[4,[5,6,0]]]],8,9]
 * [1,[2,[3,[4,[5,6,7]]]],8,9]
 * [[1],4]
 * [[2]]
 * [3]
 * [[4,4],4,4]
 * [[4,4],4,4,4]
 * [[6]]
 * [7,7,7]
 * [7,7,7,7]
 * [[8,7,6]]
 * [9]
 * Afterward, locate the divider packets. To find the decoder key for this distress signal, you need to determine the indices of the two divider packets and multiply them together. (The first packet is at index 1, the second packet is at index 2, and so on.) In this example, the divider packets are 10th and 14th, and so the decoder key is 140.
 *
 * Organize all of the packets into the correct order. What is the decoder key for the distress signal?
 */

use aoc_rust::load_input;
use std::cmp::Ordering;
use std::error::Error;

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Integer(usize),
    List(TokenList),
    OpenSquare,
    CloseSquare,
    Comma,
    Space,
    Nil,
}
impl Token {
    fn recursive_tokens(&self) -> TokenList {
        let mut tokens = vec![];
        match self {
            Token::Integer(integer) => tokens.push(Token::Integer(*integer)),
            Token::List(list) => {
                tokens.push(Token::List(list.clone()));
                for token in list {
                    tokens.extend(token.recursive_tokens())
                }
            }
            _ => (),
        };
        tokens
    }
    fn parse_str(input: &str) -> Token {
        let tokenizer = Tokenizer {};
        let tokens = tokenizer
            .tokenize(input)
            .unwrap()
            .iter()
            .map(|(t, _)| t.to_owned())
            .collect::<TokenList>();
        Token::List(tokens)
    }
}

type TokenList = Vec<Token>;

#[derive(Debug)]
struct Packets {
    left: Token,
    right: Token,
}

struct Tokenizer {}
impl Tokenizer {
    fn tokenize(&self, data: &str) -> Result<Vec<(Token, usize)>, &str> {
        let mut string = data.clone();
        let mut tokens: Vec<(Token, usize)> = Vec::new();
        while let Ok(tk) = self.next_token(string) {
            string = &string[tk.1..];
            tokens.push(tk);
        }
        Ok(tokens)
    }

    fn next_token(&self, data: &str) -> Result<(Token, usize), &str> {
        let next = match data.chars().next() {
            Some(c) => c,
            None => return Err("EOF"),
        };
        match next {
            //' ' => self.next_token(&data[1..]),
            '[' => self.tokenize_list(data),
            ']' => Ok((Token::CloseSquare, 1)),
            ',' => Ok((Token::Comma, 1)),
            '0'..='9' => self.tokenize_integer(data),
            _ => return Err("Could not tokenize input"),
        }
    }
    fn tokenize_list(&self, data: &str) -> Result<(Token, usize), &str> {
        let mut string = &data[1..];
        let mut current_index = 1;
        let mut tokens: Vec<Token> = Vec::new();
        while let Ok((token, size)) = self.next_token(string) {
            string = &string[size..];
            current_index += size;
            match token {
                Token::CloseSquare => {
                    return Ok((Token::List(tokens), current_index));
                }
                Token::Comma | Token::Space => {
                    (); // Just ignore commas and spaces
                }
                _ => tokens.push(token.clone()),
            }
        }
        Err("Could not parse list. Did not found closing square bracket")
    }
    fn tokenize_integer(&self, data: &str) -> Result<(Token, usize), &str> {
        self.take_while(data, |c| c.is_digit(10))
    }

    fn take_while<F>(&self, data: &str, mut pred: F) -> Result<(Token, usize), &str>
    where
        F: FnMut(char) -> bool,
    {
        let mut current_index = 0;
        for ch in data.chars() {
            if !pred(ch) {
                break;
            }
            current_index += ch.len_utf8();
        }
        if current_index == 0 {
            Err("No match")
        } else {
            let number: usize = data[..current_index].parse().unwrap();
            Ok((Token::Integer(number), current_index))
        }
    }
}

fn parse_input_pt1(input_text: &str) -> Vec<Packets> {
    input_text
        .split("\n\n")
        .map(|packets| {
            let mut p = packets.lines().map(|line| Token::parse_str(line));
            Packets {
                left: p.next().unwrap(),
                right: p.next().unwrap(),
            }
        })
        .collect::<Vec<Packets>>()
}

fn parse_input_pt2(input_text: &str) -> Vec<Token> {
    input_text
        .split("\n\n")
        .map(|packets| {
            let mut p = packets.lines().map(|line| Token::parse_str(line));
            vec![p.next().unwrap(), p.next().unwrap()]
        })
        .flatten()
        .collect::<Vec<Token>>()
}

fn check_order(left: &TokenList, right: &TokenList) -> Option<bool> {
    let l_len = left.len();
    let r_len = right.len();

    let mut i = 0;
    loop {
        if (i == r_len) && (i == l_len) {
            break None;
        } else if i >= l_len {
            // If the left list runs out of items first, the inputs are in the right order.
            break Some(true);
        } else if i >= r_len {
            // If the right list runs out of items first, the inputs are not in the right order.
            break Some(false);
        }
        match (&left[i], &right[i]) {
            //  - If both values are integers, the lower integer should come first.
            (Token::Integer(il), Token::Integer(ir)) => match il.cmp(&ir) {
                //  If the left integer is lower than the right integer, the inputs are in the right order.
                Ordering::Greater => break Some(false),
                //  If the left integer is higher than the right integer, the inputs are not in the right order.
                Ordering::Less => break Some(true),
                //  Otherwise, the inputs are the same integer; continue checking the next part of the input.
                Ordering::Equal => {
                    ();
                }
            },
            // - If both values are lists, compare the first value of each list, then the second value, and so on.
            (Token::List(ll), Token::List(lr)) => {
                // If the lists are the same length and no comparison makes a decision about the order,
                // continue checking the next part of the input.
                match check_order(ll, lr) {
                    Some(value) => break Some(value),
                    None => (),
                }
            }
            // - If exactly one value is an integer, convert the integer to a list which
            // contains that integer as its only value, then retry the comparison.
            // For example, if comparing [0,0,0] and 2, convert the right value to [2] (a list containing 2);
            // the result is then found by instead comparing [0,0,0] and [2].
            (Token::Integer(il), Token::List(lr)) => {
                match check_order(&vec![Token::Integer(*il)], lr) {
                    Some(value) => break Some(value),
                    None => (),
                }
            }
            (Token::List(ll), Token::Integer(ir)) => {
                match check_order(ll, &vec![Token::Integer(*ir)]) {
                    Some(value) => break Some(value),
                    None => (),
                }
            }
            e => panic!("Tokens <{e:?}> not found!"),
        }
        i += 1;
    }
}

fn solve_pt1(input_text: &str) -> u64 {
    let packets = parse_input_pt1(&input_text);
    let right_order = &packets
        .into_iter()
        .map(|packets| {
            let Packets { left, right } = packets;
            let left = left.recursive_tokens();
            let right = right.recursive_tokens();
            check_order(&left, &right).unwrap()
        })
        .collect::<Vec<bool>>();
    right_order
        .iter()
        .enumerate()
        .fold(0, |acc, (i, order)| if *order { acc + i + 1 } else { acc }) as u64
}

fn solve_pt2(input_text: &str) -> u64 {
    let mut packets = parse_input_pt2(&input_text);
    let divivder_2: Token = Token::List(vec![Token::List(vec![Token::Integer(2)])]);
    let divivder_6: Token = Token::List(vec![Token::List(vec![Token::Integer(6)])]);
    packets.push(divivder_2.clone());
    packets.push(divivder_6.clone());
    packets.sort_by(|left, right| {
        let left = left.recursive_tokens();
        let right = right.recursive_tokens();
        if check_order(&left, &right).unwrap() {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    });

    let position_2 = packets
        .clone()
        .into_iter()
        .position(|packet| packet == divivder_2)
        .unwrap()
        + 1;
    let position_6 = packets
        .into_iter()
        .position(|packet| packet == divivder_6)
        .unwrap()
        + 1;
    (position_2 * position_6) as u64
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_13_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {}", solve_pt1(&input_text));
    // Correct: 6656

    println!("Part two: {}", solve_pt2(&input_text));
    // Correct: 19716

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_rust::test_solution;

    const TEST_DATA: &str = "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]";
    const ANS_PT1: u64 = 13;
    const ANS_PT2: u64 = 140;

    test_solution!(test1, solve_pt1, 13, TEST_DATA);
    test_solution!(test2, solve_pt2, 140, TEST_DATA);
}
