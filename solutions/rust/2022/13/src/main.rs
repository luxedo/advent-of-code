/*
* ElfScript Brigade
*
* Advent Of Code 2022 Day 13
* Rust Solution
*
* Day 13: Distress Signal
*
* https://adventofcode.com/2022/day/13
*
*/

use esb_fireplace::{FireplaceError, FireplaceResult};

use std::fmt::Display;
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

fn solve_pt1(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let packets = parse_input_pt1(&input_data);
    let right_order = &packets
        .into_iter()
        .map(|packets| {
            let Packets { left, right } = packets;
            let left = left.recursive_tokens();
            let right = right.recursive_tokens();
            check_order(&left, &right).unwrap()
        })
        .collect::<Vec<bool>>();
    Ok(right_order
        .iter()
        .enumerate()
        .fold(0, |acc, (i, order)| if *order { acc + i + 1 } else { acc }))
}

fn solve_pt2(input_data: &str, _args: Vec<String>) -> FireplaceResult<impl Display> {
    let mut packets = parse_input_pt2(&input_data);
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
    Ok(position_2 * position_6)
}

fn main() -> Result<(), FireplaceError> {
    // 🎅🎄❄️☃️🎁🦌
    // Bright christmas lights HERE
    esb_fireplace::v1_run(solve_pt1, solve_pt2)?;
    Ok(())
}
