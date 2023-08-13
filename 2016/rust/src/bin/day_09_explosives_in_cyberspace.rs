/* Advent Of Code 2016 - day 9
 * https://adventofcode.com/2016/day/9
 *
 * --- Day 9: Explosives in Cyberspace ---
 * Wandering around a secure area, you come across a datalink port to a new part of the network. After briefly scanning it for interesting files, you find one file in particular that catches your attention. It's compressed with an experimental format, but fortunately, the documentation for the format is nearby.
 *
 * The format compresses a sequence of characters. Whitespace is ignored. To indicate that some sequence should be repeated, a marker is added to the file, like (10x2). To decompress this marker, take the subsequent 10 characters and repeat them 2 times. Then, continue reading the file after the repeated data.  The marker itself is not included in the decompressed output.
 *
 * If parentheses or other characters appear within the data referenced by a marker, that's okay - treat it like normal data, not a marker, and then resume looking for markers after the decompressed section.
 *
 * For example:
 *
 * ADVENT contains no markers and decompresses to itself with no changes, resulting in a decompressed length of 6.
 * A(1x5)BC repeats only the B a total of 5 times, becoming ABBBBBC for a decompressed length of 7.
 * (3x3)XYZ becomes XYZXYZXYZ for a decompressed length of 9.
 * A(2x2)BCD(2x2)EFG doubles the BC and EF, becoming ABCBCDEFEFG for a decompressed length of 11.
 * (6x1)(1x3)A simply becomes (1x3)A - the (1x3) looks like a marker, but because it's within a data section of another marker, it is not treated any differently from the A that comes after it. It has a decompressed length of 6.
 * X(8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY (for a decompressed length of 18), because the decompressed data from the (8x2) marker (the (3x3)ABC) is skipped and not processed further.
 *
 *
 * What is the decompressed length of the file (your puzzle input)? Don't count whitespace.
 *
 *
 *--- Part Two ---
 * Apparently, the file actually uses version two of the format.
 *
 * In version two, the only difference is that markers within decompressed data are decompressed. This, the documentation explains, provides much more substantial compression capabilities, allowing many-gigabyte files to be stored in only a few kilobytes.
 *
 * For example:
 *
 * (3x3)XYZ still becomes XYZXYZXYZ, as the decompressed section contains no markers.
 * X(8x2)(3x3)ABCY becomes XABCABCABCABCABCABCY, because the decompressed data from the (8x2) marker is then further decompressed, thus triggering the (3x3) marker twice for a total of six ABC sequences.
 * (27x12)(20x12)(13x14)(7x10)(1x12)A decompresses into a string of A repeated 241920 times.
 * (25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN becomes 445 characters long.
 * Unfortunately, the computer you brought probably doesn't have enough memory to actually decompress the file; you'll have to come up with another way to get its decompressed length.
 *
 * What is the decompressed length of the file using this improved format?
*/
use aoc_rust::load_input;
use std::error::Error;

#[derive(Clone, Debug)]
struct Token {
    t: String,
    m: usize,
}

trait TokenCompiler {
    fn parse(&mut self) -> &Vec<Token>;
    fn compile(&mut self) -> String;
}

struct TokenizeV1 {
    source: String,
    _parsed: bool,
    _parsed_cache: Vec<Token>,
    _compiled: bool,
    _compiled_cache: String,
}

impl TokenizeV1 {
    fn new(input_text: &str) -> Self {
        Self {
            source: input_text.to_string(),
            _parsed: false,
            _parsed_cache: vec![],
            _compiled: false,
            _compiled_cache: "".to_string(),
        }
    }
}

impl TokenCompiler for TokenizeV1 {
    fn parse(&mut self) -> &Vec<Token> {
        if self._parsed {
            return &self._parsed_cache;
        }
        let input_length = self.source.len();
        let mut i = 0;
        let source = self.source.chars().collect::<Vec<_>>();
        while i < input_length {
            let t = source[i];
            if t.is_ascii_uppercase() {
                let token = Token {
                    t: t.to_string(),
                    m: 1,
                };
                self._parsed_cache.push(token);
            } else if t == '(' {
                let (idx, token) = Self::_parse_block(&self.source[i..]);
                i += idx;
                self._parsed_cache.push(token);
            } else {
                panic!("Cannot reach here!");
            }
            i += 1;
        }
        self._parsed = true;
        &self._parsed_cache
    }
    fn compile(&mut self) -> String {
        if self._compiled {
            return self._compiled_cache.clone();
        }
        self._compiled_cache = self
            .parse()
            .iter()
            .map(|token| token.t.repeat(token.m))
            .collect();
        self._compiled = true;
        self._compiled_cache.clone()
    }
}

impl TokenizeV1 {
    fn _parse_block(source: &str) -> (usize, Token) {
        let (block, tail) = source.strip_prefix('(').unwrap().split_once(')').unwrap();
        let (chars, m) = block.split_once('x').unwrap();
        let (chars, m) = (chars.parse::<usize>().unwrap(), m.parse::<usize>().unwrap());
        let idx = block.len() + chars;
        (
            idx + 1,
            Token {
                t: tail[..chars].to_string(),
                m,
            },
        )
    }
}

struct TokenizeV2 {
    source: String,
    _parsed: bool,
    _parsed_cache: Vec<Token>,
    _compiled: bool,
    _compiled_cache: String,
}

impl TokenizeV2 {
    fn new(input_text: &str) -> Self {
        Self {
            source: input_text.to_string(),
            _parsed: false,
            _parsed_cache: vec![],
            _compiled: false,
            _compiled_cache: "".to_string(),
        }
    }
}

impl TokenCompiler for TokenizeV2 {
    fn parse(&mut self) -> &Vec<Token> {
        if self._parsed {
            return &self._parsed_cache;
        }
        self._parsed_cache = Self::_parse_tokens(vec![Token {
            t: self.source.to_string(),
            m: 1,
        }]);
        self._parsed = true;
        &self._parsed_cache
    }
    fn compile(&mut self) -> String {
        if self._compiled {
            return self._compiled_cache.clone();
        }
        self._compiled_cache = self
            .parse()
            .iter()
            .map(|token| token.t.repeat(token.m))
            .collect();
        self._compiled = true;
        self._compiled_cache.clone()
    }
}

impl TokenizeV2 {
    fn size(&mut self) -> usize {
        self.parse()
            .iter()
            .map(|token| token.m * token.t.len())
            .sum()
    }
    fn _parse_tokens(tokens: Vec<Token>) -> Vec<Token> {
        let (with_x, mut tokens): (Vec<Token>, Vec<Token>) =
            tokens.into_iter().partition(|token| token.t.contains('x'));
        if with_x.is_empty() {
            return tokens;
        }
        let with_x = with_x
            .into_iter()
            .flat_map(Self::_split_token)
            .collect::<Vec<Token>>();
        tokens.extend(with_x);
        Self::_parse_tokens(tokens)
    }

    fn _split_token(token: Token) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];
        let input_length = token.t.len();
        let mut i = 0;
        let source = token.t.chars().collect::<Vec<_>>();
        while i < input_length {
            let t = source[i];
            if t.is_ascii_uppercase() {
                let token = Token {
                    t: t.to_string(),
                    m: token.m,
                };
                tokens.push(token);
            } else if t == '(' {
                let (new_idx, mut new_token) =
                    Self::_parse_block(&source[i..].iter().collect::<String>());
                new_token.m *= token.m;
                i += new_idx;
                tokens.push(new_token);
            } else {
                panic!("Cannot reach here!");
            }
            i += 1;
        }
        tokens
    }

    fn _parse_block(source: &str) -> (usize, Token) {
        let (block, tail) = source.strip_prefix('(').unwrap().split_once(')').unwrap();
        let (chars, m) = block.split_once('x').unwrap();
        let (chars, m) = (chars.parse::<usize>().unwrap(), m.parse::<usize>().unwrap());
        let idx = block.len() + chars;
        let tail = &tail[..chars];
        (
            idx + 1,
            Token {
                t: tail.to_string(),
                m,
            },
        )
    }
}

fn solve_pt1(input_text: &str) -> u64 {
    TokenizeV1::new(input_text).compile().len() as u64
}

fn solve_pt2(input_text: &str) -> u64 {
    TokenizeV2::new(input_text).size() as u64
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_09_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {:#?}", solve_pt1(&input_text));
    // solution_pt1: 138735

    println!("Part two: {:#?}", solve_pt2(&input_text));
    // solution_pt2: 11125026826

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_rust::test_solution;

    test_solution!(test1, solve_pt1, 6, "ADVENT");
    test_solution!(test2, solve_pt1, 7, "A(1x5)BC");
    test_solution!(test3, solve_pt1, 9, "(3x3)XYZ");
    test_solution!(test4, solve_pt1, 11, "A(2x2)BCD(2x2)EFG");
    test_solution!(test5, solve_pt1, 6, "(6x1)(1x3)A");
    test_solution!(test6, solve_pt1, 18, "X(8x2)(3x3)ABCY");
    test_solution!(test7, solve_pt2, 9, "(3x3)XYZ");
    test_solution!(test8, solve_pt2, 20, "X(8x2)(3x3)ABCY");
    test_solution!(
        test9,
        solve_pt2,
        241920,
        "(27x12)(20x12)(13x14)(7x10)(1x12)A"
    );
    test_solution!(
        test10,
        solve_pt2,
        445,
        "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
    );
}
