/* Advent Of Code 2016 - day 7
 * https://adventofcode.com/2016/day/7
 *
 * --- Day 7: Internet Protocol Version 7 ---
 * While snooping around the local network of EBHQ, you compile a list of IP addresses (they're IPv7, of course; IPv6 is much too limited). You'd like to figure out which IPs support TLS (transport-layer snooping).
 *
 * An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA.  An ABBA is any four-character sequence which consists of a pair of two different characters followed by the reverse of that pair, such as xyyx or abba.  However, the IP also must not have an ABBA within any hypernet sequences, which are contained by square brackets.
 *
 * For example:
 *
 * abba[mnop]qrst supports TLS (abba outside square brackets).
 * abcd[bddb]xyyx does not support TLS (bddb is within square brackets, even though xyyx is outside square brackets).
 * aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior characters must be different).
 * ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even though it's within a larger string).
 *
 *
 * How many IPs in your puzzle input support TLS?
 *
 *
 * --- Part Two ---
 * You would also like to know which IPs support SSL (super-secret listening).
 *
 * An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in the supernet sequences (outside any square bracketed supernets), and a corresponding Byte Allocation Block, or BAB, anywhere in the hypernet sequences. An ABA is any three-character sequence which consists of the same character twice with a different character between them, such as xyx or aba. A corresponding BAB is the same characters but in reversed positions: yxy and bab, respectively.
 *
 * For example:
 *
 * aba[bab]xyz supports SSL (aba outside square brackets with corresponding bab within square brackets).
 * xyx[xyx]xyx does not support SSL (xyx, but no corresponding yxy).
 * aaa[kek]eke supports SSL (eke in supernet with corresponding kek in hypernet; the aaa sequence is not related, because the interior character must be different).
 * zazbz[bzb]cdb supports SSL (zaz has no corresponding aza, but zbz has a corresponding bzb, even though zaz and zbz overlap).
 * How many IPs in your puzzle input support SSL?
*/

use aoc_lang_rust::load_input;
use std::error::Error;

#[derive(Debug)]
struct Ip<'a> {
    supernets: Vec<&'a str>,
    hypernets: Vec<&'a str>,
}

impl<'a> Ip<'a> {
    fn parse(ip_string: &'a str) -> Self {
        let (supernets, hypernets): (Vec<_>, Vec<_>) = ip_string
            .split(['[', ']'])
            .enumerate()
            .partition(|(i, _)| i % 2 == 0);
        let supernets = supernets.into_iter().map(|(_, s)| s).collect();
        let hypernets = hypernets.into_iter().map(|(_, s)| s).collect();
        Self {
            supernets,
            hypernets,
        }
    }

    fn list_palindromes(string: &str, block_size: usize) -> Vec<String> {
        let chars = string.chars().collect::<Vec<char>>();
        chars
            .windows(block_size)
            .filter(|w| Self::is_palindrome(w))
            .map(|w| w.iter().collect::<String>())
            .collect()
    }

    fn is_palindrome(seq: &[char]) -> bool {
        let block_size = seq.len();
        (0..(block_size / 2 + 1))
            .collect::<Vec<_>>()
            .windows(2)
            .any(|c| seq[c[0]] != seq[c[1]])
            & (0..(block_size / 2)).all(|i| seq[i] == seq[block_size - 1 - i])
    }

    fn supports_tls(&self) -> bool {
        const TLS_BLOCK: usize = 4;
        (self
            .hypernets
            .clone()
            .into_iter()
            .flat_map(|h| Self::list_palindromes(h, TLS_BLOCK))
            .count()
            == 0)
            & (self
                .supernets
                .clone()
                .into_iter()
                .flat_map(|s| Self::list_palindromes(s, TLS_BLOCK))
                .count()
                != 0)
    }

    fn supports_ssl(&self) -> bool {
        const SSL_BLOCK: usize = 3;
        let abas: Vec<_> = self
            .supernets
            .clone()
            .into_iter()
            .flat_map(|s| Self::list_palindromes(s, SSL_BLOCK))
            .collect();
        let babs: Vec<_> = self
            .hypernets
            .clone()
            .into_iter()
            .flat_map(|h| Self::list_palindromes(h, SSL_BLOCK))
            .collect();
        abas.iter().any(|aba| {
            babs.iter().any(|bab| {
                (aba.chars().next() == bab.chars().nth(1))
                    & (aba.chars().nth(1) == bab.chars().next())
            })
        })
    }
}

fn solve_pt1(input_text: &str) -> u64 {
    let ips: Vec<_> = input_text
        .lines()
        .map(Ip::parse)
        .filter(|ip| ip.supports_tls())
        .collect();
    ips.len().try_into().unwrap()
}

fn solve_pt2(input_text: &str) -> u64 {
    let ips: Vec<_> = input_text
        .lines()
        .map(Ip::parse)
        .filter(|ip| ip.supports_ssl())
        .collect();
    ips.len().try_into().unwrap()
}

fn main() -> Result<(), Box<dyn Error>> {
    const FILENAME: &str = "../data/day_07_input.txt";
    let input_text = load_input(FILENAME);

    println!("Part one: {:#?}", solve_pt1(&input_text));
    // solution_pt1: 115

    println!("Part two: {:#?}", solve_pt2(&input_text));
    // solution_pt2: 231

    Ok(())
}

// Example tests
#[cfg(test)]
mod example {
    use aoc_lang_rust::test_solution;
    const INPUT_1_1: &str = "abba[mnop]qrst\n\
abcd[bddb]xyyx\n\
aaaa[qwer]tyui\n\
ioxxoj[asdfgh]zxcvbn";
    const INPUT_2_1: &str = "aba[bab]xyz\n\
xyx[xyx]xyx\n\
aaa[kek]eke\n\
zazbz[bzb]cdb";

    const INPUT_2_2: &str =
        "zgeiqtfvjgsjbcgluma[hwyhtrykkxccmfg]okqorlbwctwfgvntgmv[yiralgrosisdxzkse]tzqnsaemaeiisiy\n\
        hleplitmpnifqlj[qctinqcllgdgwbtgker]yzuduoqubabohbwzobr[trvxejtdgdjgbgrdbt]ypkeguppycuoeej";
    test_solution!(test1, solve_pt1, 2, INPUT_1_1);
    test_solution!(test2, solve_pt2, 3, INPUT_2_1);
    test_solution!(test3, solve_pt2, 1, INPUT_2_2);
}
