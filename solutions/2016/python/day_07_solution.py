"""
Advent Of Code 2016
https://adventofcode.com/2016/day/7

--- Day 7: Internet Protocol Version 7 ---
While snooping around the local network of EBHQ, you compile a list of IP addresses (they're IPv7, of course; IPv6 is much too limited). You'd like to figure out which IPs support TLS (transport-layer snooping).

An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA.  An ABBA is any four-character sequence which consists of a pair of two different characters followed by the reverse of that pair, such as xyyx or abba.  However, the IP also must not have an ABBA within any hypernet sequences, which are contained by square brackets.

For example:

abba[mnop]qrst supports TLS (abba outside square brackets).
abcd[bddb]xyyx does not support TLS (bddb is within square brackets, even though xyyx is outside square brackets).
aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior characters must be different).
ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even though it's within a larger string).


How many IPs in your puzzle input support TLS?


--- Part Two ---
You would also like to know which IPs support SSL (super-secret listening).

An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in the supernet sequences (outside any square bracketed sections), and a corresponding Byte Allocation Block, or BAB, anywhere in the hypernet sequences. An ABA is any three-character sequence which consists of the same character twice with a different character between them, such as xyx or aba. A corresponding BAB is the same characters but in reversed positions: yxy and bab, respectively.

For example:

aba[bab]xyz supports SSL (aba outside square brackets with corresponding bab within square brackets).
xyx[xyx]xyx does not support SSL (xyx, but no corresponding yxy).
aaa[kek]eke supports SSL (eke in supernet with corresponding kek in hypernet; the aaa sequence is not related, because the interior character must be different).
zazbz[bzb]cdb supports SSL (zaz has no corresponding aza, but zbz has a corresponding bzb, even though zaz and zbz overlap).
How many IPs in your puzzle input support SSL?
"""
from typing import Iterable, Iterator

from aoc.langs.python import runner


def find_palindromes(section: str, block_len: int) -> Iterator[str]:
    for i in range(len(section) - block_len + 1):
        s = section[i : i + block_len]
        if len(set(s)) != 1 and s == s[::-1]:
            yield s


def is_palindrome(section: str, block_len: int) -> bool:
    return next(find_palindromes(section, block_len), None) is not None


def match_bab(bab: str, abas: list[str]) -> bool:
    B, A = bab[0], bab[1]
    for aba in abas:
        a, b = aba[0], aba[1]
        if b == B and a == A:
            return True
    return False


def split_ip(ip: str) -> tuple[Iterable[str], Iterable[str]]:
    blocks = [section.split("]") for section in ("]" + ip).split("[")]
    transpose = list(zip(*blocks))
    hypernets, sections = transpose[0], transpose[1]
    return hypernets[1:], sections


def supports_tls(ip: str) -> int:
    hypernets, sections = split_ip(ip)
    block_len = 4
    return all(not is_palindrome(h, block_len) for h in hypernets) and any(
        is_palindrome(s, block_len) for s in sections
    )


def supports_ssl(ip: str) -> int:
    hypernets, sections = split_ip(ip)
    block_len = 3
    abas = [p for s in sections for p in list(find_palindromes(s, block_len))]
    babs = [p for s in hypernets for p in list(find_palindromes(s, block_len))]
    return any([match_bab(bab, abas) for bab in babs])


def solve_pt1(input_data: str) -> int:
    # solution_pt1: 115
    return len([ip for ip in input_data.splitlines() if supports_tls(ip)])


def solve_pt2(input_data: str) -> int:
    # solution_pt2: 231
    return len([ip for ip in input_data.splitlines() if supports_ssl(ip)])


if __name__ == "__main__":
    input_1 = """abba[mnop]qrst
abcd[bddb]xyyx
aaaa[qwer]tyui
ioxxoj[asdfgh]zxcvbn"""
    input_2 = """aba[bab]xyz
xyx[xyx]xyx
aaa[kek]eke
zazbz[bzb]cdb"""
    tests = [
        runner.AocTestCase(func=solve_pt1, output=2, input_data=input_1),
        runner.AocTestCase(func=solve_pt2, output=3, input_data=input_2),
    ]
    runner.run(solve_pt1, solve_pt2, tests)
