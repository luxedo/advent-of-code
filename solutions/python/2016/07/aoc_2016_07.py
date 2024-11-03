"""
ElfScript Brigade

Advent Of Code 2016 Day 07
Python Solution

Day 7: Internet Protocol Version 7

https://adventofcode.com/2016/day/7
"""

from typing import Iterable, Iterator


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


def solve_pt1(input_data: str, _args: list[str] | None = None) -> int:
    # solution_pt1: 115
    return len([ip for ip in input_data.splitlines() if supports_tls(ip)])


def solve_pt2(input_data: str, _args: list[str] | None = None) -> int:
    # solution_pt2: 231
    return len([ip for ip in input_data.splitlines() if supports_ssl(ip)])


if __name__ == "__main__":
    from esb.protocol import fireplace

    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    fireplace.v1_run(solve_pt1, solve_pt2)
