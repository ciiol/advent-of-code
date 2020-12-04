#! /usr/bin/env python3

import sys
import itertools as it
"""
https://adventofcode.com/2020/day/4
In your batch file, how many passports are valid?
Passport data is validated in batch files (your puzzle input).
Each passport is represented as a sequence of key:value pairs separated
by spaces or newlines. Passports are separated by blank lines.
Treat cid as optional.
"""


def parse_passport(text):
    pairs = list(it.chain.from_iterable(t.split(" ") for t in text.split("\n")))
    return {k: v for k, v in (pair.split(":") for pair in pairs if pair)}


def check_passport(fields):
    required = {
        "byr",
        "iyr",
        "eyr",
        "hgt",
        "hcl",
        "ecl",
        "pid"
    }
    return all(f in fields.keys() for f in required)


def main():
    lines = ''.join(sys.stdin.readlines())
    passports = [parse_passport(p) for p in lines.split("\n\n")]
    valid_passports_count = sum(1 for p in passports if check_passport(p))
    print(f'{valid_passports_count}')


if __name__ == '__main__':
    main()
