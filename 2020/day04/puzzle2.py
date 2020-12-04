#! /usr/bin/env python3

import sys
import re
import itertools as it
"""
https://adventofcode.com/2020/day/4#part2
You can continue to ignore the cid field, but each other field has strict rules
about what values are valid for automatic validation:

    byr (Birth Year) - four digits; at least 1920 and at most 2002.
    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    hgt (Height) - a number followed by either cm or in:
        If cm, the number must be at least 150 and at most 193.
        If in, the number must be at least 59 and at most 76.
    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    pid (Passport ID) - a nine-digit number, including leading zeroes.
    cid (Country ID) - ignored, missing or not.

"""


def parse_passport(text):
    pairs = list(it.chain.from_iterable(t.split(" ") for t in text.split("\n")))
    return {k: v for k, v in (pair.split(":") for pair in pairs if pair)}


def byr_validator(value):
    return re.match(r'^\d{4}$', value) and int(value) >= 1920 and int(value) <= 2002


def iyr_validator(value):
    return re.match(r'^\d{4}$', value) and int(value) >= 2010 and int(value) <= 2020


def eyr_validator(value):
    return re.match(r'^\d{4}$', value) and int(value) >= 2020 and int(value) <= 2030


def hgt_validator(value):
    match = re.match(r'^(\d+)(cm|in)$', value)
    if not match:
        return False
    height, unit = match.groups()
    height = int(height)
    if unit == 'cm':
        return height >= 150 and height <= 193
    elif unit == 'in':
        return height >= 59 and height <= 76


def hcl_validator(value):
    return re.match(r'^#[0-9a-f]{6}$', value) is not None


def ecl_validator(value):
    return re.match(r'^(amb|blu|brn|gry|grn|hzl|oth)$', value) is not None


def pid_validator(value):
    return re.match(r'^\d{9}$', value) is not None


def check_passport(fields):
    rules = {
        "byr": byr_validator,
        "iyr": iyr_validator,
        "eyr": eyr_validator,
        "hgt": hgt_validator,
        "hcl": hcl_validator,
        "ecl": ecl_validator,
        "pid": pid_validator
    }
    return all(
        f in fields and validator(fields[f])
        for f, validator in rules.items()
    )


def main():
    lines = ''.join(sys.stdin.readlines())
    passports = [parse_passport(p) for p in lines.split("\n\n")]
    valid_passports_count = sum(1 for p in passports if check_passport(p))
    print(f'{valid_passports_count}')


if __name__ == '__main__':
    main()
