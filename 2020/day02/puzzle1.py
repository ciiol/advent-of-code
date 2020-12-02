#! /usr/bin/env python3

import sys
"""
https://adventofcode.com/2020/day/2
How many passwords are valid according to their policies?

For example, suppose you have the following list:

1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc

Each line gives the password policy and then the password.
The password policy indicates the lowest and highest number of
times a given letter must appear for the password to be valid.
For example, 1-3 a means that the password must contain a at
least 1 time and at most 3 times.

In the above example, 2 passwords are valid. The middle password, cdefg,
is not; it contains no instances of b, but needs at least 1. The first
and third passwords are valid: they contain one a or nine c, both
within the limits of their respective policies.
"""


def check(definition):
    rule, password = definition.split(': ', maxsplit=1)
    return check_rule(parse_rule(rule), password)


def parse_rule(rule):
    number_range, letter = rule.split(' ', maxsplit=1)
    start, stop = [int(n) for n in number_range.split('-')]
    return letter, range(start, stop + 1)


def check_rule(rule, password):
    letter, letter_range = rule
    return password.count(letter) in letter_range


def main():
    valid_count = len([s for s in sys.stdin.readlines() if check(s)])
    print(f'{valid_count}')


if __name__ == '__main__':
    main()
