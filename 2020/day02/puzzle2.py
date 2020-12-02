#! /usr/bin/env python3

import sys
"""
https://adventofcode.com/2020/day/2#part2
How many passwords are valid according to their policies?

Each policy actually describes two positions in the password,
where 1 means the first character, 2 means the second character, and so on.
(Be careful; Toboggan Corporate Policies have no concept of "index zero"!)
Exactly one of these positions must contain the given letter.
Other occurrences of the letter are irrelevant for the purposes
of policy enforcement.
"""


def only_one(seq):
    return sum(1 for i in seq if i) == 1


def check(definition):
    rule, password = definition.split(': ', maxsplit=1)
    return check_rule(parse_rule(rule), password)


def parse_rule(rule):
    number_range, letter = rule.split(' ', maxsplit=1)
    indexes = [int(n) - 1 for n in number_range.split('-')]
    return letter, indexes


def check_rule(rule, password):
    letter, indexes = rule
    return only_one(password[i] == letter for i in indexes)


def main():
    valid_count = len([s for s in sys.stdin.readlines() if check(s)])
    print(f'{valid_count}')


if __name__ == '__main__':
    main()
