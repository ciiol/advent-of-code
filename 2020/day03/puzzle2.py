#! /usr/bin/env python3

import sys
import operator
from functools import reduce
"""
https://adventofcode.com/2020/day/3#part2
Determine the number of trees you would encounter if, for
each of the following slopes, you start at the top-left
corner and traverse the map all the way to the bottom:

    Right 1, down 1.
    Right 3, down 1. (This is the slope you already checked.)
    Right 5, down 1.
    Right 7, down 1.
    Right 1, down 2.

In the above example, these slopes would find 2, 7, 3, 4, and
2 tree(s) respectively; multiplied together, these produce the
answer 336.
"""


def parse_forest(lines):
    return [[c == '#' for c in line.strip()] for line in lines]


def count_trees(forest, path):
    right_steps, down_steps = path
    pattern_len = len(forest[0])
    forest_len = len(forest)
    num = 0
    horizonal_index = 0
    vertical_index = 0
    while True:
        horizonal_index = (horizonal_index + right_steps) % pattern_len
        vertical_index = vertical_index + down_steps
        if vertical_index >= forest_len:
            return num
        if forest[vertical_index][horizonal_index]:
            num += 1


def main():
    pathes = [
        (1, 1),
        (3, 1),
        (5, 1),
        (7, 1),
        (1, 2)
    ]
    forest = parse_forest(sys.stdin.readlines())
    lens = [count_trees(forest, path) for path in pathes]
    result = reduce(operator.mul, lens, 1)
    print(f'{result}')


if __name__ == '__main__':
    main()
