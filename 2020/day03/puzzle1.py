#! /usr/bin/env python3

import sys
"""
https://adventofcode.com/2020/day/3
Starting at the top-left corner of your map and following a
slope of right 3 and down 1, how many trees would you encounter?

Due to the local geology, trees in this area only grow on exact
integer coordinates in a grid. You make a map (your puzzle input)
of the open squares (.) and trees (#) you can see. For example:

..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#

These aren't the only trees, though; due to something you read
about once involving arboreal genetics and biome stability, the
same pattern repeats to the right many times
"""


def parse_forest(lines):
    return [[c == '#' for c in line.strip()] for line in lines]


def count_trees(forest, right_steps):
    pattern_len = len(forest[0])
    num = 0
    horizonal_index = 0
    for line in forest[1:]:
        horizonal_index = (horizonal_index + right_steps) % pattern_len
        if line[horizonal_index]:
            num += 1
    return num


def main():
    result = count_trees(parse_forest(sys.stdin.readlines()), 3)
    print(f'{result}')


if __name__ == '__main__':
    main()
