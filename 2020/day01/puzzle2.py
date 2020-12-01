#! /usr/bin/env python3

import sys
import itertools as it
"""
https://adventofcode.com/2020/day/1#part2
You need you to find three entries that sum to 2020 and then
multiply those three numbers together.
"""


def find_sum(numbers, target):
    if not numbers:
        return None
    left_iter = iter(numbers)
    right_iter = iter(numbers[::-1])
    left = next(left_iter)
    right = next(right_iter)
    while True:
        result = left + right
        if result == target:
            return left, right
        if left == right:
            return None
        if result > target:
            right = next(right_iter)
        else:
            left = next(left_iter)


def find_three_sum(numbers, target):
    for i, left in enumerate(numbers):
        maybe_pair = find_sum(numbers[i + 1:], target - left)
        if not maybe_pair:
            continue
        return left, maybe_pair[0], maybe_pair[1]


def main():
    numbers = sorted(int(s) for s in sys.stdin.readlines())
    number1, number2, number3 = find_three_sum(numbers, 2020)
    production = number1 * number2 * number3
    print(f'{number1} * {number2} * {number3} = {production}')


if __name__ == '__main__':
    main()
