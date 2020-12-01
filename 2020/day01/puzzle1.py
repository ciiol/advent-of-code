#! /usr/bin/env python3

import sys
"""
https://adventofcode.com/2020/day/1
You need you to find the two entries that sum to 2020 and then
multiply those two numbers together.
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


def main():
    numbers = sorted(int(s) for s in sys.stdin.readlines())
    number1, number2 = find_sum(numbers, 2020)
    production = number1 * number2
    print(f'{number1} * {number2} = {production}')


if __name__ == '__main__':
    main()
