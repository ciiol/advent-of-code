#! /usr/bin/env python3

import sys


def compute_fuel(mass):
    fuel = (mass // 3) - 2
    if fuel < 0:
        return 0
    else:
        return fuel


def compute_full_fuel(mass):
    if mass == 0:
        return 0
    fuel = compute_fuel(mass)
    return fuel + compute_full_fuel(fuel)


def main():
    lines = sys.stdin.readlines()
    numbers = [int(s) for s in lines]
    fuels_sum = sum(compute_fuel(n) for n in numbers)
    full_fuels_sum = sum(compute_full_fuel(n) for n in numbers)
    print(f'fuels sum = {fuels_sum}')
    print(f'full fuels sum = {full_fuels_sum}')


if __name__ == '__main__':
    main()
