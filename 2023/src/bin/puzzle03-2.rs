// https://adventofcode.com/2023/day/3#part2
// The missing part wasn't the only issue - one of the gears in the engine is wrong.
// A gear is any * symbol that is adjacent to exactly two part numbers. Its gear ratio
// is the result of multiplying those two numbers together.
//
// This time, you need to find the gear ratio of every gear and add them all up
// so that the engineer can figure out which gear needs to be replaced.
//
// What is the sum of all of the gear ratios in your engine schematic?
#![allow(clippy::trivially_copy_pass_by_ref)]

extern crate advent_of_code_2023 as advent;

use std::collections::HashSet;
use std::fmt::Debug;
use std::io::Error;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::satisfy;
use nom::combinator::map;
use nom::{IResult, Parser};

use advent::matrix::{Coord, Matrix};
use advent::parser::matrix;
use advent::runner::run_puzzle;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Item {
    Digit(u32),
    Symbol(char),
    Empty,
}

impl Item {
    fn is_digit(&self) -> bool {
        matches!(self, Item::Digit(_))
    }

    fn as_digit(&self) -> Option<u32> {
        match self {
            Item::Digit(d) => Some(*d),
            _ => None,
        }
    }
}

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle03.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Matrix<Item>> {
    matrix(item).parse(input)
}

fn puzzle(input: &Matrix<Item>) -> u32 {
    let gears = input.search(|v| matches!(v, Item::Symbol('*')));
    let next_coords = |c @ (x, _): Coord| input.neighbours8(c).filter(move |&(nx, _)| nx == x);
    let is_digit = |c: &Coord| input.get(*c).unwrap().is_digit();
    let start_points: Vec<Vec<Coord>> = gears
        .map(|c| input.neighbours8(c).filter(is_digit).collect())
        .collect();
    let numbers = start_points
        .into_iter()
        .map(|gear_numbers| {
            let digits: HashSet<Vec<Coord>> = gear_numbers
                .into_iter()
                .map(|start| {
                    let mut number: Vec<Coord> = input
                        .walk(start, |c| next_coords(c).collect(), Item::is_digit)
                        .collect();
                    number.sort_unstable();
                    number
                })
                .collect();
            digits
                .into_iter()
                .map(|digits| to_number(input, &digits))
                .collect()
        })
        .filter(|numbers: &Vec<u32>| numbers.len() == 2);
    let powers = numbers.map(|numbers| numbers.iter().product::<u32>());
    powers.sum()
}

fn to_number(matrix: &Matrix<Item>, coords: &[Coord]) -> u32 {
    coords.iter().fold(0, |acc, &c| {
        acc * 10 + matrix.get(c).unwrap().as_digit().unwrap()
    })
}

fn item(input: &str) -> IResult<&str, Item> {
    let is_digit = |c: char| c.is_ascii_digit();
    let is_symbol = move |c: char| !is_digit(c) && c != '.' && c != '\n';
    alt((
        map(tag("."), |_| Item::Empty),
        map(satisfy(is_digit), |c: char| {
            Item::Digit(c.to_digit(10).unwrap())
        }),
        map(satisfy(is_symbol), Item::Symbol),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use advent::parser::parse;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "467..114..\n\
                     ...*......\n\
                     ..35..633.\n\
                     ......#...\n\
                     617*......\n\
                     .....+.58.\n\
                     ..592.....\n\
                     ......755.\n\
                     ...$.*....\n\
                     .664.598..";
        assert_eq!(puzzle(&parse(input, matrix(item))?), 467_835);
        Ok(())
    }
}
