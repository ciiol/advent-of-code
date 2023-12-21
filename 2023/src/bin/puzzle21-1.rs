// https://adventofcode.com/2023/day/21
// He needs to get his steps in for the day, and so he'd like to know which garden plots
// he can reach with exactly his remaining 64 steps.
//
// He gives you an up-to-date map (your puzzle input) of his starting position (S), garden plots (.), and rocks (#).
//
// Starting from the garden plot marked S on your map, how many garden plots could the Elf reach in exactly 64 steps?
extern crate advent_of_code_2023 as advent;

use std::collections::HashSet;
use std::hash::Hash;
use std::io::Error;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::value;
use nom::{IResult, Parser};

use advent::matrix::{Coord, Matrix};
use advent::parser::matrix;
use advent::runner::run_puzzle;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
enum Tile {
    Plots,
    Rock,
    Start,
}

type Map = Matrix<Tile>;

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle21.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Map> {
    let tile = alt((
        value(Tile::Plots, tag(".")),
        value(Tile::Rock, tag("#")),
        value(Tile::Start, tag("S")),
    ));
    matrix(tile).parse(input)
}

fn puzzle(input: &Map) -> usize {
    let mut points: HashSet<Coord> = HashSet::new();
    points.extend(input.search(|&t| t == Tile::Start));
    for _ in 0..64 {
        points = points
            .iter()
            .flat_map(|&p| {
                input
                    .neighbours4(p)
                    .filter(|&n| input.get(n) != Some(&Tile::Rock))
            })
            .collect();
    }
    points.len()
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "...........\n\
                     .....###.#.\n\
                     .###.##..#.\n\
                     ..#.#...#..\n\
                     ....#.#....\n\
                     .##..S####.\n\
                     .##..#...#.\n\
                     .......##..\n\
                     .##.#.####.\n\
                     .##..##.##.\n\
                     ...........";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 42);
        Ok(())
    }
}
