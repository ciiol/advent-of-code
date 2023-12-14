// https://adventofcode.com/2023/day/14
// If you move the rocks, you can focus the dish. The platform even has a control panel on the side that lets you tilt
// it in one of four directions! The rounded rocks (O) will roll when the platform is tilted, while the cube-shaped
// rocks (#) will stay in place. You note the positions of all of the empty spaces (.) and rocks (your puzzle input).
//
// You notice that the support beams along the north side of the platform are damaged; to ensure the platform
// doesn't collapse, you should calculate the total load on the north support beams.
//
// The amount of load caused by a single rounded rock (O) is equal to the number of rows from the rock to
// the south edge of the platform, including the row the rock is on. (Cube-shaped rocks (#) don't contribute to load.)
//
// Tilt the platform so that the rounded rocks all roll north.
// Afterward, what is the total load on the north support beams?
extern crate advent_of_code_2023 as advent;

use std::io::Error;

use ndarray::ArrayView1;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::value;
use nom::{IResult, Parser};

use advent::matrix::Matrix;
use advent::parser::matrix;
use advent::runner::run_puzzle;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Tile {
    Rounded,
    Cube,
    Empty,
}

type Platform = Matrix<Tile>;

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle14.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Platform> {
    let tile = alt((
        value(Tile::Rounded, tag("O")),
        value(Tile::Cube, tag("#")),
        value(Tile::Empty, tag(".")),
    ));
    matrix(tile).parse(input)
}

fn puzzle(input: &Platform) -> usize {
    input.cols().map(|col| weigth(&move_rocks(col))).sum()
}

fn move_rocks(line: ArrayView1<Tile>) -> Vec<Tile> {
    let mut result = vec![Tile::Empty; line.len()];
    let mut free_spot = 0;
    for (i, tile) in line.iter().enumerate() {
        match tile {
            Tile::Empty => {}
            Tile::Cube => {
                free_spot = i + 1;
                result[i] = Tile::Cube;
            }
            Tile::Rounded => {
                result[free_spot] = Tile::Rounded;
                free_spot += 1;
            }
        }
    }
    result
}

fn weigth(line: &[Tile]) -> usize {
    let len = line.len();
    line.iter()
        .enumerate()
        .filter_map(|(i, tile)| match tile {
            Tile::Empty | Tile::Cube => None,
            Tile::Rounded => Some(len - i),
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "O....#....\n\
                     O.OO#....#\n\
                     .....##...\n\
                     OO.#O....O\n\
                     .O.....O#.\n\
                     O.#..O.#.#\n\
                     ..O..#O..O\n\
                     .......O..\n\
                     #....###..\n\
                     #OO..#....";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 136);
        Ok(())
    }
}
