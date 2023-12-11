// https://adventofcode.com/2023/day/11
// The researcher has collected a bunch of data and compiled the data into a single
// giant image (your puzzle input). The image includes empty space (.) and galaxies (#).
// The researcher is trying to figure out the sum of the lengths of the shortest
// path between every pair of galaxies.
//
// Due to something involving gravitational effects, only some space expands. In fact,
// the result is that any rows or columns that contain no galaxies should
// all actually be twice as big.
//
// Expand the universe, then find the length of the shortest path between
// every pair of galaxies. What is the sum of these lengths?
extern crate advent_of_code_2023 as advent;

use std::cmp::{max, min};
use std::io::Error;

use ndarray::ArrayView1;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::value;
use nom::{IResult, Parser};

use advent::matrix::{Coord, Matrix};
use advent::parser::matrix;
use advent::runner::run_puzzle;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Pixel {
    Empty,
    Galaxy,
}

impl Pixel {
    fn is_galaxy(self) -> bool {
        matches!(self, Pixel::Galaxy)
    }
}

type Image = Matrix<Pixel>;

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle11.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Image> {
    let pixel = alt((
        value(Pixel::Empty, tag(".")),
        value(Pixel::Galaxy, tag("#")),
    ));
    matrix(pixel).parse(input)
}

fn puzzle(input: &Image) -> usize {
    let rows: Vec<_> = input.rows().collect();
    let cols: Vec<_> = input.cols().collect();
    let expanding_rows: Vec<_> = expanding_indexes(&rows).collect();
    let expanding_cols: Vec<_> = expanding_indexes(&cols).collect();
    let galaxies: Vec<_> = input
        .coords()
        .filter(|coord| input.get(*coord).unwrap().is_galaxy())
        .collect();
    let mut distances: usize = 0;
    for (i, a) in galaxies.iter().enumerate() {
        for b in galaxies.iter().skip(i + 1) {
            let distance = distance(*a, *b, &expanding_rows, &expanding_cols);
            distances += distance;
        }
    }
    distances
}

fn expanding_indexes<'a>(array: &'a [ArrayView1<'a, Pixel>]) -> impl Iterator<Item = usize> + '_ {
    array.iter().enumerate().filter_map(|(i, line)| {
        if line.iter().all(|&p| !p.is_galaxy()) {
            Some(i)
        } else {
            None
        }
    })
}

fn distance(a: Coord, b: Coord, expanding_rows: &[usize], expanding_cols: &[usize]) -> usize {
    let a_row: isize = a.0.try_into().unwrap();
    let a_col: isize = a.1.try_into().unwrap();
    let b_row: isize = b.0.try_into().unwrap();
    let b_col: isize = b.1.try_into().unwrap();
    let row_distance: usize = (a_row - b_row).abs().try_into().unwrap();
    let col_distance: usize = (a_col - b_col).abs().try_into().unwrap();
    let distance = row_distance + col_distance;
    let expanded_rows = expanding_rows
        .iter()
        .filter(|i| **i >= min(a.0, b.0) && **i <= max(a.0, b.0))
        .count();
    let expanded_cols = expanding_cols
        .iter()
        .filter(|i| **i >= min(a.1, b.1) && **i <= max(a.1, b.1))
        .count();
    let multiplying_factor = 2;
    distance + (multiplying_factor - 1) * (expanded_cols + expanded_rows)
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "...#......\n\
                     .......#..\n\
                     #.........\n\
                     ..........\n\
                     ......#...\n\
                     .#........\n\
                     .........#\n\
                     ..........\n\
                     .......#..\n\
                     #...#.....";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 374);
        Ok(())
    }
}
