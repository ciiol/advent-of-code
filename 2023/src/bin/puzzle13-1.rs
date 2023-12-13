// https://adventofcode.com/2023/day/13
// You note down the patterns of ash (.) and rocks (#) that you see as you walk (your puzzle input);
// perhaps by carefully analyzing these patterns, you can figure out where the mirrors are!
//
// To summarize your pattern notes, add up the number of columns to the left of each vertical line of reflection;
// to that, also add 100 multiplied by the number of rows above each horizontal line of reflection.
//
// Find the line of reflection in each of the patterns in your notes.
// What number do you get after summarizing all of your notes?
extern crate advent_of_code_2023 as advent;

use std::fmt::Display;
use std::io::Error;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::value;
use nom::multi::separated_list1;
use nom::{IResult, Parser};

use advent::matrix::{Coord, Matrix};
use advent::parser::matrix;
use advent::runner::run_puzzle;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Tile {
    Object,
    Empty,
}

type Field = Matrix<Tile>;

impl Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tile::Object => write!(f, "#"),
            Tile::Empty => write!(f, "."),
        }
    }
}

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle13.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Vec<Field>> {
    separated_list1(tag("\n\n"), field)(input)
}

fn field(input: &str) -> IResult<&str, Field> {
    let tile = alt((value(Tile::Object, tag("#")), value(Tile::Empty, tag("."))));
    matrix(tile).parse(input)
}

fn puzzle(input: &[Field]) -> usize {
    input.iter().map(reflection_summary).sum()
}

fn reflection_summary(field: &Field) -> usize {
    let row_candidates = 1..field.rows_size();
    let col_candidates = 1..field.cols_size();
    let mut summary = 0;
    if let Some(row) = row_candidates
        .into_iter()
        .find(|row| is_vertically_reflected_from(*row, field))
    {
        summary += row * 100;
    }
    if let Some(col) = col_candidates
        .into_iter()
        .find(|col| is_horizontally_reflected_from(*col, field))
    {
        summary += col;
    }
    assert!(summary > 0);
    summary
}

fn is_vertically_reflected_from(row: usize, field: &Field) -> bool {
    field
        .iter()
        .filter(|((r, _c), _tile)| *r < row)
        .all(|(coord, tile)| {
            if let Some(mirrored) = mirror_from_row(coord, row, field) {
                field.get(mirrored) == Some(tile)
            } else {
                true
            }
        })
}

fn is_horizontally_reflected_from(col: usize, field: &Field) -> bool {
    field
        .iter()
        .filter(|((_r, c), _tile)| *c < col)
        .all(|(coord, tile)| {
            if let Some(mirrored) = mirror_from_col(coord, col, field) {
                field.get(mirrored) == Some(tile)
            } else {
                true
            }
        })
}

fn mirror_from_row(coord: Coord, mirror: usize, field: &Field) -> Option<Coord> {
    let (coord_row, col) = coord;
    assert!(coord_row < mirror);
    let mirror: isize = mirror.try_into().unwrap();
    let coord_row: isize = coord_row.try_into().unwrap();
    let mirrored = mirror + (mirror - coord_row - 1);
    let mirrored: usize = mirrored.try_into().unwrap();
    if mirrored >= field.rows_size() {
        return None;
    }
    Some((mirrored, col))
}

fn mirror_from_col(coord: Coord, mirror: usize, field: &Field) -> Option<Coord> {
    let (row, coord_col) = coord;
    assert!(coord_col < mirror);
    let mirror: isize = mirror.try_into().unwrap();
    let coord_col: isize = coord_col.try_into().unwrap();
    let mirrored = mirror + (mirror - coord_col - 1);
    let mirrored: usize = mirrored.try_into().unwrap();
    if mirrored >= field.cols_size() {
        return None;
    }
    Some((row, mirrored))
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "#.##..##.\n\
                     ..#.##.#.\n\
                     ##......#\n\
                     ##......#\n\
                     ..#.##.#.\n\
                     ..##..##.\n\
                     #.#.##.#.\n\
                     \n\
                     #...##..#\n\
                     #....#..#\n\
                     ..##..###\n\
                     #####.##.\n\
                     #####.##.\n\
                     ..##..###\n\
                     #....#..#";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 405);
        Ok(())
    }
}
