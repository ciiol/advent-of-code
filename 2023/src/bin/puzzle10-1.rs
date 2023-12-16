// https://adventofcode.com/2023/day/10
// You make a quick sketch of all of the surface pipes you can see (your puzzle input).
//
// The pipes are arranged in a two-dimensional grid of tiles:
// - | is a vertical pipe connecting north and south.
// - - is a horizontal pipe connecting east and west.
// - L is a 90-degree bend connecting north and east.
// - J is a 90-degree bend connecting north and west.
// - 7 is a 90-degree bend connecting south and west.
// - F is a 90-degree bend connecting south and east.
// - . is ground; there is no pipe in this tile.
// - S is the starting position of the animal; there is a pipe on this tile,
//   but your sketch doesn't show what shape the pipe has.
//
// Based on the acoustics of the animal's scurrying, you're confident the pipe
// that contains the animal is one large, continuous loop.
// Find the single giant loop starting at S. How many steps along the loop does it take to get
// from the starting position to the point farthest from the starting position?
extern crate advent_of_code_2023 as advent;

use std::collections::{HashSet, VecDeque};
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
    Vertical,
    Horizontal,
    PipeL,
    PipeJ,
    Pipe7,
    PipeF,
    Ground,
    Start,
}

type Sketch = Matrix<Tile>;

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle10.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Sketch> {
    let tile = alt((
        value(Tile::Vertical, tag("|")),
        value(Tile::Horizontal, tag("-")),
        value(Tile::PipeL, tag("L")),
        value(Tile::PipeJ, tag("J")),
        value(Tile::Pipe7, tag("7")),
        value(Tile::PipeF, tag("F")),
        value(Tile::Ground, tag(".")),
        value(Tile::Start, tag("S")),
    ));
    matrix(tile).parse(input)
}

fn puzzle(input: &Sketch) -> usize {
    let start = input.search(|tile| *tile == Tile::Start).next().unwrap();
    walk_with_distance(start, input)
        .max_by_key(|(distance, _)| *distance)
        .unwrap()
        .0
}

fn tile_to_direction(tile: &Tile) -> impl Iterator<Item = (isize, isize)> + '_ {
    match tile {
        Tile::Vertical => vec![(1, 0), (-1, 0)].into_iter(),
        Tile::Horizontal => vec![(0, 1), (0, -1)].into_iter(),
        Tile::PipeL => vec![(-1, 0), (0, 1)].into_iter(),
        Tile::PipeJ => vec![(-1, 0), (0, -1)].into_iter(),
        Tile::Pipe7 => vec![(1, 0), (0, -1)].into_iter(),
        Tile::PipeF => vec![(1, 0), (0, 1)].into_iter(),
        Tile::Ground => vec![].into_iter(),
        Tile::Start => vec![(1, 0), (-1, 0), (0, 1), (0, -1)].into_iter(),
    }
}

fn is_opposite_direction(direction1: (isize, isize), direction2: (isize, isize)) -> bool {
    let (row1, col1) = direction1;
    let (row2, col2) = direction2;
    row1 + row2 == 0 && col1 + col2 == 0
}

fn next_tiles(coord: Coord, sketch: &Sketch) -> impl Iterator<Item = Coord> + '_ {
    let tile = sketch.get(coord).unwrap();
    tile_to_direction(tile).filter_map(move |direction| {
        let next_coord = (coord + direction.into()).unwrap();
        let next_tile = sketch.get(next_coord).unwrap();
        if tile_to_direction(next_tile).any(|d| is_opposite_direction(d, direction)) {
            Some(next_coord)
        } else {
            None
        }
    })
}

fn walk_with_distance(start: Coord, sketch: &Sketch) -> impl Iterator<Item = (usize, Coord)> + '_ {
    let mut visited = HashSet::new();
    let mut stack = VecDeque::from([(0, start)]);
    std::iter::from_fn(move || {
        while let Some((distance, coord)) = stack.pop_front() {
            if visited.contains(&coord) {
                continue;
            }
            visited.insert(coord);
            stack.extend(next_tiles(coord, sketch).map(|c| (distance + 1, c)));
            return Some((distance, coord));
        }
        None
    })
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = ".....\n\
                     .S-7.\n\
                     .|.|.\n\
                     .L-J.\n\
                     .....";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 4);
        Ok(())
    }
}
