// https://adventofcode.com/2023/day/10#part2
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
// Figure out whether you have time to search for the nest by calculating the are
// within the loop. How many tiles are enclosed by the loop?
extern crate advent_of_code_2023 as advent;

use std::collections::HashSet;
use std::fmt::Display;
use std::io::Error;
use std::iter::repeat;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::newline;
use nom::combinator::{map, value};
use nom::multi::{many1, separated_list1};
use nom::IResult;

use advent::matrix::{Coord, Matrix};
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
    EphemeralV,
    EphemeralH,
}

impl Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match self {
            Tile::Vertical => '|',
            Tile::Horizontal => '-',
            Tile::PipeL => 'L',
            Tile::PipeJ => 'J',
            Tile::Pipe7 => '7',
            Tile::PipeF => 'F',
            Tile::Ground => '.',
            Tile::Start => 'S',
            Tile::EphemeralV => 'v',
            Tile::EphemeralH => 'h',
        };
        write!(f, "{c}")
    }
}

impl Tile {
    fn is_ephemeral(self) -> bool {
        matches!(self, Tile::EphemeralV | Tile::EphemeralH)
    }
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
    let row = map(many1(tile), |tiles| {
        tiles
            .into_iter()
            .flat_map(|tile| [Tile::EphemeralH, tile, Tile::EphemeralH])
            .collect()
    });
    let data = map(separated_list1(newline, row), |rows| {
        rows.into_iter()
            .flat_map(|row: Vec<Tile>| {
                let len = row.len();
                [
                    repeat(Tile::EphemeralV).take(len).collect(),
                    row,
                    repeat(Tile::EphemeralV).take(len).collect(),
                ]
            })
            .collect()
    });
    map(data, Matrix::new)(input)
}

fn puzzle(input: &Sketch) -> usize {
    let start_tile = input.search(|tile| *tile == Tile::Start).next().unwrap();
    let start = find_loop_start(start_tile, input);
    let start_loop: HashSet<Coord> = walk(start, input).collect();
    let outside = flood_fill((0, 0), &start_loop, input);
    let inside = input.iter().filter_map(|(coord, _)| {
        if outside.contains(&coord) || start_loop.contains(&coord) {
            None
        } else {
            Some(coord)
        }
    });
    inside
        .filter(|&c| !input.get(c).unwrap().is_ephemeral())
        .count()
}

// Find one of two matching non-ephemeral tiles around Start
fn find_loop_start(start: Coord, sketch: &Sketch) -> Coord {
    let probes = [(-1, 0), (1, 0), (0, 1), (0, -1)];
    for probe in &probes {
        let direction = (probe.0 * 3, probe.1 * 3);
        let coord = add_direction(start, direction);
        let tile = sketch.get(coord).unwrap();
        if !tile.is_ephemeral() && tile_to_direction(tile).any(|d| is_opposite_direction(d, *probe))
        {
            return coord;
        }
    }
    unreachable!();
}

fn add_direction(coord: Coord, direction: (isize, isize)) -> Coord {
    let (row, col) = coord;
    let row: isize = row.try_into().unwrap();
    let col: isize = col.try_into().unwrap();
    let row = row + direction.0;
    let col = col + direction.1;
    (row.try_into().unwrap_or(0), col.try_into().unwrap_or(0))
}

fn tile_to_direction(tile: &Tile) -> impl Iterator<Item = (isize, isize)> + '_ {
    match tile {
        Tile::Vertical | Tile::EphemeralV => vec![(1, 0), (-1, 0)].into_iter(),
        Tile::Horizontal | Tile::EphemeralH => vec![(0, 1), (0, -1)].into_iter(),
        Tile::PipeL => vec![(-1, 0), (0, 1)].into_iter(),
        Tile::PipeJ => vec![(-1, 0), (0, -1)].into_iter(),
        Tile::Pipe7 => vec![(1, 0), (0, -1)].into_iter(),
        Tile::PipeF => vec![(1, 0), (0, 1)].into_iter(),
        Tile::Ground | Tile::Start => vec![].into_iter(),
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
        let next_coord = add_direction(coord, direction);
        let next_tile = sketch.get(next_coord).unwrap();
        if matches!(next_tile, Tile::Start)
            || tile_to_direction(next_tile).any(|d| is_opposite_direction(d, direction))
        {
            Some(next_coord)
        } else {
            None
        }
    })
}

fn walk(start: Coord, sketch: &Sketch) -> impl Iterator<Item = Coord> + '_ {
    let mut visited = HashSet::new();
    let mut stack = vec![start];
    std::iter::from_fn(move || {
        while let Some(coord) = stack.pop() {
            if visited.contains(&coord) {
                continue;
            }
            visited.insert(coord);
            stack.extend(next_tiles(coord, sketch));
            return Some(coord);
        }
        None
    })
}

fn flood_fill(start: Coord, border: &HashSet<Coord>, sketch: &Sketch) -> HashSet<Coord> {
    let mut visited = HashSet::new();
    let mut stack = vec![start];
    while let Some(coord) = stack.pop() {
        if visited.contains(&coord) || border.contains(&coord) {
            continue;
        }
        visited.insert(coord);
        stack.extend(sketch.neighbours8(coord));
    }
    visited
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle1() -> Result<(), Error> {
        let input = "...........\n\
                     .S-------7.\n\
                     .|F-----7|.\n\
                     .||.....||.\n\
                     .||.....||.\n\
                     .|L-7.F-J|.\n\
                     .|..|.|..|.\n\
                     .L--J.L--J.\n\
                     ...........";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 4);
        Ok(())
    }

    #[test]
    fn test_puzzle2() -> Result<(), Error> {
        let input = ".F----7F7F7F7F-7....\n\
                     .|F--7||||||||FJ....\n\
                     .||.FJ||||||||L7....\n\
                     FJL7L7LJLJ||LJ.L-7..\n\
                     L--J.L7...LJS7F-7L7.\n\
                     ....F-J..F7FJ|L7L7L7\n\
                     ....L7.F7||L7|.L7L7|\n\
                     .....|FJLJ|FJ|F7|.LJ\n\
                     ....FJL-7.||.||||...\n\
                     ....L---J.LJ.LJLJ...";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 8);
        Ok(())
    }

    #[test]
    fn test_puzzle3() -> Result<(), Error> {
        let input = "FF7FSF7F7F7F7F7F---7\n\
                     L|LJ||||||||||||F--J\n\
                     FL-7LJLJ||||||LJL-77\n\
                     F--JF--7||LJLJ7F7FJ-\n\
                     L---JF-JLJ.||-FJLJJ7\n\
                     |F|F-JF---7F7-L7L|7|\n\
                     |FFJF7L7F-JF7|JL---7\n\
                     7-L-JL7||F7|L7F-7F7|\n\
                     L.L7LFJ|||||FJL7||LJ\n\
                     L7JLJL-JLJLJL--JLJ.L";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 10);
        Ok(())
    }
}
