// https://adventofcode.com/2023/day/21#part2
// He gives you an up-to-date map (your puzzle input) of his starting position (S), garden plots (.), and rocks (#).
//
// He also points out that the garden plots and rocks are set up so that the map repeats infinitely in every direction.
//
// Starting from the garden plot marked S on your infinite map, how many garden plots could the
// Elf reach in exactly 26501365 steps?
extern crate advent_of_code_2023 as advent;

use nalgebra::Vector2;
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
type Point = Vector2<i64>;

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
    // The input has a very specific pattern, so we can calculate the answer as
    // a member of a second-order arithmetic progression
    let reachable_65 = reachable(input, 65);
    let reachable_131 = reachable(input, 65 + 131);
    let d1 = reachable_131 - reachable_65;
    let reachable_131_2 = reachable(input, 65 + 131 * 2);
    let d2 = reachable_131_2 - reachable_131 - d1;
    let reachable_131_3 = reachable(input, 65 + 131 * 3);
    assert_eq!(d1 + d2 * 2, reachable_131_3 - reachable_131_2);
    // 26_501_365 == 65 + 131 * 202_300
    let n = 202_300;
    reachable_65 + d1 * n + d2 * n * (n - 1) / 2
}

fn reachable(input: &Map, steps: usize) -> usize {
    let mut points: HashSet<Point> = HashSet::new();
    points.extend(
        input
            .search(|&t| t == Tile::Start)
            .map(|c| Point::new(c.row.try_into().unwrap(), c.col.try_into().unwrap())),
    );
    for _ in 0..steps {
        points = points
            .iter()
            .flat_map(|&p| {
                let neighbours = neighbours(p);
                neighbours.filter(|&n| get_tile(input, n) != &Tile::Rock)
            })
            .collect();
    }
    points.len()
}

fn neighbours(p: Point) -> impl Iterator<Item = Point> {
    let steps = vec![
        Point::new(0, 1),
        Point::new(0, -1),
        Point::new(1, 0),
        Point::new(-1, 0),
    ];
    steps.into_iter().map(move |s| p + s)
}

fn get_tile(map: &Map, p: Point) -> &Tile {
    let rows: i64 = map.rows_size().try_into().unwrap();
    let cols: i64 = map.cols_size().try_into().unwrap();
    let x: usize = p.x.rem_euclid(rows).try_into().unwrap();
    let y: usize = p.y.rem_euclid(cols).try_into().unwrap();
    map.get(Coord::from((x, y))).unwrap()
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
        let input = parse(input, puzzle_input)?;
        assert_eq!(reachable(&input, 6), 16);
        assert_eq!(reachable(&input, 10), 50);
        assert_eq!(reachable(&input, 50), 1594);
        Ok(())
    }
}
