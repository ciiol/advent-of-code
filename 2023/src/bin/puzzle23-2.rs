// https://adventofcode.com/2023/day/23#part2
// There's a map of nearby hiking trails (your puzzle input) that indicates paths (.), forest (#),
// and steep slopes (^, >, v, and <).
//
// Treat all slopes as if they were normal paths (.). To make sure you have the most
// scenic hike possible, never step onto the same tile twice.
//
// Find the longest hike you can take through the hiking trails listed on your map. How many steps
// long is the longest hike?
extern crate advent_of_code_2023 as advent;

use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::io::Error;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::value;
use nom::{IResult, Parser};

use advent::matrix::{Coord, Direction, Matrix};
use advent::parser::matrix;
use advent::runner::run_puzzle;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
enum Tile {
    Path,
    Forest,
    Slope(Direction),
}

type Map = Matrix<Tile>;

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle23.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Map> {
    let tile = alt((
        value(Tile::Path, tag(".")),
        value(Tile::Forest, tag("#")),
        value(Tile::Slope(Direction::up()), tag("^")),
        value(Tile::Slope(Direction::right()), tag(">")),
        value(Tile::Slope(Direction::down()), tag("v")),
        value(Tile::Slope(Direction::left()), tag("<")),
    ));
    matrix(tile).parse(input)
}

fn puzzle(input: &Map) -> usize {
    let start = Coord::from((0, 1));
    let end = Coord::from((input.rows_size() - 1, input.cols_size() - 2));
    walk(input, start, end)
}

fn walk(map: &Map, start: Coord, end: Coord) -> usize {
    let shortcuts = shortcuts(map, &[start, end]);
    let mut visited: HashMap<Coord, usize> = HashMap::new();
    let mut stack = vec![(vec![start], 1)];
    while let Some((path, distance)) = stack.pop() {
        let &point = path.last().unwrap();
        let previous = visited.entry(point).or_insert(distance);
        if *previous < distance {
            *previous = distance;
        }
        let tile = map.get(point).unwrap();
        match tile {
            Tile::Path | Tile::Slope(_) => {
                let next = map.neighbours4(point).filter_map(|c| {
                    if map.get(c).unwrap() == &Tile::Forest {
                        return None;
                    }
                    let &(next, step_distance) = shortcuts.get(&c).unwrap_or(&(c, 1));
                    if path.contains(&next) {
                        return None;
                    }
                    Some((next, step_distance))
                });
                for (next, step_distance) in next {
                    let mut path = path.clone();
                    path.push(next);
                    stack.push((path, distance + step_distance));
                }
            }
            Tile::Forest => unreachable!(),
        };
    }
    visited.get(&end).unwrap() - 1
}

fn shortcuts(map: &Map, extra_points: &[Coord]) -> HashMap<Coord, (Coord, usize)> {
    let crossings: Vec<_> = map
        .iter()
        .filter(|(_, &tile)| tile == Tile::Path)
        .filter_map(|(coord, _)| {
            let is_crossing = map
                .neighbours4(coord)
                .filter(|&c| map.get(c).unwrap() != &Tile::Forest)
                .count()
                > 2;
            is_crossing.then_some(coord)
        })
        .collect();
    let points_of_interest: HashSet<&Coord> = extra_points.iter().chain(&crossings).collect();
    let mut shortcuts = HashMap::new();
    for &start in &points_of_interest {
        let mut stack: Vec<_> = map.neighbours4(*start).map(|p| vec![p]).collect();
        while let Some(path) = stack.pop() {
            let &last = path.last().unwrap();
            if points_of_interest.contains(&last) {
                let first = *path.first().unwrap();
                shortcuts.insert(first, (last, path.len()));
                continue;
            }
            let tile = map.get(last).unwrap();
            match tile {
                Tile::Forest => continue,
                Tile::Path | Tile::Slope(_) => {
                    let next = map
                        .neighbours4(last)
                        .filter(|&c| c != *start && map.get(c).unwrap() != &Tile::Forest)
                        .find(|&c| !path.contains(&c))
                        .unwrap();
                    let mut path = path;
                    path.push(next);
                    stack.push(path);
                }
            }
        }
    }
    assert!(!shortcuts.is_empty());
    shortcuts
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "#.#####################\n\
                     #.......#########...###\n\
                     #######.#########.#.###\n\
                     ###.....#.>.>.###.#.###\n\
                     ###v#####.#v#.###.#.###\n\
                     ###.>...#.#.#.....#...#\n\
                     ###v###.#.#.#########.#\n\
                     ###...#.#.#.......#...#\n\
                     #####.#.#.#######.#.###\n\
                     #.....#.#.#.......#...#\n\
                     #.#####.#.#.#########v#\n\
                     #.#...#...#...###...>.#\n\
                     #.#.#v#######v###.###v#\n\
                     #...#.>.#...>.>.#.###.#\n\
                     #####v#.#.###v#.#.###.#\n\
                     #.....#...#...#.#.#...#\n\
                     #.#########.###.#.#.###\n\
                     #...###...#...#...#.###\n\
                     ###.###.#.###v#####v###\n\
                     #...#...#.#.>.>.#.>.###\n\
                     #.###.###.#.###.#.#v###\n\
                     #.....###...###...#...#\n\
                     #####################.#";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 154);
        Ok(())
    }
}
