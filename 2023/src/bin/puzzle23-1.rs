// https://adventofcode.com/2023/day/23
// There's a map of nearby hiking trails (your puzzle input) that indicates paths (.), forest (#),
// and steep slopes (^, >, v, and <).
//
// Because of all the mist from the waterfall, the slopes are probably quite icy; if you step onto a slope tile,
// your next step must be downhill (in the direction the arrow is pointing). To make sure you have the most
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
    let mut visited: HashMap<Coord, HashSet<Coord>> = HashMap::new();
    let mut stack = vec![(start, HashSet::from([start]))];
    while let Some((point, path)) = stack.pop() {
        if let Some(other_path) = visited.get(&point) {
            if other_path.len() > path.len() {
                continue;
            }
        }
        let tile = map.get(point).unwrap();
        match tile {
            Tile::Path => {
                let next = map
                    .neighbours4(point)
                    .filter(|&c| {
                        let other = map.get(c).unwrap();
                        match other {
                            Tile::Slope(direction) => (c + *direction).unwrap() != point,
                            Tile::Forest => false,
                            Tile::Path => true,
                        }
                    })
                    .filter(|&c| !path.contains(&c));
                for next in next {
                    let mut path = path.clone();
                    path.insert(next);
                    stack.push((next, path));
                }
            }
            Tile::Slope(direction) => {
                let next = (point + *direction).unwrap();
                let mut path = path.clone();
                path.insert(next);
                stack.push((next, path));
            }
            Tile::Forest => unreachable!(),
        };
        visited.insert(point, path);
    }
    visited.get(&end).unwrap().len() - 1
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
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 94);
        Ok(())
    }
}
