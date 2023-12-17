// https://adventofcode.com/2023/day/17#part2
// The Elves here have a map (your puzzle input). Each city block is marked by a single digit that represents
// the amount of heat loss if the crucible enters that block.
//
// The starting point, the lava pool, is the top-left city block; the destination, the machine parts factory,
// is the bottom-right city block.
//
// Because it is difficult to keep the top-heavy crucible going in a straight line for very long, it can move
// at most three blocks in a single direction before it must turn 90 degrees left or right.
// The crucible also can't reverse direction; after entering each city block, it may only turn left, continue
// straight, or turn right.
//
// The crucibles of lava simply aren't large enough to provide an adequate supply of lava to the machine parts factory.
// Instead, the Elves are going to upgrade to ultra crucibles.
//
// Ultra crucibles are even more difficult to steer than normal crucibles. Not only do they have trouble going
// in a straight line, but they also have trouble turning!
//
// Once an ultra crucible starts moving in a direction, it needs to move a minimum of four blocks in that
// direction before it can turn (or even before it can stop at the end). However, it will eventually start
// to get wobbly: an ultra crucible can move a maximum of ten consecutive blocks without turning.
//
// Directing the ultra crucible from the lava pool to the machine parts factory,
// what is the least heat loss it can incur?
extern crate advent_of_code_2023 as advent;

use std::io::Error;

use nom::character::complete::satisfy;
use nom::{IResult, Parser};
use pathfinding::directed::dijkstra;

use advent::matrix::{Coord, Direction, Matrix};
use advent::parser::matrix;
use advent::runner::run_puzzle;

type Block = u8;
type Map = Matrix<Block>;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Crucible {
    pub position: Coord,
    pub direction: Direction,
    pub straight_path: u8,
}

impl Crucible {
    fn successors(&self, map: &Map) -> Vec<(Self, usize)> {
        let forward = if self.straight_path == 0 {
            self.move_forward(map, 4)
        } else {
            self.move_forward(map, 1)
        };
        let left = self.turn(self.direction.rotate_left()).move_forward(map, 4);
        let right = self
            .turn(self.direction.rotate_right())
            .move_forward(map, 4);
        forward.into_iter().chain(left).chain(right).collect()
    }

    fn move_forward(&self, map: &Map, distance: u8) -> Option<(Self, usize)> {
        if self.straight_path + distance > 10 {
            return None;
        }
        let new_pos = self.position + self.direction * isize::from(distance);
        new_pos.and_then(|position| {
            map.is_inside(position).then(|| {
                let weight = (1..=isize::from(distance))
                    .map(|d| {
                        let pos = (self.position + self.direction * d).unwrap();
                        usize::from(*map.get(pos).unwrap())
                    })
                    .sum();
                let new_crucible = Self {
                    position,
                    straight_path: self.straight_path + distance,
                    ..*self
                };
                (new_crucible, weight)
            })
        })
    }

    fn turn(&self, direction: Direction) -> Self {
        Self {
            straight_path: 0,
            direction,
            ..*self
        }
    }
}

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle17.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Map> {
    let block =
        satisfy(|c| c.is_ascii_digit()).map(|c| c.to_digit(10).unwrap().try_into().unwrap());
    matrix(block).parse(input)
}

fn puzzle(input: &Map) -> usize {
    let start = Crucible {
        position: Coord { row: 0, col: 0 },
        direction: Direction::right(),
        straight_path: 0,
    };
    let goal = Coord {
        row: input.rows_size() - 1,
        col: input.cols_size() - 1,
    };
    let (_path, len) = dijkstra::dijkstra(
        &start,
        |p| p.successors(input),
        |crucible| crucible.position == goal,
    )
    .unwrap();
    len
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle1() -> Result<(), Error> {
        let input = "2413432311323\n\
                     3215453535623\n\
                     3255245654254\n\
                     3446585845452\n\
                     4546657867536\n\
                     1438598798454\n\
                     4457876987766\n\
                     3637877979653\n\
                     4654967986887\n\
                     4564679986453\n\
                     1224686865563\n\
                     2546548887735\n\
                     4322674655533";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 94);
        Ok(())
    }

    #[test]
    fn test_puzzle2() -> Result<(), Error> {
        let input = "111111111111\n\
                     999999999991\n\
                     999999999991\n\
                     999999999991\n\
                     999999999991";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 71);
        Ok(())
    }
}
