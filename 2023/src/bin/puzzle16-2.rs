// https://adventofcode.com/2023/day/16#part2
// You note the layout of the contraption (your puzzle input). It appears to be a flat, two-dimensional square
// grid containing empty space (.), mirrors (/ and \), and splitters (| and -).
//
// The beam enters in the top-left corner from the left and heading to the right.
// Then, its behavior depends on what it encounters as it moves:
// - If the beam encounters empty space (.), it continues in the same direction.
// - If the beam encounters a mirror (/ or \), the beam is reflected 90 degrees depending on the angle of the mirror.
// - If the beam encounters the pointy end of a splitter (| or -), the beam passes through the splitter as if
//   the splitter were empty space. For instance, a rightward-moving beam that encounters a - splitter woul
//   continue in the same direction.
// - If the beam encounters the flat side of a splitter (| or -), the beam is split into two beams going
//   in each of the two directions the splitter's pointy ends are pointing. For instance, a rightward-moving
//   beam that encounters a | splitter would split into two beams: one that continues upward from the splitter's
//   column and one that continues downward from the splitter's column.
//
// A tile is energized if that tile has at least one beam pass through it, reflect in it, or split in it.
//
// As you try to work out what might be wrong, the reindeer tugs on your shirt and leads you to a nearby control panel.
// There, a collection of buttons lets you align the contraption so that the beam enters from any edge tile and
// heading away from that edge.
//
// Find the initial beam configuration that energizes the largest number of tiles; how many tiles are energized
// in that configuration?
extern crate advent_of_code_2023 as advent;

use std::collections::HashSet;
use std::io::Error;

use nom::branch::alt;
use nom::character::complete::one_of;
use nom::combinator::value;
use nom::{IResult, Parser};

use advent::matrix::{Coord, Direction, Matrix};
use advent::parser::matrix;
use advent::runner::run_puzzle;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Tile {
    Empty,
    Mirror,
    MirrorReversed,
    SplitHorizontally,
    SplitVertically,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Ray {
    pub direction: Direction,
    pub position: Coord,
}

impl Ray {
    fn move_forward(&self) -> Option<Self> {
        let new_pos = self.position + self.direction;
        new_pos.map(|position| Self { position, ..*self })
    }

    fn rotate_left(&self) -> Self {
        let direction = self.direction.rotate_left();
        Self { direction, ..*self }
    }

    fn rotate_right(&self) -> Self {
        let direction = self.direction.rotate_right();
        Self { direction, ..*self }
    }
}

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle16.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Matrix<Tile>> {
    let tile = alt((
        value(Tile::Empty, one_of(".")),
        value(Tile::Mirror, one_of("/")),
        value(Tile::MirrorReversed, one_of("\\")),
        value(Tile::SplitHorizontally, one_of("-")),
        value(Tile::SplitVertically, one_of("|")),
    ));
    matrix(tile).parse(input)
}

fn puzzle(input: &Matrix<Tile>) -> usize {
    let rows = 0..input.rows_size();
    let cols = 0..input.cols_size();
    let border_rays = rows
        .clone()
        .flat_map(|row| {
            vec![Ray {
                direction: Direction::right(),
                position: (row, 0).into(),
            }]
        })
        .chain(rows.clone().flat_map(|row| {
            vec![Ray {
                direction: Direction::left(),
                position: (row, input.cols_size() - 1).into(),
            }]
        }))
        .chain(cols.clone().flat_map(|col| {
            vec![Ray {
                direction: Direction::down(),
                position: (0, col).into(),
            }]
        }))
        .chain(cols.clone().flat_map(|col| {
            vec![Ray {
                direction: Direction::up(),
                position: (input.rows_size() - 1, col).into(),
            }]
        }));
    border_rays.map(|ray| trace_ray(input, ray)).max().unwrap()
}

fn trace_ray(contraption: &Matrix<Tile>, start: Ray) -> usize {
    let mut visited: HashSet<Ray> = HashSet::new();
    let mut rays = vec![start];
    while let Some(ray) = rays.pop() {
        if visited.contains(&ray) {
            continue;
        }
        visited.insert(ray);
        let tile = contraption.get(ray.position).unwrap();
        rays.extend(
            trace_ray_step(ray, *tile)
                .iter()
                .filter(|ray| contraption.is_inside(ray.position)),
        );
    }
    let energized: HashSet<Coord> = visited.iter().map(|ray| ray.position).collect();
    energized.len()
}

fn trace_ray_step(ray: Ray, tile: Tile) -> Vec<Ray> {
    match tile {
        Tile::Empty => ray.move_forward().into_iter().collect(),
        Tile::Mirror if ray.direction.is_horizontal() => {
            ray.rotate_left().move_forward().into_iter().collect()
        }
        Tile::Mirror if ray.direction.is_vertical() => {
            ray.rotate_right().move_forward().into_iter().collect()
        }
        Tile::MirrorReversed if ray.direction.is_horizontal() => {
            ray.rotate_right().move_forward().into_iter().collect()
        }
        Tile::MirrorReversed if ray.direction.is_vertical() => {
            ray.rotate_left().move_forward().into_iter().collect()
        }
        Tile::SplitHorizontally if ray.direction.is_horizontal() => {
            ray.move_forward().into_iter().collect()
        }
        Tile::SplitHorizontally if ray.direction.is_vertical() => {
            let ray1 = ray.rotate_left().move_forward();
            let ray2 = ray.rotate_right().move_forward();
            ray1.into_iter().chain(ray2).collect()
        }
        Tile::SplitVertically if ray.direction.is_horizontal() => {
            let ray1 = ray.rotate_left().move_forward();
            let ray2 = ray.rotate_right().move_forward();
            ray1.into_iter().chain(ray2).collect()
        }
        Tile::SplitVertically if ray.direction.is_vertical() => {
            ray.move_forward().into_iter().collect()
        }
        _ => unreachable!(),
    }
}
