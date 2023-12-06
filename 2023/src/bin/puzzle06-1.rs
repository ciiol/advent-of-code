// https://adventofcode.com/2023/day/6
// you get a sheet of paper (your puzzle input) that lists the time allowed for each race and
// also the best distance ever recorded in that race. To guarantee you win the grand prize,
// you need to make sure you go farther in each race than the current record holder.
//
// The organizer brings you over to the area where the boat races are held. The boats are
// much smaller than you expected - they're actually toy boats, each with a big button on top.
// Holding down the button charges the boat, and releasing the button allows the boat to move.
// Boats move faster if their button was held longer, but time spent holding the button counts
// against the total race time. You can only hold the button at the start of the race, and
// boats don't move until the button is released.
//
// Determine the number of ways you could beat the record in each race.
// What do you get if you multiply these numbers together?
extern crate advent_of_code_2023 as advent;

use std::io::Error;

use nom::bytes::complete::tag;
use nom::character::complete::{newline, space1, u32 as u32_parser};
use nom::combinator::map;
use nom::multi::separated_list1;
use nom::sequence::{preceded, terminated, tuple};
use nom::IResult;

use advent::runner::run_puzzle;

type Time = u32;
type Distance = u32;

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle06.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Vec<(Time, Distance)>> {
    let time = terminated(tag("Time:"), space1);
    let distance = terminated(tag("Distance:"), space1);
    let times = preceded(time, separated_list1(space1, u32_parser));
    let distances = preceded(distance, separated_list1(space1, u32_parser));
    map(
        tuple((terminated(times, newline), distances)),
        |(times, distances)| times.into_iter().zip(distances).collect(),
    )(input)
}

fn puzzle(races: &[(Time, Distance)]) -> usize {
    races
        .iter()
        .map(|(time, distance)| simulate_race(*time, *distance))
        .product()
}

fn simulate_race(time: Time, distance: Distance) -> usize {
    (0..=time)
        .map(|t| t * (time - t))
        .filter(|&d| d > distance)
        .count()
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "Time:      7  15   30\n\
                     Distance:  9  40  200";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 288);
        Ok(())
    }
}
