// https://adventofcode.com/2023/day/6#part2
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
// As the race is about to start, you realize the piece of paper with race times and record
// distances you got earlier actually just has very bad kerning. There's really only
// one race - ignore the spaces between the numbers on each line.
//
// How many ways can you beat the record in this one much longer race?
extern crate advent_of_code_2023 as advent;

use std::io::Error;

use nom::bytes::complete::tag;
use nom::character::complete::{newline, satisfy, space1};
use nom::combinator::map;
use nom::multi::{many1, separated_list1};
use nom::sequence::{preceded, terminated, tuple};
use nom::IResult;

use advent::runner::run_puzzle;

type Time = u64;
type Distance = u64;

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle06.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Vec<(Time, Distance)>> {
    let time = terminated(tag("Time:"), space1);
    let distance = terminated(tag("Distance:"), space1);
    let times = preceded(time, separated_list1(space1, spaced_number));
    let distances = preceded(distance, separated_list1(space1, spaced_number));
    map(
        tuple((terminated(times, newline), distances)),
        |(times, distances)| times.into_iter().zip(distances).collect(),
    )(input)
}

fn spaced_number(input: &str) -> IResult<&str, u64> {
    let digits = separated_list1(space1, many1(satisfy(|c| c.is_ascii_digit())));
    map(digits, |digits| {
        digits
            .into_iter()
            .flatten()
            .collect::<String>()
            .parse::<u64>()
            .unwrap()
    })(input)
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
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 71503);
        Ok(())
    }
}
