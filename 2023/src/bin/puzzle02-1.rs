// https://adventofcode.com/2023/day/2
// You play several games and record the information from each game (your puzzle input).
// Each game is listed with its ID number (like the 11 in Game 11: ...) followed by a
// semicolon-separated list of subsets of cubes that were revealed from the bag
// (like 3 red, 5 green, 4 blue).
//
// The Elf would first like to know which games would have been possible if
// the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?
//
// What is the sum of the IDs of those games?

extern crate advent_of_code_2023 as advent;

use std::collections::HashMap;
use std::fs::File;
use std::io::{Error, Read};

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{space1, u32 as u32_parser};
use nom::combinator::{map, value};
use nom::multi::separated_list1;
use nom::sequence::{preceded, separated_pair};
use nom::IResult;

use advent::parser::parse;

type Limit = HashMap<Colour, u32>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Colour {
    Red,
    Green,
    Blue,
}

#[derive(Debug, Clone)]
struct Round {
    pub cubes: HashMap<Colour, u32>,
}

#[derive(Debug, Clone)]
struct Game {
    pub id: u32,
    pub rounds: Vec<Round>,
}

impl Round {
    fn is_possible(&self, limit: &Limit) -> bool {
        self.cubes
            .iter()
            .all(|(colour, count)| limit.get(colour).unwrap_or(&0u32) >= count)
    }
}

impl Game {
    fn is_possible(&self, limit: &Limit) -> bool {
        self.rounds.iter().all(|round| round.is_possible(limit))
    }
}

fn main() -> Result<(), Error> {
    let mut input_file = File::open("inputs/puzzle02.txt").unwrap();
    let mut input = String::new();
    input_file.read_to_string(&mut input).unwrap();
    println!("{}", puzzle(&input)?);
    Ok(())
}

fn puzzle(input: &str) -> Result<u32, Error> {
    input.lines().map(score_game).sum()
}

fn score_game(game_line: &str) -> Result<u32, Error> {
    let game = parse(game_line, game)?;
    let limit = HashMap::from([(Colour::Red, 12), (Colour::Green, 13), (Colour::Blue, 14)]);
    if game.is_possible(&limit) {
        Ok(game.id)
    } else {
        Ok(0)
    }
}

fn game(input: &str) -> IResult<&str, Game> {
    let comma_separator = tag(", ");
    let semicolon_separator = tag("; ");
    let colour = alt((
        value(Colour::Red, tag("red")),
        value(Colour::Green, tag("green")),
        value(Colour::Blue, tag("blue")),
    ));
    let cubes = map(
        separated_pair(u32_parser, space1, colour),
        |(count, colour)| (colour, count),
    );
    let round = map(separated_list1(comma_separator, cubes), |cubes| Round {
        cubes: cubes.into_iter().collect(),
    });
    let rounds = separated_list1(semicolon_separator, round);
    let game_header = preceded(tag("Game "), u32_parser);
    map(
        separated_pair(game_header, tag(": "), rounds),
        |(id, rounds)| Game { id, rounds },
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
                     Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
                     Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
                     Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
                     Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";
        assert_eq!(puzzle(input)?, 8);
        Ok(())
    }
}
