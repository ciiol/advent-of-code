// https://adventofcode.com/2023/day/2#part2
// You play several games and record the information from each game (your puzzle input).
// Each game is listed with its ID number (like the 11 in Game 11: ...) followed by a
// semicolon-separated list of subsets of cubes that were revealed from the bag
// (like 3 red, 5 green, 4 blue).
//
// the Elf poses a second question: in each game you played, what is the fewest number
// of cubes of each color that could have been in the bag to make the game possible?
//
// The power of a set of cubes is equal to the numbers of red, green,
// and blue cubes multiplied together. For each game, find the minimum set of cubes
// that must have been present. What is the sum of the power of these sets?

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
    #[allow(dead_code)]
    pub id: u32,
    pub rounds: Vec<Round>,
}

impl Game {
    fn find_limit(&self) -> Limit {
        let mut limit = HashMap::new();
        for round in &self.rounds {
            for (colour, &count) in &round.cubes {
                limit
                    .entry(*colour)
                    .and_modify(|e: &mut u32| *e = count.max(*e))
                    .or_insert(count);
            }
        }
        limit
    }

    fn power(&self) -> u32 {
        let mut power = 1;
        let colours = [Colour::Red, Colour::Green, Colour::Blue];
        let limit = self.find_limit();
        for colour in &colours {
            power *= limit.get(colour).unwrap_or(&0);
        }
        power
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
    Ok(game.power())
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
        assert_eq!(puzzle(input)?, 2286);
        Ok(())
    }

    #[test]
    fn test_game_score() -> Result<(), Error> {
        assert_eq!(
            score_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")?,
            48
        );
        Ok(())
    }

    #[test]
    fn test_limit() -> Result<(), Error> {
        let game = parse(
            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
            game,
        )?;
        assert_eq!(
            game.find_limit(),
            [(Colour::Red, 4), (Colour::Green, 2), (Colour::Blue, 6)]
                .into_iter()
                .collect::<Limit>()
        );
        Ok(())
    }
}
