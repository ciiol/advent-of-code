// https://adventofcode.com/2023/day/5
// The almanac (your puzzle input) lists all of the seeds that need to be planted. It also
// lists what type of soil to use with each kind of seed, what type of fertilizer to use
// with each kind of soil, what type of water to use with each kind of fertilizer, and so on.
//
// The almanac starts by listing which seeds need to be planted. The rest of the almanac
// contains a list of maps which describe how to convert numbers from a source category
// into numbers in a destination category. Any source numbers that aren't mapped
// correspond to the same destination number.
//
// What is the lowest location number that corresponds to any of the initial seed numbers?
extern crate advent_of_code_2023 as advent;

use std::io::Error;
use std::ops::Range;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{newline, space0, space1, u64 as u64_parser};
use nom::combinator::{map, value};
use nom::multi::separated_list1;
use nom::sequence::{preceded, separated_pair, terminated, tuple};
use nom::IResult;

use advent::runner::run_puzzle;

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
enum Category {
    Seed,
    Soil,
    Fertilizer,
    Water,
    Light,
    Temperature,
    Humidity,
    Location,
}

type Item = (Category, u64);

#[derive(Debug)]
struct Mapping {
    pub category: Category,
    pub range: Range<u64>,
    pub next_category: Category,
    pub diff: i64,
}

impl Mapping {
    fn try_next(&self, (category, number): Item) -> Option<Item> {
        if category == self.category && self.range.contains(&number) {
            let number: i128 = number.into();
            let new_number: i128 = number + i128::from(self.diff);
            Some((self.next_category, new_number.try_into().unwrap()))
        } else {
            None
        }
    }
}

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle05.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, (Vec<Item>, Vec<Mapping>)> {
    let seed_numbers = preceded(
        terminated(tag("seeds:"), space1),
        separated_list1(space1, u64_parser),
    );
    let seeds = map(terminated(seed_numbers, tag("\n\n")), |numbers| {
        make_items(Category::Seed, numbers).collect::<Vec<_>>()
    });
    let mappings = separated_list1(tag("\n\n"), categories_mapping);
    map(tuple((seeds, mappings)), |(seeds, mappings)| {
        let mapping = mappings.into_iter().flatten().collect();
        (seeds, mapping)
    })(input)
}

fn make_items(category: Category, numbers: Vec<u64>) -> impl Iterator<Item = Item> {
    numbers.into_iter().map(move |n| (category, n))
}

fn categories_mapping(input: &str) -> IResult<&str, Vec<Mapping>> {
    let category_pair = terminated(
        separated_pair(category_name, tag("-to-"), category_name),
        tag(" map:\n"),
    );
    let mapping = tuple((
        terminated(u64_parser, space1),
        terminated(u64_parser, space1),
        terminated(u64_parser, space0),
    ));
    let mappings = separated_list1(newline, mapping);
    map(tuple((category_pair, mappings)), |((c1, c2), mappings)| {
        mappings
            .into_iter()
            .map(|(s2, s1, n)| Mapping {
                category: c1,
                range: s1..(s1 + n),
                next_category: c2,
                diff: i128::from(s2)
                    .checked_sub(i128::from(s1))
                    .unwrap()
                    .try_into()
                    .unwrap(),
            })
            .collect()
    })(input)
}

fn category_name(input: &str) -> IResult<&str, Category> {
    alt((
        value(Category::Seed, tag("seed")),
        value(Category::Soil, tag("soil")),
        value(Category::Fertilizer, tag("fertilizer")),
        value(Category::Water, tag("water")),
        value(Category::Light, tag("light")),
        value(Category::Temperature, tag("temperature")),
        value(Category::Humidity, tag("humidity")),
        value(Category::Location, tag("location")),
    ))(input)
}

fn next_item(item: Item) -> Option<Item> {
    let (category, number) = item;
    match category {
        Category::Seed => Some((Category::Soil, number)),
        Category::Soil => Some((Category::Fertilizer, number)),
        Category::Fertilizer => Some((Category::Water, number)),
        Category::Water => Some((Category::Light, number)),
        Category::Light => Some((Category::Temperature, number)),
        Category::Temperature => Some((Category::Humidity, number)),
        Category::Humidity => Some((Category::Location, number)),
        Category::Location => None,
    }
}

fn find_last_item(item: Item, mappings: &[Mapping]) -> Item {
    let mut item = item;
    loop {
        if let Some(next) = mappings.iter().find_map(|mapping| mapping.try_next(item)) {
            item = next;
        } else if let Some(next) = next_item(item) {
            item = next;
        } else {
            break;
        }
    }
    item
}

fn puzzle((start, mappings): &(Vec<Item>, Vec<Mapping>)) -> u64 {
    start
        .iter()
        .map(|item| find_last_item(*item, mappings))
        .map(|(_, number)| number)
        .min()
        .unwrap()
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "seeds: 79 14 55 13\n\
                     \n\
                     seed-to-soil map:\n\
                     50 98 2\n\
                     52 50 48\n\
                     \n\
                     soil-to-fertilizer map:\n\
                     0 15 37\n\
                     37 52 2\n\
                     39 0 15\n\
                     \n\
                     fertilizer-to-water map:\n\
                     49 53 8\n\
                     0 11 42\n\
                     42 0 7\n\
                     57 7 4\n\
                     \n\
                     water-to-light map:\n\
                     88 18 7\n\
                     18 25 70\n\
                     \n\
                     light-to-temperature map:\n\
                     45 77 23\n\
                     81 45 19\n\
                     68 64 13\n\
                     \n\
                     temperature-to-humidity map:\n\
                     0 69 1\n\
                     1 0 69\n\
                     \n\
                     humidity-to-location map:\n\
                     60 56 37\n\
                     56 93 4";
        let (start, mappings) = parse(input, puzzle_input)?;
        assert_eq!(
            find_last_item((Category::Seed, 79), &mappings),
            (Category::Location, 82)
        );
        assert_eq!(puzzle(&(start, mappings)), 35);
        Ok(())
    }
}
