// https://adventofcode.com/2023/day/5#part2
// The almanac (your puzzle input) lists all of the seeds that need to be planted. It also
// lists what type of soil to use with each kind of seed, what type of fertilizer to use
// with each kind of soil, what type of water to use with each kind of fertilizer, and so on.
//
// The almanac starts by listing which seeds need to be planted. The rest of the almanac
// contains a list of maps which describe how to convert numbers from a source category
// into numbers in a destination category. Any source numbers that aren't mapped
// correspond to the same destination number.
//
// Everyone will starve if you only plant such a small number of seeds.
// Re-reading the almanac, it looks like the seeds: line actually describes ranges of seed numbers.
//
// The values on the initial seeds: line come in pairs. Within each pair, the first
// value is the start of the range and the second value is the length of the range.

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

type Item = (Category, Range<u64>);

#[derive(Debug)]
struct Mapping {
    pub category: Category,
    pub range: Range<u64>,
    pub next_category: Category,
    pub diff: i64,
}

impl Mapping {
    fn try_next(&self, (category, range): &Item) -> Option<(Range<u64>, Item, Range<u64>)> {
        if *category == self.category {
            if let Some((left, range, right)) = intersect_range(range, &self.range) {
                let start: i128 = range.start.into();
                let end: i128 = range.end.into();
                let next_start: i128 = start + i128::from(self.diff);
                let next_end: i128 = end + i128::from(self.diff);
                let next_range = next_start.try_into().unwrap()..next_end.try_into().unwrap();
                Some((left, (self.next_category, next_range), right))
            } else {
                None
            }
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

fn intersect_range(
    range: &Range<u64>,
    with_range: &Range<u64>,
) -> Option<(Range<u64>, Range<u64>, Range<u64>)> {
    if with_range.contains(&range.start) && with_range.contains(&(range.end - 1)) {
        Some((
            range.start..range.start,
            range.to_owned(),
            range.end..range.end,
        ))
    } else if with_range.contains(&range.start) {
        Some((
            range.start..range.start,
            range.start..with_range.end,
            with_range.end..range.end,
        ))
    } else if with_range.contains(&(range.end - 1)) {
        Some((
            range.start..with_range.start,
            with_range.start..range.end,
            range.end..range.end,
        ))
    } else if range.contains(&with_range.start) && range.contains(&(with_range.end - 1)) {
        Some((
            range.start..with_range.start,
            with_range.to_owned(),
            with_range.end..range.end,
        ))
    } else {
        None
    }
}

fn make_items(category: Category, numbers: Vec<u64>) -> impl Iterator<Item = Item> {
    let mut numbers = numbers.into_iter();
    std::iter::from_fn(move || {
        if let Some(start) = numbers.next() {
            let end = start + numbers.next().unwrap();
            Some((category, start..end))
        } else {
            None
        }
    })
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

fn next_item(item: &Item) -> Option<Item> {
    let (category, range) = item;
    match category {
        Category::Seed => Some((Category::Soil, range.clone())),
        Category::Soil => Some((Category::Fertilizer, range.clone())),
        Category::Fertilizer => Some((Category::Water, range.clone())),
        Category::Water => Some((Category::Light, range.clone())),
        Category::Light => Some((Category::Temperature, range.clone())),
        Category::Temperature => Some((Category::Humidity, range.clone())),
        Category::Humidity => Some((Category::Location, range.clone())),
        Category::Location => None,
    }
}

fn find_last_items(item: &Item, mappings: &[Mapping]) -> Vec<Item> {
    let mut items = vec![item.clone()];
    let mut result = vec![];
    while let Some(item) = items.pop() {
        if let Some((left, next, right)) =
            mappings.iter().find_map(|mapping| mapping.try_next(&item))
        {
            items.push(next);
            if !left.is_empty() {
                items.push((item.0, left));
            }
            if !right.is_empty() {
                items.push((item.0, right));
            }
        } else if let Some(next) = next_item(&item) {
            items.push(next);
        } else {
            result.push(item);
        }
    }
    result
}

fn puzzle((start, mappings): &(Vec<Item>, Vec<Mapping>)) -> u64 {
    start
        .iter()
        .flat_map(|item| find_last_items(item, mappings))
        .map(|(_, range)| range.start)
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
        assert_eq!(puzzle(&(start, mappings)), 46);
        Ok(())
    }
}
