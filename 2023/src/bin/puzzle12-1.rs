// https://adventofcode.com/2023/day/12
// The condition records of which springs are damaged (your puzzle input) are damaged.
//
// In the giant field just outside, the springs are arranged into rows. For each row,
// the condition records show every spring and whether it is operational (.) or damaged (#).
// This is the part of the condition records that is itself damaged; for some springs,
// it is simply unknown (?) whether the spring is operational or damaged.
//
// However, the engineer that produced the condition records also duplicated some of this
// information in a different format! After the list of springs for a given row, the size
// of each contiguous group of damaged springs is listed in the order those groups appear
// in the row. This list always accounts for every damaged spring, and each number is the
// entire size of its contiguous group (that is, groups are always separated by at least
// one operational spring
//
// For each row, count all of the different arrangements of operational and broken springs
// that meet the given criteria. What is the sum of those counts?
extern crate advent_of_code_2023 as advent;

use std::io::Error;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{newline, space1, u32 as number};
use nom::combinator::{map, value};
use nom::multi::{many1, separated_list1};
use nom::sequence::separated_pair;
use nom::IResult;

use advent::runner::run_puzzle;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Spring {
    Broken,
    Operational,
    Unknown,
}

struct Record {
    springs: Vec<Spring>,
    groups: Vec<usize>,
}

#[derive(Debug, Clone)]
struct UnfinishedRecord {
    springs: Vec<Spring>,
    groups: Vec<usize>,
    last: Spring,
}

impl UnfinishedRecord {
    pub fn new(groups: &[usize]) -> Self {
        let mut groups = groups.to_vec();
        groups.reverse();
        Self {
            springs: vec![],
            last: Spring::Operational,
            groups,
        }
    }

    pub fn matches(&self, next: Spring) -> bool {
        let group = self.groups.last();
        match next {
            Spring::Broken => matches!(group, Some(&size) if size > 0),
            Spring::Operational => *group.unwrap_or(&0) == 0 || self.last == Spring::Operational,
            Spring::Unknown => panic!("Unknown spring"),
        }
    }

    pub fn push(&mut self, next: Spring) {
        match next {
            Spring::Broken => {
                let group = self.groups.last_mut().unwrap();
                *group -= 1;
            }
            Spring::Operational => {
                if self.last == Spring::Broken {
                    self.groups.pop().unwrap();
                }
            }
            Spring::Unknown => panic!("Unknown spring"),
        }
        self.springs.push(next);
        self.last = next;
    }

    pub fn is_finished(&self) -> bool {
        let groups = self.groups.len();
        groups == 0 || (groups == 1 && self.groups[0] == 0)
    }

    pub fn into_springs(self) -> Vec<Spring> {
        self.springs
    }
}

impl Record {
    pub fn new(springs: Vec<Spring>, groups: Vec<usize>) -> Self {
        Self { springs, groups }
    }

    pub fn resolve_unknowns(&self) -> Vec<Vec<Spring>> {
        let mut records = vec![UnfinishedRecord::new(&self.groups)];
        for &state in &self.springs {
            if state == Spring::Unknown {
                records = records
                    .into_iter()
                    .flat_map(|record| {
                        let options = [Spring::Broken, Spring::Operational];
                        options.into_iter().filter_map(move |x| {
                            if record.matches(x) {
                                let mut record = record.clone();
                                record.push(x);
                                Some(record)
                            } else {
                                None
                            }
                        })
                    })
                    .collect();
            } else {
                records = records
                    .into_iter()
                    .filter_map(|mut record| {
                        if record.matches(state) {
                            record.push(state);
                            Some(record)
                        } else {
                            None
                        }
                    })
                    .collect();
            }
        }
        records
            .into_iter()
            .filter_map(|record| {
                if record.is_finished() {
                    Some(record.into_springs())
                } else {
                    None
                }
            })
            .collect()
    }
}

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle12.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Vec<Record>> {
    separated_list1(newline, record)(input)
}

fn record(input: &str) -> IResult<&str, Record> {
    let spring = alt((
        value(Spring::Broken, tag("#")),
        value(Spring::Operational, tag(".")),
        value(Spring::Unknown, tag("?")),
    ));
    let springs = many1(spring);
    let groups = map(separated_list1(tag(","), number), |groups| {
        groups.into_iter().map(|g| g.try_into().unwrap()).collect()
    });
    map(
        separated_pair(springs, space1, groups),
        |(springs, groups)| Record::new(springs, groups),
    )(input)
}

fn puzzle(input: &[Record]) -> usize {
    input
        .iter()
        .map(|record| record.resolve_unknowns().len())
        .sum()
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "???.### 1,1,3\n\
                     .??..??...?##. 1,1,3\n\
                     ?#?#?#?#?#?#?#? 1,3,1,6\n\
                     ????.#...#... 4,1,1\n\
                     ????.######..#####. 1,6,5\n\
                     ?###???????? 3,2,1";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 21);
        Ok(())
    }

    #[test]
    fn test_resolve_unknowns() {
        let input = Record::new(vec![Spring::Unknown; 3], vec![3]);
        assert_eq!(input.resolve_unknowns().len(), 1);
        let input = parse("???.### 1,1,3", record).unwrap();
        assert_eq!(input.resolve_unknowns().len(), 1);
        let input = parse(".??..??...?##. 1,1,3", record).unwrap();
        assert_eq!(input.resolve_unknowns().len(), 4);
        let input = parse("?#?#?#?#?#?#?#? 1,3,1,6", record).unwrap();
        assert_eq!(input.resolve_unknowns().len(), 1);
        let input = parse("????.#...#... 4,1,1", record).unwrap();
        assert_eq!(input.resolve_unknowns().len(), 1);
        let input = parse("????.######..#####. 1,6,5", record).unwrap();
        assert_eq!(input.resolve_unknowns().len(), 4);
        let input = parse("?###???????? 3,2,1", record).unwrap();
        assert_eq!(input.resolve_unknowns().len(), 10);
    }
}
