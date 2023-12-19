// https://adventofcode.com/2023/day/19#part2
// A group of Elves is already here organizing the parts, and they have a system.
//
// To start, each part is rated in each of four categories:
//  - x: Extremely cool looking
//  - m: Musical (it makes a noise when you hit it)
//  - a: Aerodynamic
//  - s: Shiny
//
// Then, each part is sent through a series of workflows that will ultimately accept or reject the part.
// Each workflow has a name and contains a list of rules; each rule specifies a condition and where
// to send the part if the condition is true. The first rule that matches the part being considered is
// applied immediately, and the part moves on to the destination described by the rule.
// (The last rule in each workflow has no condition and always applies if reached.)
//
// One of the Elves comes up with a new plan: rather than sort parts individually through all of these workflows,
// maybe you can figure out in advance which combinations of ratings will be accepted or rejected.
//
// Each of the four ratings (x, m, a, s) can have an integer value ranging from a minimum of 1 to a maximum of 4000.
// Of all possible distinct combinations of ratings, your job is to figure out which ones will be accepted.
// How many distinct combinations of ratings will be accepted by the Elves' workflows?

extern crate advent_of_code_2023 as advent;

use std::collections::HashMap;
use std::io::Error;
use std::ops::RangeInclusive;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::newline;
use nom::character::complete::{alpha1, i64 as i64_parser};
use nom::combinator::{map, opt, value};
use nom::multi::separated_list1;
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::IResult;

use advent::runner::run_puzzle;

type ValueRange = RangeInclusive<i64>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Part {
    x: ValueRange,
    m: ValueRange,
    a: ValueRange,
    s: ValueRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Decision {
    Accept,
    Reject,
    Continue(WorkflowName),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Condition {
    GreaterThan(i64),
    LessThan(i64),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Rule {
    pub x: Option<Condition>,
    pub m: Option<Condition>,
    pub a: Option<Condition>,
    pub s: Option<Condition>,
    pub decision: Decision,
}

type WorkflowName = String;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Workflow {
    pub name: String,
    pub rules: Vec<Rule>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct System {
    workflows: Vec<Workflow>,
    by_name: HashMap<WorkflowName, usize>,
}

impl System {
    pub fn new(workflows: Vec<Workflow>) -> Self {
        let by_name = workflows
            .iter()
            .enumerate()
            .map(|(i, workflow)| (workflow.name.clone(), i))
            .collect();
        Self { workflows, by_name }
    }

    pub fn apply(&self, part: Part) -> Vec<(Part, Decision)> {
        let mut finished = vec![];
        let mut ongoing = vec![(&self.workflows[self.by_name["in"]], part)];
        while let Some((workflow, part)) = ongoing.pop() {
            for (part, decision) in workflow.apply(part) {
                match decision {
                    Decision::Accept | Decision::Reject => finished.push((part, decision)),
                    Decision::Continue(name) => {
                        ongoing.push((&self.workflows[self.by_name[&name]], part));
                    }
                }
            }
        }
        finished
    }
}

impl Condition {
    pub fn match_range(&self, range: ValueRange) -> (Option<ValueRange>, Option<ValueRange>) {
        match self {
            Condition::GreaterThan(value) => {
                let start = range.start();
                let end = range.end();
                if value >= end {
                    (None, Some(range))
                } else if value < start {
                    (Some(range), None)
                } else {
                    let new_start = value + 1;
                    (Some(new_start..=*end), Some(*start..=*value))
                }
            }
            Condition::LessThan(value) => {
                let start = range.start();
                let end = range.end();
                if value <= start {
                    (Some(range), None)
                } else if value > end {
                    (None, Some(range))
                } else {
                    let new_end = value - 1;
                    (Some(*start..=new_end), Some(*value..=*end))
                }
            }
        }
    }
}

impl Workflow {
    pub fn apply(&self, part: Part) -> Vec<(Part, Decision)> {
        let mut parts = vec![];
        let mut part = part;
        for rule in &self.rules {
            let (matched, unmatched) = rule.apply(part);
            if let Some((matched, decision)) = matched {
                parts.push((matched, decision));
            }
            if let Some(unmatched) = unmatched {
                part = unmatched;
            } else {
                break;
            }
        }
        parts
    }
}

impl Rule {
    pub fn apply(&self, part: Part) -> (Option<(Part, Decision)>, Option<Part>) {
        let matched;
        let unmatched;
        if let Some(condition) = &self.x {
            let (matched_range, unmatched_range) = condition.match_range(part.x.clone());
            matched = matched_range.map(|range| part.with_different_x(range));
            unmatched = unmatched_range.map(|range| part.with_different_x(range));
        } else if let Some(condition) = &self.m {
            let (matched_range, unmatched_range) = condition.match_range(part.m.clone());
            matched = matched_range.map(|range| part.with_different_m(range));
            unmatched = unmatched_range.map(|range| part.with_different_m(range));
        } else if let Some(condition) = &self.a {
            let (matched_range, unmatched_range) = condition.match_range(part.a.clone());
            matched = matched_range.map(|range| part.with_different_a(range));
            unmatched = unmatched_range.map(|range| part.with_different_a(range));
        } else if let Some(condition) = &self.s {
            let (matched_range, unmatched_range) = condition.match_range(part.s.clone());
            matched = matched_range.map(|range| part.with_different_s(range));
            unmatched = unmatched_range.map(|range| part.with_different_s(range));
        } else {
            matched = Some(part);
            unmatched = None;
        };
        (matched.map(|part| (part, self.decision.clone())), unmatched)
    }
}

impl Part {
    pub fn size(&self) -> i64 {
        (self.x.end() - self.x.start() + 1)
            * (self.m.end() - self.m.start() + 1)
            * (self.a.end() - self.a.start() + 1)
            * (self.s.end() - self.s.start() + 1)
    }

    pub fn with_different_x(&self, x: ValueRange) -> Self {
        let mut part = self.clone();
        part.x = x;
        part
    }

    pub fn with_different_m(&self, m: ValueRange) -> Self {
        let mut part = self.clone();
        part.m = m;
        part
    }

    pub fn with_different_a(&self, a: ValueRange) -> Self {
        let mut part = self.clone();
        part.a = a;
        part
    }

    pub fn with_different_s(&self, s: ValueRange) -> Self {
        let mut part = self.clone();
        part.s = s;
        part
    }

    pub fn intersection(&self, other: &Self) -> Option<Self> {
        let x = Self::range_intersection(self.x.clone(), other.x.clone())?;
        let m = Self::range_intersection(self.m.clone(), other.m.clone())?;
        let a = Self::range_intersection(self.a.clone(), other.a.clone())?;
        let s = Self::range_intersection(self.s.clone(), other.s.clone())?;
        Some(Self { x, m, a, s })
    }

    fn range_intersection(range: ValueRange, other: ValueRange) -> Option<ValueRange> {
        let start = range.start();
        let end = range.end();
        let other_start = other.start();
        let other_end = other.end();
        if other_start > end || other_end < start {
            None
        } else {
            Some(*start.max(other_start)..=*end.min(other_end))
        }
    }
}

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle19.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, System> {
    let decision = alt((
        value(Decision::Accept, tag("A")),
        value(Decision::Reject, tag("R")),
        map(workflow_name, Decision::Continue),
    ));
    let condition = alt((
        map(preceded(tag(">"), i64_parser), Condition::GreaterThan),
        map(preceded(tag("<"), i64_parser), Condition::LessThan),
    ));
    let rule = map(
        tuple((opt(tuple((alpha1, condition, tag(":")))), decision)),
        |(condition_block, decision)| {
            let mut rule = Rule {
                x: None,
                m: None,
                a: None,
                s: None,
                decision,
            };
            match condition_block {
                Some(("x", condition, _)) => rule.x = Some(condition),
                Some(("m", condition, _)) => rule.m = Some(condition),
                Some(("a", condition, _)) => rule.a = Some(condition),
                Some(("s", condition, _)) => rule.s = Some(condition),
                None => {}
                _ => panic!("Unexpected condition: {condition_block:?}"),
            }
            rule
        },
    );
    let workflow = map(
        tuple((
            workflow_name,
            tag("{"),
            separated_list1(tag(","), rule),
            tag("}"),
        )),
        |(name, _, rules, _)| Workflow { name, rules },
    );
    let system = map(separated_list1(newline, workflow), System::new);
    let part = delimited(
        tag("{"),
        tuple((
            delimited(tag("x="), i64_parser, tag(",")),
            delimited(tag("m="), i64_parser, tag(",")),
            delimited(tag("a="), i64_parser, tag(",")),
            preceded(tag("s="), i64_parser),
        )),
        tag("}"),
    );
    let parts = separated_list1(newline, part);
    terminated(system, tuple((tag("\n\n"), parts)))(input)
}

fn workflow_name(input: &str) -> IResult<&str, WorkflowName> {
    map(alpha1, |s: &str| s.to_string())(input)
}

fn puzzle(system: &System) -> i64 {
    let start = Part {
        x: 1..=4000,
        m: 1..=4000,
        a: 1..=4000,
        s: 1..=4000,
    };
    let accepted: Vec<_> = system
        .apply(start)
        .into_iter()
        .filter_map(|(part, decision)| (decision == Decision::Accept).then_some(part))
        .collect();
    let mut size: i64 = 0;
    let mut applied = vec![];
    for part in accepted {
        size += part.size();
        for other in &applied {
            size -= part.intersection(other).map_or(0, |p| p.size());
        }
        applied.push(part);
    }
    size
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "px{a<2006:qkq,m>2090:A,rfg}\n\
                     pv{a>1716:R,A}\n\
                     lnx{m>1548:A,A}\n\
                     rfg{s<537:gd,x>2440:R,A}\n\
                     qs{s>3448:A,lnx}\n\
                     qkq{x<1416:A,crn}\n\
                     crn{x>2662:A,R}\n\
                     in{s<1351:px,qqz}\n\
                     qqz{s>2770:qs,m<1801:hdj,R}\n\
                     gd{a>3333:R,R}\n\
                     hdj{m>838:A,pv}\n\
                     \n\
                     {x=787,m=2655,a=1222,s=2876}\n\
                     {x=1679,m=44,a=2067,s=496}\n\
                     {x=2036,m=264,a=79,s=2244}\n\
                     {x=2461,m=1339,a=466,s=291}\n\
                     {x=2127,m=1623,a=2188,s=1013}";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 167_409_079_868_000);
        Ok(())
    }
}
