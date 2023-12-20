// https://adventofcode.com/2023/day/20#part2
// Modules communicate using pulses. Each pulse is either a high pulse or a low pulse. When a module sends a pulse,
// it sends that type of pulse to each module in its list of destination modules.
//
// There are several different types of modules:
// - Flip-flop modules (prefix %) are either on or off; they are initially off. If a flip-flop module receives
//   a high pulse, it is ignored and nothing happens. However, if a flip-flop module receives a low pulse,
//   it flips between on and off. If it was off, it turns on and sends a high pulse. If it was on, it turns off
//   and sends a low pulse.
// - Conjunction modules (prefix &) remember the type of the most recent pulse received from each of their
//   connected input modules; they initially default to remembering a low pulse for each input. When a pulse is
//   received, the conjunction module first updates its memory for that input. Then, if it remembers high pulses
//   for all inputs, it sends a low pulse; otherwise, it sends a high pulse.
// - There is a single broadcast module (named broadcaster). When it receives a pulse, it sends the same
//   pulse to all of its destination modules.
// - Here at Desert Machine Headquarters, there is a module with a single button on it called, aptly,
//   the button module. When you push the button, a single low pulse is sent directly to the broadcaster module.
//
// The final machine responsible for moving the sand down to Island Island has a module attached named rx.
// The machine turns on when a single low pulse is sent to rx.
//
// Reset all modules to their default states. Waiting for all pulses to be fully handled after each button press,
// what is the fewest number of button presses required to deliver a single low pulse to the module named rx?

extern crate advent_of_code_2023 as advent;

use std::collections::{BTreeMap, HashMap, VecDeque};
use std::hash::Hash;
use std::io::Error;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::newline;
use nom::combinator::map;
use nom::multi::separated_list1;
use nom::sequence::{separated_pair, tuple};
use nom::{IResult, Parser};

use advent::runner::run_puzzle;

type Name = String;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum ModuleKind {
    FlipFlop(Pulse),
    Conjunction(BTreeMap<Name, Pulse>),
    Broadcaster,
}

#[derive(Debug, PartialEq, Eq, Hash, Ord, PartialOrd, Clone, Copy)]
enum Pulse {
    High,
    Low,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Module {
    pub name: Name,
    pub kind: ModuleKind,
    pub destinations: Vec<Name>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Signal {
    pub from: Name,
    pub to: Name,
    pub pulse: Pulse,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Configuration {
    modules: BTreeMap<Name, Module>,
}

impl Configuration {
    pub fn new(mut modules: Vec<Module>) -> Self {
        let mut reversed_links: HashMap<Name, Vec<Name>> = HashMap::new();
        for module in &modules {
            for destination in &module.destinations {
                reversed_links
                    .entry(destination.clone())
                    .or_default()
                    .push(module.name.clone());
            }
        }
        for module in &mut modules {
            if let ModuleKind::Conjunction(ref mut states) = module.kind {
                states.extend(
                    reversed_links[&module.name]
                        .iter()
                        .map(|name| (name.clone(), Pulse::Low)),
                );
            }
        }
        let modules = modules
            .into_iter()
            .map(|module| (module.name.clone(), module))
            .collect();
        Configuration { modules }
    }

    pub fn get_mut(&mut self, name: &Name) -> Option<&mut Module> {
        self.modules.get_mut(name)
    }
}

impl Pulse {
    pub fn flip(self) -> Self {
        match self {
            Pulse::High => Pulse::Low,
            Pulse::Low => Pulse::High,
        }
    }
}

impl Module {
    pub fn apply(&mut self, signal: Signal) -> Vec<Signal> {
        let pulse = signal.pulse;
        match &mut self.kind {
            ModuleKind::FlipFlop(_) if pulse == Pulse::High => vec![],
            ModuleKind::FlipFlop(state) if pulse == Pulse::Low => {
                *state = state.flip();
                Module::emit(&self.name, *state, &self.destinations)
            }
            ModuleKind::FlipFlop(_) => unreachable!(),
            ModuleKind::Conjunction(states) => {
                states.insert(signal.from, pulse);
                let pulse = if states.values().all(|&pulse| pulse == Pulse::High) {
                    Pulse::Low
                } else {
                    Pulse::High
                };
                Module::emit(&self.name, pulse, &self.destinations)
            }
            ModuleKind::Broadcaster => Module::emit(&self.name, pulse, &self.destinations),
        }
    }

    fn emit(from: &Name, pulse: Pulse, destinations: &[Name]) -> Vec<Signal> {
        destinations
            .iter()
            .map(|name| Signal {
                from: from.clone(),
                to: name.clone(),
                pulse,
            })
            .collect()
    }
}

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle20.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Configuration> {
    let kind = alt((
        map(tuple((tag("%"), module_name)), |(_, name)| {
            (ModuleKind::FlipFlop(Pulse::Low), name)
        }),
        map(tuple((tag("&"), module_name)), |(_, name)| {
            (ModuleKind::Conjunction(BTreeMap::new()), name)
        }),
        map(tag("broadcaster"), |_| {
            (ModuleKind::Broadcaster, "broadcaster".to_string())
        }),
    ));
    let module = map(
        separated_pair(kind, tag(" -> "), separated_list1(tag(", "), module_name)),
        |((kind, name), destinations)| Module {
            name,
            kind,
            destinations,
        },
    );
    let mut configuration = map(separated_list1(newline, module), Configuration::new);
    configuration.parse(input)
}

fn module_name(input: &str) -> IResult<&str, Name> {
    map(alpha1, |s: &str| s.to_string())(input)
}

fn puzzle(input: &Configuration) -> usize {
    let mut configuration: Configuration = input.clone();
    let mut i = 0;
    let interesting_module = "gh";
    let sources_number = configuration
        .modules
        .iter()
        .filter(|(_, module)| module.destinations.iter().any(|s| s == interesting_module))
        .count();
    let mut stats: HashMap<Name, usize> = HashMap::new();
    let mut periods: HashMap<Name, usize> = HashMap::new();
    loop {
        let signals = press_button(&mut configuration);
        i += 1;
        for signal in &signals {
            if signal.to == interesting_module && signal.pulse == Pulse::High {
                if let Some(&j) = stats.get(&signal.from) {
                    periods.insert(signal.from.clone(), i - j);
                }
                stats.insert(signal.from.clone(), i);
            }
        }
        if periods.len() == sources_number {
            break;
        }
    }
    let periods: Vec<_> = periods.into_values().collect();
    lcm(&periods)
}

fn lcm(values: &[usize]) -> usize {
    let mut result = 1;
    let mut factors = vec![];
    for value in values {
        let mut value = *value;
        let mut divisor = 2;
        while value > 1 {
            if value % divisor == 0 {
                value /= divisor;
                factors.push(divisor);
            } else {
                divisor += 1;
            }
        }
    }
    factors.sort_unstable();
    factors.dedup();
    for factor in factors {
        result *= factor;
    }
    result
}

fn press_button(configuration: &mut Configuration) -> Vec<Signal> {
    let mut result = vec![];
    let mut signals = VecDeque::from([Signal {
        from: "button".to_string(),
        to: "broadcaster".to_string(),
        pulse: Pulse::Low,
    }]);
    while let Some(signal) = signals.pop_front() {
        result.push(signal.clone());
        if let Some(module) = configuration.get_mut(&signal.to) {
            let new_signals = module.apply(signal);
            signals.extend(new_signals);
        }
    }
    result
}
