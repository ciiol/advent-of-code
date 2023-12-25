// https://adventofcode.com/2023/day/25
// Someone left a wiring diagram (your puzzle input) that shows how the components are connected.
// Connections aren't directional; abc: xyz and xyz: abc both represent the same configuration.
//
// Find the three wires you need to disconnect in order to divide the components into two separate groups.
// What do you get if you multiply the sizes of these two groups together?
extern crate advent_of_code_2023 as advent;

use std::collections::{HashMap, HashSet};
use std::io::Error;

use itertools::Itertools;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, newline, space1};
use nom::combinator::map;
use nom::multi::separated_list1;
use nom::sequence::{separated_pair, tuple};
use nom::IResult;
use pathfinding::directed::bfs::bfs;
use pathfinding::undirected::connected_components::connected_components;

use advent::runner::run_puzzle;

type Component = u16;
type Diagram = HashMap<Component, Vec<Component>>;

fn main() -> Result<(), Error> {
    run_puzzle("inputs/puzzle25.txt", puzzle_input, puzzle)
}

fn puzzle_input(input: &str) -> IResult<&str, Diagram> {
    let connected = separated_list1(space1, component_name);
    let component = separated_pair(component_name, tuple((tag(":"), space1)), connected);
    map(separated_list1(newline, component), |components| {
        let names: HashMap<String, u16> = components
            .iter()
            .map(|(k, _)| k)
            .cloned()
            .chain(components.iter().flat_map(|(_, v)| v.iter().cloned()))
            .unique()
            .enumerate()
            .map(|(i, name)| (name, i.try_into().unwrap()))
            .collect();
        let mut diagram: Diagram = HashMap::new();
        for (k, v) in &components {
            let k = names[k];
            let v: Vec<_> = v.iter().map(|n| names[n]).collect();
            diagram.insert(k, v);
        }
        for (k, v) in components {
            let k = names[&k];
            let v = v.into_iter().map(|n| names[&n]);
            for c in v {
                diagram.entry(c).or_default().push(k);
            }
        }
        diagram
    })(input)
}

fn component_name(input: &str) -> IResult<&str, String> {
    map(alpha1, |s: &str| s.to_string())(input)
}

fn puzzle(input: &Diagram) -> usize {
    let mut statistics: HashMap<Vec<Component>, usize> = HashMap::new();
    let nodes: HashSet<_> = input.keys().copied().collect();
    let mut paths_handled = 0;
    for (i, a) in nodes.iter().enumerate() {
        for b in nodes.iter().skip(i + 1) {
            let path = bfs(a, |c| input.get(c).unwrap().iter().copied(), |c| c == b).unwrap();
            let edges = path.iter().zip(path.iter().chain(&path).skip(1));
            for (n1, n2) in edges {
                let mut edge = vec![*n1, *n2];
                edge.sort_unstable();
                statistics.entry(edge).and_modify(|c| *c += 1).or_insert(1);
            }
            paths_handled += 1;
            if paths_handled > 10_000 {
                break;
            }
        }
    }
    let top_edges: Vec<_> = statistics
        .into_iter()
        .sorted_by_key(|(_, c)| *c)
        .rev()
        .take(3)
        .map(|(e, _)| e)
        .collect();
    let components = components_without(input, &top_edges);
    assert_eq!(components.len(), 2);
    components.into_iter().product()
}

fn components_without(graph: &Diagram, exclude: &[Vec<Component>]) -> Vec<usize> {
    let start: Vec<_> = graph.keys().collect();
    connected_components(&start, |&c| {
        if let Some(neighbors) = graph.get(c) {
            let neighbors: Vec<_> = neighbors
                .iter()
                .filter(|n| !exclude.iter().any(|e| e.contains(c) && e.contains(n)))
                .collect();
            neighbors
        } else {
            vec![]
        }
    })
    .into_iter()
    .map(|c| c.len())
    .collect()
}

#[cfg(test)]
mod tests {
    use advent::parser::parse;

    use super::*;

    #[test]
    fn test_puzzle() -> Result<(), Error> {
        let input = "jqt: rhn xhk nvd\n\
                     rsh: frs pzl lsr\n\
                     xhk: hfx\n\
                     cmg: qnr nvd lhk bvb\n\
                     rhn: xhk bvb hfx\n\
                     bvb: xhk hfx\n\
                     pzl: lsr hfx nvd\n\
                     qnr: nvd\n\
                     ntq: jqt hfx bvb xhk\n\
                     nvd: lhk\n\
                     lsr: lhk\n\
                     rzs: qnr cmg lsr rsh\n\
                     frs: qnr lhk lsr";
        assert_eq!(puzzle(&parse(input, puzzle_input)?), 54);
        Ok(())
    }
}
