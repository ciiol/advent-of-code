use std::collections::HashSet;
use std::fmt::{Debug, Display};
use std::ops::{Add, Mul, Sub};

use ndarray::{Array2, ArrayView1};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Coord {
    pub row: usize,
    pub col: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Direction {
    pub row: isize,
    pub col: isize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Matrix<T> {
    rows: usize,
    cols: usize,
    data: Array2<T>,
}

impl<T> Matrix<T> {
    /// # Panics
    /// Panics if the input data is not rectangular.
    pub fn new(data: Vec<Vec<T>>) -> Self {
        let rows = data.len();
        let cols = data[0].len();
        let data =
            Array2::from_shape_vec((rows, cols), data.into_iter().flatten().collect()).unwrap();
        Self { rows, cols, data }
    }

    pub fn get(&self, coord: Coord) -> Option<&T> {
        self.data.get(coord.into_tuple())
    }

    pub fn get_mut(&mut self, coord: Coord) -> Option<&mut T> {
        self.data.get_mut(coord.into_tuple())
    }

    pub fn rows_size(&self) -> usize {
        self.rows
    }

    pub fn cols_size(&self) -> usize {
        self.cols
    }

    pub fn coords(&self) -> impl Iterator<Item = Coord> + '_ {
        (0..self.rows).flat_map(|row| (0..self.cols).map(move |col| (row, col).into()))
    }

    pub fn iter(&self) -> impl Iterator<Item = (Coord, &T)> + '_ {
        self.coords().zip(self.data.iter())
    }

    pub fn rows(&self) -> impl Iterator<Item = ArrayView1<T>> + '_ {
        self.data.axis_iter(ndarray::Axis(0))
    }

    pub fn cols(&self) -> impl Iterator<Item = ArrayView1<T>> + '_ {
        self.data.axis_iter(ndarray::Axis(1))
    }

    pub fn search<'a, P>(&'a self, predicate: P) -> impl Iterator<Item = Coord> + 'a
    where
        P: Fn(&T) -> bool + 'a,
    {
        self.iter().filter_map(
            move |(coord, item)| {
                if predicate(item) {
                    Some(coord)
                } else {
                    None
                }
            },
        )
    }

    pub fn walk<'a, S, P>(
        &'a self,
        start: Coord,
        next_coords: S,
        predicate: P,
    ) -> impl Iterator<Item = Coord> + 'a
    where
        S: Fn(Coord) -> Vec<Coord> + 'a,
        P: Fn(&T) -> bool + 'a,
    {
        let mut visited = HashSet::new();
        let mut stack = vec![start];
        std::iter::from_fn(move || {
            while let Some(coord) = stack.pop() {
                if visited.contains(&coord) {
                    continue;
                }
                visited.insert(coord);
                if self.get(coord).map_or(false, &predicate) {
                    stack.extend(next_coords(coord));
                    return Some(coord);
                }
            }
            None
        })
    }

    pub fn neighbours8(&self, coord: Coord) -> impl Iterator<Item = Coord> + '_ {
        (-1..=1)
            .flat_map(|d_row| (-1..=1).map(move |d_col| (d_row, d_col)))
            .filter(|(d_row, d_col)| *d_row != 0 || *d_col != 0)
            .filter_map(move |(d_row, d_col)| {
                let d = Direction {
                    row: d_row,
                    col: d_col,
                };
                (coord + d).and_then(|coord| {
                    if self.is_inside(coord) {
                        Some(coord)
                    } else {
                        None
                    }
                })
            })
    }

    pub fn is_inside(&self, coord: Coord) -> bool {
        (coord.row < self.rows) && (coord.col < self.cols)
    }
}

impl<T> Display for Matrix<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in 0..self.rows {
            for col in 0..self.cols {
                write!(f, "{}", self.get((row, col).into()).unwrap())?;
            }
            if row < self.rows - 1 {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

impl Coord {
    pub fn into_tuple(self) -> (usize, usize) {
        (self.row, self.col)
    }
}

impl From<(usize, usize)> for Coord {
    fn from((row, col): (usize, usize)) -> Self {
        Self { row, col }
    }
}

impl TryFrom<Direction> for Coord {
    type Error = &'static str;

    fn try_from(direction: Direction) -> Result<Self, Self::Error> {
        if direction.row < 0 || direction.col < 0 {
            Err("Negative coordinates")
        } else {
            Ok(Self {
                row: direction.row.try_into().unwrap(),
                col: direction.col.try_into().unwrap(),
            })
        }
    }
}

impl Direction {
    pub fn left() -> Self {
        Self { row: 0, col: -1 }
    }

    pub fn right() -> Self {
        Self { row: 0, col: 1 }
    }

    pub fn up() -> Self {
        Self { row: -1, col: 0 }
    }

    pub fn down() -> Self {
        Self { row: 1, col: 0 }
    }

    pub fn into_tuple(self) -> (isize, isize) {
        (self.row, self.col)
    }

    #[must_use]
    pub fn rotate_left(self) -> Self {
        Self {
            row: -self.col,
            col: self.row,
        }
    }

    #[must_use]
    pub fn rotate_right(self) -> Self {
        Self {
            row: self.col,
            col: -self.row,
        }
    }

    pub fn is_horizontal(self) -> bool {
        self.row == 0
    }

    pub fn is_vertical(self) -> bool {
        self.col == 0
    }

    pub fn manhattan_distance(self) -> usize {
        (self.row.abs() + self.col.abs()).try_into().unwrap()
    }
}

impl From<(isize, isize)> for Direction {
    fn from((row, col): (isize, isize)) -> Self {
        Self { row, col }
    }
}

impl From<Coord> for Direction {
    fn from(coord: Coord) -> Self {
        Self {
            row: coord.row.try_into().unwrap(),
            col: coord.col.try_into().unwrap(),
        }
    }
}

impl Add<Direction> for Direction {
    type Output = Self;

    fn add(self, other: Direction) -> Self::Output {
        Self {
            row: self.row + other.row,
            col: self.col + other.col,
        }
    }
}

impl Add<Direction> for Coord {
    type Output = Option<Self>;

    fn add(self, direction: Direction) -> Self::Output {
        let coord: Direction = self.into();
        (coord + direction).try_into().ok()
    }
}

impl Add<Direction> for Option<Coord> {
    type Output = Option<Coord>;

    fn add(self, other: Direction) -> Self::Output {
        self.and_then(|coord| coord + other)
    }
}

impl Sub<Direction> for Direction {
    type Output = Direction;

    fn sub(self, other: Direction) -> Self::Output {
        Self {
            row: self.row - other.row,
            col: self.col - other.col,
        }
    }
}

impl Sub<Coord> for Coord {
    type Output = Direction;

    fn sub(self, other: Coord) -> Self::Output {
        Direction::from(self) - Direction::from(other)
    }
}

impl Mul<isize> for Direction {
    type Output = Direction;

    fn mul(self, multiplier: isize) -> Self::Output {
        Self {
            row: self.row * multiplier,
            col: self.col * multiplier,
        }
    }
}
