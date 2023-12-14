use std::collections::HashSet;
use std::fmt::{Debug, Display};

use ndarray::{Array2, ArrayView1};

pub type Coord = (usize, usize);

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
        self.data.get(coord)
    }

    pub fn get_mut(&mut self, coord: Coord) -> Option<&mut T> {
        self.data.get_mut(coord)
    }

    pub fn rows_size(&self) -> usize {
        self.rows
    }

    pub fn cols_size(&self) -> usize {
        self.cols
    }

    pub fn coords(&self) -> impl Iterator<Item = Coord> + '_ {
        (0..self.rows).flat_map(|row| (0..self.cols).map(move |col| (row, col)))
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
        let (row, col) = coord;
        let col: isize = col.try_into().unwrap();
        let row: isize = row.try_into().unwrap();
        let cols: isize = self.cols.try_into().unwrap();
        let rows: isize = self.rows.try_into().unwrap();
        (-1..=1)
            .flat_map(|d_row| (-1..=1).map(move |d_col| (d_row, d_col)))
            .filter(|(d_row, d_col)| *d_row != 0 || *d_col != 0)
            .filter_map(move |(d_row, d_col)| {
                if (row < -d_row) || (col < -d_col) || d_row >= rows - row || d_col >= cols - col {
                    None
                } else {
                    Some((
                        (row + d_row).try_into().unwrap(),
                        (col + d_col).try_into().unwrap(),
                    ))
                }
            })
    }
}

impl<T> Display for Matrix<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in 0..self.rows {
            for col in 0..self.cols {
                write!(f, "{}", self.get((row, col)).unwrap())?;
            }
            if row < self.rows - 1 {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}
