extern crate js_sys;
extern crate fixedbitset;
#[macro_use]
mod utils;

use wasm_bindgen::prelude::*;
use fixedbitset::FixedBitSet;

#[wasm_bindgen]
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cell {
    Dead = 0,
    Alive = 1,
}

#[wasm_bindgen]
pub struct Universe {
    width: u32,
    height: u32,
    cells: FixedBitSet,
}

impl Universe {
    fn get_index(&self, row: u32, column: u32) -> usize {
        (row * self.width + column) as usize
    }

    fn live_neighbor_count(&self, row: u32, column: u32) -> u8 {
        let mut count = 0;

        let north = if row == 0 {
            self.height - 1
        } else {
            row - 1
        };

        let south = if row == self.height - 1 {
            0
        } else {
            row + 1
        };

        let west = if column == 0 {
            self.width - 1
        } else {
            column - 1
        };

        let east = if column == self.width - 1 {
            0
        } else {
            column + 1
        };

        let nw = self.get_index(north, west);
        count += self.cells[nw] as u8;

        let n = self.get_index(north, column);
        count += self.cells[n] as u8;

        let ne = self.get_index(north, east);
        count += self.cells[ne] as u8;

        let w = self.get_index(row, west);
        count += self.cells[w] as u8;

        let e = self.get_index(row, east);
        count += self.cells[e] as u8;

        let sw = self.get_index(south, west);
        count += self.cells[sw] as u8;

        let s = self.get_index(south, column);
        count += self.cells[s] as u8;

        let se = self.get_index(south, east);
        count += self.cells[se] as u8;

        count
    }

    pub fn set_cells(&mut self, cells: &[(u32,u32)]) {
        for(row,col) in cells.iter().cloned() {
            let idx = self.get_index(row,col);
            self.cells.set(idx,true);
        }
    }

    pub fn get_cells(&self) -> FixedBitSet {
        self.cells.clone()
    }
}

#[wasm_bindgen]
impl Universe {
    pub fn size(&self) -> usize {
        (self.width() * self.height()) as usize
    }

    pub fn width(&self) -> u32 {
        self.width
    }

    pub fn set_width(&mut self, width: u32) {
        self.resize(self.height, width);
    }

    pub fn height(&self) -> u32 {
        self.height
    }

    pub fn set_height(&mut self, height: u32) {
        self.resize(height, self.width);
    }

    pub fn resize(&mut self, height: u32, width: u32) {
        self.height = height;
        self.width = width;
        let mut cells = FixedBitSet::with_capacity(self.size());
        cells.clear();
        self.cells = cells;
    }

    pub fn cells(&self) -> *const u32 {
        self.cells.as_slice().as_ptr()
    }

    pub fn tick(&mut self) {
        // let _timer = utils::Timer::new("Universe::tick");
        let mut next = self.cells.clone();
        for row in 0..self.height {
            for col in 0..self.width {
                let idx = self.get_index(row, col);
                let cell = self.cells[idx];
                let live_neighbors = self.live_neighbor_count(row,col);
                let next_cell = match (cell, live_neighbors) {
                    (true, x) if x < 2 => false,
                    (true, 2) | (true, 3) => true,
                    (true, x) if x> 3 => false,
                    (false, 3) => true,
                    (otherwise, _ ) => otherwise,
                };
                next.set(idx, next_cell);
            }
        }
        self.cells = next;
    }

    pub fn random(&mut self) {
        for i in 0..self.size() {
            let cell = if js_sys::Math::random() < 0.5 {
                true
            } else {
                false
            };
            log!(
                "cell[{}] is initially {:?}",
                i,
                if cell { Cell::Alive } else { Cell::Dead }
             );
            self.cells.set(i, cell);
        }
    }

    pub fn new() -> Universe {
        let mut res = Self::with_capacity(128, 128);
        res.random();
        res
    }

    pub fn with_capacity(height: u32, width: u32) -> Universe {
        let size = (width * height) as usize;
        let cells= FixedBitSet::with_capacity(size);
        Universe {
            width,
            height,
            cells,
        }
    }

    pub fn toggle_cell(&mut self, row: u32, column: u32) {
        let idx = self.get_index(row, column);
        self.cells.set(idx, !self.cells[idx]);
    }
}



// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
