#![feature(test)]

extern crate crossbeam;
extern crate test;

// TODO change these
mod ctrie;
mod iter;
mod node;
mod persistent_list;

pub use ctrie::CTrie;
pub use ctrie::DefaultHashBuilder;
