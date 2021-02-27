pub extern crate glam;
#[cfg(feature = "text")]
pub extern crate ttf_parser;
pub extern crate wgpu;

mod complex_number;
pub mod convex_hull;
pub mod curve;
pub mod error;
mod fill;
pub mod path;
pub mod polynomial;
pub mod renderer;
mod stroke;
#[cfg(feature = "text")]
pub mod text;
pub mod utils;
mod vertex;
