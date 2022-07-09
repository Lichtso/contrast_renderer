//! Contrast is a web-gpu based 2D render engine written in Rust.
#![warn(missing_docs)]

pub extern crate geometric_algebra;
#[cfg(feature = "text")]
pub extern crate ttf_parser;
pub extern crate wgpu;

pub mod convex_hull;
pub mod curve;
pub mod error;
mod fill;
pub mod path;
pub mod polynomial;
pub mod renderer;
pub mod safe_float;
mod stroke;
#[cfg(feature = "text")]
pub mod text;
pub mod utils;
mod vertex;
