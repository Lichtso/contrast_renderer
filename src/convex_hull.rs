//! 2D convex hull algorithms

use crate::{error::ERROR_MARGIN, safe_float::SafeFloat, utils::vec_to_point};
use geometric_algebra::RegressiveProduct;

/// Andrew's (monotone chain) convex hull algorithm
pub fn andrew(input_points: &[SafeFloat<f32, 2>]) -> Vec<[f32; 2]> {
    let mut input_points = input_points.to_owned();
    if input_points.len() < 3 {
        return input_points.iter().map(|input_point| input_point.unwrap()).collect();
    }
    input_points.sort();
    let mut hull = Vec::with_capacity(2 * input_points.len());
    for input_point in input_points.iter().cloned() {
        while hull.len() > 1
            && vec_to_point(hull[hull.len() - 2])
                .regressive_product(vec_to_point(hull[hull.len() - 1]))
                .regressive_product(vec_to_point(input_point.unwrap()))
                .g0
                <= ERROR_MARGIN
        {
            hull.pop();
        }
        hull.push(input_point.unwrap());
    }
    hull.pop();
    let t = hull.len() + 1;
    for input_point in input_points.iter().rev().cloned() {
        while hull.len() > t
            && vec_to_point(hull[hull.len() - 2])
                .regressive_product(vec_to_point(hull[hull.len() - 1]))
                .regressive_product(vec_to_point(input_point.unwrap()))
                .g0
                <= ERROR_MARGIN
        {
            hull.pop();
        }
        hull.push(input_point.unwrap());
    }
    hull.pop();
    hull
}
