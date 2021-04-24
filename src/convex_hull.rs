//! 2D convex hull algorithms

use crate::{error::ERROR_MARGIN, utils::vec_to_point};
use geometric_algebra::RegressiveProduct;

/// Andrew's (monotone chain) convex hull algorithm
pub fn andrew(input_points: &[[f32; 2]]) -> Vec<[f32; 2]> {
    let mut input_points = input_points.to_owned();
    if input_points.len() < 3 {
        return input_points;
    }
    input_points.sort_by(|a, b| {
        a[0].partial_cmp(&b[0])
            .unwrap_or_else(|| unreachable!())
            .then(a[1].partial_cmp(&b[1]).unwrap_or_else(|| unreachable!()))
    });
    let mut hull = Vec::with_capacity(2 * input_points.len());
    for input_point in input_points.iter().cloned() {
        while hull.len() > 1
            && vec_to_point(hull[hull.len() - 2])
                .regressive_product(vec_to_point(hull[hull.len() - 1]))
                .regressive_product(vec_to_point(input_point))
                .g0
                <= ERROR_MARGIN
        {
            hull.pop();
        }
        hull.push(input_point);
    }
    hull.pop();
    let t = hull.len() + 1;
    for input_point in input_points.iter().rev().cloned() {
        while hull.len() > t
            && vec_to_point(hull[hull.len() - 2])
                .regressive_product(vec_to_point(hull[hull.len() - 1]))
                .regressive_product(vec_to_point(input_point))
                .g0
                <= ERROR_MARGIN
        {
            hull.pop();
        }
        hull.push(input_point);
    }
    hull.pop();
    hull
}
