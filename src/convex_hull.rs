use crate::utils::signed_triangle_area;

pub fn andrew(input_points: &[glam::Vec2]) -> Vec<glam::Vec2> {
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
        while hull.len() > 1 && signed_triangle_area(&[hull[hull.len() - 2], hull[hull.len() - 1], input_point]) <= 0.0 {
            hull.pop();
        }
        hull.push(input_point);
    }
    hull.pop();
    let t = hull.len() + 1;
    for input_point in input_points.iter().rev().cloned() {
        while hull.len() > t && signed_triangle_area(&[hull[hull.len() - 2], hull[hull.len() - 1], input_point]) <= 0.0 {
            hull.pop();
        }
        hull.push(input_point);
    }
    hull.pop();
    hull
}