pub fn andrew(input_points: &[[f32; 2]]) -> Vec<[f32; 2]> {
    let mut input_points = input_points.to_owned();
    if input_points.len() < 3 {
        return input_points;
    }
    fn cross(a: [f32; 2], b: [f32; 2], c: [f32; 2]) -> f32 {
        (a[0] - c[0]) * (b[1] - c[1]) - (a[1] - c[1]) * (b[0] - c[0])
    }
    input_points.sort_by(|a, b| {
        a[0].partial_cmp(&b[0])
            .unwrap_or_else(|| unreachable!())
            .then(a[1].partial_cmp(&b[1]).unwrap_or_else(|| unreachable!()))
    });
    let mut hull = Vec::with_capacity(2 * input_points.len());
    for input_point in input_points.iter().cloned() {
        while hull.len() > 1
            && cross(hull[hull.len() - 2], hull[hull.len() - 1], input_point) <= 0.0
        {
            hull.pop();
        }
        hull.push(input_point);
    }
    hull.pop();
    let t = hull.len() + 1;
    for input_point in input_points.iter().rev().cloned() {
        while hull.len() > t
            && cross(hull[hull.len() - 2], hull[hull.len() - 1], input_point) <= 0.0
        {
            hull.pop();
        }
        hull.push(input_point);
    }
    hull.pop();
    hull
}
