//! Various math helper functions to work with bezier curves.
//!
//! All of these functions use the power basis form.

use crate::{
    error::ERROR_MARGIN,
    polynomial::{solve_cubic, solve_linear, solve_quadratic, solve_quartic, Root},
    utils::rotate_90_degree_clockwise,
};
use geometric_algebra::{epga1d, ppga2d, ppga3d, Dual, InnerProduct, Powi, RegressiveProduct, Signum, Zero};

macro_rules! mat_vec_transform {
    (expand, $power_basis:expr, $i:expr, $at:expr) => {
        ppga2d::Scalar { g0: $at } * $power_basis[$i]
    };
    (expand, $power_basis:expr, $i:expr, $at:expr $(, $rest:expr)+) => {
        mat_vec_transform!(expand, $power_basis, $i, $at) +
        mat_vec_transform!(expand, $power_basis, $i + 1 $(, $rest)+)
    };
    ($power_basis:expr, $($at:expr),+ $(,)?) => {
        mat_vec_transform!(expand, $power_basis, 0, $($at),+)
    };
}

/// Transforms the given control points of a rational quadratic bezier curve into the power basis form.
pub fn rational_quadratic_control_points_to_power_basis(control_points: &[ppga2d::Point; 3]) -> [ppga2d::Point; 3] {
    [
        mat_vec_transform!(control_points, 1.0),
        mat_vec_transform!(control_points, -2.0, 2.0),
        mat_vec_transform!(control_points, 1.0, -2.0, 1.0),
    ]
}

/// Transforms the given control points of a rational cubic bezier curve into the power basis form.
pub fn rational_cubic_control_points_to_power_basis(control_points: &[ppga2d::Point; 4]) -> [ppga2d::Point; 4] {
    [
        mat_vec_transform!(control_points, 1.0),
        mat_vec_transform!(control_points, -3.0, 3.0),
        mat_vec_transform!(control_points, 3.0, -6.0, 3.0),
        mat_vec_transform!(control_points, -1.0, 3.0, -3.0, 1.0),
    ]
}

/// Reparametrizes a rational quadratic bezier curve linearly to a new parameter interval between `a` and `b`.
///
/// Can be used for splitting, trimming and bloosoming.
pub fn reparametrize_rational_quadratic(power_basis: &[ppga2d::Point; 3], a: f32, b: f32) -> [ppga2d::Point; 3] {
    [
        mat_vec_transform!(power_basis, 1.0, a, a.powi(2)),
        mat_vec_transform!(power_basis, 0.0, b - a, -2.0 * a.powi(2) + 2.0 * a * b),
        mat_vec_transform!(power_basis, 0.0, 0.0, (a - b).powi(2)),
    ]
}

/// Reparametrizes a rational cubic bezier curve linearly to a new parameter interval between `a` and `b`.
///
/// Can be used for splitting, trimming and bloosoming.
pub fn reparametrize_rational_cubic(power_basis: &[ppga2d::Point; 4], a: f32, b: f32) -> [ppga2d::Point; 4] {
    [
        mat_vec_transform!(power_basis, 1.0, a, a.powi(2), a.powi(3)),
        mat_vec_transform!(
            power_basis,
            0.0,
            b - a,
            -2.0 * a.powi(2) + 2.0 * a * b,
            3.0 * a.powi(2) * b - 3.0 * a.powi(3),
        ),
        mat_vec_transform!(
            power_basis,
            0.0,
            0.0,
            (a - b).powi(2),
            -6.0 * a.powi(2) * b + 3.0 * a * b.powi(2) + 3.0 * a.powi(3),
        ),
        mat_vec_transform!(
            power_basis,
            0.0,
            0.0,
            0.0,
            3.0 * a.powi(2) * b - 3.0 * a * b.powi(2) - a.powi(3) + b.powi(3),
        ),
    ]
}

/*fn scale_tangent(p: ppga2d::Point, t: ppga2d::Plane) -> ppga2d::Plane {
    t / ppga2d::Scalar { g0: 1.0 / p.g0[0].powi(2) }
}*/

/// Calculates the point at parameter t of a rational quadratic bezier curve.
pub fn rational_quadratic_point(power_basis: &[ppga2d::Point; 3], t: f32) -> ppga2d::Point {
    mat_vec_transform!(power_basis, 1.0, t, t.powi(2))
}

/// Calculates the first order derivative at parameter t of a rational quadratic bezier curve.
pub fn rational_quadratic_first_order_derivative(power_basis: &[ppga2d::Point; 3], t: f32) -> ppga2d::Plane {
    let p = mat_vec_transform!(power_basis, 1.0, t, t.powi(2));
    let d1 = mat_vec_transform!(power_basis, 0.0, 1.0, 2.0 * t);
    p.regressive_product(d1)
}

/// Calculates the second order derivative at parameter t of a rational quadratic bezier curve.
pub fn rational_quadratic_second_order_derivative(power_basis: &[ppga2d::Point; 3], t: f32) -> ppga2d::Plane {
    let p = mat_vec_transform!(power_basis, 1.0, t, t.powi(2));
    let d2 = mat_vec_transform!(power_basis, 0.0, 0.0, 2.0);
    p.regressive_product(d2)
}

/// Calculates the point at parameter t of a rational cubic bezier curve.
pub fn rational_cubic_point(power_basis: &[ppga2d::Point; 4], t: f32) -> ppga2d::Point {
    mat_vec_transform!(power_basis, 1.0, t, t.powi(2), t.powi(3))
}

/// Calculates the first order derivative at parameter t of a rational cubic bezier curve.
pub fn rational_cubic_first_order_derivative(power_basis: &[ppga2d::Point; 4], t: f32) -> ppga2d::Plane {
    let p = mat_vec_transform!(power_basis, 1.0, t, t.powi(2), t.powi(3));
    let d1 = mat_vec_transform!(power_basis, 0.0, 1.0, 2.0 * t, 3.0 * t.powi(2));
    p.regressive_product(d1)
}

/// Calculates the second order derivative at parameter t of a rational cubic bezier curve.
pub fn rational_cubic_second_order_derivative(power_basis: &[ppga2d::Point; 4], t: f32) -> ppga2d::Plane {
    let p = mat_vec_transform!(power_basis, 1.0, t, t.powi(2), t.powi(3));
    let d2 = mat_vec_transform!(power_basis, 0.0, 0.0, 2.0, 6.0 * t);
    p.regressive_product(d2)
}

/// Calculates the third order derivative at parameter t of a rational cubic bezier curve.
pub fn rational_cubic_third_order_derivative(power_basis: &[ppga2d::Point; 4], t: f32) -> ppga2d::Plane {
    let p = mat_vec_transform!(power_basis, 1.0, t, t.powi(2), t.powi(3));
    let d1 = mat_vec_transform!(power_basis, 0.0, 1.0, 2.0 * t, 3.0 * t.powi(2));
    let d2 = mat_vec_transform!(power_basis, 0.0, 0.0, 2.0, 6.0 * t);
    let d3 = mat_vec_transform!(power_basis, 0.0, 0.0, 0.0, 6.0);
    p.regressive_product(d3) + d1.regressive_product(d2)
}

/// Calculates the coefficients of the inflection point polynomial for an integral or rational cubic bezier curve.
pub fn inflection_point_polynomial_coefficients(power_basis: &[ppga2d::Point; 4], integral: bool) -> [f32; 4] {
    let mut ippc = ppga3d::Rotor::zero();
    for j in (integral as usize)..4 {
        let mut iter = (0..4).filter(|i| *i != j).map(|i| power_basis[i]);
        ippc.g0[j] = (iter.next().unwrap())
            .regressive_product(iter.next().unwrap())
            .regressive_product(iter.next().unwrap())
            .g0
            * (j as isize % 2 * 2 - 1) as f32;
    }
    ippc = ippc.signum();
    unsafe { ippc.g0.f32x4 }
}

/// Finds the roots of the inflection point polynomial for an integral cubic bezier curve.
///
/// It also returns the discriminant which can be used for classification of the curve.
/// In case there is a loop (discriminant < 0.0) and the `loop_self_intersection` flag is [true],
/// two different roots will be located at the self intersection point.
pub fn integral_inflection_points(ippc: &[f32; 4], loop_self_intersection: bool) -> (f32, [Root; 3]) {
    let discriminant = 3.0 * ippc[2].powi(2) - 4.0 * ippc[1] * ippc[3];
    if ippc[1].abs() <= ERROR_MARGIN {
        if ippc[2].abs() <= ERROR_MARGIN {
            (-1.0, [Root::new(-1.0, 0.0, 1.0), Root::new(1.0, 0.0, 0.0), Root::new(1.0, 0.0, 0.0)])
        } else {
            (
                1.0,
                [Root::new(ippc[3], 0.0, 3.0 * ippc[2]), Root::new(1.0, 0.0, 0.0), Root::new(1.0, 0.0, 0.0)],
            )
        }
    } else {
        let d = (discriminant
            * if discriminant < 0.0 {
                if loop_self_intersection {
                    -1.0
                } else {
                    0.0
                }
            } else {
                1.0f32 / 3.0f32
            })
        .sqrt();
        (
            discriminant,
            [
                Root::new(ippc[2] + d, 0.0, 2.0 * ippc[1]),
                Root::new(ippc[2] - d, 0.0, 2.0 * ippc[1]),
                Root::new(1.0, 0.0, 0.0),
            ],
        )
    }
}

/// Finds the roots of the inflection point polynomial for a rational cubic bezier curve.
///
/// It also returns the discriminant which can be used for classification of the curve.
/// In case there is a loop (discriminant < 0.0) and the `loop_self_intersection` flag is [true],
/// two different roots will be located at the self intersection point.
pub fn rational_inflection_points(ippc: &[f32; 4], loop_self_intersection: bool) -> (f32, [Root; 3]) {
    if ippc[0].abs() <= ERROR_MARGIN {
        return integral_inflection_points(ippc, loop_self_intersection);
    }
    let (discriminant, roots, real_root) = solve_cubic([ippc[3] * -1.0, ippc[2] * 3.0, ippc[1] * -3.0, ippc[0]]);
    let mut roots = [roots[0], roots[1], roots[2]];
    if !loop_self_intersection {
        return (discriminant, roots);
    }
    let (discriminant, hessian_roots) = solve_quadratic([
        ippc[1] * ippc[3] - ippc[2] * ippc[2],
        ippc[1] * ippc[2] - ippc[0] * ippc[3],
        ippc[0] * ippc[2] - ippc[1] * ippc[1],
    ]);
    if discriminant > 0.0 {
        roots[2] = roots[real_root];
        match hessian_roots.len() {
            2 => roots[0..2].clone_from_slice(hessian_roots.as_slice()),
            1 => {
                roots[0] = hessian_roots[0];
                roots[1] = Root::new(1.0, 0.0, 0.0);
            }
            _ => {}
        }
    }
    (-discriminant, roots)
}

macro_rules! interpolate_normal {
    ($start_tangent:expr, $end_tangent:expr, $angle_step:expr, $normal:ident, $solutions:expr $(,)?) => {{
        let polar_start = epga1d::ComplexNumber::new($start_tangent.g0[1], $start_tangent.g0[2]);
        let polar_end = epga1d::ComplexNumber::new($end_tangent.g0[1], $end_tangent.g0[2]);
        let polar_range = polar_end / polar_start;
        let steps = ((polar_range.arg() / $angle_step).abs() + 0.5) as usize;
        let polar_step = polar_range.powf(1.0 / steps as f32);
        (1..steps)
            .map(|i| {
                let interpolated = polar_start * polar_step.powi(i as isize);
                let $normal = ppga2d::Plane {
                    g0: [0.0, interpolated.real(), interpolated.imaginary()].into(),
                };
                for solution in $solutions {
                    if solution.denominator == 0.0 {
                        continue;
                    }
                    let parameter: f32 = solution.numerator_real / solution.denominator;
                    if (0.0..=1.0).contains(&parameter) {
                        return parameter;
                    }
                }
                0.0
            })
            .collect::<Vec<f32>>()
    }};
}

macro_rules! cubic_uniform_tangent_angle {
    ($power_basis:expr, $angle_step:expr, $discriminant_and_roots:expr, $trimmed_power_basis:ident,
     $per_interval:expr, $normal:ident, $solutions:expr $(,)?) => {{
        let mut split_parameters = $discriminant_and_roots
            .1
            .iter()
            .filter(|root| root.denominator != 0.0)
            .map(|root| root.numerator_real / root.denominator)
            .filter(|parameter| (0.0..=1.0).contains(parameter))
            .collect::<Vec<f32>>();
        split_parameters.sort_by(|a, b| a.partial_cmp(b).unwrap_or_else(|| unreachable!()));
        {
            let mut i = 1;
            while i < split_parameters.len() {
                if split_parameters[i] - split_parameters[i - 1] < ERROR_MARGIN {
                    split_parameters.remove(i);
                } else {
                    i += 1;
                }
            }
        }
        let mut previous_split = 0.0;
        let mut intervals = Vec::with_capacity(split_parameters.len() + 1);
        for split_parameter in &split_parameters {
            if $discriminant_and_roots.0.abs() < ERROR_MARGIN {
                intervals.push((previous_split, *split_parameter - f32::EPSILON));
                previous_split = *split_parameter + f32::EPSILON;
            } else {
                intervals.push((previous_split, *split_parameter));
                previous_split = *split_parameter;
            }
        }
        intervals.push((previous_split, 1.0));
        let mut parameters = Vec::new();
        for (a, b) in intervals.iter() {
            let $trimmed_power_basis = reparametrize_rational_cubic($power_basis, *a, *b);
            let start_tangent = rational_cubic_first_order_derivative($power_basis, *a).signum();
            let end_tangent = rational_cubic_first_order_derivative($power_basis, *b).signum();
            $per_interval
            let mut interval_parameters = interpolate_normal!(start_tangent, end_tangent, $angle_step, $normal, $solutions)
                .iter()
                .map(|t| *a + (*b - *a) * t)
                .collect::<Vec<f32>>();
            interval_parameters.sort_by(|a, b| a.partial_cmp(b).unwrap_or_else(|| unreachable!()));
            parameters.append(&mut interval_parameters);
            parameters.push(*b);
        }
        parameters
    }};
}

/// Returns parameters of an integral quadratic bezier curve, distributed so that they have uniform tangent angles of the given angle step size
pub fn integral_quadratic_uniform_tangent_angle(
    power_basis: &[ppga2d::Point; 3],
    start_tangent: ppga2d::Plane,
    end_tangent: ppga2d::Plane,
    angle_step: f32,
) -> Vec<f32> {
    let planes = [power_basis[1].dual(), power_basis[2].dual() * ppga2d::Scalar { g0: 2.0 }];
    let mut parameters = interpolate_normal!(
        start_tangent,
        end_tangent,
        angle_step,
        normal,
        solve_linear([normal.inner_product(planes[0]).g0, normal.inner_product(planes[1]).g0]).1,
    );
    parameters.push(1.0);
    parameters
}

/// Returns parameters of an integral cubic bezier curve, distributed so that they have uniform tangent angles of the given angle step size
pub fn integral_cubic_uniform_tangent_angle(power_basis: &[ppga2d::Point; 4], angle_step: f32) -> Vec<f32> {
    let ippc = inflection_point_polynomial_coefficients(&power_basis, true);
    let discriminant_and_roots = integral_inflection_points(&ippc, false);
    let mut planes;
    cubic_uniform_tangent_angle!(
        power_basis,
        angle_step,
        discriminant_and_roots,
        trimmed_power_basis,
        {
            planes = [
                trimmed_power_basis[1].dual(),
                trimmed_power_basis[2].dual() * ppga2d::Scalar { g0: 2.0 },
                trimmed_power_basis[3].dual() * ppga2d::Scalar { g0: 3.0 },
            ];
        },
        normal,
        solve_quadratic([
            normal.inner_product(planes[0]).g0,
            normal.inner_product(planes[1]).g0,
            normal.inner_product(planes[2]).g0,
        ])
        .1
    )
}

/// Returns parameters of an rational quadratic bezier curve, distributed so that they have uniform tangent angles of the given angle step size
pub fn rational_quadratic_uniform_tangent_angle(
    power_basis: &[ppga2d::Point; 3],
    start_tangent: ppga2d::Plane,
    end_tangent: ppga2d::Plane,
    angle_step: f32,
) -> Vec<f32> {
    let planes = [
        power_basis[1].regressive_product(power_basis[0]),
        power_basis[2].regressive_product(power_basis[0]) * ppga2d::Scalar { g0: 2.0 },
        power_basis[2].regressive_product(power_basis[1]),
    ];
    let mut parameters = interpolate_normal!(start_tangent, end_tangent, angle_step, normal, {
        let normal = rotate_90_degree_clockwise(normal);
        solve_quadratic([
            normal.inner_product(planes[0]).g0,
            normal.inner_product(planes[1]).g0,
            normal.inner_product(planes[2]).g0,
        ])
        .1
    });
    parameters.push(1.0);
    parameters
}

/// Returns parameters of an rational cubic bezier curve, distributed so that they have uniform tangent angles of the given angle step size
pub fn rational_cubic_uniform_tangent_angle(power_basis: &[ppga2d::Point; 4], angle_step: f32) -> Vec<f32> {
    let ippc = inflection_point_polynomial_coefficients(&power_basis, false);
    let discriminant_and_roots = rational_inflection_points(&ippc, false);
    let mut planes;
    cubic_uniform_tangent_angle!(
        power_basis,
        angle_step,
        discriminant_and_roots,
        trimmed_power_basis,
        {
            planes = [
                trimmed_power_basis[1].regressive_product(trimmed_power_basis[0]),
                trimmed_power_basis[2].regressive_product(trimmed_power_basis[0]) * ppga2d::Scalar { g0: 2.0 },
                trimmed_power_basis[2].regressive_product(trimmed_power_basis[1])
                    + trimmed_power_basis[3].regressive_product(trimmed_power_basis[0]) * ppga2d::Scalar { g0: 3.0 },
                trimmed_power_basis[3].regressive_product(trimmed_power_basis[1]) * ppga2d::Scalar { g0: 2.0 },
                trimmed_power_basis[3].regressive_product(trimmed_power_basis[2]),
            ];
        },
        normal,
        {
            let normal = rotate_90_degree_clockwise(normal);
            solve_quartic([
                normal.inner_product(planes[0]).g0,
                normal.inner_product(planes[1]).g0,
                normal.inner_product(planes[2]).g0,
                normal.inner_product(planes[3]).g0,
                normal.inner_product(planes[4]).g0,
            ])
            .1
        }
    )
}
