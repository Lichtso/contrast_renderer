use crate::{
    complex_number::ComplexNumber,
    error::ERROR_MARGIN,
    polynomial::{solve_cubic, solve_linear, solve_quadratic, solve_quartic, Root},
    utils::rotate_90_degree_clockwise,
};
use glam::{const_mat3, const_mat4};

const QUADRATIC_POWER_BASIS: glam::Mat3 = const_mat3!([1.0, 0.0, 0.0, -2.0, 2.0, 0.0, 1.0, -2.0, 1.0]);
const CUBIC_POWER_BASIS: glam::Mat4 = const_mat4!([1.0, 0.0, 0.0, 0.0, -3.0, 3.0, 0.0, 0.0, 3.0, -6.0, 3.0, 0.0, -1.0, 3.0, -3.0, 1.0]);

/// Transforms the given control points of a rational quadratic bezier curve into the power basis matrix form.
pub fn rational_quadratic_control_points_to_power_basis(control_points: &[glam::Vec3; 3]) -> glam::Mat3 {
    let control_points = control_points
        .iter()
        .map(|control_point| glam::vec3(control_point[0] * control_point[2], control_point[1] * control_point[2], control_point[2]))
        .collect::<Vec<_>>();
    glam::Mat3::from_cols(control_points[0], control_points[1], control_points[2]) * QUADRATIC_POWER_BASIS
}

/// Transforms the given control points of a rational cubic bezier curve into the power basis matrix form.
pub fn rational_cubic_control_points_to_power_basis(control_points: &[glam::Vec3; 4]) -> glam::Mat4 {
    let control_points = control_points
        .iter()
        .map(|control_point| {
            glam::vec4(
                control_point[0] * control_point[2],
                control_point[1] * control_point[2],
                control_point[2],
                0.0,
            )
        })
        .collect::<Vec<_>>();
    glam::Mat4::from_cols(control_points[0], control_points[1], control_points[2], control_points[3]) * CUBIC_POWER_BASIS
}

/// Reparametrizes a rational quadratic bezier curve described by its power basis matrix form to a new parameter interval between `a` and `b`.
pub fn reparametrize_rational_quadratic_power_basis(power_basis: &glam::Mat3, a: f32, b: f32) -> glam::Mat3 {
    glam::Mat3::from_cols(
        power_basis.x_axis + a * power_basis.y_axis + a.powi(2) * power_basis.z_axis,
        (b - a) * power_basis.y_axis + (-2.0 * a.powi(2) + 2.0 * a * b) * power_basis.z_axis,
        (a - b).powi(2) * power_basis.z_axis,
    )
}

/// Reparametrizes a rational cubic bezier curve described by its power basis matrix form to a new parameter interval between `a` and `b`.
pub fn reparametrize_rational_cubic_power_basis(power_basis: &glam::Mat4, a: f32, b: f32) -> glam::Mat4 {
    glam::Mat4::from_cols(
        power_basis.x_axis + a * power_basis.y_axis + a.powi(2) * power_basis.z_axis + a.powi(3) * power_basis.w_axis,
        (-a + b) * power_basis.y_axis
            + (-2.0 * a.powi(2) + 2.0 * a * b) * power_basis.z_axis
            + (3.0 * a.powi(2) * b - 3.0 * a.powi(3)) * power_basis.w_axis,
        (a - b).powi(2) * power_basis.z_axis + (-6.0 * a.powi(2) * b + 3.0 * a * b.powi(2) + 3.0 * a.powi(3)) * power_basis.w_axis,
        (3.0 * a.powi(2) * b - 3.0 * a * b.powi(2) - a.powi(3) + b.powi(3)) * power_basis.w_axis,
    )
}

/// Calculates the point at parameter t of a rational quadratic bezier curve described by its power basis matrix form.
pub fn rational_quadratic_point(power_basis: &glam::Mat3, t: f32) -> glam::Vec2 {
    let p = *power_basis * glam::vec3(1.0, t, t * t);
    p.truncate() / p[2]
}

/// Calculates the first order derivative at parameter t of a rational quadratic bezier curve described by its power basis matrix form.
pub fn rational_quadratic_first_order_derivative(power_basis: &glam::Mat3, t: f32) -> glam::Vec2 {
    let p = *power_basis * glam::vec3(1.0, t, t * t);
    let d1 = *power_basis * glam::vec3(0.0, 1.0, 2.0 * t);
    (d1.truncate() * p[2] - p.truncate() * d1[2]) / (p[2] * p[2])
}

/// Calculates the second order derivative at parameter t of a rational quadratic bezier curve described by its power basis matrix form.
pub fn rational_quadratic_second_order_derivative(power_basis: &glam::Mat3, t: f32) -> glam::Vec2 {
    let p = *power_basis * glam::vec3(1.0, t, t * t);
    let d2 = *power_basis * glam::vec3(0.0, 0.0, 2.0);
    (d2.truncate() * p[2] - p.truncate() * d2[2]) / (p[2] * p[2])
}

/// Calculates the point at parameter t of a rational cubic bezier curve described by its power basis matrix form.
pub fn rational_cubic_point(power_basis: &glam::Mat4, t: f32) -> glam::Vec2 {
    let p = *power_basis * glam::vec4(1.0, t, t * t, t * t * t);
    p.truncate().truncate() / p[2]
}

/// Calculates the first order derivative at parameter t of a rational cubic bezier curve described by its power basis matrix form.
pub fn rational_cubic_first_order_derivative(power_basis: &glam::Mat4, t: f32) -> glam::Vec2 {
    let p = *power_basis * glam::vec4(1.0, t, t * t, t * t * t);
    let d1 = *power_basis * glam::vec4(0.0, 1.0, 2.0 * t, 3.0 * t * t);
    (d1.truncate().truncate() * p[2] - p.truncate().truncate() * d1[2]) / (p[2] * p[2])
}

/// Calculates the second order derivative at parameter t of a rational cubic bezier curve described by its power basis matrix form.
pub fn rational_cubic_second_order_derivative(power_basis: &glam::Mat4, t: f32) -> glam::Vec2 {
    let p = *power_basis * glam::vec4(1.0, t, t * t, t * t * t);
    let d2 = *power_basis * glam::vec4(0.0, 0.0, 2.0, 6.0 * t);
    (d2.truncate().truncate() * p[2] - p.truncate().truncate() * d2[2]) / (p[2] * p[2])
}

/// Calculates the third order derivative at parameter t of a rational cubic bezier curve described by its power basis matrix form.
pub fn rational_cubic_third_order_derivative(power_basis: &glam::Mat4, t: f32) -> glam::Vec2 {
    let p = *power_basis * glam::vec4(1.0, t, t * t, t * t * t);
    let d2 = *power_basis * glam::vec4(0.0, 0.0, 2.0, 6.0 * t);
    let d3 = *power_basis * glam::vec4(0.0, 0.0, 0.0, 6.0);
    (d3.truncate().truncate() * p[2] - p.truncate().truncate() * d3[2]) / (p[2] * p[2] * p[2])
        + (d2.truncate().truncate() * p[2] - p.truncate().truncate() * d2[2]) / (p[2] * p[2])
}

/// Calculates the coefficients of the inflection point polynomial for an integral cubic bezier curve.
pub fn integral_inflection_point_polynomial_coefficients(power_basis: &glam::Mat4) -> glam::Vec4 {
    glam::vec4(
        0.0,
        -glam::mat3(
            power_basis.w_axis.truncate(),
            power_basis.z_axis.truncate(),
            power_basis.x_axis.truncate(),
        )
        .determinant(),
        glam::mat3(
            power_basis.w_axis.truncate(),
            power_basis.y_axis.truncate(),
            power_basis.x_axis.truncate(),
        )
        .determinant(),
        -glam::mat3(
            power_basis.z_axis.truncate(),
            power_basis.y_axis.truncate(),
            power_basis.x_axis.truncate(),
        )
        .determinant(),
    )
    .normalize()
}

/// Calculates the coefficients of the inflection point polynomial for a rational cubic bezier curve.
pub fn rational_inflection_point_polynomial_coefficients(power_basis: &glam::Mat4) -> glam::Vec4 {
    glam::vec4(
        glam::mat3(
            power_basis.w_axis.truncate(),
            power_basis.z_axis.truncate(),
            power_basis.y_axis.truncate(),
        )
        .determinant(),
        -glam::mat3(
            power_basis.w_axis.truncate(),
            power_basis.z_axis.truncate(),
            power_basis.x_axis.truncate(),
        )
        .determinant(),
        glam::mat3(
            power_basis.w_axis.truncate(),
            power_basis.y_axis.truncate(),
            power_basis.x_axis.truncate(),
        )
        .determinant(),
        -glam::mat3(
            power_basis.z_axis.truncate(),
            power_basis.y_axis.truncate(),
            power_basis.x_axis.truncate(),
        )
        .determinant(),
    )
    .normalize()
    /*glam::Vec4::new(
        power_basis.w_axis.truncate().dot(power_basis.z_axis.truncate().cross(power_basis.y_axis.truncate())),
        -power_basis.w_axis.truncate().dot(power_basis.z_axis.truncate().cross(power_basis.x_axis.truncate())),
        power_basis.w_axis.truncate().dot(power_basis.y_axis.truncate().cross(power_basis.x_axis.truncate())),
        -power_basis.z_axis.truncate().dot(power_basis.y_axis.truncate().cross(power_basis.x_axis.truncate())),
    )
    .normalize()*/
}

/// Finds the roots of the inflection point polynomial for an integral cubic bezier curve.
///
/// It also returns the discriminant which can be used for classification of the curve.
/// In case there is a loop (discriminant < 0.0) and the `loop_self_intersection` flag is `true`,
/// two different roots will be located at the self intersection point.
pub fn integral_inflection_points(ippc: glam::Vec4, loop_self_intersection: bool) -> (f32, [Root; 3]) {
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
/// In case there is a loop (discriminant < 0.0) and the `loop_self_intersection` flag is `true`,
/// two different roots will be located at the self intersection point.
pub fn rational_inflection_points(ippc: glam::Vec4, loop_self_intersection: bool) -> (f32, [Root; 3]) {
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
        let polar_start: ComplexNumber<f32> = rotate_90_degree_clockwise($start_tangent).into();
        let polar_end: ComplexNumber<f32> = rotate_90_degree_clockwise($end_tangent).into();
        let polar_range = polar_end / polar_start;
        let steps = ((polar_range.arg() / $angle_step).abs() + 0.5) as usize;
        let polar_step = polar_range.powf(1.0 / steps as f32);
        (1..steps)
            .map(|i| {
                let $normal: glam::Vec2 = (polar_start * polar_step.powi(i as isize)).into();
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
        let mut split_parameters = $discriminant_and_roots.1
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
        for (i, (a, b)) in intervals.iter().enumerate() {
            let $trimmed_power_basis = reparametrize_rational_cubic_power_basis($power_basis, *a, *b);
            let start_tangent = (rational_cubic_first_order_derivative($power_basis, *a)).normalize();
            let end_tangent = (rational_cubic_first_order_derivative($power_basis, *b)).normalize();
            $per_interval
            if i > 0 {
                parameters.push(*a);
            }
            let mut interval_parameters = interpolate_normal!(
                start_tangent,
                end_tangent,
                $angle_step,
                $normal,
                $solutions,
            )
                .iter()
                .map(|t| *a + (*b - *a) * t)
                .collect::<Vec<f32>>();
            interval_parameters.sort_by(|a, b| a.partial_cmp(b).unwrap_or_else(|| unreachable!()));
            parameters.append(&mut interval_parameters);
        }
        parameters
    }};
}

/// Returns parameters of an integral quadratic bezier curve, distributed so that they have uniform tangent angles of the given angle step size
pub fn integral_quadratic_uniform_tangent_angle(
    power_basis: &glam::Mat3,
    start_tangent: glam::Vec2,
    end_tangent: glam::Vec2,
    angle_step: f32,
) -> Vec<f32> {
    let vec_p1 = power_basis.y_axis.truncate();
    let vec_p2 = power_basis.z_axis.truncate() * 2.0;
    interpolate_normal!(
        start_tangent,
        end_tangent,
        angle_step,
        normal,
        solve_linear([vec_p1.dot(normal), vec_p2.dot(normal)]).1,
    )
}

/// Returns parameters of an integral cubic bezier curve, distributed so that they have uniform tangent angles of the given angle step size
pub fn integral_cubic_uniform_tangent_angle(power_basis: &glam::Mat4, angle_step: f32) -> Vec<f32> {
    let ippc = integral_inflection_point_polynomial_coefficients(&power_basis);
    let discriminant_and_roots = integral_inflection_points(ippc, false);
    let mut vec_p1;
    let mut vec_p2;
    let mut vec_p3;
    cubic_uniform_tangent_angle!(
        power_basis,
        angle_step,
        discriminant_and_roots,
        trimmed_power_basis,
        {
            vec_p1 = trimmed_power_basis.y_axis.truncate().truncate();
            vec_p2 = trimmed_power_basis.z_axis.truncate().truncate() * 2.0;
            vec_p3 = trimmed_power_basis.w_axis.truncate().truncate() * 3.0;
        },
        normal,
        solve_quadratic([vec_p1.dot(normal), vec_p2.dot(normal), vec_p3.dot(normal)]).1
    )
}

/// Returns parameters of an rational quadratic bezier curve, distributed so that they have uniform tangent angles of the given angle step size
pub fn rational_quadratic_uniform_tangent_angle(
    power_basis: &glam::Mat3,
    start_tangent: glam::Vec2,
    end_tangent: glam::Vec2,
    angle_step: f32,
) -> Vec<f32> {
    let vec_p0 = power_basis.x_axis;
    let vec_p1 = power_basis.y_axis;
    let vec_p2 = power_basis.z_axis;
    interpolate_normal!(start_tangent, end_tangent, angle_step, normal, {
        let vec_a0 = glam::vec2(vec_p0.truncate().dot(normal), vec_p0[2]);
        let vec_a1 = glam::vec2(vec_p1.truncate().dot(normal), vec_p1[2]);
        let vec_a2 = glam::vec2(vec_p2.truncate().dot(normal), vec_p2[2]);
        solve_quadratic([
            glam::Mat2::from_cols(vec_a1, vec_a0).determinant(),
            2.0 * glam::Mat2::from_cols(vec_a2, vec_a0).determinant(),
            glam::Mat2::from_cols(vec_a2, vec_a1).determinant(),
        ])
        .1
    },)
}

/// Returns parameters of an rational cubic bezier curve, distributed so that they have uniform tangent angles of the given angle step size
pub fn rational_cubic_uniform_tangent_angle(power_basis: &glam::Mat4, angle_step: f32) -> Vec<f32> {
    let ippc = rational_inflection_point_polynomial_coefficients(&power_basis);
    let discriminant_and_roots = rational_inflection_points(ippc, false);
    cubic_uniform_tangent_angle!(power_basis, angle_step, discriminant_and_roots, trimmed_power_basis, {}, normal, {
        let vec_a0 = glam::vec2(
            trimmed_power_basis.x_axis.truncate().truncate().dot(normal),
            trimmed_power_basis.x_axis[2],
        );
        let vec_a1 = glam::vec2(
            trimmed_power_basis.y_axis.truncate().truncate().dot(normal),
            trimmed_power_basis.y_axis[2],
        );
        let vec_a2 = glam::vec2(
            trimmed_power_basis.z_axis.truncate().truncate().dot(normal),
            trimmed_power_basis.z_axis[2],
        );
        let vec_a3 = glam::vec2(
            trimmed_power_basis.w_axis.truncate().truncate().dot(normal),
            trimmed_power_basis.w_axis[2],
        );
        solve_quartic([
            glam::Mat2::from_cols(vec_a1, vec_a0).determinant(),
            2.0 * glam::Mat2::from_cols(vec_a2, vec_a0).determinant(),
            glam::Mat2::from_cols(vec_a2, vec_a1).determinant() + 3.0 * glam::Mat2::from_cols(vec_a3, vec_a0).determinant(),
            2.0 * glam::Mat2::from_cols(vec_a3, vec_a1).determinant(),
            glam::Mat2::from_cols(vec_a3, vec_a2).determinant(),
        ])
        .1
    })
}
