use crate::{
    error::ERROR_MARGIN,
    polynomial::{solve_cubic, solve_quadratic},
};
use glam::{const_mat3, const_mat4};
use std::convert::TryFrom;

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
/// It also returns the discreminant which can be used for classification of the curve.
pub fn integral_inflection_points(ippc: glam::Vec4) -> (f32, [glam::Vec2; 3]) {
    let discreminant = 3.0 * ippc[2] * ippc[2] - 4.0 * ippc[1] * ippc[3];
    if ippc[1].abs() <= ERROR_MARGIN {
        if ippc[2].abs() <= ERROR_MARGIN {
            (-1.0, [glam::vec2(-1.0, 1.0), glam::vec2(1.0, 0.0), glam::vec2(1.0, 0.0)])
        } else {
            (1.0, [glam::vec2(ippc[3], 3.0 * ippc[2]), glam::vec2(1.0, 0.0), glam::vec2(1.0, 0.0)])
        }
    } else {
        let d = (discreminant * if discreminant < 0.0 { -1.0f32 } else { 1.0f32 / 3.0f32 }).sqrt();
        (
            discreminant,
            [
                glam::vec2(ippc[2] + d, 2.0 * ippc[1]),
                glam::vec2(ippc[2] - d, 2.0 * ippc[1]),
                glam::vec2(1.0, 0.0),
            ],
        )
    }
}

/// Finds the roots of the inflection point polynomial for a rational cubic bezier curve.
///
/// It also returns the discreminant which can be used for classification of the curve.
pub fn rational_inflection_points(ippc: glam::Vec4) -> (f32, [glam::Vec2; 3]) {
    if ippc[0].abs() <= ERROR_MARGIN {
        return integral_inflection_points(ippc);
    }
    let (_discreminant, roots, real_root) = solve_cubic([ippc[3] * -1.0, ippc[2] * 3.0, ippc[1] * -3.0, ippc[0]]);
    let mut roots = <[glam::Vec2; 3]>::try_from(roots).unwrap();
    let (discreminant, hessian_roots) = solve_quadratic([
        ippc[1] * ippc[3] - ippc[2] * ippc[2],
        ippc[1] * ippc[2] - ippc[0] * ippc[3],
        ippc[0] * ippc[2] - ippc[1] * ippc[1],
    ]);
    if discreminant > 0.0 {
        roots[2] = roots[real_root];
        match hessian_roots.len() {
            2 => roots[0..2].clone_from_slice(hessian_roots.as_slice()),
            1 => {
                roots[0] = hessian_roots[0];
                roots[1] = glam::vec2(1.0, 0.0);
            }
            _ => {}
        }
    }
    (-discreminant, roots)
}
