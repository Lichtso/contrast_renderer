//! Miscellaneous utility and helper functions

use geometric_algebra::{ppga2d, ppga3d, OuterProduct, Transformation, Zero};
use std::convert::TryInto;

/// Transmutes a vector.
pub fn transmute_vec<S, T>(mut vec: Vec<S>) -> Vec<T> {
    let ptr = vec.as_mut_ptr() as *mut T;
    let len = vec.len() * std::mem::size_of::<S>() / std::mem::size_of::<T>();
    let capacity = vec.capacity() * std::mem::size_of::<S>() / std::mem::size_of::<T>();
    std::mem::forget(vec);
    unsafe { Vec::from_raw_parts(ptr, len, capacity) }
}

/// Transmutes a slice.
pub fn transmute_slice<S, T>(slice: &[S]) -> &[T] {
    let ptr = slice.as_ptr() as *const T;
    let len = slice.len() * std::mem::size_of::<S>() / std::mem::size_of::<T>();
    unsafe { std::slice::from_raw_parts(ptr, len) }
}

/// Transmutes a mutable slice.
pub fn transmute_slice_mut<S, T>(slice: &mut [S]) -> &mut [T] {
    let ptr = slice.as_mut_ptr() as *mut T;
    let len = slice.len() * std::mem::size_of::<S>() / std::mem::size_of::<T>();
    unsafe { std::slice::from_raw_parts_mut(ptr, len) }
}

/// Returns the intersection point of two 2D lines (origin, direction).
pub fn line_line_intersection(a: ppga2d::Plane, b: ppga2d::Plane) -> ppga2d::Point {
    let p = a.outer_product(b);
    p / ppga2d::Scalar { g0: p.g0[0] }
}

/// Rotates a [ppga2d::Plane] 90° clockwise.
pub fn rotate_90_degree_clockwise(v: ppga2d::Plane) -> ppga2d::Plane {
    ppga2d::Plane {
        g0: [0.0, v.g0[2], -v.g0[1]].into(),
    }
}

/// Projects a [ppga2d::Point].
pub fn point_to_vec(p: ppga2d::Point) -> [f32; 2] {
    [p.g0[1] / p.g0[0], p.g0[2] / p.g0[0]]
}

/// Creates an unweighted [ppga2d::Point].
pub fn vec_to_point(v: [f32; 2]) -> ppga2d::Point {
    ppga2d::Point {
        g0: [1.0, v[0], v[1]].into(),
    }
}

/// Creates a weighted [ppga2d::Point].
pub fn weighted_vec_to_point(w: f32, v: [f32; 2]) -> ppga2d::Point {
    ppga2d::Point {
        g0: [w, v[0] * w, v[1] * w].into(),
    }
}

/// Creates a [ppga3d::Rotor] which represents a rotation by `angle` radians around `axis`.
pub fn rotate_around_axis(angle: f32, axis: &[f32; 3]) -> ppga3d::Rotor {
    let sinus = (angle * 0.5).sin();
    ppga3d::Rotor {
        g0: [(angle * 0.5).cos(), axis[0] * sinus, axis[1] * sinus, axis[2] * sinus].into(),
    }
}

/// Converts a [ppga2d::Motor] to a [ppga3d::Motor].
pub fn motor2d_to_motor3d(motor: &ppga2d::Motor) -> ppga3d::Motor {
    ppga3d::Motor {
        g0: [motor.g0[0], 0.0, 0.0, motor.g0[1]].into(),
        g1: [0.0, motor.g0[3], -motor.g0[2], 0.0].into(),
    }
}

/// Converts a [ppga2d::Motor] to a 3x3 matrix for WebGPU.
pub fn motor2d_to_mat3(motor: &ppga2d::Motor) -> [ppga2d::Point; 3] {
    let result = [1, 2, 0]
        .iter()
        .map(|index| {
            let mut point = ppga2d::Point::zero();
            point.g0[*index] = 1.0;
            let row = motor.transformation(point);
            ppga2d::Point {
                g0: [row.g0[1], row.g0[2], row.g0[0]].into(),
            }
        })
        .collect::<Vec<_>>();
    result.try_into().unwrap()
}

/// Converts a [ppga3d::Motor] to a 4x4 matrix for WebGPU.
pub fn motor3d_to_mat4(motor: &ppga3d::Motor) -> [ppga3d::Point; 4] {
    let result = [1, 2, 3, 0]
        .iter()
        .map(|index| {
            let mut point = ppga3d::Point::zero();
            point.g0[*index] = 1.0;
            let row = motor.transformation(point);
            ppga3d::Point {
                g0: [row.g0[1], row.g0[2], row.g0[3], row.g0[0]].into(),
            }
        })
        .collect::<Vec<_>>();
    result.try_into().unwrap()
}

/// Creates a 4x4 perspective projection matrix for GLSL.
pub fn perspective_projection(field_of_view_y: f32, aspect_ratio: f32, near: f32, far: f32) -> [ppga3d::Point; 4] {
    let height = 1.0 / (field_of_view_y * 0.5).tan();
    let denominator = 1.0 / (near - far);
    [
        ppga3d::Point {
            g0: [height / aspect_ratio, 0.0, 0.0, 0.0].into(),
        },
        ppga3d::Point {
            g0: [0.0, height, 0.0, 0.0].into(),
        },
        ppga3d::Point {
            g0: [0.0, 0.0, -far * denominator, 1.0].into(),
        },
        ppga3d::Point {
            g0: [0.0, 0.0, near * far * denominator, 0.0].into(),
        },
    ]
}

/// Calculates the product of two 4x4 matrices for GLSL.
pub fn matrix_multiplication(a: &[ppga3d::Point; 4], b: &[ppga3d::Point; 4]) -> [ppga3d::Point; 4] {
    use ppga3d::Scalar;
    [
        Scalar { g0: b[0].g0[0] } * a[0] + Scalar { g0: b[0].g0[1] } * a[1] + Scalar { g0: b[0].g0[2] } * a[2] + Scalar { g0: b[0].g0[3] } * a[3],
        Scalar { g0: b[1].g0[0] } * a[0] + Scalar { g0: b[1].g0[1] } * a[1] + Scalar { g0: b[1].g0[2] } * a[2] + Scalar { g0: b[1].g0[3] } * a[3],
        Scalar { g0: b[2].g0[0] } * a[0] + Scalar { g0: b[2].g0[1] } * a[1] + Scalar { g0: b[2].g0[2] } * a[2] + Scalar { g0: b[2].g0[3] } * a[3],
        Scalar { g0: b[3].g0[0] } * a[0] + Scalar { g0: b[3].g0[1] } * a[1] + Scalar { g0: b[3].g0[2] } * a[2] + Scalar { g0: b[3].g0[3] } * a[3],
    ]
}

/// Converts a 4x4 matrix to a 16 element array for GLSL.
pub fn transmute_matrix(matrix: [ppga3d::Point; 4]) -> [f32; 16] {
    unsafe { std::mem::transmute(matrix) }
}
