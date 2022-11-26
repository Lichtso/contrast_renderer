//! Miscellaneous utility and helper functions

use geometric_algebra::{ppga2d, ppga3d, GeometricQuotient, OuterProduct, RegressiveProduct, Scale, Transformation, Zero};
use std::convert::TryInto;

/// Like `vec!` but for `HashSet`
#[macro_export]
macro_rules! hash_set(
    [ $($key:expr),*$(,)? ] => {
        {
            #[allow(unused_mut)]
            let mut set = ::std::collections::HashSet::new();
            $(set.insert($key);)*
            set
        }
    };
);

/// Like `vec!` but for `HashMap`
#[macro_export]
macro_rules! hash_map(
    { $($key:expr => $value:expr),*$(,)? } => {
        {
            #[allow(unused_mut)]
            let mut map = ::std::collections::HashMap::new();
            $(map.insert($key, $value);)*
            map
        }
    };
);

/// Like `matches!` but returns an option of the matched value
#[macro_export]
macro_rules! match_option {
    ($value:expr, $value_kind:path) => {
        match $value {
            $value_kind(value) => Some(value),
            _ => None,
        }
    };
}

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
    p.scale(1.0 / p[0])
}

/// Converts a axis aligned bounding box into 4 vertices.
pub fn aabb_to_convex_polygon(bounding_box: &[f32; 4]) -> [ppga2d::Point; 4] {
    [
        ppga2d::Point::new(1.0, bounding_box[0], bounding_box[1]),
        ppga2d::Point::new(1.0, bounding_box[0], bounding_box[3]),
        ppga2d::Point::new(1.0, bounding_box[2], bounding_box[3]),
        ppga2d::Point::new(1.0, bounding_box[2], bounding_box[1]),
    ]
}

/// Implements the separating axis theorem.
///
/// Expects the vertices to be ordered clockwise.
pub fn do_convex_polygons_overlap(a: &[ppga2d::Point], b: &[ppga2d::Point]) -> bool {
    for (a, b) in [(a, b), (b, a)] {
        'outer: for index in 0..a.len() {
            let plane = a[(index + 1) % a.len()].regressive_product(a[index]);
            for point in b {
                if point.regressive_product(plane)[0] <= 0.0 {
                    continue 'outer;
                }
            }
            return false;
        }
    }
    true
}

/// Rotates a [ppga2d::Plane] 90° clockwise.
pub fn rotate_90_degree_clockwise(v: ppga2d::Plane) -> ppga2d::Plane {
    ppga2d::Plane::new(0.0, v[2], -v[1])
}

/// Projects a [ppga2d::Point].
pub fn point_to_vec(p: ppga2d::Point) -> [f32; 2] {
    [p[1] / p[0], p[2] / p[0]]
}

/// Creates an unweighted [ppga2d::Point].
pub fn vec_to_point(v: [f32; 2]) -> ppga2d::Point {
    ppga2d::Point::new(1.0, v[0], v[1])
}

/// Creates a weighted [ppga2d::Point].
pub fn weighted_vec_to_point(w: f32, v: [f32; 2]) -> ppga2d::Point {
    ppga2d::Point::new(w, v[0] * w, v[1] * w)
}

/// Creates a [ppga2d::Motor] from an angle in radians.
pub fn rotate2d(mut angle: f32) -> ppga2d::Motor {
    angle *= 0.5;
    ppga2d::Motor::new(angle.cos(), angle.sin(), 0.0, 0.0)
}

/// Creates a [ppga2d::Motor] from a vector.
pub fn translate2d(v: [f32; 2]) -> ppga2d::Motor {
    ppga2d::Motor::new(1.0, 0.0, -0.5 * v[1], 0.5 * v[0])
}

/// Returns the rotation angle in radians of the given [ppga2d::Motor].
pub fn rotation2d(motor: ppga2d::Motor) -> f32 {
    2.0 * motor[1].atan2(motor[0])
}

/// Returns the translation of the given [ppga2d::Motor].
pub fn translation2d(mut motor: ppga2d::Motor) -> [f32; 2] {
    motor = motor.geometric_quotient(ppga2d::Rotor::new(motor[0], motor[1]));
    [2.0 * motor[3], -2.0 * motor[2]]
}

/// Creates a [ppga3d::Rotor] which represents a rotation by `angle` radians around `axis`.
pub fn rotate_around_axis(angle: f32, axis: &[f32; 3]) -> ppga3d::Rotor {
    let sinus = (angle * 0.5).sin();
    ppga3d::Rotor::new((angle * 0.5).cos(), axis[0] * sinus, axis[1] * sinus, axis[2] * sinus)
}

/// Converts a [ppga2d::Motor] to a [ppga3d::Motor].
pub fn motor2d_to_motor3d(motor: &ppga2d::Motor) -> ppga3d::Motor {
    ppga3d::Motor::new(motor[0], 0.0, 0.0, motor[1], 0.0, -motor[3], motor[2], 0.0)
}

/// Converts a [ppga2d::Motor] to a 3x3 matrix for WebGPU.
pub fn motor2d_to_mat3(motor: &ppga2d::Motor) -> [ppga2d::Point; 3] {
    let result = [1, 2, 0]
        .iter()
        .map(|index| {
            let mut point = ppga2d::Point::zero();
            point[*index] = 1.0;
            let row = motor.transformation(point);
            ppga2d::Point::new(row[1], row[2], row[0])
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
            point[*index] = 1.0;
            let row = motor.transformation(point);
            ppga3d::Point::new(row[1], row[2], row[3], row[0])
        })
        .collect::<Vec<_>>();
    result.try_into().unwrap()
}

/// Creates a 4x4 perspective projection matrix for GLSL.
pub fn perspective_projection(field_of_view_y: f32, aspect_ratio: f32, near: f32, far: f32) -> [ppga3d::Point; 4] {
    let height = 1.0 / (field_of_view_y * 0.5).tan();
    let denominator = 1.0 / (near - far);
    [
        ppga3d::Point::new(height / aspect_ratio, 0.0, 0.0, 0.0),
        ppga3d::Point::new(0.0, height, 0.0, 0.0),
        ppga3d::Point::new(0.0, 0.0, -far * denominator, 1.0),
        ppga3d::Point::new(0.0, 0.0, near * far * denominator, 0.0),
    ]
}

/// Calculates the product of two 4x4 matrices for GLSL.
pub fn matrix_multiplication(a: &[ppga3d::Point; 4], b: &[ppga3d::Point; 4]) -> [ppga3d::Point; 4] {
    [
        a[0].scale(b[0][0]) + a[1].scale(b[0][1]) + a[2].scale(b[0][2]) + a[3].scale(b[0][3]),
        a[0].scale(b[1][0]) + a[1].scale(b[1][1]) + a[2].scale(b[1][2]) + a[3].scale(b[1][3]),
        a[0].scale(b[2][0]) + a[1].scale(b[2][1]) + a[2].scale(b[2][2]) + a[3].scale(b[2][3]),
        a[0].scale(b[3][0]) + a[1].scale(b[3][1]) + a[2].scale(b[3][2]) + a[3].scale(b[3][3]),
    ]
}

/// Converts from srgb color space to linear color space
pub fn srgb_to_linear(mut color: [f32; 4]) -> [f32; 4] {
    for channel in color.iter_mut().take(3) {
        *channel = if *channel > 0.04045 {
            ((*channel + 0.055) / 1.055).powf(2.4)
        } else {
            *channel / 12.92
        };
    }
    color
}

/// Converts from linear color space to srgb color space
pub fn linear_to_srgb(mut color: [f32; 4]) -> [f32; 4] {
    for channel in color.iter_mut().take(3) {
        *channel = if *channel > 0.0031308 {
            1.055 * (*channel).powf(1.0 / 2.4) - 0.055
        } else {
            12.92 * *channel
        };
    }
    color
}
