//! Miscellaneous utility and helper functions

use geometric_algebra::{ppga2d, OuterProduct};

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
