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

/// Rotates a 2D vector 90° clockwise.
pub fn rotate_90_degree_clockwise(v: glam::Vec2) -> glam::Vec2 {
    glam::vec2(v[1], -v[0])
}

/// Returns the intersection point of two 2D lines.
pub fn line_line_intersection(origin_a: glam::Vec2, direction_a: glam::Vec2, origin_b: glam::Vec2, direction_b: glam::Vec2) -> glam::Vec2 {
    let param_a =
        glam::Mat2::from_cols(origin_b - origin_a, direction_b).determinant() / glam::Mat2::from_cols(direction_a, direction_b).determinant();
    origin_a + direction_a * param_a
}

/// Returns double the area of a triangle defined by the three given points.
pub fn signed_triangle_area(t: &[glam::Vec2]) -> f32 {
    glam::Mat2::from_cols(t[0] - t[2], t[1] - t[2]).determinant()
}
