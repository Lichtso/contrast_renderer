pub type Vertex0 = [f32; 2];

#[allow(dead_code)]
#[derive(Clone, Copy)]
#[repr(packed)]
pub struct Vertex2f(pub [f32; 2], pub [f32; 2]);

#[allow(dead_code)]
#[derive(Clone, Copy)]
#[repr(packed)]
pub struct Vertex2f1i(pub [f32; 2], pub [f32; 2], pub u32);

#[allow(dead_code)]
#[derive(Clone, Copy)]
#[repr(packed)]
pub struct Vertex3f(pub [f32; 2], pub [f32; 3]);

#[allow(dead_code)]
#[derive(Clone, Copy)]
#[repr(packed)]
pub struct Vertex3f1i(pub [f32; 2], pub [f32; 3], pub u32);

#[allow(dead_code)]
#[derive(Clone, Copy)]
#[repr(packed)]
pub struct Vertex4f(pub [f32; 2], pub [f32; 4]);

pub fn triangle_fan_to_strip<T: Copy>(vertices: Vec<T>) -> Vec<T> {
    let gather_indices = (0..vertices.len()).map(|i| if (i & 1) == 0 { i >> 1 } else { vertices.len() - 1 - (i >> 1) });
    let mut result = Vec::with_capacity(vertices.len());
    for src in gather_indices {
        result.push(vertices[src]);
    }
    result
}
