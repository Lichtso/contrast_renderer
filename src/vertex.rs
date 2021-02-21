pub type Vertex0 = glam::Vec2;

#[derive(Clone, Copy)]
#[repr(packed)]
pub struct Vertex2(pub glam::Vec2, pub glam::Vec2);

#[derive(Clone, Copy)]
#[repr(packed)]
pub struct Vertex3(pub glam::Vec2, pub glam::Vec3);

#[derive(Clone, Copy)]
#[repr(packed)]
pub struct Vertex4(pub glam::Vec2, pub glam::Vec4);

pub fn triangle_fan_to_strip<T: Copy>(vertices: Vec<T>) -> Vec<T> {
    let gather_indices = (0..vertices.len()).map(|i| if (i & 1) == 0 { i >> 1 } else { vertices.len() - 1 - (i >> 1) });
    let mut result = Vec::with_capacity(vertices.len());
    for src in gather_indices {
        result.push(vertices[src]);
    }
    result
}
