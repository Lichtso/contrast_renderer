use crate::{
    complex_number::ComplexNumber,
    error::Error,
    path::{Path, SegmentType},
};
use glam::const_mat4;
use wgpu::{include_spirv, util::DeviceExt, vertex_attr_array};

const ERROR_MARGIN: f32 = 0.00001;

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

/*fn point_at(c: &glam::Mat4, t: f32) -> glam::Vec2 {
    let p = *c * glam::vec4(1.0, t, t * t, t * t * t);
    p.truncate().truncate() / p[2]
}*/

fn tangent_at(c: &glam::Mat4, t: f32) -> glam::Vec2 {
    let p = *c * glam::vec4(1.0, t, t * t, t * t * t);
    let d1 = *c * glam::vec4(0.0, 1.0, 2.0 * t, 3.0 * t * t);
    (d1.truncate().truncate() * p[2] - p.truncate().truncate() * d1[2]) / (p[2] * p[2])
}

/*fn second_order_derivative_at(c: &glam::Mat4, t: f32) -> glam::Vec2 {
    let p = *c * glam::vec4(1.0, t, t * t, t * t * t);
    let d2 = *c * glam::vec4(0.0, 0.0, 2.0, 6.0 * t);
    (d2.truncate().truncate() * p[2] - p.truncate().truncate() * d2[2]) / (p[2] * p[2])
}

fn third_order_derivative_at(c: &glam::Mat4, t: f32) -> glam::Vec2 {
    let p = *c * glam::vec4(1.0, t, t * t, t * t * t);
    let d2 = *c * glam::vec4(0.0, 0.0, 2.0, 6.0 * t);
    let d3 = *c * glam::vec4(0.0, 0.0, 0.0, 6.0);
    (d3.truncate().truncate() * p[2] - p.truncate().truncate() * d3[2]) / (p[2] * p[2] * p[2])
        + (d2.truncate().truncate() * p[2] - p.truncate().truncate() * d2[2]) / (p[2] * p[2])
}*/

const CUBIC_POWER_BASIS: glam::Mat4 = const_mat4!([1.0, 0.0, 0.0, 0.0, -3.0, 3.0, 0.0, 0.0, 3.0, -6.0, 3.0, 0.0, -1.0, 3.0, -3.0, 1.0]);

fn control_points_power_basis(control_points: &[glam::Vec3; 4]) -> glam::Mat4 {
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

fn integral_inflection_point_polynomial_coefficients(c: &glam::Mat4) -> glam::Vec4 {
    glam::vec4(
        0.0,
        -glam::mat3(c.w_axis.truncate(), c.z_axis.truncate(), c.x_axis.truncate()).determinant(),
        glam::mat3(c.w_axis.truncate(), c.y_axis.truncate(), c.x_axis.truncate()).determinant(),
        -glam::mat3(c.z_axis.truncate(), c.y_axis.truncate(), c.x_axis.truncate()).determinant(),
    )
    .normalize()
}

fn rational_inflection_point_polynomial_coefficients(c: &glam::Mat4) -> glam::Vec4 {
    glam::vec4(
        glam::mat3(c.w_axis.truncate(), c.z_axis.truncate(), c.y_axis.truncate()).determinant(),
        -glam::mat3(c.w_axis.truncate(), c.z_axis.truncate(), c.x_axis.truncate()).determinant(),
        glam::mat3(c.w_axis.truncate(), c.y_axis.truncate(), c.x_axis.truncate()).determinant(),
        -glam::mat3(c.z_axis.truncate(), c.y_axis.truncate(), c.x_axis.truncate()).determinant(),
    )
    .normalize()
    /*glam::Vec4::new(
        c.w_axis.truncate().dot(c.z_axis.truncate().cross(c.y_axis.truncate())),
        -c.w_axis.truncate().dot(c.z_axis.truncate().cross(c.x_axis.truncate())),
        c.w_axis.truncate().dot(c.y_axis.truncate().cross(c.x_axis.truncate())),
        -c.z_axis.truncate().dot(c.y_axis.truncate().cross(c.x_axis.truncate())),
    )
    .normalize()*/
}

fn integral_inflection_points(ippc: glam::Vec4) -> (f32, [glam::Vec2; 3]) {
    let discreminant = 3.0 * ippc[2] * ippc[2] - 4.0 * ippc[1] * ippc[3];
    let d = (discreminant * if discreminant < 0.0 { -1.0f32 } else { 1.0f32 / 3.0f32 }).sqrt();
    if ippc[1].abs() < ERROR_MARGIN {
        if ippc[2].abs() < ERROR_MARGIN {
            (-1.0, [glam::vec2(-1.0, 1.0), glam::vec2(1.0, 0.0), glam::vec2(1.0, 0.0)])
        } else {
            (1.0, [glam::vec2(ippc[3], 3.0 * ippc[2]), glam::vec2(1.0, 0.0), glam::vec2(1.0, 0.0)])
        }
    } else {
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

const ROOTS_OF_UNITY_3: [ComplexNumber<f32>; 3] = [
    // 0.8660254037844386467637231707529361834714026269051903140279034897
    // ComplexNumber::from_polar(1.0, -120.0/180.0*std::f32::consts::PI),
    // ComplexNumber::from_polar(1.0, 120.0/180.0*std::f32::consts::PI),
    // ComplexNumber::from_polar(1.0, 0.0),
    ComplexNumber {
        real: -0.5,
        imag: -0.8660254,
    },
    ComplexNumber { real: -0.5, imag: 0.8660254 },
    ComplexNumber { real: 1.0, imag: 0.0 },
];

fn rational_inflection_points(ippc: glam::Vec4) -> (f32, [glam::Vec2; 3]) {
    if ippc[0].abs() < ERROR_MARGIN {
        return integral_inflection_points(ippc);
    }
    let d = glam::vec2(
        9.0 * (ippc[1] * ippc[1] - ippc[0] * ippc[2]),
        27.0 * (-2.0 * ippc[1] * ippc[1] * ippc[1] + 3.0 * ippc[0] * ippc[1] * ippc[2] - ippc[0] * ippc[0] * ippc[3]),
    );
    let mut roots = [glam::Vec2::default(); 3];
    let mut c = ComplexNumber::from_sqrt_real(d[1] * d[1] - 4.0 * d[0] * d[0] * d[0]);
    c = ((c + if c.real + d[1] == 0.0 { -d[1] } else { d[1] }) * 0.5).exp_real(1.0 / 3.0);
    for i in 0..3 {
        let ci = c * ROOTS_OF_UNITY_3[i];
        let numerator = ci * (3.0 * ippc[1]) - ci * ci - d[0];
        let denominator = ci * (3.0 * ippc[0]);
        roots[i] = glam::vec2((numerator * denominator.conjugate()).real, denominator.squared_norm());
    }
    let hc = glam::vec3(
        ippc[0] * ippc[2] - ippc[1] * ippc[1],
        ippc[1] * ippc[2] - ippc[0] * ippc[3],
        ippc[1] * ippc[3] - ippc[2] * ippc[2],
    );
    let discreminant = 4.0 * hc[0] * hc[2] - hc[1] * hc[1];
    if discreminant < 0.0 {
        let real_root = ((std::f32::consts::PI - c.angle()) / (std::f32::consts::PI * 2.0 / 3.0)) as usize;
        roots[2] = roots[(real_root + 1) % 3];
        let d = (-discreminant).sqrt();
        roots[0] = glam::vec2(-hc[1] + d, 2.0 * hc[0]);
        roots[1] = glam::vec2(-hc[1] - d, 2.0 * hc[0]);
    }
    (discreminant, roots)
}

fn find_double_point_issue(discreminant: f32, roots: &[glam::Vec2; 3]) -> Option<f32> {
    if discreminant < 0.0 {
        let mut result = -1.0;
        let mut inside = 0;
        for root in roots {
            if root[1] != 0.0 {
                let parameter = root[0] / root[1];
                if 0.0 < parameter && parameter < 1.0 {
                    result = parameter;
                    inside += 1;
                }
            }
        }
        if inside == 1 {
            return Some(result);
        }
    }
    None
}

fn weight_derivatives(roots: &[glam::Vec2; 3]) -> glam::Vec4 {
    glam::vec4(
        roots[0].x * roots[1].x * roots[2].x,
        -roots[0].y * roots[1].x * roots[2].x - roots[0].x * roots[1].y * roots[2].x - roots[0].x * roots[1].x * roots[2].y,
        roots[0].x * roots[1].y * roots[2].y + roots[0].y * roots[1].x * roots[2].y + roots[0].y * roots[1].y * roots[2].x,
        -roots[0].y * roots[1].y * roots[2].y,
    )
}

/*
const QUADRATIC_CUBIC_CURVE_POWER_BASIS: glam::Mat4 = const_mat4!([0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]);
const QUADRATIC_CUBIC_CURVE: glam::Mat4 = const_mat4!([
    0.0,
    0.0,
    0.0,
    1.0,
    -1.0 / 3.0,
    0.0,
    1.0 / 3.0,
    1.0,
    -2.0 / 3.0,
    -1.0 / 3.0,
    2.0 / 3.0,
    1.0,
    -1.0,
    -1.0,
    1.0,
    1.0
]);*/

const CUBIC_POWER_BASIS_INVERSE: glam::Mat4 = const_mat4!([
    1.0,
    0.0,
    0.0,
    0.0,
    1.0,
    1.0 / 3.0,
    0.0,
    0.0,
    1.0,
    2.0 / 3.0,
    1.0 / 3.0,
    0.0,
    1.0,
    1.0,
    1.0,
    1.0,
]);

fn weights(discreminant: f32, roots: &[glam::Vec2; 3]) -> glam::Mat4 {
    if discreminant == 0.0 {
        glam::mat4(
            weight_derivatives(&[roots[0], roots[0], roots[2]]),
            weight_derivatives(&[roots[0], roots[0], roots[0]]),
            weight_derivatives(&[roots[0], roots[0], roots[0]]),
            weight_derivatives(&[roots[2], roots[2], roots[2]]),
        )
    } else if discreminant < 0.0 {
        glam::mat4(
            weight_derivatives(&[roots[0], roots[1], roots[2]]),
            weight_derivatives(&[roots[0], roots[0], roots[1]]),
            weight_derivatives(&[roots[1], roots[1], roots[0]]),
            weight_derivatives(&[roots[2], roots[2], roots[2]]),
        )
    } else {
        glam::mat4(
            weight_derivatives(&[roots[0], roots[1], roots[2]]),
            weight_derivatives(&[roots[0], roots[0], roots[0]]),
            weight_derivatives(&[roots[1], roots[1], roots[1]]),
            weight_derivatives(&[roots[2], roots[2], roots[2]]),
        )
    }
    .transpose()
        * CUBIC_POWER_BASIS_INVERSE
    // let phi = f * c.transpose() * (c * c.transpose()).inverse();
}

fn weight_planes(control_points: &[glam::Vec3; 4], weights: &glam::Mat4) -> [glam::Vec4; 4] {
    let mut planes = [glam::Vec4::default(); 4];
    for (i, plane) in planes.iter_mut().enumerate() {
        let point_a = glam::vec3(control_points[0][0], control_points[0][1], weights.x_axis[i]);
        let point_b = glam::vec3(control_points[1][0], control_points[1][1], weights.y_axis[i]);
        let point_c = glam::vec3(control_points[2][0], control_points[2][1], weights.z_axis[i]);
        let mut normal = (point_b - point_a).cross(point_c - point_a);
        if normal.length_squared() < ERROR_MARGIN {
            let point_d = glam::vec3(control_points[3][0], control_points[3][1], weights.z_axis[i]);
            normal = (point_b - point_a).cross(point_d - point_a);
        }
        normal = normal.normalize();
        if normal[2] < 0.0 {
            normal *= -1.0;
        }
        *plane = glam::vec4(normal[0], normal[1], normal[2], normal.dot(point_a));
    }
    planes
}

/*fn weights_at(planes: &[glam::Vec4; 4], at: glam::Vec2) -> glam::Vec4 {
    let mut weights = glam::Vec4::default();
    for i in 0..4 {
        weights[i] = (planes[i].truncate() * planes[i][3] - at.extend(0.0)).dot(planes[i].truncate()) / planes[i][2];
    }
    weights
}*/

fn implicit_curve_value(weights: glam::Vec4) -> f32 {
    weights[0] * weights[0] * weights[0] - weights[1] * weights[2] * weights[3]
}

fn implicit_curve_gradient(planes: &[glam::Vec4; 4], weights: glam::Vec4) -> glam::Vec2 {
    (3.0 * weights[0] * weights[0]) * planes[0].truncate().truncate() / planes[0].z
        - (weights[2] * weights[3]) * planes[1].truncate().truncate() / planes[1].z
        - (weights[1] * weights[3]) * planes[2].truncate().truncate() / planes[2].z
        - (weights[1] * weights[2]) * planes[3].truncate().truncate() / planes[3].z
}

fn normalize_implicit_curve_side(planes: &mut [glam::Vec4; 4], weights: &mut glam::Mat4, c: &glam::Mat4, gradient: glam::Vec2) {
    let tangent = tangent_at(c, 0.0);
    if glam::vec2(tangent[1], -tangent[0]).dot(gradient) < 0.0 {
        for plane in planes {
            *plane *= glam::vec4(-1.0, -1.0, 1.0, -1.0);
        }
        *weights = glam::Mat4::from_scale(glam::vec3(-1.0, -1.0, 1.0)) * *weights;
    }
}

macro_rules! emit_triangle {
    ($triangles:expr, $signed_triangle_areas:expr, $control_points:expr, $weights:expr, $v:ident, $w:ident, $emit_vertex:expr, $triangle_index:expr) => {
        let mut triangle = Vec::new();
        for vertex_index in (0..4).filter(|i| *i != $triangle_index) {
            let $v = $control_points[vertex_index].truncate().into();
            let $w = $weights[vertex_index];
            triangle.push($emit_vertex);
        }
        let signed_triangle_area = $signed_triangle_areas[$triangle_index];
        if signed_triangle_area.abs() > ERROR_MARGIN {
            if signed_triangle_area < 0.0 {
                triangle.reverse();
            }
            $triangles.append(&mut triangle);
        }
    };
}

macro_rules! triangulate_quadrilateral {
    ($anchors:expr, $cubic_curve_triangles:expr,
     $control_points:expr, $weights:expr, $v:ident, $w:ident, $emit_vertex:expr) => {{
        for (weights, control_point) in $weights.iter_mut().zip($control_points.iter()) {
            *weights /= control_point[2];
        }
        let mut triangles = Vec::new();
        let signed_triangle_areas: Vec<f32> = (0..4)
            .map(|i| {
                let control_points: Vec<_> = (0..4).filter(|j| i != *j).map(|j| $control_points[j].truncate()).collect();
                crate::utils::signed_triangle_area(&control_points[0..3])
            })
            .collect();
        let triangle_area_sum =
            signed_triangle_areas[0].abs() + signed_triangle_areas[1].abs() + signed_triangle_areas[2].abs() + signed_triangle_areas[3].abs();
        let mut enclosing_triangle = None;
        for (triangle_index, signed_triangle_area) in signed_triangle_areas.iter().enumerate() {
            let equilibrium = 0.5 * triangle_area_sum;
            if (equilibrium - signed_triangle_area.abs()).abs() < ERROR_MARGIN {
                assert!(enclosing_triangle.is_none());
                enclosing_triangle = Some(triangle_index);
            }
        }
        if let Some(enclosing_triangle) = enclosing_triangle {
            emit_triangle!(
                triangles,
                signed_triangle_areas,
                $control_points,
                $weights,
                $v,
                $w,
                $emit_vertex,
                enclosing_triangle
            );
        } else {
            let mut opposite_triangle = 0;
            for j in 1..4 {
                /*let mut i = (1..4).filter(|i| *i != j);
                let point_b = $control_points[i.next().unwrap()].truncate();
                let point_c = $control_points[i.next().unwrap()].truncate();
                let side_of_a = crate::utils::signed_triangle_area(&[$control_points[0].truncate(), point_b, point_c]);
                let side_of_d = crate::utils::signed_triangle_area(&[$control_points[j].truncate(), point_b, point_c]);*/
                let side_of_a = signed_triangle_areas[j];
                let side_of_d = signed_triangle_areas[0] * if j == 2 { -1.0 } else { 1.0 };
                if side_of_a * side_of_d < 0.0 {
                    assert_eq!(opposite_triangle, 0);
                    opposite_triangle = j;
                }
            }
            assert_ne!(opposite_triangle, 0);
            emit_triangle!(triangles, signed_triangle_areas, $control_points, $weights, $v, $w, $emit_vertex, 0);
            emit_triangle!(
                triangles,
                signed_triangle_areas,
                $control_points,
                $weights,
                $v,
                $w,
                $emit_vertex,
                opposite_triangle
            );
        }
        let mut additional_anchors = 0;
        for i in 1..3 {
            if enclosing_triangle != Some(i) && implicit_curve_value($weights[i]) < 0.0 {
                $anchors.push($control_points[i].truncate());
                additional_anchors += 1;
            }
        }
        if additional_anchors == 2 && signed_triangle_areas[0] * signed_triangle_areas[1] < 0.0 {
            let length = $anchors.len();
            $anchors.swap(length - 2, length - 1);
        }
        $cubic_curve_triangles.append(&mut triangles);
    }};
}

macro_rules! split_curve_at {
    ($control_points:expr, $param:expr) => {{
        let p10 = $control_points[0].lerp($control_points[1], $param);
        let p11 = $control_points[1].lerp($control_points[2], $param);
        let p12 = $control_points[2].lerp($control_points[3], $param);
        let p20 = p10.lerp(p11, $param);
        let p21 = p11.lerp(p12, $param);
        let p30 = p20.lerp(p21, $param);
        ([$control_points[0], p10, p20, p30], [p30, p21, p12, $control_points[3]])
    }};
}

macro_rules! emit_cubic_curve {
    ($proto_hull:expr, $anchors:expr, $cubic_curve_triangles:expr,
     $control_points:expr, $c:expr, $discreminant:expr, $roots:expr,
     $v:ident, $w:ident, $emit_vertex:expr) => {{
        let mut weights = weights($discreminant, &$roots);
        let mut planes = weight_planes(&$control_points, &weights);
        let gradient = implicit_curve_gradient(&planes, weights.x_axis);
        normalize_implicit_curve_side(&mut planes, &mut weights, &$c, gradient);
        let mut weights = [weights.x_axis, weights.y_axis, weights.z_axis, weights.w_axis];
        if let Some(param) = find_double_point_issue($discreminant, &$roots) {
            let (control_points_a, control_points_b) = split_curve_at!(&$control_points, param);
            let (mut weights_a, mut weights_b) = split_curve_at!(&weights, param);
            triangulate_quadrilateral!($anchors, $cubic_curve_triangles, &control_points_a, weights_a, $v, $w, $emit_vertex);
            $anchors.push(control_points_b[0].truncate().into());
            for weights in &mut weights_b {
                *weights = glam::vec4(-weights[0], -weights[1], weights[2], weights[3]);
            }
            triangulate_quadrilateral!($anchors, $cubic_curve_triangles, &control_points_b, weights_b, $v, $w, $emit_vertex);
        } else {
            triangulate_quadrilateral!($anchors, $cubic_curve_triangles, $control_points, weights, $v, $w, $emit_vertex);
        }
        $proto_hull.push($control_points[1].truncate());
        $proto_hull.push($control_points[2].truncate());
        $proto_hull.push($control_points[3].truncate());
        $anchors.push($control_points[3].truncate());
    }};
}

fn triangle_fan_to_strip<T: Copy>(vertices: Vec<T>) -> Vec<T> {
    let gather_indices = (0..vertices.len()).map(|i| if (i & 1) == 0 { i >> 1 } else { vertices.len() - 1 - (i >> 1) });
    let mut result = Vec::with_capacity(vertices.len());
    for src in gather_indices {
        result.push(vertices[src]);
    }
    result
}

pub struct Shape {
    anchor_elements: Vec<usize>,
    offsets: [usize; 6],
    vertex_buffer: wgpu::Buffer,
}

impl Shape {
    pub fn new(device: &wgpu::Device, paths: &[Path]) -> Self {
        let mut max_elements = [0; 6];
        for path in paths {
            max_elements[0] += 1
                + path.line_segments.len()
                + path.integral_quadratic_curve_segments.len()
                + path.integral_cubic_curve_segments.len() * 5
                + path.rational_quadratic_curve_segments.len()
                + path.rational_cubic_curve_segments.len() * 5;
            max_elements[1] += 1
                + path.line_segments.len()
                + path.integral_quadratic_curve_segments.len() * 2
                + path.integral_cubic_curve_segments.len() * 3
                + path.rational_quadratic_curve_segments.len() * 2
                + path.rational_cubic_curve_segments.len() * 3;
            max_elements[2] += path.integral_quadratic_curve_segments.len() * 3;
            max_elements[3] += path.integral_cubic_curve_segments.len() * 6;
            max_elements[4] += path.rational_quadratic_curve_segments.len() * 3;
            max_elements[5] += path.rational_cubic_curve_segments.len() * 6;
        }
        let mut anchor_elements = Vec::with_capacity(paths.len());
        let mut all_anchors: Vec<Vertex0> = Vec::with_capacity(max_elements[0]);
        let mut proto_hull = Vec::with_capacity(max_elements[1]);
        let mut integral_quadratic_curve_triangles = Vec::with_capacity(max_elements[2]);
        let mut integral_cubic_curve_triangles = Vec::with_capacity(max_elements[3]);
        let mut rational_quadratic_curve_triangles = Vec::with_capacity(max_elements[4]);
        let mut rational_cubic_curve_triangles = Vec::with_capacity(max_elements[5]);
        for path in paths {
            let mut anchors: Vec<Vertex0> = Vec::with_capacity(
                1 + path.line_segments.len()
                    + path.integral_quadratic_curve_segments.len()
                    + path.integral_cubic_curve_segments.len() * 5
                    + path.rational_quadratic_curve_segments.len()
                    + path.rational_cubic_curve_segments.len() * 5,
            );
            anchors.push(path.start);
            proto_hull.push(path.start);
            let mut line_segment_iter = path.line_segments.iter();
            let mut integral_quadratic_curve_segment_iter = path.integral_quadratic_curve_segments.iter();
            let mut integral_cubic_curve_segment_iter = path.integral_cubic_curve_segments.iter();
            let mut rational_quadratic_curve_segment_iter = path.rational_quadratic_curve_segments.iter();
            let mut rational_cubic_curve_segment_iter = path.rational_cubic_curve_segments.iter();
            for segement_type in &path.segement_types {
                let previous_anchor = anchors.last().unwrap();
                match segement_type {
                    SegmentType::Line => {
                        let segment = line_segment_iter.next().unwrap();
                        proto_hull.push(segment.control_points[0]);
                        anchors.push(segment.control_points[0]);
                    }
                    SegmentType::IntegralQuadraticCurve => {
                        let segment = integral_quadratic_curve_segment_iter.next().unwrap();
                        integral_quadratic_curve_triangles.push(Vertex2(segment.control_points[1], glam::vec2(1.0, 1.0)));
                        integral_quadratic_curve_triangles.push(Vertex2(segment.control_points[0], glam::vec2(0.5, 0.0)));
                        integral_quadratic_curve_triangles.push(Vertex2(*anchors.last().unwrap(), glam::vec2(0.0, 0.0)));
                        proto_hull.push(segment.control_points[0]);
                        proto_hull.push(segment.control_points[1]);
                        anchors.push(segment.control_points[1]);
                    }
                    SegmentType::IntegralCubicCurve => {
                        let segment = integral_cubic_curve_segment_iter.next().unwrap();
                        let control_points = [
                            previous_anchor.extend(1.0),
                            segment.control_points[0].extend(1.0),
                            segment.control_points[1].extend(1.0),
                            segment.control_points[2].extend(1.0),
                        ];
                        let c = control_points_power_basis(&control_points);
                        let ippc = integral_inflection_point_polynomial_coefficients(&c);
                        let (discreminant, roots) = integral_inflection_points(ippc);
                        emit_cubic_curve!(
                            proto_hull,
                            anchors,
                            integral_cubic_curve_triangles,
                            control_points,
                            c,
                            discreminant,
                            roots,
                            v,
                            w,
                            Vertex3(v, w.truncate())
                        );
                    }
                    SegmentType::RationalQuadraticCurve => {
                        let segment = rational_quadratic_curve_segment_iter.next().unwrap();
                        let weight = 1.0 / segment.weights[2];
                        rational_quadratic_curve_triangles.push(Vertex3(segment.control_points[1], glam::vec3(weight, weight, weight)));
                        let weight = 1.0 / segment.weights[1];
                        rational_quadratic_curve_triangles.push(Vertex3(segment.control_points[0], glam::vec3(0.5 * weight, 0.0, weight)));
                        let weight = 1.0 / segment.weights[0];
                        rational_quadratic_curve_triangles.push(Vertex3(*anchors.last().unwrap(), glam::vec3(0.0, 0.0, weight)));
                        proto_hull.push(segment.control_points[0]);
                        proto_hull.push(segment.control_points[1]);
                        anchors.push(segment.control_points[1]);
                    }
                    SegmentType::RationalCubicCurve => {
                        let segment = rational_cubic_curve_segment_iter.next().unwrap();
                        let control_points = [
                            previous_anchor.extend(segment.weights[0]),
                            segment.control_points[0].extend(segment.weights[1]),
                            segment.control_points[1].extend(segment.weights[2]),
                            segment.control_points[2].extend(segment.weights[3]),
                        ];
                        let c = control_points_power_basis(&control_points);
                        let ippc = rational_inflection_point_polynomial_coefficients(&c);
                        let (discreminant, roots) = rational_inflection_points(ippc);
                        emit_cubic_curve!(
                            proto_hull,
                            anchors,
                            rational_cubic_curve_triangles,
                            control_points,
                            c,
                            discreminant,
                            roots,
                            v,
                            w,
                            Vertex4(v, w)
                        );
                    }
                }
            }
            anchor_elements.push(anchors.len());
            all_anchors.append(&mut triangle_fan_to_strip(anchors));
        }
        let convex_hull = crate::convex_hull::andrew(&proto_hull);
        let mut vertex_buffers = [
            crate::utils::transmute_vec::<_, u8>(all_anchors),
            crate::utils::transmute_vec::<_, u8>(integral_quadratic_curve_triangles),
            crate::utils::transmute_vec::<_, u8>(integral_cubic_curve_triangles),
            crate::utils::transmute_vec::<_, u8>(rational_quadratic_curve_triangles),
            crate::utils::transmute_vec::<_, u8>(rational_cubic_curve_triangles),
            crate::utils::transmute_vec::<_, u8>(triangle_fan_to_strip(convex_hull)),
        ];
        let mut vertex_buffer_length = 0;
        let mut offsets = [0; 6];
        for (i, vertex_buffer) in vertex_buffers.iter().enumerate() {
            vertex_buffer_length += vertex_buffer.len();
            offsets[i] = vertex_buffer_length;
        }
        let mut vertex_buffer_data: Vec<u8> = Vec::with_capacity(vertex_buffer_length);
        for mut vertex_buffer in &mut vertex_buffers {
            vertex_buffer_data.append(&mut vertex_buffer);
        }
        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: &vertex_buffer_data,
            usage: wgpu::BufferUsage::VERTEX,
        });
        Self {
            anchor_elements,
            offsets,
            vertex_buffer,
        }
    }

    pub fn render_stencil<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>) {
        render_pass.set_bind_group(0, &renderer.transform_bind_group, &[]);
        render_pass.set_pipeline(&renderer.stencil_solid_pipeline);
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(0..self.offsets[0] as u64));
        let mut begin_index = 0;
        for element_count in &self.anchor_elements {
            let end_index = begin_index + element_count;
            render_pass.draw(begin_index as u32..end_index as u32, 0..1);
            begin_index = end_index;
        }
        for (i, (pipeline, vertex_size)) in [
            (&renderer.stencil_integral_quadratic_curve_pipeline, std::mem::size_of::<Vertex2>()),
            (&renderer.stencil_integral_cubic_curve_pipeline, std::mem::size_of::<Vertex3>()),
            (&renderer.stencil_rational_quadratic_curve_pipeline, std::mem::size_of::<Vertex3>()),
            (&renderer.stencil_rational_cubic_curve_pipeline, std::mem::size_of::<Vertex4>()),
        ]
        .iter()
        .enumerate()
        {
            if self.offsets[i] < self.offsets[i + 1] {
                render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(self.offsets[i] as u64..self.offsets[i + 1] as u64));
                render_pass.set_pipeline(pipeline);
                render_pass.draw(0..((self.offsets[i + 1] - self.offsets[i]) / vertex_size) as u32, 0..1);
            }
        }
    }

    fn render_cover<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>) {
        render_pass.set_bind_group(0, &renderer.transform_bind_group, &[]);
        render_pass.set_bind_group(1, &renderer.fill_solid_bind_group, &[]);
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(self.offsets[4] as u64..self.offsets[5] as u64));
        render_pass.draw(0..((self.offsets[5] - self.offsets[4]) / std::mem::size_of::<Vertex0>()) as u32, 0..1);
    }

    pub fn render_solid_fill<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>, clip_stack_height: usize) {
        render_pass.set_stencil_reference((clip_stack_height << renderer.winding_counter_bits) as u32);
        render_pass.set_pipeline(&renderer.fill_solid_pipeline);
        self.render_cover(renderer, render_pass);
    }
}

#[derive(Default)]
pub struct ClipStack<'a> {
    stack: Vec<&'a Shape>,
}

impl<'b, 'a: 'b> ClipStack<'a> {
    pub fn height(&self) -> usize {
        self.stack.len()
    }

    pub fn push(&mut self, renderer: &'b Renderer, render_pass: &mut wgpu::RenderPass<'b>, shape: &'a Shape) -> Result<(), Error> {
        if self.height() >= (1 << renderer.clip_nesting_counter_bits) {
            return Err(Error::ClipStackOverflow);
        }
        self.stack.push(shape);
        render_pass.set_stencil_reference((self.height() << renderer.winding_counter_bits) as u32);
        render_pass.set_pipeline(&renderer.increment_clip_nesting_counter_pipeline);
        shape.render_cover(renderer, render_pass);
        Ok(())
    }

    pub fn pop(&mut self, renderer: &'b Renderer, render_pass: &mut wgpu::RenderPass<'b>) -> Result<(), Error> {
        match self.stack.pop() {
            Some(shape) => {
                render_pass.set_stencil_reference((self.height() << renderer.winding_counter_bits) as u32);
                render_pass.set_pipeline(&renderer.decrement_clip_nesting_counter_pipeline);
                shape.render_cover(renderer, render_pass);
                Ok(())
            }
            None => Err(Error::ClipStackUnderflow),
        }
    }
}

macro_rules! stencil_descriptor {
    ($compare:ident, $fail:ident, $pass:ident) => {
        wgpu::StencilFaceState {
            compare: wgpu::CompareFunction::$compare,
            fail_op: wgpu::StencilOperation::$fail,
            depth_fail_op: wgpu::StencilOperation::Keep,
            pass_op: wgpu::StencilOperation::$pass,
        }
    };
}

macro_rules! render_pipeline_descriptor {
    ($pipeline_layout:expr,
     $vertex_module:expr, $fragment_module:expr,
     $primitive_topology:ident, $color_states:expr,
     $stencil_state:expr,
     $vertex_buffer:expr, $sample_count:expr $(,)?) => {
        wgpu::RenderPipelineDescriptor {
            label: None,
            layout: Some($pipeline_layout),
            vertex: wgpu::VertexState {
                module: $vertex_module,
                entry_point: "main",
                buffers: &[$vertex_buffer],
            },
            fragment: Some(wgpu::FragmentState {
                module: $fragment_module,
                entry_point: "main",
                targets: $color_states,
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::$primitive_topology,
                strip_index_format: None,
                front_face: wgpu::FrontFace::Ccw,
                cull_mode: wgpu::CullMode::None,
                polygon_mode: wgpu::PolygonMode::Fill,
            },
            depth_stencil: Some(wgpu::DepthStencilState {
                format: wgpu::TextureFormat::Depth24PlusStencil8,
                depth_write_enabled: false,
                depth_compare: wgpu::CompareFunction::Always,
                bias: wgpu::DepthBiasState::default(),
                clamp_depth: false,
                stencil: $stencil_state,
            }),
            multisample: wgpu::MultisampleState {
                count: $sample_count,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
        }
    };
}

pub struct Renderer {
    winding_counter_bits: usize,
    clip_nesting_counter_bits: usize,
    transform_uniform_buffer: wgpu::Buffer,
    transform_bind_group: wgpu::BindGroup,
    stencil_solid_pipeline: wgpu::RenderPipeline,
    stencil_integral_quadratic_curve_pipeline: wgpu::RenderPipeline,
    stencil_integral_cubic_curve_pipeline: wgpu::RenderPipeline,
    stencil_rational_quadratic_curve_pipeline: wgpu::RenderPipeline,
    stencil_rational_cubic_curve_pipeline: wgpu::RenderPipeline,
    increment_clip_nesting_counter_pipeline: wgpu::RenderPipeline,
    decrement_clip_nesting_counter_pipeline: wgpu::RenderPipeline,
    fill_solid_uniform_buffer: wgpu::Buffer,
    fill_solid_bind_group: wgpu::BindGroup,
    fill_solid_pipeline: wgpu::RenderPipeline,
}

impl Renderer {
    pub fn new(
        device: &wgpu::Device,
        fill_color_state: wgpu::ColorTargetState,
        sample_count: u32,
        clip_nesting_counter_bits: usize,
        winding_counter_bits: usize,
    ) -> Result<Self, Error> {
        if winding_counter_bits == 0 || clip_nesting_counter_bits + winding_counter_bits > 8 {
            return Err(Error::NumberOfStencilBitsIsUnsupported);
        }

        let segment_0_vertex_module = device.create_shader_module(&include_spirv!("../target/shader_modules/segment0_vert.spv"));
        let segment_3_vertex_module = device.create_shader_module(&include_spirv!("../target/shader_modules/segment3_vert.spv"));
        let stencil_solid_fragment_module = device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_solid_frag.spv"));
        let segment_0_vertex_buffer_descriptor = wgpu::VertexBufferLayout {
            array_stride: (2 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::InputStepMode::Vertex,
            attributes: &vertex_attr_array![0 => Float2],
        };
        let segment_2_vertex_buffer_descriptor = wgpu::VertexBufferLayout {
            array_stride: (4 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::InputStepMode::Vertex,
            attributes: &vertex_attr_array![0 => Float2, 1 => Float2],
        };
        let segment_3_vertex_buffer_descriptor = wgpu::VertexBufferLayout {
            array_stride: (5 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::InputStepMode::Vertex,
            attributes: &vertex_attr_array![0 => Float2, 1 => Float3],
        };
        let segment_4_vertex_buffer_descriptor = wgpu::VertexBufferLayout {
            array_stride: (6 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::InputStepMode::Vertex,
            attributes: &vertex_attr_array![0 => Float2, 1 => Float4],
        };

        const TRANSFORM_UNIFORM_BUFFER_SIZE: usize = std::mem::size_of::<[f32; 4 * 4]>();
        let transform_uniform_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            mapped_at_creation: false,
            size: TRANSFORM_UNIFORM_BUFFER_SIZE as u64,
            usage: wgpu::BufferUsage::UNIFORM | wgpu::BufferUsage::COPY_DST,
        });
        let transform_bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: None,
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStage::VERTEX,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: wgpu::BufferSize::new(TRANSFORM_UNIFORM_BUFFER_SIZE as u64),
                },
                count: None,
            }],
        });
        let transform_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &transform_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Buffer {
                    buffer: &transform_uniform_buffer,
                    offset: 0,
                    size: wgpu::BufferSize::new(TRANSFORM_UNIFORM_BUFFER_SIZE as u64),
                },
            }],
            label: None,
        });

        let winding_counter_mask = (1 << winding_counter_bits) - 1;
        let clip_nesting_counter_mask = ((1 << clip_nesting_counter_bits) - 1) << winding_counter_bits;
        let stencil_state = wgpu::StencilState {
            front: stencil_descriptor!(LessEqual, Keep, IncrementWrap),
            back: stencil_descriptor!(LessEqual, Keep, DecrementWrap),
            read_mask: clip_nesting_counter_mask | winding_counter_mask,
            write_mask: winding_counter_mask,
        };
        let stencil_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&transform_bind_group_layout],
            push_constant_ranges: &[],
        });
        let stencil_solid_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &stencil_solid_fragment_module,
            TriangleStrip,
            &[],
            stencil_state.clone(),
            segment_0_vertex_buffer_descriptor.clone(),
            sample_count,
        ));
        let stencil_integral_quadratic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/segment2_vert.spv")),
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_integral_quadratic_curve_frag.spv")),
            TriangleList,
            &[],
            stencil_state.clone(),
            segment_2_vertex_buffer_descriptor,
            sample_count,
        ));
        let stencil_integral_cubic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_3_vertex_module,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_integral_cubic_curve_frag.spv")),
            TriangleList,
            &[],
            stencil_state.clone(),
            segment_3_vertex_buffer_descriptor.clone(),
            sample_count,
        ));
        let stencil_rational_quadratic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_3_vertex_module,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_rational_quadratic_curve_frag.spv")),
            TriangleList,
            &[],
            stencil_state.clone(),
            segment_3_vertex_buffer_descriptor.clone(),
            sample_count,
        ));
        let stencil_rational_cubic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/segment4_vert.spv")),
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_rational_cubic_curve_frag.spv")),
            TriangleList,
            &[],
            stencil_state,
            segment_4_vertex_buffer_descriptor,
            sample_count,
        ));

        let increment_clip_nesting_counter_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &stencil_solid_fragment_module,
            TriangleStrip,
            &[],
            wgpu::StencilState {
                front: stencil_descriptor!(NotEqual, Keep, Replace),
                back: stencil_descriptor!(NotEqual, Keep, Replace),
                read_mask: winding_counter_mask,
                write_mask: clip_nesting_counter_mask | winding_counter_mask,
            },
            segment_0_vertex_buffer_descriptor.clone(),
            sample_count,
        ));
        let decrement_clip_nesting_counter_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &stencil_solid_fragment_module,
            TriangleStrip,
            &[],
            wgpu::StencilState {
                front: stencil_descriptor!(Less, Keep, Replace),
                back: stencil_descriptor!(Less, Keep, Replace),
                read_mask: clip_nesting_counter_mask,
                write_mask: clip_nesting_counter_mask | winding_counter_mask,
            },
            segment_0_vertex_buffer_descriptor.clone(),
            sample_count,
        ));

        const FILL_SOLID_UNIFORM_BUFFER_SIZE: usize = std::mem::size_of::<[f32; 4]>();
        let fill_solid_uniform_data: [f32; 4] = [1.0, 1.0, 1.0, 1.0];
        let fill_solid_uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: crate::utils::transmute_slice(&fill_solid_uniform_data),
            usage: wgpu::BufferUsage::UNIFORM | wgpu::BufferUsage::COPY_DST,
        });
        let fill_solid_bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: None,
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStage::FRAGMENT,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: wgpu::BufferSize::new(FILL_SOLID_UNIFORM_BUFFER_SIZE as u64),
                },
                count: None,
            }],
        });
        let fill_solid_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &fill_solid_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Buffer {
                    buffer: &fill_solid_uniform_buffer,
                    offset: 0,
                    size: wgpu::BufferSize::new(FILL_SOLID_UNIFORM_BUFFER_SIZE as u64),
                },
            }],
            label: None,
        });
        let fill_solid_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&transform_bind_group_layout, &fill_solid_bind_group_layout],
            push_constant_ranges: &[],
        });
        let fill_solid_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &fill_solid_pipeline_layout,
            &segment_0_vertex_module,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/fill_solid_frag.spv")),
            TriangleStrip,
            &[fill_color_state],
            wgpu::StencilState {
                front: stencil_descriptor!(Less, Zero, Zero),
                back: stencil_descriptor!(Less, Zero, Zero),
                read_mask: clip_nesting_counter_mask | winding_counter_mask,
                write_mask: winding_counter_mask,
            },
            segment_0_vertex_buffer_descriptor.clone(),
            sample_count,
        ));

        Ok(Self {
            winding_counter_bits,
            clip_nesting_counter_bits,
            transform_uniform_buffer,
            transform_bind_group,
            stencil_solid_pipeline,
            stencil_integral_quadratic_curve_pipeline,
            stencil_integral_cubic_curve_pipeline,
            stencil_rational_quadratic_curve_pipeline,
            stencil_rational_cubic_curve_pipeline,
            increment_clip_nesting_counter_pipeline,
            decrement_clip_nesting_counter_pipeline,
            fill_solid_uniform_buffer,
            fill_solid_bind_group,
            fill_solid_pipeline,
        })
    }

    pub fn set_transform(&self, queue: &wgpu::Queue, transform: &[[f32; 4]; 4]) {
        let data = crate::utils::transmute_slice(transform);
        queue.write_buffer(&self.transform_uniform_buffer, 0, &data);
    }

    pub fn set_solid_fill_color(&self, queue: &wgpu::Queue, color: &[f32; 4]) {
        let data = crate::utils::transmute_slice(color);
        queue.write_buffer(&self.fill_solid_uniform_buffer, 0, &data);
    }
}
