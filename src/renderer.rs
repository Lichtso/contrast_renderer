use crate::{
    complex_number::ComplexNumber,
    error::Error,
    path::{Cap, Join, Path, SegmentType, StrokeOptions},
    utils::{line_line_intersection, rotate_90_degree_clockwise, signed_triangle_area, transmute_slice, transmute_vec},
};
use glam::const_mat4;
use wgpu::{include_spirv, util::DeviceExt, vertex_attr_array};

const ERROR_MARGIN: f32 = 0.00001;

type Vertex0 = glam::Vec2;
#[derive(Clone, Copy)]
#[repr(packed)]
struct Vertex2(pub glam::Vec2, pub glam::Vec2);
#[derive(Clone, Copy)]
#[repr(packed)]
struct Vertex3(pub glam::Vec2, pub glam::Vec3);
#[derive(Clone, Copy)]
#[repr(packed)]
struct Vertex4(pub glam::Vec2, pub glam::Vec4);

fn emit_stroke_vertex(proto_hull: &mut Vec<Vertex0>, path_stroke_solid_vertices: &mut Vec<Vertex0>, vertex: glam::Vec2) {
    proto_hull.push(vertex);
    path_stroke_solid_vertices.push(vertex);
}

fn emit_stroke_vertices(
    proto_hull: &mut Vec<Vertex0>,
    path_stroke_solid_vertices: &mut Vec<Vertex0>,
    stroke_options: &StrokeOptions,
    control_point: glam::Vec2,
    tangent: glam::Vec2,
) {
    let normal = rotate_90_degree_clockwise(tangent);
    let clamped_offset = stroke_options.offset.clamp(-0.5, 0.5);
    let width_absolute = stroke_options.width.abs();
    emit_stroke_vertex(
        proto_hull,
        path_stroke_solid_vertices,
        control_point + normal * (clamped_offset - 0.5) * width_absolute,
    );
    emit_stroke_vertex(
        proto_hull,
        path_stroke_solid_vertices,
        control_point + normal * (clamped_offset + 0.5) * width_absolute,
    );
}

fn emit_stroke_rounding(
    proto_hull: &mut Vec<Vertex0>,
    stroke_rounding_triangles: &mut Vec<Vertex3>,
    begin: (glam::Vec2, glam::Vec2),
    end: (glam::Vec2, glam::Vec2),
) {
    if begin.0.distance_squared(end.0) == 0.0 {
        return;
    }
    let intersection_point = line_line_intersection(begin, end);
    proto_hull.push(intersection_point);
    let weight = std::f32::consts::SQRT_2 / (1.0 + begin.1.dot(end.1)).sqrt();
    stroke_rounding_triangles.push(Vertex3(end.0, glam::vec3(1.0, 1.0, 1.0)));
    stroke_rounding_triangles.push(Vertex3(intersection_point, glam::vec3(0.5 * weight, 0.0, weight)));
    stroke_rounding_triangles.push(Vertex3(begin.0, glam::vec3(0.0, 0.0, 1.0)));
}

fn emit_stroke_split_rounding(
    proto_hull: &mut Vec<Vertex0>,
    stroke_rounding_triangles: &mut Vec<Vertex3>,
    begin: (glam::Vec2, glam::Vec2),
    tip: (glam::Vec2, glam::Vec2),
    end: (glam::Vec2, glam::Vec2),
) {
    emit_stroke_rounding(proto_hull, stroke_rounding_triangles, begin, tip);
    emit_stroke_rounding(proto_hull, stroke_rounding_triangles, tip, end);
}

fn emit_stroke_join(
    proto_hull: &mut Vec<Vertex0>,
    path_stroke_solid_vertices: &mut Vec<Vertex0>,
    stroke_rounding_triangles: &mut Vec<Vertex3>,
    stroke_options: &StrokeOptions,
    control_point: glam::Vec2,
    start_tangent: glam::Vec2,
    end_tangent: glam::Vec2,
) {
    emit_stroke_vertices(proto_hull, path_stroke_solid_vertices, stroke_options, control_point, start_tangent);
    let side_sign = rotate_90_degree_clockwise(start_tangent).dot(end_tangent);
    if side_sign == 0.0 {
        return;
    }
    let base_index = if side_sign < 0.0 { 1 } else { 2 };
    let start_point_and_tagent = (path_stroke_solid_vertices[path_stroke_solid_vertices.len() - base_index], start_tangent);
    let mid_tangent = (start_tangent + end_tangent).normalize();
    let tangets_dot_product = start_tangent.dot(end_tangent);
    if stroke_options.join == Join::Miter || (stroke_options.join == Join::Round && tangets_dot_product < 0.0) {
        emit_stroke_vertices(proto_hull, path_stroke_solid_vertices, stroke_options, control_point, mid_tangent);
    }
    emit_stroke_vertices(proto_hull, path_stroke_solid_vertices, stroke_options, control_point, end_tangent);
    let end_point_and_tagent = (path_stroke_solid_vertices[path_stroke_solid_vertices.len() - base_index], end_tangent);
    match stroke_options.join {
        Join::Miter => {
            let mid_index = path_stroke_solid_vertices.len() - base_index - 2;
            path_stroke_solid_vertices[mid_index] = line_line_intersection(start_point_and_tagent, end_point_and_tagent);
        }
        Join::Bevel => {}
        Join::Round => {
            if tangets_dot_product < 0.0 {
                emit_stroke_split_rounding(
                    proto_hull,
                    stroke_rounding_triangles,
                    start_point_and_tagent,
                    (path_stroke_solid_vertices[path_stroke_solid_vertices.len() - base_index - 2], mid_tangent),
                    end_point_and_tagent,
                );
            } else {
                emit_stroke_rounding(proto_hull, stroke_rounding_triangles, start_point_and_tagent, end_point_and_tagent);
            }
        }
    }
}

fn cut_stroke_polygon(
    proto_hull: &mut Vec<Vertex0>,
    stroke_solid_indices: &mut Vec<u16>,
    stroke_solid_vertices: &mut Vec<Vertex0>,
    path_stroke_solid_vertices: &mut Vec<Vertex0>,
) {
    if !path_stroke_solid_vertices.is_empty() {
        proto_hull.append(&mut path_stroke_solid_vertices.clone());
        let start_index = stroke_solid_vertices.len();
        stroke_solid_vertices.append(path_stroke_solid_vertices);
        let mut indices: Vec<u16> = (start_index as u16..(stroke_solid_vertices.len() + 1) as u16).collect();
        *indices.iter_mut().last().unwrap() = (-1isize) as u16;
        stroke_solid_indices.append(&mut indices);
    }
}

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

macro_rules! emit_cubic_curve_triangle {
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

macro_rules! triangulate_cubic_curve_quadrilateral {
    ($fill_solid_vertices:expr, $cubic_curve_triangles:expr,
     $control_points:expr, $weights:expr, $v:ident, $w:ident, $emit_vertex:expr) => {{
        for (weights, control_point) in $weights.iter_mut().zip($control_points.iter()) {
            *weights /= control_point[2];
        }
        let mut triangles = Vec::new();
        let signed_triangle_areas: Vec<f32> = (0..4)
            .map(|i| {
                let control_points: Vec<_> = (0..4).filter(|j| i != *j).map(|j| $control_points[j].truncate()).collect();
                signed_triangle_area(&control_points[0..3])
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
            emit_cubic_curve_triangle!(
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
                let side_of_a = signed_triangle_area(&[$control_points[0].truncate(), point_b, point_c]);
                let side_of_d = signed_triangle_area(&[$control_points[j].truncate(), point_b, point_c]);*/
                let side_of_a = signed_triangle_areas[j];
                let side_of_d = signed_triangle_areas[0] * if j == 2 { -1.0 } else { 1.0 };
                if side_of_a * side_of_d < 0.0 {
                    assert_eq!(opposite_triangle, 0);
                    opposite_triangle = j;
                }
            }
            assert_ne!(opposite_triangle, 0);
            emit_cubic_curve_triangle!(triangles, signed_triangle_areas, $control_points, $weights, $v, $w, $emit_vertex, 0);
            emit_cubic_curve_triangle!(
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
        let mut additional_vertices = 0;
        for i in 1..3 {
            if enclosing_triangle != Some(i) && implicit_curve_value($weights[i]) < 0.0 {
                $fill_solid_vertices.push($control_points[i].truncate());
                additional_vertices += 1;
            }
        }
        if additional_vertices == 2 && signed_triangle_areas[0] * signed_triangle_areas[1] < 0.0 {
            let length = $fill_solid_vertices.len();
            $fill_solid_vertices.swap(length - 2, length - 1);
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
    ($proto_hull:expr, $fill_solid_vertices:expr, $cubic_curve_triangles:expr,
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
            triangulate_cubic_curve_quadrilateral!(
                $fill_solid_vertices,
                $cubic_curve_triangles,
                &control_points_a,
                weights_a,
                $v,
                $w,
                $emit_vertex
            );
            $fill_solid_vertices.push(control_points_b[0].truncate().into());
            for weights in &mut weights_b {
                *weights = glam::vec4(-weights[0], -weights[1], weights[2], weights[3]);
            }
            triangulate_cubic_curve_quadrilateral!(
                $fill_solid_vertices,
                $cubic_curve_triangles,
                &control_points_b,
                weights_b,
                $v,
                $w,
                $emit_vertex
            );
        } else {
            triangulate_cubic_curve_quadrilateral!(
                $fill_solid_vertices,
                $cubic_curve_triangles,
                $control_points,
                weights,
                $v,
                $w,
                $emit_vertex
            );
        }
        $proto_hull.push($control_points[1].truncate());
        $proto_hull.push($control_points[2].truncate());
        $proto_hull.push($control_points[3].truncate());
        $fill_solid_vertices.push($control_points[3].truncate());
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

macro_rules! concat_buffers {
    ($device:expr, $usage:ident, $buffer_count:expr, [$($buffer:expr,)*]) => {{
        let mut buffers = [
            $(transmute_vec::<_, u8>($buffer)),*
        ];
        let mut offsets = [0; $buffer_count];
        let mut buffer_length = 0;
        for (i, buffer) in buffers.iter().enumerate() {
            buffer_length += buffer.len();
            offsets[i] = buffer_length;
        }
        let mut buffer_data: Vec<u8> = Vec::with_capacity(buffer_length);
        for mut buffer in &mut buffers {
            buffer_data.append(&mut buffer);
        }
        (
            offsets,
            $device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: None,
                contents: &buffer_data,
                usage: wgpu::BufferUsage::$usage,
            })
        )
    }};
}

/// A set of `Path`s which is always rendered together
pub struct Shape {
    vertex_offsets: [usize; 8],
    index_offsets: [usize; 2],
    vertex_buffer: wgpu::Buffer,
    index_buffer: wgpu::Buffer,
}

impl Shape {
    /// Constructs a `Shape` from a set of `Path`s.
    pub fn from_paths(device: &wgpu::Device, paths: &[Path]) -> Self {
        let mut max_elements = [0; 10];
        for path in paths {
            if let Some(stroke_options) = &path.stroke_options {
                let number_of_segments = if stroke_options.cap == Cap::Closed {
                    path.segement_types.len() + 1
                } else {
                    path.segement_types.len()
                };
                max_elements[0] += number_of_segments * 5;
                if stroke_options.join == Join::Miter {
                    max_elements[1] += number_of_segments * 6;
                    max_elements[5] += number_of_segments * 6;
                } else {
                    max_elements[1] += number_of_segments * 4;
                    max_elements[5] += number_of_segments * 4;
                }
                if stroke_options.join == Join::Round {
                    max_elements[2] += number_of_segments * 6;
                    max_elements[5] += number_of_segments * 2;
                }
            } else {
                let solid_vertices = 1
                    + path.line_segments.len()
                    + path.integral_quadratic_curve_segments.len()
                    + path.integral_cubic_curve_segments.len() * 5
                    + path.rational_quadratic_curve_segments.len()
                    + path.rational_cubic_curve_segments.len() * 5;
                max_elements[3] += solid_vertices + 1;
                max_elements[4] += solid_vertices;
                max_elements[5] += 1
                    + path.line_segments.len()
                    + path.integral_quadratic_curve_segments.len() * 2
                    + path.integral_cubic_curve_segments.len() * 3
                    + path.rational_quadratic_curve_segments.len() * 2
                    + path.rational_cubic_curve_segments.len() * 3;
                max_elements[6] += path.integral_quadratic_curve_segments.len() * 3;
                max_elements[7] += path.integral_cubic_curve_segments.len() * 6;
                max_elements[8] += path.rational_quadratic_curve_segments.len() * 3;
                max_elements[9] += path.rational_cubic_curve_segments.len() * 6;
            }
        }
        let mut stroke_solid_indices: Vec<u16> = Vec::with_capacity(max_elements[1]);
        let mut stroke_solid_vertices = Vec::with_capacity(max_elements[1]);
        let mut stroke_rounding_triangles: Vec<Vertex3> = Vec::with_capacity(max_elements[2]);
        let mut fill_solid_indices: Vec<u16> = Vec::with_capacity(max_elements[4]);
        let mut fill_solid_vertices = Vec::with_capacity(max_elements[4]);
        let mut proto_hull = Vec::with_capacity(max_elements[5]);
        let mut integral_quadratic_curve_triangles = Vec::with_capacity(max_elements[6]);
        let mut integral_cubic_curve_triangles = Vec::with_capacity(max_elements[7]);
        let mut rational_quadratic_curve_triangles = Vec::with_capacity(max_elements[8]);
        let mut rational_cubic_curve_triangles = Vec::with_capacity(max_elements[9]);
        for path in paths {
            if let Some(stroke_options) = &path.stroke_options {
                let mut path_stroke_solid_vertices: Vec<Vertex0> = Vec::with_capacity(path.line_segments.len() * 4);
                let mut previous_control_point = path.start;
                let mut first_tangent = glam::Vec2::default();
                let mut previous_tangent = glam::Vec2::default();
                let mut line_segment_iter = path.line_segments.iter();
                let mut integral_quadratic_curve_segment_iter = path.integral_quadratic_curve_segments.iter();
                let mut integral_cubic_curve_segment_iter = path.integral_cubic_curve_segments.iter();
                let mut rational_quadratic_curve_segment_iter = path.rational_quadratic_curve_segments.iter();
                let mut rational_cubic_curve_segment_iter = path.rational_cubic_curve_segments.iter();
                for (i, segement_type) in path.segement_types.iter().enumerate() {
                    let next_control_point;
                    let segment_start_tangent;
                    let segment_end_tangent;
                    match segement_type {
                        SegmentType::Line => {
                            let segment = line_segment_iter.next().unwrap();
                            next_control_point = segment.control_points[0];
                            segment_start_tangent = (next_control_point - previous_control_point).normalize();
                            segment_end_tangent = segment_start_tangent;
                        }
                        SegmentType::IntegralQuadraticCurve => {
                            let segment = integral_quadratic_curve_segment_iter.next().unwrap();
                            next_control_point = segment.control_points[1];
                            segment_start_tangent = (segment.control_points[0] - previous_control_point).normalize();
                            segment_end_tangent = (next_control_point - segment.control_points[0]).normalize();
                            // error!("IntegralQuadraticCurve stroking is not implemented");
                        }
                        SegmentType::IntegralCubicCurve => {
                            let segment = integral_cubic_curve_segment_iter.next().unwrap();
                            next_control_point = segment.control_points[2];
                            segment_start_tangent = (segment.control_points[0] - previous_control_point).normalize();
                            segment_end_tangent = (next_control_point - segment.control_points[1]).normalize();
                            // error!("IntegralCubicCurve stroking is not implemented");
                        }
                        SegmentType::RationalQuadraticCurve => {
                            let segment = rational_quadratic_curve_segment_iter.next().unwrap();
                            next_control_point = segment.control_points[1];
                            segment_start_tangent = (segment.control_points[0] - previous_control_point).normalize();
                            segment_end_tangent = (next_control_point - segment.control_points[0]).normalize();
                            // error!("RationalQuadraticCurve stroking is not implemented");
                        }
                        SegmentType::RationalCubicCurve => {
                            let segment = rational_cubic_curve_segment_iter.next().unwrap();
                            next_control_point = segment.control_points[2];
                            segment_start_tangent = (segment.control_points[0] - previous_control_point).normalize();
                            segment_end_tangent = (next_control_point - segment.control_points[1]).normalize();
                            // error!("RationalCubicCurve stroking is not implemented");
                        }
                    }
                    if i == 0 {
                        first_tangent = segment_start_tangent;
                        let tip_point = previous_control_point - segment_start_tangent * stroke_options.width.abs() * 0.5;
                        match stroke_options.cap {
                            Cap::Closed | Cap::Butt => {}
                            Cap::Square => {
                                emit_stroke_vertices(
                                    &mut proto_hull,
                                    &mut path_stroke_solid_vertices,
                                    &stroke_options,
                                    tip_point,
                                    segment_start_tangent,
                                );
                            }
                            Cap::Triangle | Cap::Round => {
                                emit_stroke_vertex(&mut proto_hull, &mut path_stroke_solid_vertices, tip_point);
                            }
                        }
                        if (stroke_options.cap == Cap::Round || stroke_options.cap == Cap::Triangle)
                            || (stroke_options.cap == Cap::Square && *segement_type != SegmentType::Line)
                            || ((stroke_options.cap == Cap::Butt || stroke_options.cap == Cap::Closed) && *segement_type == SegmentType::Line)
                        {
                            emit_stroke_vertices(
                                &mut proto_hull,
                                &mut path_stroke_solid_vertices,
                                &stroke_options,
                                previous_control_point,
                                segment_start_tangent,
                            );
                        }
                        if stroke_options.cap == Cap::Round {
                            emit_stroke_split_rounding(
                                &mut proto_hull,
                                &mut stroke_rounding_triangles,
                                (path_stroke_solid_vertices[path_stroke_solid_vertices.len() - 2], -segment_start_tangent),
                                (tip_point, rotate_90_degree_clockwise(segment_start_tangent)),
                                (path_stroke_solid_vertices[path_stroke_solid_vertices.len() - 1], segment_start_tangent),
                            );
                        }
                    } else {
                        emit_stroke_join(
                            &mut proto_hull,
                            &mut path_stroke_solid_vertices,
                            &mut stroke_rounding_triangles,
                            &stroke_options,
                            previous_control_point,
                            previous_tangent,
                            segment_start_tangent,
                        );
                    }
                    if *segement_type != SegmentType::Line {
                        cut_stroke_polygon(
                            &mut proto_hull,
                            &mut stroke_solid_indices,
                            &mut stroke_solid_vertices,
                            &mut path_stroke_solid_vertices,
                        );
                    }
                    previous_control_point = next_control_point;
                    previous_tangent = segment_end_tangent;
                }
                if (stroke_options.cap == Cap::Round || stroke_options.cap == Cap::Triangle)
                    || (stroke_options.cap == Cap::Square && *path.segement_types.last().unwrap() != SegmentType::Line)
                    || (stroke_options.cap == Cap::Butt && *path.segement_types.last().unwrap() == SegmentType::Line)
                {
                    emit_stroke_vertices(
                        &mut proto_hull,
                        &mut path_stroke_solid_vertices,
                        &stroke_options,
                        previous_control_point,
                        previous_tangent,
                    );
                }
                let tip_point = previous_control_point + previous_tangent * stroke_options.width.abs() * 0.5;
                if stroke_options.cap == Cap::Round {
                    emit_stroke_split_rounding(
                        &mut proto_hull,
                        &mut stroke_rounding_triangles,
                        (path_stroke_solid_vertices[path_stroke_solid_vertices.len() - 2], -previous_tangent),
                        (tip_point, rotate_90_degree_clockwise(previous_tangent)),
                        (path_stroke_solid_vertices[path_stroke_solid_vertices.len() - 1], previous_tangent),
                    );
                }
                match stroke_options.cap {
                    Cap::Closed => {
                        let line_segment = path.start - previous_control_point;
                        if line_segment.length_squared() > 0.0 {
                            let segment_tangent = line_segment.normalize();
                            emit_stroke_join(
                                &mut proto_hull,
                                &mut path_stroke_solid_vertices,
                                &mut stroke_rounding_triangles,
                                &stroke_options,
                                previous_control_point,
                                previous_tangent,
                                segment_tangent,
                            );
                            emit_stroke_join(
                                &mut proto_hull,
                                &mut path_stroke_solid_vertices,
                                &mut stroke_rounding_triangles,
                                &stroke_options,
                                path.start,
                                segment_tangent,
                                first_tangent,
                            );
                        } else {
                            emit_stroke_join(
                                &mut proto_hull,
                                &mut path_stroke_solid_vertices,
                                &mut stroke_rounding_triangles,
                                &stroke_options,
                                path.start,
                                previous_tangent,
                                first_tangent,
                            );
                        }
                    }
                    Cap::Butt => {}
                    Cap::Square => {
                        emit_stroke_vertices(
                            &mut proto_hull,
                            &mut path_stroke_solid_vertices,
                            &stroke_options,
                            tip_point,
                            previous_tangent,
                        );
                    }
                    Cap::Triangle | Cap::Round => {
                        emit_stroke_vertex(&mut proto_hull, &mut path_stroke_solid_vertices, tip_point);
                    }
                }
                cut_stroke_polygon(
                    &mut proto_hull,
                    &mut stroke_solid_indices,
                    &mut stroke_solid_vertices,
                    &mut path_stroke_solid_vertices,
                );
            } else {
                let mut path_fill_solid_vertices: Vec<Vertex0> = Vec::with_capacity(
                    1 + path.line_segments.len()
                        + path.integral_quadratic_curve_segments.len()
                        + path.integral_cubic_curve_segments.len() * 5
                        + path.rational_quadratic_curve_segments.len()
                        + path.rational_cubic_curve_segments.len() * 5,
                );
                path_fill_solid_vertices.push(path.start);
                proto_hull.push(path.start);
                let mut line_segment_iter = path.line_segments.iter();
                let mut integral_quadratic_curve_segment_iter = path.integral_quadratic_curve_segments.iter();
                let mut integral_cubic_curve_segment_iter = path.integral_cubic_curve_segments.iter();
                let mut rational_quadratic_curve_segment_iter = path.rational_quadratic_curve_segments.iter();
                let mut rational_cubic_curve_segment_iter = path.rational_cubic_curve_segments.iter();
                for segement_type in &path.segement_types {
                    match segement_type {
                        SegmentType::Line => {
                            let segment = line_segment_iter.next().unwrap();
                            proto_hull.push(segment.control_points[0]);
                            path_fill_solid_vertices.push(segment.control_points[0]);
                        }
                        SegmentType::IntegralQuadraticCurve => {
                            let segment = integral_quadratic_curve_segment_iter.next().unwrap();
                            integral_quadratic_curve_triangles.push(Vertex2(segment.control_points[1], glam::vec2(1.0, 1.0)));
                            integral_quadratic_curve_triangles.push(Vertex2(segment.control_points[0], glam::vec2(0.5, 0.0)));
                            integral_quadratic_curve_triangles.push(Vertex2(*path_fill_solid_vertices.last().unwrap(), glam::vec2(0.0, 0.0)));
                            proto_hull.push(segment.control_points[0]);
                            proto_hull.push(segment.control_points[1]);
                            path_fill_solid_vertices.push(segment.control_points[1]);
                        }
                        SegmentType::IntegralCubicCurve => {
                            let segment = integral_cubic_curve_segment_iter.next().unwrap();
                            let control_points = [
                                path_fill_solid_vertices.last().unwrap().extend(1.0),
                                segment.control_points[0].extend(1.0),
                                segment.control_points[1].extend(1.0),
                                segment.control_points[2].extend(1.0),
                            ];
                            let c = control_points_power_basis(&control_points);
                            let ippc = integral_inflection_point_polynomial_coefficients(&c);
                            let (discreminant, roots) = integral_inflection_points(ippc);
                            emit_cubic_curve!(
                                proto_hull,
                                path_fill_solid_vertices,
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
                            rational_quadratic_curve_triangles.push(Vertex3(*path_fill_solid_vertices.last().unwrap(), glam::vec3(0.0, 0.0, weight)));
                            proto_hull.push(segment.control_points[0]);
                            proto_hull.push(segment.control_points[1]);
                            path_fill_solid_vertices.push(segment.control_points[1]);
                        }
                        SegmentType::RationalCubicCurve => {
                            let segment = rational_cubic_curve_segment_iter.next().unwrap();
                            let control_points = [
                                path_fill_solid_vertices.last().unwrap().extend(segment.weights[0]),
                                segment.control_points[0].extend(segment.weights[1]),
                                segment.control_points[1].extend(segment.weights[2]),
                                segment.control_points[2].extend(segment.weights[3]),
                            ];
                            let c = control_points_power_basis(&control_points);
                            let ippc = rational_inflection_point_polynomial_coefficients(&c);
                            let (discreminant, roots) = rational_inflection_points(ippc);
                            emit_cubic_curve!(
                                proto_hull,
                                path_fill_solid_vertices,
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
                let start_index = fill_solid_vertices.len();
                fill_solid_vertices.append(&mut triangle_fan_to_strip(path_fill_solid_vertices));
                let mut indices: Vec<u16> = (start_index as u16..(fill_solid_vertices.len() + 1) as u16).collect();
                *indices.iter_mut().last().unwrap() = (-1isize) as u16;
                fill_solid_indices.append(&mut indices);
            }
        }
        let convex_hull = crate::convex_hull::andrew(&proto_hull);
        let (vertex_offsets, vertex_buffer) = concat_buffers!(
            device,
            VERTEX,
            8,
            [
                stroke_solid_vertices,
                stroke_rounding_triangles,
                fill_solid_vertices,
                integral_quadratic_curve_triangles,
                integral_cubic_curve_triangles,
                rational_quadratic_curve_triangles,
                rational_cubic_curve_triangles,
                triangle_fan_to_strip(convex_hull),
            ]
        );
        let (index_offsets, index_buffer) = concat_buffers!(device, INDEX, 2, [stroke_solid_indices, fill_solid_indices,]);
        Self {
            vertex_offsets,
            index_offsets,
            vertex_buffer,
            index_buffer,
        }
    }

    /// Renderes the `Shape` into the stencil buffer.
    pub fn render_stencil<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>) {
        render_pass.set_bind_group(0, &renderer.transform_bind_group, &[]);
        render_pass.set_pipeline(&renderer.stroke_solid_pipeline);
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(0..self.vertex_offsets[0] as u64));
        render_pass.set_index_buffer(self.index_buffer.slice(0..self.index_offsets[0] as u64), wgpu::IndexFormat::Uint16);
        render_pass.draw_indexed(0..(self.index_offsets[0] / std::mem::size_of::<u16>()) as u32, 0, 0..1);
        for (i, (pipeline, vertex_size)) in [(&renderer.stroke_rounding_pipeline, std::mem::size_of::<Vertex3>())].iter().enumerate() {
            let begin_offset = self.vertex_offsets[i];
            let end_offset = self.vertex_offsets[i + 1];
            if begin_offset < end_offset {
                render_pass.set_pipeline(pipeline);
                render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(begin_offset as u64..end_offset as u64));
                render_pass.draw(0..((end_offset - begin_offset) / vertex_size) as u32, 0..1);
            }
        }
        render_pass.set_pipeline(&renderer.fill_solid_pipeline);
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(self.vertex_offsets[1] as u64..self.vertex_offsets[2] as u64));
        render_pass.set_index_buffer(
            self.index_buffer.slice(self.index_offsets[0] as u64..self.index_offsets[1] as u64),
            wgpu::IndexFormat::Uint16,
        );
        render_pass.draw_indexed(
            0..((self.index_offsets[1] - self.index_offsets[0]) / std::mem::size_of::<u16>()) as u32,
            0,
            0..1,
        );
        for (i, (pipeline, vertex_size)) in [
            (&renderer.fill_integral_quadratic_curve_pipeline, std::mem::size_of::<Vertex2>()),
            (&renderer.fill_integral_cubic_curve_pipeline, std::mem::size_of::<Vertex3>()),
            (&renderer.fill_rational_quadratic_curve_pipeline, std::mem::size_of::<Vertex3>()),
            (&renderer.fill_rational_cubic_curve_pipeline, std::mem::size_of::<Vertex4>()),
        ]
        .iter()
        .enumerate()
        {
            let begin_offset = self.vertex_offsets[i + 2];
            let end_offset = self.vertex_offsets[i + 3];
            if begin_offset < end_offset {
                render_pass.set_pipeline(pipeline);
                render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(begin_offset as u64..end_offset as u64));
                render_pass.draw(0..((end_offset - begin_offset) / vertex_size) as u32, 0..1);
            }
        }
    }

    fn render_cover<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>) {
        let begin_offset = self.vertex_offsets[6];
        let end_offset = self.vertex_offsets[7];
        render_pass.set_bind_group(0, &renderer.transform_bind_group, &[]);
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(begin_offset as u64..end_offset as u64));
        render_pass.draw(0..((end_offset - begin_offset) / std::mem::size_of::<Vertex0>()) as u32, 0..1);
    }

    /// Renderes the `Shape` into the color buffer. (Requires it to be stenciled first)
    pub fn render_color_solid<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>, clip_stack_height: usize) {
        render_pass.set_stencil_reference((clip_stack_height << renderer.winding_counter_bits) as u32);
        render_pass.set_bind_group(1, &renderer.color_solid_bind_group, &[]);
        render_pass.set_pipeline(&renderer.color_solid_pipeline);
        self.render_cover(renderer, render_pass);
    }
}

/// Use `Shape`s as stencil for other `Shape`s
///
/// When using a `ClipStack`, color is only rendered inside the logical AND (CSG intersection)
/// of all the `Shape`s on the stack with the `Shape` to be rendered.
#[derive(Default)]
pub struct ClipStack<'a> {
    stack: Vec<&'a Shape>,
}

impl<'b, 'a: 'b> ClipStack<'a> {
    /// The number of clip `Shape`s on the stack.
    pub fn height(&self) -> usize {
        self.stack.len()
    }

    /// Adds a clip `Shape` on top of the stack.
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

    /// Removes the clip `Shape` at the top of the stack.
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
     $primitive_topology:ident, $primitive_index_format:expr,
     $color_states:expr, $stencil_state:expr,
     $vertex_buffer:expr, $msaa_sample_count:expr $(,)?) => {
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
                strip_index_format: $primitive_index_format,
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
                count: $msaa_sample_count,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
        }
    };
}

/// The rendering interface for `wgpu`
pub struct Renderer {
    winding_counter_bits: usize,
    clip_nesting_counter_bits: usize,
    transform_uniform_buffer: wgpu::Buffer,
    transform_bind_group: wgpu::BindGroup,
    stroke_solid_pipeline: wgpu::RenderPipeline,
    stroke_rounding_pipeline: wgpu::RenderPipeline,
    fill_solid_pipeline: wgpu::RenderPipeline,
    fill_integral_quadratic_curve_pipeline: wgpu::RenderPipeline,
    fill_integral_cubic_curve_pipeline: wgpu::RenderPipeline,
    fill_rational_quadratic_curve_pipeline: wgpu::RenderPipeline,
    fill_rational_cubic_curve_pipeline: wgpu::RenderPipeline,
    increment_clip_nesting_counter_pipeline: wgpu::RenderPipeline,
    decrement_clip_nesting_counter_pipeline: wgpu::RenderPipeline,
    color_solid_uniform_buffer: wgpu::Buffer,
    color_solid_bind_group: wgpu::BindGroup,
    color_solid_pipeline: wgpu::RenderPipeline,
}

impl Renderer {
    /// Constructs a new `Renderer`.
    ///
    /// A `ClipStack` used with this `Renderer` can contain up to 2 to the power of `clip_nesting_counter_bits` `Shape`s.
    /// The winding rule is: Non zero modulo 2 to the power of `winding_counter_bits`.
    /// Thus, setting `winding_counter_bits` to 1 will result in the even-odd winding rule.
    /// Wgpu only supports 8 stencil bits so the sum of `clip_nesting_counter_bits` and `winding_counter_bits` can be 8 at most.
    pub fn new(
        device: &wgpu::Device,
        blending: wgpu::ColorTargetState,
        msaa_sample_count: u32,
        clip_nesting_counter_bits: usize,
        winding_counter_bits: usize,
    ) -> Result<Self, Error> {
        if winding_counter_bits == 0 || clip_nesting_counter_bits + winding_counter_bits > 8 {
            return Err(Error::NumberOfStencilBitsIsUnsupported);
        }

        let segment_0_vertex_module = device.create_shader_module(&include_spirv!("../target/shader_modules/segment0_vert.spv"));
        let segment_3_vertex_module = device.create_shader_module(&include_spirv!("../target/shader_modules/segment3_vert.spv"));
        let stencil_solid_fragment_module = device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_solid_frag.spv"));
        let stencil_rational_quadratic_curve_fragment_module =
            device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_rational_quadratic_curve_frag.spv"));
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
        let stroke_stencil_state = wgpu::StencilState {
            front: stencil_descriptor!(Equal, Keep, IncrementWrap),
            back: stencil_descriptor!(Equal, Keep, IncrementWrap),
            read_mask: clip_nesting_counter_mask | winding_counter_mask,
            write_mask: winding_counter_mask,
        };
        let fill_stencil_state = wgpu::StencilState {
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
        let stroke_solid_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &stencil_solid_fragment_module,
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            &[],
            stroke_stencil_state.clone(),
            segment_0_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let stroke_rounding_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_3_vertex_module,
            &stencil_rational_quadratic_curve_fragment_module,
            TriangleList,
            None,
            &[],
            stroke_stencil_state,
            segment_3_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let fill_solid_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &stencil_solid_fragment_module,
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            &[],
            fill_stencil_state.clone(),
            segment_0_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let fill_integral_quadratic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/segment2_vert.spv")),
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_integral_quadratic_curve_frag.spv")),
            TriangleList,
            None,
            &[],
            fill_stencil_state.clone(),
            segment_2_vertex_buffer_descriptor,
            msaa_sample_count,
        ));
        let fill_integral_cubic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_3_vertex_module,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_integral_cubic_curve_frag.spv")),
            TriangleList,
            None,
            &[],
            fill_stencil_state.clone(),
            segment_3_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let fill_rational_quadratic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_3_vertex_module,
            &stencil_rational_quadratic_curve_fragment_module,
            TriangleList,
            None,
            &[],
            fill_stencil_state.clone(),
            segment_3_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let fill_rational_cubic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/segment4_vert.spv")),
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_rational_cubic_curve_frag.spv")),
            TriangleList,
            None,
            &[],
            fill_stencil_state,
            segment_4_vertex_buffer_descriptor,
            msaa_sample_count,
        ));

        let increment_clip_nesting_counter_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &stencil_solid_fragment_module,
            TriangleStrip,
            None,
            &[],
            wgpu::StencilState {
                front: stencil_descriptor!(NotEqual, Keep, Replace),
                back: stencil_descriptor!(NotEqual, Keep, Replace),
                read_mask: winding_counter_mask,
                write_mask: clip_nesting_counter_mask | winding_counter_mask,
            },
            segment_0_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let decrement_clip_nesting_counter_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &stencil_solid_fragment_module,
            TriangleStrip,
            None,
            &[],
            wgpu::StencilState {
                front: stencil_descriptor!(Less, Keep, Replace),
                back: stencil_descriptor!(Less, Keep, Replace),
                read_mask: clip_nesting_counter_mask,
                write_mask: clip_nesting_counter_mask | winding_counter_mask,
            },
            segment_0_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));

        const COLOR_SOLID_UNIFORM_BUFFER_SIZE: usize = std::mem::size_of::<[f32; 4]>();
        let color_solid_uniform_data: [f32; 4] = [1.0, 1.0, 1.0, 1.0];
        let color_solid_uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: transmute_slice(&color_solid_uniform_data),
            usage: wgpu::BufferUsage::UNIFORM | wgpu::BufferUsage::COPY_DST,
        });
        let color_solid_bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: None,
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStage::FRAGMENT,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: wgpu::BufferSize::new(COLOR_SOLID_UNIFORM_BUFFER_SIZE as u64),
                },
                count: None,
            }],
        });
        let color_solid_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &color_solid_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Buffer {
                    buffer: &color_solid_uniform_buffer,
                    offset: 0,
                    size: wgpu::BufferSize::new(COLOR_SOLID_UNIFORM_BUFFER_SIZE as u64),
                },
            }],
            label: None,
        });
        let color_solid_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&transform_bind_group_layout, &color_solid_bind_group_layout],
            push_constant_ranges: &[],
        });
        let color_solid_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &color_solid_pipeline_layout,
            &segment_0_vertex_module,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/fill_solid_frag.spv")),
            TriangleStrip,
            None,
            &[blending],
            wgpu::StencilState {
                front: stencil_descriptor!(Less, Zero, Zero),
                back: stencil_descriptor!(Less, Zero, Zero),
                read_mask: clip_nesting_counter_mask | winding_counter_mask,
                write_mask: winding_counter_mask,
            },
            segment_0_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));

        Ok(Self {
            winding_counter_bits,
            clip_nesting_counter_bits,
            transform_uniform_buffer,
            transform_bind_group,
            stroke_solid_pipeline,
            stroke_rounding_pipeline,
            fill_solid_pipeline,
            fill_integral_quadratic_curve_pipeline,
            fill_integral_cubic_curve_pipeline,
            fill_rational_quadratic_curve_pipeline,
            fill_rational_cubic_curve_pipeline,
            increment_clip_nesting_counter_pipeline,
            decrement_clip_nesting_counter_pipeline,
            color_solid_uniform_buffer,
            color_solid_bind_group,
            color_solid_pipeline,
        })
    }

    /// Set the model view projection matrix for subsequent stencil and color rendering calls.
    ///
    /// Call before creating the next `wgpu::RenderPass`.
    pub fn set_transform(&self, queue: &wgpu::Queue, transform: &glam::Mat4) {
        let transform = transform.to_cols_array();
        let data = transmute_slice(&transform);
        queue.write_buffer(&self.transform_uniform_buffer, 0, &data);
    }

    /// Set the fill color for subsequent color rendering calls.
    ///
    /// Call before creating the next `wgpu::RenderPass`.
    pub fn set_solid_fill_color(&self, queue: &wgpu::Queue, color: &[f32; 4]) {
        let data = transmute_slice(color);
        queue.write_buffer(&self.color_solid_uniform_buffer, 0, &data);
    }
}
