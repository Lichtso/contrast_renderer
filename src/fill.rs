use crate::{
    complex_number::ComplexNumber,
    path::{Path, SegmentType},
    utils::signed_triangle_area,
    vertex::{triangle_fan_to_strip, Vertex0, Vertex2, Vertex3, Vertex4},
};
use glam::const_mat4;

const ERROR_MARGIN: f32 = 0.00001;

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
    ($fill_solid_vertices:expr, $cubic_vertices:expr,
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
        $cubic_vertices.append(&mut triangles);
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
    ($proto_hull:expr, $fill_solid_vertices:expr, $cubic_vertices:expr,
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
            triangulate_cubic_curve_quadrilateral!($fill_solid_vertices, $cubic_vertices, &control_points_a, weights_a, $v, $w, $emit_vertex);
            $fill_solid_vertices.push(control_points_b[0].truncate().into());
            for weights in &mut weights_b {
                *weights = glam::vec4(-weights[0], -weights[1], weights[2], weights[3]);
            }
            triangulate_cubic_curve_quadrilateral!($fill_solid_vertices, $cubic_vertices, &control_points_b, weights_b, $v, $w, $emit_vertex);
        } else {
            triangulate_cubic_curve_quadrilateral!($fill_solid_vertices, $cubic_vertices, $control_points, weights, $v, $w, $emit_vertex);
        }
        $proto_hull.push($control_points[1].truncate());
        $proto_hull.push($control_points[2].truncate());
        $proto_hull.push($control_points[3].truncate());
        $fill_solid_vertices.push($control_points[3].truncate());
    }};
}

#[derive(Default)]
pub struct FillBuilder {
    pub solid_indices: Vec<u16>,
    pub solid_vertices: Vec<Vertex0>,
    pub integral_quadratic_vertices: Vec<Vertex2>,
    pub integral_cubic_vertices: Vec<Vertex3>,
    pub rational_quadratic_vertices: Vec<Vertex3>,
    pub rational_cubic_vertices: Vec<Vertex4>,
}

impl FillBuilder {
    pub fn add_path(&mut self, proto_hull: &mut Vec<Vertex0>, path: &Path) {
        let mut path_solid_vertices: Vec<Vertex0> = Vec::with_capacity(
            1 + path.line_segments.len()
                + path.integral_quadratic_curve_segments.len()
                + path.integral_cubic_curve_segments.len() * 5
                + path.rational_quadratic_curve_segments.len()
                + path.rational_cubic_curve_segments.len() * 5,
        );
        path_solid_vertices.push(path.start);
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
                    path_solid_vertices.push(segment.control_points[0]);
                }
                SegmentType::IntegralQuadraticCurve => {
                    let segment = integral_quadratic_curve_segment_iter.next().unwrap();
                    self.integral_quadratic_vertices
                        .push(Vertex2(segment.control_points[1], glam::vec2(1.0, 1.0)));
                    self.integral_quadratic_vertices
                        .push(Vertex2(segment.control_points[0], glam::vec2(0.5, 0.0)));
                    self.integral_quadratic_vertices
                        .push(Vertex2(*path_solid_vertices.last().unwrap(), glam::vec2(0.0, 0.0)));
                    proto_hull.push(segment.control_points[0]);
                    proto_hull.push(segment.control_points[1]);
                    path_solid_vertices.push(segment.control_points[1]);
                }
                SegmentType::IntegralCubicCurve => {
                    let segment = integral_cubic_curve_segment_iter.next().unwrap();
                    let control_points = [
                        path_solid_vertices.last().unwrap().extend(1.0),
                        segment.control_points[0].extend(1.0),
                        segment.control_points[1].extend(1.0),
                        segment.control_points[2].extend(1.0),
                    ];
                    let c = control_points_power_basis(&control_points);
                    let ippc = integral_inflection_point_polynomial_coefficients(&c);
                    let (discreminant, roots) = integral_inflection_points(ippc);
                    emit_cubic_curve!(
                        proto_hull,
                        path_solid_vertices,
                        self.integral_cubic_vertices,
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
                    self.rational_quadratic_vertices
                        .push(Vertex3(segment.control_points[1], glam::vec3(weight, weight, weight)));
                    let weight = 1.0 / segment.weights[1];
                    self.rational_quadratic_vertices
                        .push(Vertex3(segment.control_points[0], glam::vec3(0.5 * weight, 0.0, weight)));
                    let weight = 1.0 / segment.weights[0];
                    self.rational_quadratic_vertices
                        .push(Vertex3(*path_solid_vertices.last().unwrap(), glam::vec3(0.0, 0.0, weight)));
                    proto_hull.push(segment.control_points[0]);
                    proto_hull.push(segment.control_points[1]);
                    path_solid_vertices.push(segment.control_points[1]);
                }
                SegmentType::RationalCubicCurve => {
                    let segment = rational_cubic_curve_segment_iter.next().unwrap();
                    let control_points = [
                        path_solid_vertices.last().unwrap().extend(segment.weights[0]),
                        segment.control_points[0].extend(segment.weights[1]),
                        segment.control_points[1].extend(segment.weights[2]),
                        segment.control_points[2].extend(segment.weights[3]),
                    ];
                    let c = control_points_power_basis(&control_points);
                    let ippc = rational_inflection_point_polynomial_coefficients(&c);
                    let (discreminant, roots) = rational_inflection_points(ippc);
                    emit_cubic_curve!(
                        proto_hull,
                        path_solid_vertices,
                        self.rational_cubic_vertices,
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
        let start_index = self.solid_vertices.len();
        self.solid_vertices.append(&mut triangle_fan_to_strip(path_solid_vertices));
        let mut indices: Vec<u16> = (start_index as u16..(self.solid_vertices.len() + 1) as u16).collect();
        *indices.iter_mut().last().unwrap() = (-1isize) as u16;
        self.solid_indices.append(&mut indices);
    }
}
