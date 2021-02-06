use crate::{
    complex_number::ComplexNumber,
    path_builder::{PathBuilder, SegmentType},
    renderer::{Vertex0, Vertex2, Vertex3, Vertex4},
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
    glam::Mat4::from_cols(
        control_points[0].extend(0.0),
        control_points[1].extend(0.0),
        control_points[2].extend(0.0),
        control_points[3].extend(0.0),
    ) * CUBIC_POWER_BASIS
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
        let a = glam::vec3(control_points[0][0], control_points[0][1], weights.x_axis[i]) / control_points[0][2];
        let b = glam::vec3(control_points[1][0], control_points[1][1], weights.y_axis[i]) / control_points[1][2];
        let c = glam::vec3(control_points[2][0], control_points[2][1], weights.z_axis[i]) / control_points[2][2];
        let mut n = (b - a).cross(c - a);
        if n.length_squared() < ERROR_MARGIN {
            let d = glam::vec3(control_points[3][0], control_points[3][1], weights.z_axis[i]) / control_points[3][2];
            n = (b - a).cross(d - a);
        }
        n = n.normalize();
        if n[2] < 0.0 {
            n *= -1.0;
        }
        *plane = glam::vec4(n[0], n[1], n[2], n.dot(a));
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
        let mut weights = weights($discreminant, $roots);
        let mut planes = weight_planes($control_points, &weights);
        let gradient = implicit_curve_gradient(&planes, weights.x_axis);
        normalize_implicit_curve_side(&mut planes, &mut weights, $c, gradient);
        let weights = [weights.x_axis, weights.y_axis, weights.z_axis, weights.w_axis];
        if let Some(param) = find_double_point_issue($discreminant, $roots) {
            let (control_points_a, control_points_b) = split_curve_at!($control_points, param);
            let (weights_a, mut weights_b) = split_curve_at!(&weights, param);
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

pub struct Path {
    pub anchors: Vec<Vertex0>,
    pub proto_hull: Option<Vec<Vertex0>>,
    pub integral_quadratic_curve_triangles: Option<Vec<Vertex2>>,
    pub integral_cubic_curve_triangles: Option<Vec<Vertex3>>,
    pub rational_quadratic_curve_triangles: Option<Vec<Vertex3>>,
    pub rational_cubic_curve_triangles: Option<Vec<Vertex4>>,
}

impl Path {
    pub fn from_path_builder(path_builder: &PathBuilder) -> Self {
        let mut line_segment_iter = path_builder.line_segments.iter();
        let mut integral_quadratic_curve_segment_iter = path_builder.integral_quadratic_curve_segments.iter();
        let mut integral_cubic_curve_segment_iter = path_builder.integral_cubic_curve_segments.iter();
        let mut rational_quadratic_curve_segment_iter = path_builder.rational_quadratic_curve_segments.iter();
        let mut rational_cubic_curve_segment_iter = path_builder.rational_cubic_curve_segments.iter();
        let mut anchors = Vec::with_capacity(
            1 + path_builder.line_segments.len()
                + path_builder.integral_quadratic_curve_segments.len()
                + path_builder.integral_cubic_curve_segments.len() * 3
                + path_builder.rational_quadratic_curve_segments.len()
                + path_builder.rational_cubic_curve_segments.len() * 3,
        );
        anchors.push(path_builder.anchor);
        let mut proto_hull = Vec::with_capacity(
            1 + path_builder.line_segments.len()
                + path_builder.integral_quadratic_curve_segments.len() * 2
                + path_builder.integral_cubic_curve_segments.len() * 3
                + path_builder.rational_quadratic_curve_segments.len() * 2
                + path_builder.rational_cubic_curve_segments.len() * 3,
        );
        proto_hull.push(path_builder.anchor);
        let mut integral_quadratic_curve_triangles = Vec::with_capacity(path_builder.integral_quadratic_curve_segments.len() * 3);
        let mut integral_cubic_curve_triangles = Vec::with_capacity(path_builder.integral_cubic_curve_segments.len() * 6);
        let mut rational_quadratic_curve_triangles = Vec::with_capacity(path_builder.rational_quadratic_curve_segments.len() * 3);
        let mut rational_cubic_curve_triangles = Vec::with_capacity(path_builder.rational_cubic_curve_segments.len() * 6);
        for segement_type in &path_builder.segement_types {
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
                        &mut proto_hull,
                        &mut anchors,
                        &mut integral_cubic_curve_triangles,
                        &control_points,
                        &c,
                        discreminant,
                        &roots,
                        v,
                        w,
                        Vertex3(v, w.truncate())
                    );
                }
                SegmentType::RationalQuadraticCurve => {
                    let segment = rational_quadratic_curve_segment_iter.next().unwrap();
                    let weight = 1.0 / segment.control_points[1][2];
                    rational_quadratic_curve_triangles.push(Vertex3(segment.control_points[1].truncate(), glam::vec3(weight, weight, weight)));
                    let weight = 1.0 / segment.control_points[0][2];
                    rational_quadratic_curve_triangles.push(Vertex3(segment.control_points[0].truncate(), glam::vec3(0.5 * weight, 0.0, weight)));
                    rational_quadratic_curve_triangles.push(Vertex3(*anchors.last().unwrap(), glam::vec3(0.0, 0.0, 1.0 / segment.first_weight)));
                    proto_hull.push(segment.control_points[0].truncate());
                    proto_hull.push(segment.control_points[1].truncate());
                    anchors.push(segment.control_points[1].truncate());
                }
                SegmentType::RationalCubicCurve => {
                    let segment = rational_cubic_curve_segment_iter.next().unwrap();
                    let mut control_points = [
                        previous_anchor.extend(segment.first_weight),
                        segment.control_points[0],
                        segment.control_points[1],
                        segment.control_points[2],
                    ];
                    for control_point in &mut control_points {
                        *control_point *= glam::vec3(control_point[2], control_point[2], 1.0);
                    }
                    let c = control_points_power_basis(&control_points);
                    let ippc = rational_inflection_point_polynomial_coefficients(&c);
                    let (discreminant, roots) = rational_inflection_points(ippc);
                    emit_cubic_curve!(
                        &mut proto_hull,
                        &mut anchors,
                        &mut rational_cubic_curve_triangles,
                        &control_points,
                        &c,
                        discreminant,
                        &roots,
                        v,
                        w,
                        Vertex4(v, w)
                    );
                }
            }
        }
        Self {
            anchors,
            proto_hull: Some(proto_hull),
            integral_quadratic_curve_triangles: if integral_quadratic_curve_triangles.is_empty() {
                None
            } else {
                Some(integral_quadratic_curve_triangles)
            },
            integral_cubic_curve_triangles: if integral_cubic_curve_triangles.is_empty() {
                None
            } else {
                Some(integral_cubic_curve_triangles)
            },
            rational_quadratic_curve_triangles: if rational_quadratic_curve_triangles.is_empty() {
                None
            } else {
                Some(rational_quadratic_curve_triangles)
            },
            rational_cubic_curve_triangles: if rational_cubic_curve_triangles.is_empty() {
                None
            } else {
                Some(rational_cubic_curve_triangles)
            },
        }
    }

    pub fn from_polygon(anchors: Vec<glam::Vec2>) -> Self {
        Self {
            anchors,
            proto_hull: None,
            integral_quadratic_curve_triangles: None,
            integral_cubic_curve_triangles: None,
            rational_quadratic_curve_triangles: None,
            rational_cubic_curve_triangles: None,
        }
    }

    pub fn from_rect(center: glam::Vec2, half_extent: glam::Vec2) -> Self {
        let anchors = vec![
            glam::vec2(center[0] - half_extent[0], center[1] - half_extent[1]),
            glam::vec2(center[0] - half_extent[0], center[1] + half_extent[1]),
            glam::vec2(center[0] + half_extent[0], center[1] + half_extent[1]),
            glam::vec2(center[0] + half_extent[0], center[1] - half_extent[1]),
        ];
        Self {
            anchors,
            proto_hull: None,
            integral_quadratic_curve_triangles: None,
            integral_cubic_curve_triangles: None,
            rational_quadratic_curve_triangles: None,
            rational_cubic_curve_triangles: None,
        }
    }

    pub fn from_rounded_rect(center: glam::Vec2, half_extent: glam::Vec2, radius: f32) -> Self {
        let radius = radius.min(half_extent[0]).min(half_extent[1]);
        let anchors = vec![
            glam::vec2(center[0] - half_extent[0] + radius, center[1] - half_extent[1]),
            glam::vec2(center[0] - half_extent[0], center[1] - half_extent[1] + radius),
            glam::vec2(center[0] - half_extent[0], center[1] + half_extent[1] - radius),
            glam::vec2(center[0] - half_extent[0] + radius, center[1] + half_extent[1]),
            glam::vec2(center[0] + half_extent[0] - radius, center[1] + half_extent[1]),
            glam::vec2(center[0] + half_extent[0], center[1] + half_extent[1] - radius),
            glam::vec2(center[0] + half_extent[0], center[1] - half_extent[1] + radius),
            glam::vec2(center[0] + half_extent[0] - radius, center[1] - half_extent[1]),
        ];
        let hull = vec![
            glam::vec2(center[0] - half_extent[0], center[1] - half_extent[1]),
            glam::vec2(center[0] - half_extent[0], center[1] + half_extent[1]),
            glam::vec2(center[0] + half_extent[0], center[1] + half_extent[1]),
            glam::vec2(center[0] + half_extent[0], center[1] - half_extent[1]),
        ];
        let sqrt_2 = 2.0f32.sqrt();
        let half_sqrt_2 = 0.5 * sqrt_2;
        let rational_quadratic_curve_triangles = vec![
            Vertex3(anchors[1], glam::vec3(1.0, 1.0, 1.0)),
            Vertex3(hull[0], glam::vec3(half_sqrt_2, 0.0, sqrt_2)),
            Vertex3(anchors[0], glam::vec3(0.0, 0.0, 1.0)),
            Vertex3(anchors[3], glam::vec3(1.0, 1.0, 1.0)),
            Vertex3(hull[1], glam::vec3(half_sqrt_2, 0.0, sqrt_2)),
            Vertex3(anchors[2], glam::vec3(0.0, 0.0, 1.0)),
            Vertex3(anchors[5], glam::vec3(1.0, 1.0, 1.0)),
            Vertex3(hull[2], glam::vec3(half_sqrt_2, 0.0, sqrt_2)),
            Vertex3(anchors[4], glam::vec3(0.0, 0.0, 1.0)),
            Vertex3(anchors[7], glam::vec3(1.0, 1.0, 1.0)),
            Vertex3(hull[3], glam::vec3(half_sqrt_2, 0.0, sqrt_2)),
            Vertex3(anchors[6], glam::vec3(0.0, 0.0, 1.0)),
        ];
        Self {
            anchors,
            proto_hull: Some(hull),
            integral_quadratic_curve_triangles: None,
            integral_cubic_curve_triangles: None,
            rational_quadratic_curve_triangles: Some(rational_quadratic_curve_triangles),
            rational_cubic_curve_triangles: None,
        }
    }

    pub fn from_ellipse(center: glam::Vec2, half_extent: glam::Vec2) -> Self {
        let anchors = vec![
            glam::vec2(center[0] - half_extent[0], center[1]),
            glam::vec2(center[0], center[1] + half_extent[1]),
            glam::vec2(center[0] + half_extent[0], center[1]),
            glam::vec2(center[0], center[1] - half_extent[1]),
        ];
        let hull = vec![
            glam::vec2(center[0] - half_extent[0], center[1] - half_extent[1]),
            glam::vec2(center[0] - half_extent[0], center[1] + half_extent[1]),
            glam::vec2(center[0] + half_extent[0], center[1] + half_extent[1]),
            glam::vec2(center[0] + half_extent[0], center[1] - half_extent[1]),
        ];
        let sqrt_2 = 2.0f32.sqrt();
        let half_sqrt_2 = 0.5 * sqrt_2;
        let rational_quadratic_curve_triangles = vec![
            Vertex3(anchors[0], glam::vec3(1.0, 1.0, 1.0)),
            Vertex3(hull[0], glam::vec3(half_sqrt_2, 0.0, sqrt_2)),
            Vertex3(anchors[3], glam::vec3(0.0, 0.0, 1.0)),
            Vertex3(anchors[1], glam::vec3(1.0, 1.0, 1.0)),
            Vertex3(hull[1], glam::vec3(half_sqrt_2, 0.0, sqrt_2)),
            Vertex3(anchors[0], glam::vec3(0.0, 0.0, 1.0)),
            Vertex3(anchors[2], glam::vec3(1.0, 1.0, 1.0)),
            Vertex3(hull[2], glam::vec3(half_sqrt_2, 0.0, sqrt_2)),
            Vertex3(anchors[1], glam::vec3(0.0, 0.0, 1.0)),
            Vertex3(anchors[3], glam::vec3(1.0, 1.0, 1.0)),
            Vertex3(hull[3], glam::vec3(half_sqrt_2, 0.0, sqrt_2)),
            Vertex3(anchors[2], glam::vec3(0.0, 0.0, 1.0)),
        ];
        Self {
            anchors,
            proto_hull: Some(hull),
            integral_quadratic_curve_triangles: None,
            integral_cubic_curve_triangles: None,
            rational_quadratic_curve_triangles: Some(rational_quadratic_curve_triangles),
            rational_cubic_curve_triangles: None,
        }
    }

    pub fn from_circle(center: glam::Vec2, radius: f32) -> Self {
        Self::from_ellipse(center, glam::vec2(radius, radius))
    }

    pub fn reverse(&mut self) {
        self.anchors.reverse();
        if let Some(triangles) = &mut self.integral_quadratic_curve_triangles {
            triangles.reverse();
        }
        if let Some(triangles) = &mut self.integral_cubic_curve_triangles {
            triangles.reverse();
        }
        if let Some(triangles) = &mut self.rational_quadratic_curve_triangles {
            triangles.reverse();
        }
        if let Some(triangles) = &mut self.rational_cubic_curve_triangles {
            triangles.reverse();
        }
    }

    pub fn transform(&mut self, transform: glam::Mat3) {
        for control_point in &mut self.anchors {
            *control_point = transform.transform_point2(*control_point);
        }
        if let Some(proto_hull) = &mut self.proto_hull {
            for control_point in proto_hull {
                *control_point = transform.transform_point2(*control_point);
            }
        }
        if let Some(triangles) = &mut self.integral_quadratic_curve_triangles {
            for control_point in triangles {
                control_point.0 = transform.transform_point2(control_point.0);
            }
        }
        if let Some(triangles) = &mut self.integral_cubic_curve_triangles {
            for control_point in triangles {
                control_point.0 = transform.transform_point2(control_point.0);
            }
        }
        if let Some(triangles) = &mut self.rational_quadratic_curve_triangles {
            for control_point in triangles {
                control_point.0 = transform.transform_point2(control_point.0);
            }
        }
        if let Some(triangles) = &mut self.rational_cubic_curve_triangles {
            for control_point in triangles {
                control_point.0 = transform.transform_point2(control_point.0);
            }
        }
    }
}
