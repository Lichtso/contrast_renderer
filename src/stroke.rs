use crate::{
    curve::{
        integral_cubic_uniform_tangent_angle, integral_quadratic_uniform_tangent_angle, rational_cubic_control_points_to_power_basis,
        rational_cubic_first_order_derivative, rational_cubic_point, rational_cubic_uniform_tangent_angle,
        rational_quadratic_control_points_to_power_basis, rational_quadratic_first_order_derivative, rational_quadratic_point,
        rational_quadratic_uniform_tangent_angle,
    },
    error::{Error, ERROR_MARGIN},
    path::{CurveApproximation, Path, SegmentType, StrokeOptions},
    safe_float::SafeFloat,
    utils::{line_line_intersection, point_to_vec, rotate_90_degree_clockwise, vec_to_point, weighted_vec_to_point},
    vertex::{Vertex2f1i, Vertex3f1i},
};
use geometric_algebra::{ppga2d, Dual, GeometricProduct, InnerProduct, Magnitude, OuterProduct, RegressiveProduct, Signum, SquaredMagnitude, Zero};

fn offset_control_point(control_point: ppga2d::Point, tangent: ppga2d::Plane, offset: f32) -> ppga2d::Point {
    let mut direction = tangent.dual();
    direction.g0[0] = 0.0;
    direction *= ppga2d::Scalar { g0: offset };
    control_point + direction
}

fn emit_stroke_vertex(path_line_vertices: &mut Vec<Vertex2f1i>, path_index: usize, offset_along_path: f32, vertex: ppga2d::Point, side: f32) {
    path_line_vertices.push(Vertex2f1i(point_to_vec(vertex), [side, offset_along_path], path_index as u32));
}

fn emit_stroke_vertices(
    builder: &mut StrokeBuilder,
    stroke_options: &StrokeOptions,
    path_index: usize,
    length_accumulator: f32,
    point: ppga2d::Point,
    tangent: ppga2d::Plane,
) {
    let offset_along_path = length_accumulator / stroke_options.width.unwrap();
    emit_stroke_vertex(
        &mut builder.path_line_vertices,
        path_index,
        offset_along_path,
        offset_control_point(point, tangent, (stroke_options.offset.unwrap() - 0.5) * stroke_options.width.unwrap()),
        -0.5,
    );
    emit_stroke_vertex(
        &mut builder.path_line_vertices,
        path_index,
        offset_along_path,
        offset_control_point(point, tangent, (stroke_options.offset.unwrap() + 0.5) * stroke_options.width.unwrap()),
        0.5,
    );
}

fn emit_stroke_join(
    builder: &mut StrokeBuilder,
    proto_hull: &mut Vec<SafeFloat<f32, 2>>,
    stroke_options: &StrokeOptions,
    length_accumulator: &mut f32,
    control_point: ppga2d::Point,
    previous_tangent: ppga2d::Plane,
    next_tangent: ppga2d::Plane,
) {
    let tangets_dot_product = previous_tangent.inner_product(next_tangent).g0;
    if (tangets_dot_product - 1.0).abs() <= ERROR_MARGIN {
        return;
    }
    let side_sign = previous_tangent.outer_product(next_tangent).g0[0].signum();
    let miter_clip = stroke_options.width.unwrap() * stroke_options.miter_clip.unwrap();
    let side_offset = (stroke_options.offset.unwrap() - side_sign * 0.5) * stroke_options.width.unwrap();
    let previous_edge_vertex = offset_control_point(control_point, previous_tangent, side_offset);
    let next_edge_vertex = offset_control_point(control_point, next_tangent, side_offset);
    let previous_edge_tangent: ppga2d::Plane = previous_tangent
        .inner_product(previous_edge_vertex)
        .geometric_product(previous_edge_vertex)
        .into();
    let next_edge_tangent: ppga2d::Plane = next_tangent.inner_product(next_edge_vertex).geometric_product(next_edge_vertex).into();
    let intersection = line_line_intersection(previous_edge_tangent, next_edge_tangent);
    let mut vertices = [control_point, previous_edge_vertex, next_edge_vertex, intersection, intersection];
    let anti_parallel = (tangets_dot_product + 1.0).abs() <= ERROR_MARGIN;
    if anti_parallel || control_point.regressive_product(intersection).magnitude().g0 > miter_clip {
        let mid_tangent = if anti_parallel {
            -rotate_90_degree_clockwise(previous_tangent)
        } else {
            (previous_tangent + next_tangent).signum()
        };
        let clipping_vertex = offset_control_point(control_point, mid_tangent, -side_sign * miter_clip);
        let clipping_plane: ppga2d::Plane = mid_tangent.inner_product(clipping_vertex).geometric_product(clipping_vertex).into();
        vertices[3] = line_line_intersection(previous_edge_tangent, clipping_plane);
        vertices[4] = line_line_intersection(clipping_plane, next_edge_tangent);
        proto_hull.push(point_to_vec(vertices[3]).into());
        proto_hull.push(point_to_vec(vertices[4]).into());
    } else {
        proto_hull.push(point_to_vec(vertices[3]).into());
    }
    let scaled_tangent = previous_tangent
        / ppga2d::Scalar {
            g0: -stroke_options.width.unwrap(),
        };
    let start_index = builder.joint_vertices.len();
    let offset_along_path = *length_accumulator / stroke_options.width.unwrap();
    for vertex in vertices.iter() {
        builder.joint_vertices.push(Vertex3f1i(
            point_to_vec(*vertex),
            [
                side_sign * vertex.regressive_product(scaled_tangent).g0,
                vertex.regressive_product(control_point).inner_product(scaled_tangent).g0,
                offset_along_path,
            ],
            stroke_options.dynamic_stroke_options_group as u32,
        ));
    }
    let mut indices: Vec<u16> = (start_index as u16..(builder.joint_vertices.len() + 1) as u16).collect();
    *indices.iter_mut().last().unwrap() = (-1isize) as u16;
    builder.joint_indices.append(&mut indices);
    *length_accumulator += tangets_dot_product.acos() / (std::f32::consts::PI * 2.0) * stroke_options.width.unwrap();
    cut_stroke_polygon(builder, proto_hull);
    emit_stroke_vertices(
        builder,
        stroke_options,
        stroke_options.dynamic_stroke_options_group,
        *length_accumulator,
        control_point,
        next_tangent,
    );
}

fn cut_stroke_polygon(builder: &mut StrokeBuilder, proto_hull: &mut Vec<SafeFloat<f32, 2>>) {
    if !builder.path_line_vertices.is_empty() {
        proto_hull.append(&mut builder.path_line_vertices.iter().map(|vertex| vertex.0.into()).collect());
        let start_index = builder.line_vertices.len();
        builder.line_vertices.append(&mut builder.path_line_vertices);
        let mut indices: Vec<u16> = (start_index as u16..(builder.line_vertices.len() + 1) as u16).collect();
        *indices.iter_mut().last().unwrap() = (-1isize) as u16;
        builder.line_indices.append(&mut indices);
    }
}

macro_rules! emit_curve_stroke {
    ($builder:expr, $stroke_options:expr, $length_accumulator:expr,
     $previous_control_point:expr, $power_basis:expr, $angle_step:ident, $uniform_tangent_angle:expr,
     $point:ident, $tangent:ident $(,)?) => {
        let parameters: Vec<f32> = match $stroke_options.curve_approximation {
            CurveApproximation::UniformlySpacedParameters(steps) => (1..steps + 1).map(|i| i as f32 / steps as f32).collect(),
            CurveApproximation::UniformTangentAngle($angle_step) => $uniform_tangent_angle,
        };
        let mut previous_point = $previous_control_point;
        for mut t in parameters {
            let mut tangent = $tangent(&$power_basis, t);
            if tangent.squared_magnitude().g0 == 0.0 {
                if t < 0.5 {
                    t += f32::EPSILON;
                } else {
                    t -= f32::EPSILON;
                }
                tangent = $tangent(&$power_basis, t);
            }
            tangent = tangent.signum();
            let mut point = $point(&$power_basis, t);
            point = point / ppga2d::Scalar { g0: point.g0[0] };
            $length_accumulator += previous_point.regressive_product(point).magnitude().g0;
            emit_stroke_vertices(
                $builder,
                &$stroke_options,
                $stroke_options.dynamic_stroke_options_group,
                $length_accumulator,
                point,
                tangent,
            );
            previous_point = point;
        }
    };
}

#[derive(Default)]
pub struct StrokeBuilder {
    pub line_indices: Vec<u16>,
    pub joint_indices: Vec<u16>,
    pub line_vertices: Vec<Vertex2f1i>,
    pub joint_vertices: Vec<Vertex3f1i>,
    path_line_vertices: Vec<Vertex2f1i>,
}

impl StrokeBuilder {
    pub fn add_path(&mut self, proto_hull: &mut Vec<SafeFloat<f32, 2>>, path: &Path) -> Result<(), Error> {
        let stroke_options = path.stroke_options.as_ref().unwrap();
        let mut previous_control_point = vec_to_point(path.start.unwrap());
        let mut first_tangent = ppga2d::Plane::zero();
        let mut previous_tangent = ppga2d::Plane::zero();
        let mut line_segment_iter = path.line_segments.iter();
        let mut integral_quadratic_curve_segment_iter = path.integral_quadratic_curve_segments.iter().peekable();
        let mut integral_cubic_curve_segment_iter = path.integral_cubic_curve_segments.iter().peekable();
        let mut rational_quadratic_curve_segment_iter = path.rational_quadratic_curve_segments.iter().peekable();
        let mut rational_cubic_curve_segment_iter = path.rational_cubic_curve_segments.iter().peekable();
        let mut length_accumulator = 0.0;
        for (i, segment_type) in path.segment_types.iter().enumerate() {
            let next_control_point;
            let segment_start_tangent;
            let segment_end_tangent;
            match segment_type {
                SegmentType::Line => {
                    let segment = line_segment_iter.next().unwrap();
                    next_control_point = vec_to_point(segment.control_points[0].unwrap());
                    segment_start_tangent = previous_control_point.regressive_product(next_control_point).signum();
                    segment_end_tangent = segment_start_tangent;
                }
                SegmentType::IntegralQuadraticCurve => {
                    let segment = integral_quadratic_curve_segment_iter.peek().unwrap();
                    next_control_point = vec_to_point(segment.control_points[1].unwrap());
                    segment_start_tangent = previous_control_point
                        .regressive_product(vec_to_point(segment.control_points[0].unwrap()))
                        .signum();
                    segment_end_tangent = vec_to_point(segment.control_points[0].unwrap())
                        .regressive_product(next_control_point)
                        .signum();
                }
                SegmentType::IntegralCubicCurve => {
                    let segment = integral_cubic_curve_segment_iter.peek().unwrap();
                    next_control_point = vec_to_point(segment.control_points[2].unwrap());
                    segment_start_tangent = previous_control_point
                        .regressive_product(vec_to_point(segment.control_points[0].unwrap()))
                        .signum();
                    segment_end_tangent = vec_to_point(segment.control_points[1].unwrap())
                        .regressive_product(next_control_point)
                        .signum();
                }
                SegmentType::RationalQuadraticCurve => {
                    let segment = rational_quadratic_curve_segment_iter.peek().unwrap();
                    next_control_point = vec_to_point(segment.control_points[1].unwrap());
                    segment_start_tangent = previous_control_point
                        .regressive_product(vec_to_point(segment.control_points[0].unwrap()))
                        .signum();
                    segment_end_tangent = vec_to_point(segment.control_points[0].unwrap())
                        .regressive_product(next_control_point)
                        .signum();
                }
                SegmentType::RationalCubicCurve => {
                    let segment = rational_cubic_curve_segment_iter.peek().unwrap();
                    next_control_point = vec_to_point(segment.control_points[2].unwrap());
                    segment_start_tangent = previous_control_point
                        .regressive_product(vec_to_point(segment.control_points[0].unwrap()))
                        .signum();
                    segment_end_tangent = vec_to_point(segment.control_points[1].unwrap())
                        .regressive_product(next_control_point)
                        .signum();
                }
            }
            if i == 0 {
                first_tangent = segment_start_tangent;
                if !stroke_options.closed {
                    let normal = rotate_90_degree_clockwise(segment_start_tangent);
                    emit_stroke_vertices(
                        self,
                        &stroke_options,
                        stroke_options.dynamic_stroke_options_group,
                        length_accumulator - 0.5 * stroke_options.width.unwrap(),
                        offset_control_point(previous_control_point, normal, 0.5 * stroke_options.width.unwrap().abs()),
                        segment_start_tangent,
                    );
                }
                if stroke_options.closed || *segment_type != SegmentType::Line {
                    emit_stroke_vertices(
                        self,
                        &stroke_options,
                        stroke_options.dynamic_stroke_options_group,
                        length_accumulator,
                        previous_control_point,
                        segment_start_tangent,
                    );
                }
            } else {
                emit_stroke_join(
                    self,
                    proto_hull,
                    &stroke_options,
                    &mut length_accumulator,
                    previous_control_point,
                    previous_tangent,
                    segment_start_tangent,
                );
            }
            match segment_type {
                SegmentType::Line => {
                    length_accumulator += previous_control_point.regressive_product(next_control_point).magnitude().g0;
                    emit_stroke_vertices(
                        self,
                        &stroke_options,
                        stroke_options.dynamic_stroke_options_group,
                        length_accumulator,
                        next_control_point,
                        segment_end_tangent,
                    );
                }
                SegmentType::IntegralQuadraticCurve => {
                    let segment = integral_quadratic_curve_segment_iter.next().unwrap();
                    let power_basis = rational_quadratic_control_points_to_power_basis(&[
                        previous_control_point,
                        vec_to_point(segment.control_points[0].unwrap()),
                        vec_to_point(segment.control_points[1].unwrap()),
                    ]);
                    emit_curve_stroke!(
                        self,
                        stroke_options,
                        length_accumulator,
                        previous_control_point,
                        power_basis,
                        angle_step,
                        integral_quadratic_uniform_tangent_angle(&power_basis, segment_start_tangent, segment_end_tangent, angle_step.unwrap()),
                        rational_quadratic_point,
                        rational_quadratic_first_order_derivative,
                    );
                }
                SegmentType::IntegralCubicCurve => {
                    let segment = integral_cubic_curve_segment_iter.next().unwrap();
                    let power_basis = rational_cubic_control_points_to_power_basis(&[
                        previous_control_point,
                        vec_to_point(segment.control_points[0].unwrap()),
                        vec_to_point(segment.control_points[1].unwrap()),
                        vec_to_point(segment.control_points[2].unwrap()),
                    ]);
                    emit_curve_stroke!(
                        self,
                        stroke_options,
                        length_accumulator,
                        previous_control_point,
                        power_basis,
                        angle_step,
                        integral_cubic_uniform_tangent_angle(&power_basis, angle_step.unwrap()),
                        rational_cubic_point,
                        rational_cubic_first_order_derivative,
                    );
                }
                SegmentType::RationalQuadraticCurve => {
                    let segment = rational_quadratic_curve_segment_iter.next().unwrap();
                    let power_basis = rational_quadratic_control_points_to_power_basis(&[
                        previous_control_point,
                        weighted_vec_to_point(segment.weight.unwrap(), segment.control_points[0].unwrap()),
                        vec_to_point(segment.control_points[1].unwrap()),
                    ]);
                    emit_curve_stroke!(
                        self,
                        stroke_options,
                        length_accumulator,
                        previous_control_point,
                        power_basis,
                        angle_step,
                        rational_quadratic_uniform_tangent_angle(&power_basis, segment_start_tangent, segment_end_tangent, angle_step.unwrap()),
                        rational_quadratic_point,
                        rational_quadratic_first_order_derivative,
                    );
                }
                SegmentType::RationalCubicCurve => {
                    let segment = rational_cubic_curve_segment_iter.next().unwrap();
                    let weights = segment.weights.unwrap();
                    let power_basis = rational_cubic_control_points_to_power_basis(&[
                        weighted_vec_to_point(weights[0], point_to_vec(previous_control_point)),
                        weighted_vec_to_point(weights[1], segment.control_points[0].unwrap()),
                        weighted_vec_to_point(weights[2], segment.control_points[1].unwrap()),
                        weighted_vec_to_point(weights[3], segment.control_points[2].unwrap()),
                    ]);
                    emit_curve_stroke!(
                        self,
                        stroke_options,
                        length_accumulator,
                        previous_control_point,
                        power_basis,
                        angle_step,
                        rational_cubic_uniform_tangent_angle(&power_basis, angle_step.unwrap()),
                        rational_cubic_point,
                        rational_cubic_first_order_derivative,
                    );
                }
            }
            previous_control_point = next_control_point;
            previous_tangent = segment_end_tangent;
        }
        if stroke_options.closed {
            let line_segment = previous_control_point.regressive_product(vec_to_point(path.start.unwrap()));
            let length = line_segment.magnitude();
            if length.g0 > 0.0 {
                let segment_tangent = line_segment / length;
                emit_stroke_join(
                    self,
                    proto_hull,
                    &stroke_options,
                    &mut length_accumulator,
                    previous_control_point,
                    previous_tangent,
                    segment_tangent,
                );
                length_accumulator += length.g0;
                emit_stroke_vertices(
                    self,
                    &stroke_options,
                    stroke_options.dynamic_stroke_options_group,
                    length_accumulator,
                    vec_to_point(path.start.unwrap()),
                    segment_tangent,
                );
                emit_stroke_join(
                    self,
                    proto_hull,
                    &stroke_options,
                    &mut length_accumulator,
                    vec_to_point(path.start.unwrap()),
                    segment_tangent,
                    first_tangent,
                );
            } else {
                emit_stroke_join(
                    self,
                    proto_hull,
                    &stroke_options,
                    &mut length_accumulator,
                    vec_to_point(path.start.unwrap()),
                    previous_tangent,
                    first_tangent,
                );
            }
        } else {
            cut_stroke_polygon(self, proto_hull);
            emit_stroke_vertices(
                self,
                &stroke_options,
                stroke_options.dynamic_stroke_options_group | 0x10000,
                length_accumulator,
                previous_control_point,
                previous_tangent,
            );
            let normal = rotate_90_degree_clockwise(previous_tangent);
            emit_stroke_vertices(
                self,
                &stroke_options,
                stroke_options.dynamic_stroke_options_group | 0x10000,
                length_accumulator + 0.5 * stroke_options.width.unwrap(),
                offset_control_point(previous_control_point, normal, -0.5 * stroke_options.width.unwrap().abs()),
                previous_tangent,
            );
        }
        cut_stroke_polygon(self, proto_hull);
        Ok(())
    }
}
