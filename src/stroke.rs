use crate::{
    curve::{
        integral_cubic_uniform_tangent_angle, integral_quadratic_uniform_tangent_angle, rational_cubic_control_points_to_power_basis,
        rational_cubic_first_order_derivative, rational_cubic_point, rational_cubic_uniform_tangent_angle,
        rational_quadratic_control_points_to_power_basis, rational_quadratic_first_order_derivative, rational_quadratic_point,
        rational_quadratic_uniform_tangent_angle,
    },
    error::{Error, ERROR_MARGIN},
    path::{CurveApproximation, Path, SegmentType, StrokeOptions},
    utils::{line_line_intersection, rotate_90_degree_clockwise},
    vertex::{Vertex0, Vertex2f1i, Vertex3f1i},
};

fn emit_stroke_vertex(path_line_vertices: &mut Vec<Vertex2f1i>, path_index: usize, offset_along_path: f32, vertex: glam::Vec2, side: f32) {
    path_line_vertices.push(Vertex2f1i(vertex, glam::vec2(side, offset_along_path), path_index as u32));
}

fn emit_stroke_vertices(
    builder: &mut StrokeBuilder,
    stroke_options: &StrokeOptions,
    path_index: usize,
    length_accumulator: f32,
    point: glam::Vec2,
    tangent: glam::Vec2,
) {
    let offset_along_path = length_accumulator / stroke_options.width;
    let normal = rotate_90_degree_clockwise(tangent);
    emit_stroke_vertex(
        &mut builder.path_line_vertices,
        path_index,
        offset_along_path,
        point + normal * ((stroke_options.offset - 0.5) * stroke_options.width),
        -0.5,
    );
    emit_stroke_vertex(
        &mut builder.path_line_vertices,
        path_index,
        offset_along_path,
        point + normal * ((stroke_options.offset + 0.5) * stroke_options.width),
        0.5,
    );
}

fn emit_stroke_join(
    builder: &mut StrokeBuilder,
    proto_hull: &mut Vec<Vertex0>,
    stroke_options: &StrokeOptions,
    length_accumulator: &mut f32,
    control_point: glam::Vec2,
    previous_tangent: glam::Vec2,
    next_tangent: glam::Vec2,
) {
    let tangets_dot_product = previous_tangent.dot(next_tangent);
    if (tangets_dot_product - 1.0).abs() <= ERROR_MARGIN {
        return;
    }
    let side_sign = rotate_90_degree_clockwise(previous_tangent).dot(next_tangent).signum();
    let miter_clip = stroke_options.width * stroke_options.miter_clip;
    let previous_normal = rotate_90_degree_clockwise(previous_tangent);
    let next_normal = rotate_90_degree_clockwise(next_tangent);
    let previous_edge_vertex = control_point + previous_normal * ((stroke_options.offset - side_sign * 0.5) * stroke_options.width);
    let next_edge_vertex = control_point + next_normal * ((stroke_options.offset - side_sign * 0.5) * stroke_options.width);
    let intersection = line_line_intersection((previous_edge_vertex, previous_tangent), (next_edge_vertex, next_tangent));
    let mut vertices = [control_point, previous_edge_vertex, next_edge_vertex, intersection, intersection];
    let anti_parallel = (tangets_dot_product + 1.0).abs() <= ERROR_MARGIN;
    if anti_parallel || control_point.distance(intersection) > miter_clip {
        let mid_tangent = if anti_parallel {
            rotate_90_degree_clockwise(-previous_tangent)
        } else {
            (previous_tangent + next_tangent).normalize()
        };
        let mid_normal = rotate_90_degree_clockwise(mid_tangent);
        let clip_line = (control_point - mid_normal * (side_sign * miter_clip), mid_tangent);
        vertices[3] = line_line_intersection((previous_edge_vertex, previous_tangent), clip_line);
        vertices[4] = line_line_intersection((next_edge_vertex, next_tangent), clip_line);
        proto_hull.push(vertices[3]);
        proto_hull.push(vertices[4]);
    } else {
        proto_hull.push(vertices[3]);
    }
    let start_index = builder.joint_vertices.len();
    let offset_along_path = *length_accumulator / stroke_options.width;
    for vertex in &vertices {
        builder.joint_vertices.push(Vertex3f1i(
            *vertex,
            glam::vec3(
                previous_normal.dot(*vertex - control_point) / (stroke_options.width * -side_sign),
                previous_tangent.dot(*vertex - control_point) / stroke_options.width,
                offset_along_path,
            ),
            stroke_options.dynamic_stroke_options_group as u32,
        ));
    }
    let mut indices: Vec<u16> = (start_index as u16..(builder.joint_vertices.len() + 1) as u16).collect();
    *indices.iter_mut().last().unwrap() = (-1isize) as u16;
    builder.joint_indices.append(&mut indices);
    *length_accumulator += tangets_dot_product.acos() / (std::f32::consts::PI * 2.0) * stroke_options.width;
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

fn cut_stroke_polygon(builder: &mut StrokeBuilder, proto_hull: &mut Vec<Vertex0>) {
    if !builder.path_line_vertices.is_empty() {
        proto_hull.append(&mut builder.path_line_vertices.iter().map(|vertex| vertex.0).collect());
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
            if tangent.length_squared() == 0.0 {
                if t < 0.5 {
                    t += f32::EPSILON;
                } else {
                    t -= f32::EPSILON;
                }
                tangent = $tangent(&$power_basis, t);
            }
            tangent = tangent.normalize();
            let point = $point(&$power_basis, t);
            $length_accumulator += previous_point.distance(point);
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
    pub fn add_path(&mut self, proto_hull: &mut Vec<Vertex0>, path: &Path) -> Result<(), Error> {
        let stroke_options = path.stroke_options.as_ref().unwrap();
        let mut previous_control_point = path.start;
        let mut first_tangent = glam::Vec2::default();
        let mut previous_tangent = glam::Vec2::default();
        let mut line_segment_iter = path.line_segments.iter();
        let mut integral_quadratic_curve_segment_iter = path.integral_quadratic_curve_segments.iter().peekable();
        let mut integral_cubic_curve_segment_iter = path.integral_cubic_curve_segments.iter().peekable();
        let mut rational_quadratic_curve_segment_iter = path.rational_quadratic_curve_segments.iter().peekable();
        let mut rational_cubic_curve_segment_iter = path.rational_cubic_curve_segments.iter().peekable();
        let mut length_accumulator = 0.0;
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
                    let segment = integral_quadratic_curve_segment_iter.peek().unwrap();
                    next_control_point = segment.control_points[1];
                    segment_start_tangent = (segment.control_points[0] - previous_control_point).normalize();
                    segment_end_tangent = (next_control_point - segment.control_points[0]).normalize();
                }
                SegmentType::IntegralCubicCurve => {
                    let segment = integral_cubic_curve_segment_iter.peek().unwrap();
                    next_control_point = segment.control_points[2];
                    segment_start_tangent = (segment.control_points[0] - previous_control_point).normalize();
                    segment_end_tangent = (next_control_point - segment.control_points[1]).normalize();
                }
                SegmentType::RationalQuadraticCurve => {
                    let segment = rational_quadratic_curve_segment_iter.peek().unwrap();
                    next_control_point = segment.control_points[1];
                    segment_start_tangent = (segment.control_points[0] - previous_control_point).normalize();
                    segment_end_tangent = (next_control_point - segment.control_points[0]).normalize();
                }
                SegmentType::RationalCubicCurve => {
                    let segment = rational_cubic_curve_segment_iter.peek().unwrap();
                    next_control_point = segment.control_points[2];
                    segment_start_tangent = (segment.control_points[0] - previous_control_point).normalize();
                    segment_end_tangent = (next_control_point - segment.control_points[1]).normalize();
                }
            }
            if i == 0 {
                first_tangent = segment_start_tangent;
                if !stroke_options.closed {
                    emit_stroke_vertices(
                        self,
                        &stroke_options,
                        stroke_options.dynamic_stroke_options_group,
                        length_accumulator - 0.5 * stroke_options.width,
                        previous_control_point - segment_start_tangent * stroke_options.width.abs() * 0.5,
                        segment_start_tangent,
                    );
                }
                if stroke_options.closed || *segement_type != SegmentType::Line {
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
            match segement_type {
                SegmentType::Line => {
                    length_accumulator += previous_control_point.distance(next_control_point);
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
                        previous_control_point.extend(1.0),
                        segment.control_points[0].extend(1.0),
                        segment.control_points[1].extend(1.0),
                    ]);
                    emit_curve_stroke!(
                        self,
                        stroke_options,
                        length_accumulator,
                        previous_control_point,
                        power_basis,
                        angle_step,
                        integral_quadratic_uniform_tangent_angle(&power_basis, segment_start_tangent, segment_end_tangent, angle_step),
                        rational_quadratic_point,
                        rational_quadratic_first_order_derivative,
                    );
                }
                SegmentType::IntegralCubicCurve => {
                    let segment = integral_cubic_curve_segment_iter.next().unwrap();
                    let power_basis = rational_cubic_control_points_to_power_basis(&[
                        previous_control_point.extend(1.0),
                        segment.control_points[0].extend(1.0),
                        segment.control_points[1].extend(1.0),
                        segment.control_points[2].extend(1.0),
                    ]);
                    emit_curve_stroke!(
                        self,
                        stroke_options,
                        length_accumulator,
                        previous_control_point,
                        power_basis,
                        angle_step,
                        integral_cubic_uniform_tangent_angle(&power_basis, angle_step),
                        rational_cubic_point,
                        rational_cubic_first_order_derivative,
                    );
                }
                SegmentType::RationalQuadraticCurve => {
                    let segment = rational_quadratic_curve_segment_iter.next().unwrap();
                    let power_basis = rational_quadratic_control_points_to_power_basis(&[
                        previous_control_point.extend(1.0),
                        segment.control_points[0].extend(segment.weight),
                        segment.control_points[1].extend(1.0),
                    ]);
                    emit_curve_stroke!(
                        self,
                        stroke_options,
                        length_accumulator,
                        previous_control_point,
                        power_basis,
                        angle_step,
                        rational_quadratic_uniform_tangent_angle(&power_basis, segment_start_tangent, segment_end_tangent, angle_step),
                        rational_quadratic_point,
                        rational_quadratic_first_order_derivative,
                    );
                }
                SegmentType::RationalCubicCurve => {
                    let segment = rational_cubic_curve_segment_iter.next().unwrap();
                    let power_basis = rational_cubic_control_points_to_power_basis(&[
                        previous_control_point.extend(segment.weights[0]),
                        segment.control_points[0].extend(segment.weights[1]),
                        segment.control_points[1].extend(segment.weights[2]),
                        segment.control_points[2].extend(segment.weights[3]),
                    ]);
                    emit_curve_stroke!(
                        self,
                        stroke_options,
                        length_accumulator,
                        previous_control_point,
                        power_basis,
                        angle_step,
                        rational_cubic_uniform_tangent_angle(&power_basis, angle_step),
                        rational_cubic_point,
                        rational_cubic_first_order_derivative,
                    );
                }
            }
            previous_control_point = next_control_point;
            previous_tangent = segment_end_tangent;
        }
        if stroke_options.closed {
            let line_segment = path.start - previous_control_point;
            let length = line_segment.length();
            if length > 0.0 {
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
                length_accumulator += length;
                emit_stroke_vertices(
                    self,
                    &stroke_options,
                    stroke_options.dynamic_stroke_options_group,
                    length_accumulator,
                    path.start,
                    segment_tangent,
                );
                emit_stroke_join(
                    self,
                    proto_hull,
                    &stroke_options,
                    &mut length_accumulator,
                    path.start,
                    segment_tangent,
                    first_tangent,
                );
            } else {
                emit_stroke_join(
                    self,
                    proto_hull,
                    &stroke_options,
                    &mut length_accumulator,
                    path.start,
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
            emit_stroke_vertices(
                self,
                &stroke_options,
                stroke_options.dynamic_stroke_options_group | 0x10000,
                length_accumulator + 0.5 * stroke_options.width,
                previous_control_point + previous_tangent * stroke_options.width.abs() * 0.5,
                previous_tangent,
            );
        }
        cut_stroke_polygon(self, proto_hull);
        Ok(())
    }
}
