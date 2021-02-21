use crate::{
    path::{Cap, Join, Path, SegmentType, StrokeOptions},
    utils::{line_line_intersection, rotate_90_degree_clockwise},
    vertex::{Vertex0, Vertex3},
};

fn emit_stroke_vertex(proto_hull: &mut Vec<Vertex0>, path_solid_vertices: &mut Vec<Vertex0>, vertex: glam::Vec2) {
    proto_hull.push(vertex);
    path_solid_vertices.push(vertex);
}

fn emit_stroke_vertices(
    proto_hull: &mut Vec<Vertex0>,
    path_solid_vertices: &mut Vec<Vertex0>,
    stroke_options: &StrokeOptions,
    control_point: glam::Vec2,
    tangent: glam::Vec2,
) {
    let normal = rotate_90_degree_clockwise(tangent);
    let clamped_offset = stroke_options.offset.clamp(-0.5, 0.5);
    let width_absolute = stroke_options.width.abs();
    emit_stroke_vertex(
        proto_hull,
        path_solid_vertices,
        control_point + normal * (clamped_offset - 0.5) * width_absolute,
    );
    emit_stroke_vertex(
        proto_hull,
        path_solid_vertices,
        control_point + normal * (clamped_offset + 0.5) * width_absolute,
    );
}

fn emit_stroke_rounding(proto_hull: &mut Vec<Vertex0>, builder: &mut StrokeBuilder, begin: (glam::Vec2, glam::Vec2), end: (glam::Vec2, glam::Vec2)) {
    if begin.0.distance_squared(end.0) == 0.0 {
        return;
    }
    let intersection_point = line_line_intersection(begin, end);
    proto_hull.push(intersection_point);
    let weight = std::f32::consts::SQRT_2 / (1.0 + begin.1.dot(end.1)).sqrt();
    builder.quadratic_vertices.push(Vertex3(end.0, glam::vec3(1.0, 1.0, 1.0)));
    builder
        .quadratic_vertices
        .push(Vertex3(intersection_point, glam::vec3(0.5 * weight, 0.0, weight)));
    builder.quadratic_vertices.push(Vertex3(begin.0, glam::vec3(0.0, 0.0, 1.0)));
}

fn emit_stroke_split_rounding(
    proto_hull: &mut Vec<Vertex0>,
    builder: &mut StrokeBuilder,
    begin: (glam::Vec2, glam::Vec2),
    tip: (glam::Vec2, glam::Vec2),
    end: (glam::Vec2, glam::Vec2),
) {
    emit_stroke_rounding(proto_hull, builder, begin, tip);
    emit_stroke_rounding(proto_hull, builder, tip, end);
}

fn emit_stroke_join(
    proto_hull: &mut Vec<Vertex0>,
    builder: &mut StrokeBuilder,
    path_solid_vertices: &mut Vec<Vertex0>,
    stroke_options: &StrokeOptions,
    control_point: glam::Vec2,
    start_tangent: glam::Vec2,
    end_tangent: glam::Vec2,
) {
    emit_stroke_vertices(proto_hull, path_solid_vertices, stroke_options, control_point, start_tangent);
    let side_sign = rotate_90_degree_clockwise(start_tangent).dot(end_tangent);
    if side_sign == 0.0 {
        return;
    }
    let base_index = if side_sign < 0.0 { 1 } else { 2 };
    let start_point_and_tagent = (path_solid_vertices[path_solid_vertices.len() - base_index], start_tangent);
    let mid_tangent = (start_tangent + end_tangent).normalize();
    let tangets_dot_product = start_tangent.dot(end_tangent);
    if stroke_options.join == Join::Miter || (stroke_options.join == Join::Round && tangets_dot_product < 0.0) {
        emit_stroke_vertices(proto_hull, path_solid_vertices, stroke_options, control_point, mid_tangent);
    }
    emit_stroke_vertices(proto_hull, path_solid_vertices, stroke_options, control_point, end_tangent);
    let end_point_and_tagent = (path_solid_vertices[path_solid_vertices.len() - base_index], end_tangent);
    match stroke_options.join {
        Join::Miter => {
            let mid_index = path_solid_vertices.len() - base_index - 2;
            path_solid_vertices[mid_index] = line_line_intersection(start_point_and_tagent, end_point_and_tagent);
        }
        Join::Bevel => {}
        Join::Round => {
            if tangets_dot_product < 0.0 {
                emit_stroke_split_rounding(
                    proto_hull,
                    builder,
                    start_point_and_tagent,
                    (path_solid_vertices[path_solid_vertices.len() - base_index - 2], mid_tangent),
                    end_point_and_tagent,
                );
            } else {
                emit_stroke_rounding(proto_hull, builder, start_point_and_tagent, end_point_and_tagent);
            }
        }
    }
}

fn cut_stroke_polygon(proto_hull: &mut Vec<Vertex0>, builder: &mut StrokeBuilder, path_solid_vertices: &mut Vec<Vertex0>) {
    if !path_solid_vertices.is_empty() {
        proto_hull.append(&mut path_solid_vertices.clone());
        let start_index = builder.solid_vertices.len();
        builder.solid_vertices.append(path_solid_vertices);
        let mut indices: Vec<u16> = (start_index as u16..(builder.solid_vertices.len() + 1) as u16).collect();
        *indices.iter_mut().last().unwrap() = (-1isize) as u16;
        builder.solid_indices.append(&mut indices);
    }
}

#[derive(Default)]
pub struct StrokeBuilder {
    pub solid_indices: Vec<u16>,
    pub solid_vertices: Vec<Vertex0>,
    pub quadratic_vertices: Vec<Vertex3>,
}

impl StrokeBuilder {
    pub fn add_path(&mut self, proto_hull: &mut Vec<Vertex0>, path: &Path) {
        let stroke_options = path.stroke_options.unwrap();
        let mut path_solid_vertices: Vec<Vertex0> = Vec::with_capacity(path.line_segments.len() * 4);
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
                        emit_stroke_vertices(proto_hull, &mut path_solid_vertices, &stroke_options, tip_point, segment_start_tangent);
                    }
                    Cap::Triangle | Cap::Round => {
                        emit_stroke_vertex(proto_hull, &mut path_solid_vertices, tip_point);
                    }
                }
                if (stroke_options.cap == Cap::Round || stroke_options.cap == Cap::Triangle)
                    || (stroke_options.cap == Cap::Square && *segement_type != SegmentType::Line)
                    || ((stroke_options.cap == Cap::Butt || stroke_options.cap == Cap::Closed) && *segement_type == SegmentType::Line)
                {
                    emit_stroke_vertices(
                        proto_hull,
                        &mut path_solid_vertices,
                        &stroke_options,
                        previous_control_point,
                        segment_start_tangent,
                    );
                }
                if stroke_options.cap == Cap::Round {
                    emit_stroke_split_rounding(
                        proto_hull,
                        self,
                        (path_solid_vertices[path_solid_vertices.len() - 2], -segment_start_tangent),
                        (tip_point, rotate_90_degree_clockwise(segment_start_tangent)),
                        (path_solid_vertices[path_solid_vertices.len() - 1], segment_start_tangent),
                    );
                }
            } else {
                emit_stroke_join(
                    proto_hull,
                    self,
                    &mut path_solid_vertices,
                    &stroke_options,
                    previous_control_point,
                    previous_tangent,
                    segment_start_tangent,
                );
            }
            if *segement_type != SegmentType::Line {
                cut_stroke_polygon(proto_hull, self, &mut path_solid_vertices);
            }
            previous_control_point = next_control_point;
            previous_tangent = segment_end_tangent;
        }
        if (stroke_options.cap == Cap::Round || stroke_options.cap == Cap::Triangle)
            || (stroke_options.cap == Cap::Square && *path.segement_types.last().unwrap() != SegmentType::Line)
            || (stroke_options.cap == Cap::Butt && *path.segement_types.last().unwrap() == SegmentType::Line)
        {
            emit_stroke_vertices(
                proto_hull,
                &mut path_solid_vertices,
                &stroke_options,
                previous_control_point,
                previous_tangent,
            );
        }
        let tip_point = previous_control_point + previous_tangent * stroke_options.width.abs() * 0.5;
        if stroke_options.cap == Cap::Round {
            emit_stroke_split_rounding(
                proto_hull,
                self,
                (path_solid_vertices[path_solid_vertices.len() - 2], -previous_tangent),
                (tip_point, rotate_90_degree_clockwise(previous_tangent)),
                (path_solid_vertices[path_solid_vertices.len() - 1], previous_tangent),
            );
        }
        match stroke_options.cap {
            Cap::Closed => {
                let line_segment = path.start - previous_control_point;
                if line_segment.length_squared() > 0.0 {
                    let segment_tangent = line_segment.normalize();
                    emit_stroke_join(
                        proto_hull,
                        self,
                        &mut path_solid_vertices,
                        &stroke_options,
                        previous_control_point,
                        previous_tangent,
                        segment_tangent,
                    );
                    emit_stroke_join(
                        proto_hull,
                        self,
                        &mut path_solid_vertices,
                        &stroke_options,
                        path.start,
                        segment_tangent,
                        first_tangent,
                    );
                } else {
                    emit_stroke_join(
                        proto_hull,
                        self,
                        &mut path_solid_vertices,
                        &stroke_options,
                        path.start,
                        previous_tangent,
                        first_tangent,
                    );
                }
            }
            Cap::Butt => {}
            Cap::Square => {
                emit_stroke_vertices(proto_hull, &mut path_solid_vertices, &stroke_options, tip_point, previous_tangent);
            }
            Cap::Triangle | Cap::Round => {
                emit_stroke_vertex(proto_hull, &mut path_solid_vertices, tip_point);
            }
        }
        cut_stroke_polygon(proto_hull, self, &mut path_solid_vertices);
    }
}
