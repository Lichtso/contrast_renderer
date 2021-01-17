use crate::path_builder::{PathBuilder, SegmentType};

pub struct Path {
    pub anchors: Vec<[f32; 2]>,
    pub proto_hull: Option<Vec<[f32; 2]>>,
    pub quadratic_curve_triangles: Option<Vec<([f32; 2], [f32; 2])>>,
    pub cubic_curve_triangles: Option<Vec<([f32; 2], [f32; 3])>>,
    pub rational_quadratic_curve_triangles: Option<Vec<([f32; 2], [f32; 3])>>,
    pub rational_cubic_curve_triangles: Option<Vec<([f32; 2], [f32; 4])>>,
}

impl Path {
    pub fn from_path_builder(path_builder: &PathBuilder) -> Self {
        let mut line_segment_iter = path_builder.line_segments.iter();
        let mut quadratic_curve_segment_iter = path_builder.quadratic_curve_segments.iter();
        let mut cubic_curve_segment_iter = path_builder.cubic_curve_segments.iter();
        let mut rational_quadratic_curve_segment_iter =
            path_builder.rational_quadratic_curve_segments.iter();
        let mut rational_cubic_curve_segment_iter =
            path_builder.rational_cubic_curve_segments.iter();
        let mut anchors = Vec::with_capacity(
            1 + path_builder.line_segments.len()
                + path_builder.quadratic_curve_segments.len()
                + path_builder.cubic_curve_segments.len()
                + path_builder.rational_quadratic_curve_segments.len()
                + path_builder.rational_cubic_curve_segments.len(),
        );
        anchors.push(path_builder.anchor);
        let mut proto_hull = Vec::with_capacity(
            1 + path_builder.line_segments.len()
                + path_builder.quadratic_curve_segments.len() * 2
                + path_builder.cubic_curve_segments.len() * 3
                + path_builder.rational_quadratic_curve_segments.len() * 2
                + path_builder.rational_cubic_curve_segments.len() * 3,
        );
        proto_hull.push(path_builder.anchor);
        let mut quadratic_curve_triangles =
            Vec::with_capacity(path_builder.quadratic_curve_segments.len() * 3);
        let cubic_curve_triangles =
            Vec::with_capacity(path_builder.cubic_curve_segments.len() * 3);
        let mut rational_quadratic_curve_triangles =
            Vec::with_capacity(path_builder.rational_quadratic_curve_segments.len() * 3);
        let rational_cubic_curve_triangles =
            Vec::with_capacity(path_builder.rational_cubic_curve_segments.len() * 3);
        fn truncate(v: [f32; 3]) -> [f32; 2] {
            [v[0], v[1]]
        }
        for segement_type in &path_builder.segement_types {
            let previous_anchor = anchors.last().unwrap();
            match segement_type {
                SegmentType::Line => {
                    let segment = line_segment_iter.next().unwrap();
                    proto_hull.push(segment.control_points[0]);
                    anchors.push(segment.control_points[0]);
                }
                SegmentType::QuadraticCurve => {
                    let segment = quadratic_curve_segment_iter.next().unwrap();
                    quadratic_curve_triangles.push((segment.control_points[1], [1.0, 1.0]));
                    quadratic_curve_triangles.push((segment.control_points[0], [0.5, 0.0]));
                    quadratic_curve_triangles.push((*anchors.last().unwrap(), [0.0, 0.0]));
                    proto_hull.push(segment.control_points[0]);
                    proto_hull.push(segment.control_points[1]);
                    anchors.push(segment.control_points[1]);
                }
                SegmentType::CubicCurve => {
                    let segment = cubic_curve_segment_iter.next().unwrap();
                    // TODO: Generate discard triangles
                    proto_hull.push(segment.control_points[0]);
                    proto_hull.push(segment.control_points[1]);
                    proto_hull.push(segment.control_points[2]);
                    anchors.push(segment.control_points[2]);
                }
                SegmentType::RationalQuadraticCurve => {
                    let segment = rational_quadratic_curve_segment_iter.next().unwrap();
                    let weight = 1.0 / segment.control_points[1][2];
                    rational_quadratic_curve_triangles.push((
                        truncate(segment.control_points[1]),
                        [weight, weight, weight],
                    ));
                    let weight = 1.0 / segment.control_points[0][2];
                    rational_quadratic_curve_triangles.push((
                        truncate(segment.control_points[0]),
                        [0.5 * weight, 0.0, weight],
                    ));
                    rational_quadratic_curve_triangles.push((
                        *anchors.last().unwrap(),
                        [0.0, 0.0, 1.0 / segment.first_weight],
                    ));
                    proto_hull.push(truncate(segment.control_points[0]));
                    proto_hull.push(truncate(segment.control_points[1]));
                    anchors.push(truncate(segment.control_points[1]));
                }
                SegmentType::RationalCubicCurve => {
                    let segment = rational_cubic_curve_segment_iter.next().unwrap();
                    // TODO: Generate discard triangles
                    proto_hull.push(truncate(segment.control_points[0]));
                    proto_hull.push(truncate(segment.control_points[1]));
                    proto_hull.push(truncate(segment.control_points[2]));
                    anchors.push(truncate(segment.control_points[2]));
                }
            }
        }
        Self {
            anchors,
            proto_hull: Some(proto_hull),
            quadratic_curve_triangles: if quadratic_curve_triangles.is_empty() {
                None
            } else {
                Some(quadratic_curve_triangles)
            },
            cubic_curve_triangles: if cubic_curve_triangles.is_empty() {
                None
            } else {
                Some(cubic_curve_triangles)
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

    pub fn from_polygon(anchors: Vec<[f32; 2]>) -> Self {
        Self {
            anchors,
            proto_hull: None,
            quadratic_curve_triangles: None,
            cubic_curve_triangles: None,
            rational_quadratic_curve_triangles: None,
            rational_cubic_curve_triangles: None,
        }
    }

    pub fn from_rect(center: [f32; 2], half_extent: [f32; 2]) -> Self {
        let anchors = vec![
            [center[0] - half_extent[0], center[1] - half_extent[1]],
            [center[0] - half_extent[0], center[1] + half_extent[1]],
            [center[0] + half_extent[0], center[1] + half_extent[1]],
            [center[0] + half_extent[0], center[1] - half_extent[1]],
        ];
        Self {
            anchors,
            proto_hull: None,
            quadratic_curve_triangles: None,
            cubic_curve_triangles: None,
            rational_quadratic_curve_triangles: None,
            rational_cubic_curve_triangles: None,
        }
    }

    pub fn from_rounded_rect(center: [f32; 2], half_extent: [f32; 2], radius: f32) -> Self {
        let radius = radius.min(half_extent[0]).min(half_extent[1]);
        let anchors = vec![
            [
                center[0] - half_extent[0] + radius,
                center[1] - half_extent[1],
            ],
            [
                center[0] - half_extent[0],
                center[1] - half_extent[1] + radius,
            ],
            [
                center[0] - half_extent[0],
                center[1] + half_extent[1] - radius,
            ],
            [
                center[0] - half_extent[0] + radius,
                center[1] + half_extent[1],
            ],
            [
                center[0] + half_extent[0] - radius,
                center[1] + half_extent[1],
            ],
            [
                center[0] + half_extent[0],
                center[1] + half_extent[1] - radius,
            ],
            [
                center[0] + half_extent[0],
                center[1] - half_extent[1] + radius,
            ],
            [
                center[0] + half_extent[0] - radius,
                center[1] - half_extent[1],
            ],
        ];
        let hull = vec![
            [center[0] - half_extent[0], center[1] - half_extent[1]],
            [center[0] - half_extent[0], center[1] + half_extent[1]],
            [center[0] + half_extent[0], center[1] + half_extent[1]],
            [center[0] + half_extent[0], center[1] - half_extent[1]],
        ];
        let sqrt_2 = 2.0f32.sqrt();
        let half_sqrt_2 = 0.5 * sqrt_2;
        let rational_quadratic_curve_triangles = vec![
            (anchors[1], [1.0, 1.0, 1.0]),
            (hull[0], [half_sqrt_2, 0.0, sqrt_2]),
            (anchors[0], [0.0, 0.0, 1.0]),
            (anchors[3], [1.0, 1.0, 1.0]),
            (hull[1], [half_sqrt_2, 0.0, sqrt_2]),
            (anchors[2], [0.0, 0.0, 1.0]),
            (anchors[5], [1.0, 1.0, 1.0]),
            (hull[2], [half_sqrt_2, 0.0, sqrt_2]),
            (anchors[4], [0.0, 0.0, 1.0]),
            (anchors[7], [1.0, 1.0, 1.0]),
            (hull[3], [half_sqrt_2, 0.0, sqrt_2]),
            (anchors[6], [0.0, 0.0, 1.0]),
        ];
        Self {
            anchors,
            proto_hull: Some(hull),
            quadratic_curve_triangles: None,
            cubic_curve_triangles: None,
            rational_quadratic_curve_triangles: Some(rational_quadratic_curve_triangles),
            rational_cubic_curve_triangles: None,
        }
    }

    pub fn from_ellipse(center: [f32; 2], half_extent: [f32; 2]) -> Self {
        let anchors = vec![
            [center[0] - half_extent[0], center[1]],
            [center[0], center[1] + half_extent[1]],
            [center[0] + half_extent[0], center[1]],
            [center[0], center[1] - half_extent[1]],
        ];
        let hull = vec![
            [center[0] - half_extent[0], center[1] - half_extent[1]],
            [center[0] - half_extent[0], center[1] + half_extent[1]],
            [center[0] + half_extent[0], center[1] + half_extent[1]],
            [center[0] + half_extent[0], center[1] - half_extent[1]],
        ];
        let sqrt_2 = 2.0f32.sqrt();
        let half_sqrt_2 = 0.5 * sqrt_2;
        let rational_quadratic_curve_triangles = vec![
            (anchors[0], [1.0, 1.0, 1.0]),
            (hull[0], [half_sqrt_2, 0.0, sqrt_2]),
            (anchors[3], [0.0, 0.0, 1.0]),
            (anchors[1], [1.0, 1.0, 1.0]),
            (hull[1], [half_sqrt_2, 0.0, sqrt_2]),
            (anchors[0], [0.0, 0.0, 1.0]),
            (anchors[2], [1.0, 1.0, 1.0]),
            (hull[2], [half_sqrt_2, 0.0, sqrt_2]),
            (anchors[1], [0.0, 0.0, 1.0]),
            (anchors[3], [1.0, 1.0, 1.0]),
            (hull[3], [half_sqrt_2, 0.0, sqrt_2]),
            (anchors[2], [0.0, 0.0, 1.0]),
        ];
        Self {
            anchors,
            proto_hull: Some(hull),
            quadratic_curve_triangles: None,
            cubic_curve_triangles: None,
            rational_quadratic_curve_triangles: Some(rational_quadratic_curve_triangles),
            rational_cubic_curve_triangles: None,
        }
    }

    pub fn from_circle(center: [f32; 2], radius: f32) -> Self {
        Self::from_ellipse(center, [radius, radius])
    }

    pub fn reverse(&mut self) {
        self.anchors.reverse();
        if let Some(triangles) = &mut self.quadratic_curve_triangles {
            triangles.reverse();
        }
        if let Some(triangles) = &mut self.cubic_curve_triangles {
            triangles.reverse();
        }
        if let Some(triangles) = &mut self.rational_quadratic_curve_triangles {
            triangles.reverse();
        }
        if let Some(triangles) = &mut self.rational_cubic_curve_triangles {
            triangles.reverse();
        }
    }

    pub fn scale_and_translate(&mut self, scale: f32, translate: [f32; 2]) {
        for control_point in &mut self.anchors {
            (*control_point)[0] = (*control_point)[0] * scale + translate[0];
            (*control_point)[1] = (*control_point)[1] * scale + translate[1];
        }
        if let Some(proto_hull) = &mut self.proto_hull {
            for control_point in proto_hull {
                (*control_point)[0] = (*control_point)[0] * scale + translate[0];
                (*control_point)[1] = (*control_point)[1] * scale + translate[1];
            }
        }
        if let Some(triangles) = &mut self.quadratic_curve_triangles {
            for control_point in triangles {
                (*control_point).0[0] = (*control_point).0[0] * scale + translate[0];
                (*control_point).0[1] = (*control_point).0[1] * scale + translate[1];
            }
        }
        if let Some(triangles) = &mut self.cubic_curve_triangles {
            for control_point in triangles {
                (*control_point).0[0] = (*control_point).0[0] * scale + translate[0];
                (*control_point).0[1] = (*control_point).0[1] * scale + translate[1];
            }
        }
        if let Some(triangles) = &mut self.rational_quadratic_curve_triangles {
            for control_point in triangles {
                (*control_point).0[0] = (*control_point).0[0] * scale + translate[0];
                (*control_point).0[1] = (*control_point).0[1] * scale + translate[1];
            }
        }
        if let Some(triangles) = &mut self.rational_cubic_curve_triangles {
            for control_point in triangles {
                (*control_point).0[0] = (*control_point).0[0] * scale + translate[0];
                (*control_point).0[1] = (*control_point).0[1] * scale + translate[1];
            }
        }
    }
}
