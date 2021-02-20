#[derive(Debug, Clone, Copy)]
pub struct LineSegment {
    pub control_points: [glam::Vec2; 1],
}

#[derive(Debug, Clone, Copy)]
pub struct IntegralQuadraticCurveSegment {
    pub control_points: [glam::Vec2; 2],
}

#[derive(Debug, Clone, Copy)]
pub struct IntegralCubicCurveSegment {
    pub control_points: [glam::Vec2; 3],
}

#[derive(Debug, Clone, Copy)]
pub struct RationalQuadraticCurveSegment {
    pub weights: [f32; 3],
    pub control_points: [glam::Vec2; 2],
}

#[derive(Debug, Clone, Copy)]
pub struct RationalCubicCurveSegment {
    pub weights: [f32; 4],
    pub control_points: [glam::Vec2; 3],
}

/*#[derive(Debug, Clone, Copy)]
pub enum Segment {
    Line(LineSegment),
    IntegralQuadraticCurve(IntegralQuadraticCurveSegment),
    IntegralCubicCurve(IntegralCubicCurveSegment),
    RationalQuadraticCurve(RationalQuadraticCurveSegment),
    RationalCubicCurve(RationalCubicCurveSegment),
}

impl From<LineSegment> for Segment {
    fn from(segment: LineSegment) -> Self {
        Self::Line(segment)
    }
}

impl From<IntegralQuadraticCurveSegment> for Segment {
    fn from(segment: IntegralQuadraticCurveSegment) -> Self {
        Self::IntegralQuadraticCurve(segment)
    }
}

impl From<IntegralCubicCurveSegment> for Segment {
    fn from(segment: IntegralCubicCurveSegment) -> Self {
        Self::IntegralCubicCurve(segment)
    }
}

impl From<RationalQuadraticCurveSegment> for Segment {
    fn from(segment: RationalQuadraticCurveSegment) -> Self {
        Self::RationalQuadraticCurve(segment)
    }
}

impl From<RationalCubicCurveSegment> for Segment {
    fn from(segment: RationalCubicCurveSegment) -> Self {
        Self::RationalCubicCurve(segment)
    }
}*/

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SegmentType {
    Line,
    IntegralQuadraticCurve,
    IntegralCubicCurve,
    RationalQuadraticCurve,
    RationalCubicCurve,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Join {
    Bevel,
    Round,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Cap {
    Butt,
    // Square,
    // Triangle,
    // Round,
}

pub struct StrokeOptions {
    pub closed: bool,
    pub width: f32,
    pub offset: f32,
    pub join: Join,
    pub cap: Cap,
}

#[derive(Default)]
pub struct Path {
    pub stroke_options: Option<StrokeOptions>,
    pub start: glam::Vec2,
    pub line_segments: Vec<LineSegment>,
    pub integral_quadratic_curve_segments: Vec<IntegralQuadraticCurveSegment>,
    pub integral_cubic_curve_segments: Vec<IntegralCubicCurveSegment>,
    pub rational_quadratic_curve_segments: Vec<RationalQuadraticCurveSegment>,
    pub rational_cubic_curve_segments: Vec<RationalCubicCurveSegment>,
    pub segement_types: Vec<SegmentType>,
}

impl Path {
    pub fn push_line(&mut self, segment: LineSegment) {
        self.line_segments.push(segment);
        self.segement_types.push(SegmentType::Line);
    }

    pub fn push_integral_quadratic_curve(&mut self, segment: IntegralQuadraticCurveSegment) {
        self.integral_quadratic_curve_segments.push(segment);
        self.segement_types.push(SegmentType::IntegralQuadraticCurve);
    }

    pub fn push_integral_cubic_curve(&mut self, segment: IntegralCubicCurveSegment) {
        self.integral_cubic_curve_segments.push(segment);
        self.segement_types.push(SegmentType::IntegralCubicCurve);
    }

    pub fn push_rational_quadratic_curve(&mut self, segment: RationalQuadraticCurveSegment) {
        self.rational_quadratic_curve_segments.push(segment);
        self.segement_types.push(SegmentType::RationalQuadraticCurve);
    }

    pub fn push_rational_cubic_curve(&mut self, segment: RationalCubicCurveSegment) {
        self.rational_cubic_curve_segments.push(segment);
        self.segement_types.push(SegmentType::RationalCubicCurve);
    }

    /*pub fn push_segment(&mut self, segment: Segment) {
        match segment {
            Segment::Line(segment) => {
                self.push_line(segment);
            }
            Segment::IntegralQuadraticCurve(segment) => {
                self.push_integral_quadratic_curve(segment);
            }
            Segment::IntegralCubicCurve(segment) => {
                self.push_integral_cubic_curve(segment);
            }
            Segment::RationalQuadraticCurve(segment) => {
                self.push_rational_quadratic_curve(segment);
            }
            Segment::RationalCubicCurve(segment) => {
                self.push_rational_cubic_curve(segment);
            }
        }
    }*/

    pub fn get_current_end(&self) -> glam::Vec2 {
        match self.segement_types.last() {
            Some(SegmentType::Line) => {
                let segment = self.line_segments.last().unwrap();
                segment.control_points[0]
            }
            Some(SegmentType::IntegralQuadraticCurve) => {
                let segment = self.integral_quadratic_curve_segments.last().unwrap();
                segment.control_points[1]
            }
            Some(SegmentType::IntegralCubicCurve) => {
                let segment = self.integral_cubic_curve_segments.last().unwrap();
                segment.control_points[2]
            }
            Some(SegmentType::RationalQuadraticCurve) => {
                let segment = self.rational_quadratic_curve_segments.last().unwrap();
                segment.control_points[1]
            }
            Some(SegmentType::RationalCubicCurve) => {
                let segment = self.rational_cubic_curve_segments.last().unwrap();
                segment.control_points[2]
            }
            None => self.start,
        }
    }

    pub fn append(&mut self, other: &mut Self) {
        self.line_segments.append(&mut other.line_segments);
        self.integral_quadratic_curve_segments
            .append(&mut other.integral_quadratic_curve_segments);
        self.integral_cubic_curve_segments.append(&mut other.integral_cubic_curve_segments);
        self.rational_quadratic_curve_segments
            .append(&mut other.rational_quadratic_curve_segments);
        self.rational_cubic_curve_segments.append(&mut other.rational_cubic_curve_segments);
    }

    pub fn transform(&mut self, transform: &glam::Mat3) {
        self.start = transform.transform_point2(self.start);
        let mut line_segment_iter = self.line_segments.iter_mut();
        let mut integral_quadratic_curve_segment_iter = self.integral_quadratic_curve_segments.iter_mut();
        let mut integral_cubic_curve_segment_iter = self.integral_cubic_curve_segments.iter_mut();
        let mut rational_quadratic_curve_segment_iter = self.rational_quadratic_curve_segments.iter_mut();
        let mut rational_cubic_curve_segment_iter = self.rational_cubic_curve_segments.iter_mut();
        for segement_type in &mut self.segement_types {
            match *segement_type {
                SegmentType::Line => {
                    let segment = line_segment_iter.next().unwrap();
                    for control_point in &mut segment.control_points {
                        *control_point = transform.transform_point2(*control_point);
                    }
                }
                SegmentType::IntegralQuadraticCurve => {
                    let segment = integral_quadratic_curve_segment_iter.next().unwrap();
                    for control_point in &mut segment.control_points {
                        *control_point = transform.transform_point2(*control_point);
                    }
                }
                SegmentType::IntegralCubicCurve => {
                    let segment = integral_cubic_curve_segment_iter.next().unwrap();
                    for control_point in &mut segment.control_points {
                        *control_point = transform.transform_point2(*control_point);
                    }
                }
                SegmentType::RationalQuadraticCurve => {
                    let segment = rational_quadratic_curve_segment_iter.next().unwrap();
                    for control_point in &mut segment.control_points {
                        *control_point = transform.transform_point2(*control_point);
                    }
                }
                SegmentType::RationalCubicCurve => {
                    let segment = rational_cubic_curve_segment_iter.next().unwrap();
                    for control_point in &mut segment.control_points {
                        *control_point = transform.transform_point2(*control_point);
                    }
                }
            }
        }
    }

    pub fn reverse(&mut self) {
        let mut previous_control_point = self.start;
        let mut line_segment_iter = self.line_segments.iter_mut();
        let mut integral_quadratic_curve_segment_iter = self.integral_quadratic_curve_segments.iter_mut();
        let mut integral_cubic_curve_segment_iter = self.integral_cubic_curve_segments.iter_mut();
        let mut rational_quadratic_curve_segment_iter = self.rational_quadratic_curve_segments.iter_mut();
        let mut rational_cubic_curve_segment_iter = self.rational_cubic_curve_segments.iter_mut();
        for segement_type in &mut self.segement_types {
            match *segement_type {
                SegmentType::Line => {
                    let segment = line_segment_iter.next().unwrap();
                    std::mem::swap(&mut previous_control_point, &mut segment.control_points[0]);
                }
                SegmentType::IntegralQuadraticCurve => {
                    let segment = integral_quadratic_curve_segment_iter.next().unwrap();
                    std::mem::swap(&mut previous_control_point, &mut segment.control_points[1]);
                }
                SegmentType::IntegralCubicCurve => {
                    let segment = integral_cubic_curve_segment_iter.next().unwrap();
                    segment.control_points.swap(0, 1);
                    std::mem::swap(&mut previous_control_point, &mut segment.control_points[2]);
                }
                SegmentType::RationalQuadraticCurve => {
                    let segment = rational_quadratic_curve_segment_iter.next().unwrap();
                    segment.weights.swap(0, 2);
                    std::mem::swap(&mut previous_control_point, &mut segment.control_points[1]);
                }
                SegmentType::RationalCubicCurve => {
                    let segment = rational_cubic_curve_segment_iter.next().unwrap();
                    segment.weights.reverse();
                    segment.control_points.swap(0, 1);
                    std::mem::swap(&mut previous_control_point, &mut segment.control_points[2]);
                }
            }
        }
        self.start = previous_control_point;
        self.segement_types.reverse();
        self.line_segments.reverse();
        self.integral_quadratic_curve_segments.reverse();
        self.integral_cubic_curve_segments.reverse();
        self.rational_quadratic_curve_segments.reverse();
        self.rational_cubic_curve_segments.reverse();
    }

    pub fn convert_integral_curves_to_rational_curves(&mut self) {
        let mut integral_quadratic_curve_segment_iter = self.integral_quadratic_curve_segments.iter();
        let mut integral_cubic_curve_segment_iter = self.integral_cubic_curve_segments.iter();
        let mut rational_quadratic_curve_segment_index = 0;
        let mut rational_cubic_curve_segment_index = 0;
        for segement_type in &mut self.segement_types {
            match *segement_type {
                SegmentType::Line => {}
                SegmentType::IntegralQuadraticCurve => {
                    let segment = integral_quadratic_curve_segment_iter.next().unwrap();
                    self.rational_quadratic_curve_segments.insert(
                        rational_quadratic_curve_segment_index,
                        RationalQuadraticCurveSegment {
                            weights: [1.0, 1.0, 1.0],
                            control_points: segment.control_points,
                        },
                    );
                    rational_quadratic_curve_segment_index += 1;
                    *segement_type = SegmentType::RationalQuadraticCurve;
                }
                SegmentType::IntegralCubicCurve => {
                    let segment = integral_cubic_curve_segment_iter.next().unwrap();
                    self.rational_cubic_curve_segments.insert(
                        rational_cubic_curve_segment_index,
                        RationalCubicCurveSegment {
                            weights: [1.0, 1.0, 1.0, 1.0],
                            control_points: segment.control_points,
                        },
                    );
                    rational_cubic_curve_segment_index += 1;
                    *segement_type = SegmentType::RationalCubicCurve;
                }
                SegmentType::RationalQuadraticCurve => {
                    rational_quadratic_curve_segment_index += 1;
                }
                SegmentType::RationalCubicCurve => {
                    rational_cubic_curve_segment_index += 1;
                }
            }
        }
        self.integral_quadratic_curve_segments.clear();
        self.integral_cubic_curve_segments.clear();
    }

    pub fn convert_quadratic_curves_to_cubic_curves(&mut self) {
        let mut line_segment_iter = self.line_segments.iter();
        let mut integral_quadratic_curve_segment_iter = self.integral_quadratic_curve_segments.iter();
        let mut integral_cubic_curve_segment_index = 0;
        let mut rational_quadratic_curve_segment_iter = self.rational_quadratic_curve_segments.iter();
        let mut rational_cubic_curve_segment_index = 0;
        let mut previous_control_point = self.start;
        for segement_type in &mut self.segement_types {
            match *segement_type {
                SegmentType::Line => {
                    let segment = line_segment_iter.next().unwrap();
                    previous_control_point = segment.control_points[0];
                }
                SegmentType::IntegralQuadraticCurve => {
                    let segment = integral_quadratic_curve_segment_iter.next().unwrap();
                    self.integral_cubic_curve_segments.insert(
                        integral_cubic_curve_segment_index,
                        IntegralCubicCurveSegment {
                            control_points: [
                                previous_control_point + 2.0 / 3.0 * (segment.control_points[0] - previous_control_point),
                                segment.control_points[1] + 2.0 / 3.0 * (segment.control_points[0] - segment.control_points[1]),
                                segment.control_points[1],
                            ],
                        },
                    );
                    integral_cubic_curve_segment_index += 1;
                    *segement_type = SegmentType::IntegralCubicCurve;
                    previous_control_point = segment.control_points[1];
                }
                SegmentType::IntegralCubicCurve => {
                    previous_control_point = self.integral_cubic_curve_segments[integral_cubic_curve_segment_index].control_points[2];
                    integral_cubic_curve_segment_index += 1;
                }
                SegmentType::RationalQuadraticCurve => {
                    let segment = rational_quadratic_curve_segment_iter.next().unwrap();
                    let mut control_points = [
                        previous_control_point.extend(segment.weights[0]),
                        segment.control_points[0].extend(segment.weights[1]),
                        segment.control_points[1].extend(segment.weights[2]),
                    ];
                    for control_point in &mut control_points {
                        *control_point *= glam::vec3(control_point[2], control_point[2], 1.0);
                    }
                    let mut new_control_points = [
                        control_points[0] + 2.0 / 3.0 * (control_points[1] - control_points[0]),
                        control_points[2] + 2.0 / 3.0 * (control_points[1] - control_points[2]),
                    ];
                    for control_point in &mut new_control_points {
                        let factor = 1.0 / control_point[2];
                        *control_point *= glam::vec3(factor, factor, 1.0);
                    }
                    self.rational_cubic_curve_segments.insert(
                        rational_cubic_curve_segment_index,
                        RationalCubicCurveSegment {
                            weights: [segment.weights[0], new_control_points[0][2], new_control_points[1][2], segment.weights[2]],
                            control_points: [
                                new_control_points[0].truncate(),
                                new_control_points[1].truncate(),
                                segment.control_points[1],
                            ],
                        },
                    );
                    rational_cubic_curve_segment_index += 1;
                    *segement_type = SegmentType::RationalCubicCurve;
                    previous_control_point = segment.control_points[1];
                }
                SegmentType::RationalCubicCurve => {
                    previous_control_point = self.rational_cubic_curve_segments[rational_cubic_curve_segment_index].control_points[2];
                    rational_cubic_curve_segment_index += 1;
                }
            }
        }
        self.integral_quadratic_curve_segments.clear();
        self.rational_quadratic_curve_segments.clear();
    }

    pub fn push_arc(&mut self, tangent_crossing: glam::Vec2, to: glam::Vec2) {
        self.push_rational_quadratic_curve(RationalQuadraticCurveSegment {
            weights: [
                1.0,
                ((tangent_crossing - self.get_current_end()).angle_between(tangent_crossing - to) * 0.5).sin(),
                1.0,
            ],
            control_points: [tangent_crossing, to],
        });
    }

    fn push_quarter_ellipse(&mut self, tangent_crossing: glam::Vec2, to: glam::Vec2) {
        self.push_rational_quadratic_curve(RationalQuadraticCurveSegment {
            weights: [1.0, std::f32::consts::FRAC_1_SQRT_2, 1.0],
            control_points: [tangent_crossing, to],
        });
    }

    pub fn from_polygon(vertices: &[glam::Vec2]) -> Self {
        let mut vertices = vertices.iter();
        let mut result = Path::default();
        result.start = *vertices.next().unwrap();
        for control_point in vertices {
            result.push_line(LineSegment {
                control_points: [*control_point],
            });
        }
        result
    }

    pub fn from_regular_polygon(center: glam::Vec2, radius: f32, rotation: f32, vertex_count: usize) -> Self {
        let mut vertices = Vec::with_capacity(vertex_count);
        for i in 0..vertex_count {
            let angle = rotation + i as f32 / vertex_count as f32 * std::f32::consts::PI * 2.0;
            vertices.push(center + radius * glam::vec2(angle.cos(), angle.sin()));
        }
        Self::from_polygon(&vertices)
    }

    pub fn from_rect(center: glam::Vec2, half_extent: glam::Vec2) -> Self {
        Self::from_polygon(&[
            glam::vec2(center[0] - half_extent[0], center[1] - half_extent[1]),
            glam::vec2(center[0] - half_extent[0], center[1] + half_extent[1]),
            glam::vec2(center[0] + half_extent[0], center[1] + half_extent[1]),
            glam::vec2(center[0] + half_extent[0], center[1] - half_extent[1]),
        ])
    }

    pub fn from_rounded_rect(center: glam::Vec2, half_extent: glam::Vec2, radius: f32) -> Self {
        let vertices = [
            (
                glam::vec2(center[0] - half_extent[0] + radius, center[1] - half_extent[1]),
                glam::vec2(center[0] - half_extent[0], center[1] - half_extent[1]),
                glam::vec2(center[0] - half_extent[0], center[1] - half_extent[1] + radius),
            ),
            (
                glam::vec2(center[0] - half_extent[0], center[1] + half_extent[1] - radius),
                glam::vec2(center[0] - half_extent[0], center[1] + half_extent[1]),
                glam::vec2(center[0] - half_extent[0] + radius, center[1] + half_extent[1]),
            ),
            (
                glam::vec2(center[0] + half_extent[0] - radius, center[1] + half_extent[1]),
                glam::vec2(center[0] + half_extent[0], center[1] + half_extent[1]),
                glam::vec2(center[0] + half_extent[0], center[1] + half_extent[1] - radius),
            ),
            (
                glam::vec2(center[0] + half_extent[0], center[1] - half_extent[1] + radius),
                glam::vec2(center[0] + half_extent[0], center[1] - half_extent[1]),
                glam::vec2(center[0] + half_extent[0] - radius, center[1] - half_extent[1]),
            ),
        ];
        let mut result = Path::default();
        result.start = vertices[3].2;
        for (from, corner, to) in &vertices {
            result.push_line(LineSegment { control_points: [*from] });
            result.push_quarter_ellipse(*corner, *to);
        }
        result
    }

    pub fn from_ellipse(center: glam::Vec2, half_extent: glam::Vec2) -> Self {
        let vertices = [
            (
                glam::vec2(center[0] - half_extent[0], center[1] - half_extent[1]),
                glam::vec2(center[0] - half_extent[0], center[1]),
            ),
            (
                glam::vec2(center[0] - half_extent[0], center[1] + half_extent[1]),
                glam::vec2(center[0], center[1] + half_extent[1]),
            ),
            (
                glam::vec2(center[0] + half_extent[0], center[1] + half_extent[1]),
                glam::vec2(center[0] + half_extent[0], center[1]),
            ),
            (
                glam::vec2(center[0] + half_extent[0], center[1] - half_extent[1]),
                glam::vec2(center[0], center[1] - half_extent[1]),
            ),
        ];
        let mut result = Path::default();
        result.start = vertices[3].1;
        for (corner, to) in &vertices {
            result.push_quarter_ellipse(*corner, *to);
        }
        result
    }

    pub fn from_circle(center: glam::Vec2, radius: f32) -> Self {
        Self::from_ellipse(center, glam::vec2(radius, radius))
    }
}
