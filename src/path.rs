//! Defining the geometry and rendering options of [Path]s

use crate::{
    error::ERROR_MARGIN,
    utils::{point_to_vec, vec_to_point, weighted_vec_to_point},
};
use geometric_algebra::{ppga2d, InnerProduct, RegressiveProduct, Signum, SquaredMagnitude, Transformation, Zero};

/// A line
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LineSegment {
    /// The start is excluded as it is implicitly defined as the end of the previous [Path] segment.
    pub control_points: [[f32; 2]; 1],
}

/// An integral quadratic bezier curve
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IntegralQuadraticCurveSegment {
    /// The start is excluded as it is implicitly defined as the end of the previous [Path] segment.
    pub control_points: [[f32; 2]; 2],
}

/// An integral cubic bezier curve
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IntegralCubicCurveSegment {
    /// The start is excluded as it is implicitly defined as the end of the previous [Path] segment.
    pub control_points: [[f32; 2]; 3],
}

/// A rational quadratic bezier curve
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RationalQuadraticCurveSegment {
    /// Weight of `control_points[0]` (the middle).
    ///
    /// The weights of the start and end control points are fixed to [1.0].
    pub weight: f32,
    /// The start is excluded as it is implicitly defined as the end of the previous [Path] segment.
    pub control_points: [[f32; 2]; 2],
}

/// A rational cubic bezier curve
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RationalCubicCurveSegment {
    /// Weights including the start, thus shifted by one compared to the control_points.
    pub weights: [f32; 4],
    /// The start is excluded as it is implicitly defined as the end of the previous [Path] segment.
    pub control_points: [[f32; 2]; 3],
}

/// Different types of [Path] segments as an enum
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SegmentType {
    /// For lines
    Line,
    /// For integral quadratic bezier curves
    IntegralQuadraticCurve,
    /// For integral cubic bezier curves
    IntegralCubicCurve,
    /// For rational quadratic bezier curves
    RationalQuadraticCurve,
    /// For rational cubic bezier curves
    RationalCubicCurve,
}

/// Defines what geometry is generated where [Path] segments meet.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Join {
    /// Polygon of the intersection of the adjacent [Path] segments
    ///
    /// To prevent the intersection from extending too far out at sharp angles,
    /// the polygon is clipped by a line which is perpendicular to the angle bisector of the adjacent [Path] segments.
    /// Where this line is located is defined by [miter_clip](StrokeOptions::miter_clip).
    Miter,
    /// Polygon of the vertices perpendicular to the tangents of the adjacent [Path] segments
    Bevel,
    /// Circular arc with a radius of half the [width](StrokeOptions::width), centered where the adjacent [Path] segments meet
    Round,
}

/// Defines what geometry is generated at the start and the end of a dash.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cap {
    /// Rectangular polygon extending half the [width](StrokeOptions::width) beyond the end of the dash
    Square,
    /// Circular arc with a radius of half the [width](StrokeOptions::width), centered at the end of the dash
    Round,
    /// Triangular polygon extending half the [width](StrokeOptions::width) beyond the end of the dash
    Out,
    /// Triangular cut out from a rectangular polygon extending half the [width](StrokeOptions::width) beyond the end of the dash
    In,
    /// Ramp shaped polygon extending [width](StrokeOptions::width) beyond the end of the dash, facing to the right of the [Path]s forward direction
    Right,
    /// Ramp shaped polygon extending [width](StrokeOptions::width) beyond the end of the dash, facing to the left of the [Path]s forward direction
    Left,
    /// Perpendicular clean cut exactly at the end of the dash
    Butt,
}

/// Defines the gaps in a stroked [Path].
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DashInterval {
    /// Start of the gap to the next dash, thus end of the current dash.
    ///
    /// It is measured in terms of the [StrokeOptions::width].
    pub gap_start: f32,
    /// End of the current gap, thus start of the next dash.
    ///
    /// It is measured in terms of the [StrokeOptions::width].
    pub gap_end: f32,
    /// Cap at the start of the current dash, thus at the end of the last gap.
    pub dash_start: Cap,
    /// Cap at the end of the current dash, thus at the start of the next gap.
    pub dash_end: Cap,
}

/// Maximum number of [DashInterval]s in [DynamicStrokeOptions]
pub const MAX_DASH_INTERVALS: usize = 4;

/// Dynamic part of [StrokeOptions].
///
/// It is grouped and can be used by multiple [Path]s in the same [Shape](crate::renderer::Shape).
#[derive(Debug, Clone, PartialEq)]
pub enum DynamicStrokeOptions {
    /// Defines a dashed stroke pattern.
    Dashed {
        /// Defines what geometry is generated where [Path] segments meet.
        join: Join,
        /// Defines the [DashInterval]s which will be repeated along the stroked [Path].
        pattern: Vec<DashInterval>,
        /// Translates the [DashInterval]s along the stroked [Path].
        ///
        /// Positive values translate towards the forward direction of the stroked [Path].
        /// It is measured in terms of the [width](StrokeOptions::width).
        phase: f32,
    },
    /// Defines a solid stroke pattern.
    Solid {
        /// Defines what geometry is generated where [Path] segments meet.
        join: Join,
        /// Defines what geometry is generated at the start of the [Path].
        start: Cap,
        /// Defines what geometry is generated at the end of the [Path].
        end: Cap,
    },
}

/// Defines the parametric sampling strategy for stroking curves.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CurveApproximation {
    /// Parametric step size is `1.0 / n`.
    ///
    /// Thus there are `n + 1` parameters (including start and end).
    UniformlySpacedParameters(usize),
    /// Tangent step angle in radians is `a`.
    ///
    /// Thus there are `(polar_range.arg() / a + 0.5) as usize + 1` parameters (including start and end).
    UniformTangentAngle(f32),
    /*
    /// Euclidian distance is `d`.
    ///
    /// Thus there are `(arc_length / d + 0.5) as usize + 1` parameters (including start and end).
    UniformArcLength(f32),*/
}

/// Defines how a [Path] is stroked.
#[derive(Debug, Clone, PartialEq)]
pub struct StrokeOptions {
    /// The width of the stroked [Path]
    ///
    /// The absolute value is used, so the sign has no effect.
    pub width: f32,
    /// Offsets the stroke relative to the actual [Path].
    ///
    /// It is measured in terms of the [width](StrokeOptions::width) and clamped to [-0.5, 0.5].
    /// Negative values shift the stroke to the left and positive value shift the stroke to the right (in forward direction).
    pub offset: f32,
    /// Distance from the point where the adjacent [Path] segments meet to the clip line.
    ///
    /// It is measured in terms of the [width](StrokeOptions::width).
    /// The absolute value is used, so the sign has no effect.
    pub miter_clip: f32,
    /// If set to [true] the start and the end of the [Path] will be connected by an implicit [LineSegment].
    pub closed: bool,
    /// Index of the [DynamicStrokeOptions] group to use
    pub dynamic_stroke_options_group: usize,
    /// Defines the parametric sampling strategy for stroking curves.
    pub curve_approximation: CurveApproximation,
}

impl StrokeOptions {
    /// Call this to make sure all parameters are within the allowed limits
    pub fn legalize(&mut self) {
        self.width = self.width.abs();
        self.offset = self.offset.clamp(-0.5, 0.5);
        self.miter_clip = self.miter_clip.abs();
    }
}

fn tangent_from_points(a: [f32; 2], b: [f32; 2]) -> ppga2d::Plane {
    vec_to_point(a).regressive_product(vec_to_point(b))
}

/// A sequence of segments that can be either stroked or filled
///
/// Every "move to" command requires a new [Path].
/// The order of the segments defines the direction of the [Path] and its clockwise or counterclockwise orientation.
/// Filled [Path]s increment the winding counter when they are counterclockwise and decrement it when they are clockwise.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Path {
    /// If [Some] then the [Path] will be stroked otherwise (if [None]) it will be filled.
    pub stroke_options: Option<StrokeOptions>,
    /// Beginning of the [Path] (position of "move to" command).
    pub start: [f32; 2],
    /// Storage for all the line segments of the [Path].
    pub line_segments: Vec<LineSegment>,
    /// Storage for all the integral quadratic curve segments of the [Path].
    pub integral_quadratic_curve_segments: Vec<IntegralQuadraticCurveSegment>,
    /// Storage for all the integral cubic curve segments of the [Path].
    pub integral_cubic_curve_segments: Vec<IntegralCubicCurveSegment>,
    /// Storage for all the rational quadratic curve segments of the [Path].
    pub rational_quadratic_curve_segments: Vec<RationalQuadraticCurveSegment>,
    /// Storage for all the rational cubic curve segments of the [Path].
    pub rational_cubic_curve_segments: Vec<RationalCubicCurveSegment>,
    /// Defines how the segments of different types are interleaved.
    pub segement_types: Vec<SegmentType>,
}

impl Path {
    /// "line to" command
    pub fn push_line(&mut self, segment: LineSegment) {
        self.line_segments.push(segment);
        self.segement_types.push(SegmentType::Line);
    }

    /// "quadratic to" command
    pub fn push_integral_quadratic_curve(&mut self, segment: IntegralQuadraticCurveSegment) {
        self.integral_quadratic_curve_segments.push(segment);
        self.segement_types.push(SegmentType::IntegralQuadraticCurve);
    }

    /// "cubic to" command
    pub fn push_integral_cubic_curve(&mut self, segment: IntegralCubicCurveSegment) {
        self.integral_cubic_curve_segments.push(segment);
        self.segement_types.push(SegmentType::IntegralCubicCurve);
    }

    /// "quadratic to" command with weights
    pub fn push_rational_quadratic_curve(&mut self, segment: RationalQuadraticCurveSegment) {
        self.rational_quadratic_curve_segments.push(segment);
        self.segement_types.push(SegmentType::RationalQuadraticCurve);
    }

    /// "cubic to" command with weights
    pub fn push_rational_cubic_curve(&mut self, segment: RationalCubicCurveSegment) {
        self.rational_cubic_curve_segments.push(segment);
        self.segement_types.push(SegmentType::RationalCubicCurve);
    }

    /// Returns the current end of the [Path].
    ///
    /// Returns the `start` if the [Path] is empty (has no segments).
    pub fn get_end(&self) -> [f32; 2] {
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

    /// Returns the normalized tangent at the start in direction of the [Path].
    ///
    /// Returns zero if the [Path] is empty (has no segments).
    /// Useful for arrow heads / tails.
    pub fn get_start_tangent(&self) -> ppga2d::Plane {
        match self.segement_types.last() {
            Some(SegmentType::Line) => {
                let segment = self.line_segments.last().unwrap();
                tangent_from_points(self.start, segment.control_points[0]).signum()
            }
            Some(SegmentType::IntegralQuadraticCurve) => {
                let segment = self.integral_quadratic_curve_segments.last().unwrap();
                tangent_from_points(self.start, segment.control_points[0]).signum()
            }
            Some(SegmentType::IntegralCubicCurve) => {
                let segment = self.integral_cubic_curve_segments.last().unwrap();
                tangent_from_points(self.start, segment.control_points[0]).signum()
            }
            Some(SegmentType::RationalQuadraticCurve) => {
                let segment = self.rational_quadratic_curve_segments.last().unwrap();
                tangent_from_points(self.start, segment.control_points[0]).signum()
            }
            Some(SegmentType::RationalCubicCurve) => {
                let segment = self.rational_cubic_curve_segments.last().unwrap();
                tangent_from_points(self.start, segment.control_points[0]).signum()
            }
            None => ppga2d::Plane::zero(),
        }
    }

    /// Returns the normalized tangent at the end in direction of the [Path].
    ///
    /// Returns zero if the [Path] is empty (has no segments).
    /// Useful for arrow heads / tails.
    pub fn get_end_tangent(&self) -> ppga2d::Plane {
        match self.segement_types.last() {
            Some(SegmentType::Line) => {
                let previous_point = match self.segement_types.iter().rev().nth(1) {
                    Some(SegmentType::Line) => {
                        let segment = self.line_segments.iter().rev().nth(1).unwrap();
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
                };
                let segment = self.line_segments.last().unwrap();
                tangent_from_points(previous_point, segment.control_points[0]).signum()
            }
            Some(SegmentType::IntegralQuadraticCurve) => {
                let segment = self.integral_quadratic_curve_segments.last().unwrap();
                tangent_from_points(segment.control_points[0], segment.control_points[1]).signum()
            }
            Some(SegmentType::IntegralCubicCurve) => {
                let segment = self.integral_cubic_curve_segments.last().unwrap();
                tangent_from_points(segment.control_points[1], segment.control_points[2]).signum()
            }
            Some(SegmentType::RationalQuadraticCurve) => {
                let segment = self.rational_quadratic_curve_segments.last().unwrap();
                tangent_from_points(segment.control_points[0], segment.control_points[1]).signum()
            }
            Some(SegmentType::RationalCubicCurve) => {
                let segment = self.rational_cubic_curve_segments.last().unwrap();
                tangent_from_points(segment.control_points[1], segment.control_points[2]).signum()
            }
            None => ppga2d::Plane::zero(),
        }
    }

    /// Concatenates two [Path]s, leaving the `other` [Path] empty.
    pub fn append(&mut self, other: &mut Self) {
        self.line_segments.append(&mut other.line_segments);
        self.integral_quadratic_curve_segments
            .append(&mut other.integral_quadratic_curve_segments);
        self.integral_cubic_curve_segments.append(&mut other.integral_cubic_curve_segments);
        self.rational_quadratic_curve_segments
            .append(&mut other.rational_quadratic_curve_segments);
        self.rational_cubic_curve_segments.append(&mut other.rational_cubic_curve_segments);
    }

    /// Transforms all control points of the [Path] (including the `start` and all segments).
    pub fn transform(&mut self, scalator: &ppga2d::Scalar, motor: &ppga2d::Motor) {
        let transformator = [
            motor.transformation(ppga2d::Point { g0: [1.0, 0.0, 0.0].into() }),
            motor.transformation(ppga2d::Point {
                g0: [0.0, scalator.g0, 0.0].into(),
            }),
            motor.transformation(ppga2d::Point {
                g0: [0.0, 0.0, scalator.g0].into(),
            }),
        ];
        fn transform_point(transformator: &[ppga2d::Point; 3], p: [f32; 2]) -> [f32; 2] {
            [
                transformator[0].g0[1] + p[0] * transformator[1].g0[1] + p[1] * transformator[2].g0[1],
                transformator[0].g0[2] + p[0] * transformator[1].g0[2] + p[1] * transformator[2].g0[2],
            ]
        }
        self.start = transform_point(&transformator, self.start);
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
                        *control_point = transform_point(&transformator, *control_point);
                    }
                }
                SegmentType::IntegralQuadraticCurve => {
                    let segment = integral_quadratic_curve_segment_iter.next().unwrap();
                    for control_point in &mut segment.control_points {
                        *control_point = transform_point(&transformator, *control_point);
                    }
                }
                SegmentType::IntegralCubicCurve => {
                    let segment = integral_cubic_curve_segment_iter.next().unwrap();
                    for control_point in &mut segment.control_points {
                        *control_point = transform_point(&transformator, *control_point);
                    }
                }
                SegmentType::RationalQuadraticCurve => {
                    let segment = rational_quadratic_curve_segment_iter.next().unwrap();
                    for control_point in &mut segment.control_points {
                        *control_point = transform_point(&transformator, *control_point);
                    }
                }
                SegmentType::RationalCubicCurve => {
                    let segment = rational_cubic_curve_segment_iter.next().unwrap();
                    for control_point in &mut segment.control_points {
                        *control_point = transform_point(&transformator, *control_point);
                    }
                }
            }
        }
    }

    /// Reverses the direction of the [Path] and all its segments.
    ///
    /// Thus, swaps the values of `start` and `get_end()`.
    /// Also flips the clockwise or counterclockwise orientation.
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

    /// Turns integral quadratic curve segments into rational quadratic curve segments and
    /// integral cubic curve segments into rational cubic curve segments.
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
                            weight: 1.0,
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

    /// Turns integral quadratic curve segments into integral cubic curve segments and
    /// rational quadratic curve segments into rational cubic curve segments.
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
                                [
                                    previous_control_point[0] + (segment.control_points[0][0] - previous_control_point[0]) * 2.0 / 3.0,
                                    previous_control_point[1] + (segment.control_points[0][1] - previous_control_point[1]) * 2.0 / 3.0,
                                ],
                                [
                                    segment.control_points[1][0] + (segment.control_points[0][0] - segment.control_points[1][0]) * 2.0 / 3.0,
                                    segment.control_points[1][1] + (segment.control_points[0][1] - segment.control_points[1][1]) * 2.0 / 3.0,
                                ],
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
                    let control_points = [
                        vec_to_point(previous_control_point),
                        weighted_vec_to_point(segment.weight, segment.control_points[0]),
                        vec_to_point(segment.control_points[1]),
                    ];
                    let new_control_points = [
                        control_points[0] + (control_points[1] - control_points[0]) * ppga2d::Scalar { g0: 2.0 / 3.0 },
                        control_points[2] + (control_points[1] - control_points[2]) * ppga2d::Scalar { g0: 2.0 / 3.0 },
                    ];
                    self.rational_cubic_curve_segments.insert(
                        rational_cubic_curve_segment_index,
                        RationalCubicCurveSegment {
                            weights: [1.0, new_control_points[0].g0[0], new_control_points[1].g0[0], 1.0],
                            control_points: [
                                point_to_vec(new_control_points[0]),
                                point_to_vec(new_control_points[1]),
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

    /// "close" command
    ///
    /// A filled [Path] or a closed stroked [Path] already has an implicit [LineSegment] at the end.
    /// But this method can still be useful for reversing a closed stroked [Path] when the start and end should stay at the same location.
    pub fn close(&mut self) {
        if tangent_from_points(self.start, self.get_end()).squared_magnitude().g0 <= ERROR_MARGIN {
            return;
        }
        self.push_line(LineSegment {
            control_points: [self.start],
        });
    }

    /// "arc to" command
    pub fn push_arc(&mut self, tangent_crossing: [f32; 2], to: [f32; 2]) {
        let inner_product = tangent_from_points(tangent_crossing, self.get_end()).inner_product(tangent_from_points(tangent_crossing, to));
        self.push_rational_quadratic_curve(RationalQuadraticCurveSegment {
            weight: (inner_product.g0.acos() * 0.5).sin(),
            control_points: [tangent_crossing, to],
        });
    }

    fn push_quarter_ellipse(&mut self, tangent_crossing: [f32; 2], to: [f32; 2]) {
        self.push_rational_quadratic_curve(RationalQuadraticCurveSegment {
            weight: std::f32::consts::FRAC_1_SQRT_2,
            control_points: [tangent_crossing, to],
        });
    }

    /// Construct a polygon [Path] from a sequence of points.
    pub fn from_polygon(vertices: &[[f32; 2]]) -> Self {
        let mut vertices = vertices.iter();
        let mut result = Path {
            start: *vertices.next().unwrap(),
            ..Path::default()
        };
        for control_point in vertices {
            result.push_line(LineSegment {
                control_points: [*control_point],
            });
        }
        result
    }

    /// Construct a polygon [Path] by approximating a circle using a finite number of points.
    pub fn from_regular_polygon(center: [f32; 2], radius: f32, rotation: f32, vertex_count: usize) -> Self {
        let mut vertices = Vec::with_capacity(vertex_count);
        for i in 0..vertex_count {
            let angle = rotation + i as f32 / vertex_count as f32 * std::f32::consts::PI * 2.0;
            vertices.push([center[0] + radius * angle.cos(), center[1] + radius * angle.sin()]);
        }
        Self::from_polygon(&vertices)
    }

    /// Construct a polygon [Path] from a rectangle.
    pub fn from_rect(center: [f32; 2], half_extent: [f32; 2]) -> Self {
        Self::from_polygon(&[
            [center[0] - half_extent[0], center[1] - half_extent[1]],
            [center[0] - half_extent[0], center[1] + half_extent[1]],
            [center[0] + half_extent[0], center[1] + half_extent[1]],
            [center[0] + half_extent[0], center[1] - half_extent[1]],
        ])
    }

    /// Construct a [Path] from a rectangle with quarter circle roundings at the corners.
    pub fn from_rounded_rect(center: [f32; 2], half_extent: [f32; 2], radius: f32) -> Self {
        let vertices = [
            (
                [center[0] - half_extent[0] + radius, center[1] - half_extent[1]],
                [center[0] - half_extent[0], center[1] - half_extent[1]],
                [center[0] - half_extent[0], center[1] - half_extent[1] + radius],
            ),
            (
                [center[0] - half_extent[0], center[1] + half_extent[1] - radius],
                [center[0] - half_extent[0], center[1] + half_extent[1]],
                [center[0] - half_extent[0] + radius, center[1] + half_extent[1]],
            ),
            (
                [center[0] + half_extent[0] - radius, center[1] + half_extent[1]],
                [center[0] + half_extent[0], center[1] + half_extent[1]],
                [center[0] + half_extent[0], center[1] + half_extent[1] - radius],
            ),
            (
                [center[0] + half_extent[0], center[1] - half_extent[1] + radius],
                [center[0] + half_extent[0], center[1] - half_extent[1]],
                [center[0] + half_extent[0] - radius, center[1] - half_extent[1]],
            ),
        ];
        let mut result = Path {
            start: vertices[3].2,
            ..Path::default()
        };
        for (from, corner, to) in &vertices {
            result.push_line(LineSegment { control_points: [*from] });
            result.push_quarter_ellipse(*corner, *to);
        }
        result
    }

    /// Constructs a [Path] from an ellipse.
    pub fn from_ellipse(center: [f32; 2], half_extent: [f32; 2]) -> Self {
        let vertices = [
            (
                [center[0] - half_extent[0], center[1] - half_extent[1]],
                [center[0] - half_extent[0], center[1]],
            ),
            (
                [center[0] - half_extent[0], center[1] + half_extent[1]],
                [center[0], center[1] + half_extent[1]],
            ),
            (
                [center[0] + half_extent[0], center[1] + half_extent[1]],
                [center[0] + half_extent[0], center[1]],
            ),
            (
                [center[0] + half_extent[0], center[1] - half_extent[1]],
                [center[0], center[1] - half_extent[1]],
            ),
        ];
        let mut result = Path {
            start: vertices[3].1,
            ..Path::default()
        };
        for (corner, to) in &vertices {
            result.push_quarter_ellipse(*corner, *to);
        }
        result
    }

    /// Constructs a [Path] from a circle.
    pub fn from_circle(center: [f32; 2], radius: f32) -> Self {
        Self::from_ellipse(center, [radius, radius])
    }
}
