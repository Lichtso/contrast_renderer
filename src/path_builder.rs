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
    pub first_weight: f32,
    pub control_points: [glam::Vec3; 2],
}

#[derive(Debug, Clone, Copy)]
pub struct RationalCubicCurveSegment {
    pub first_weight: f32,
    pub control_points: [glam::Vec3; 3],
}

#[derive(Debug, Clone, Copy)]
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
}

pub enum SegmentType {
    Line,
    IntegralQuadraticCurve,
    IntegralCubicCurve,
    RationalQuadraticCurve,
    RationalCubicCurve,
}

#[derive(Default)]
pub struct PathBuilder {
    pub anchor: glam::Vec2,
    pub line_segments: Vec<LineSegment>,
    pub integral_quadratic_curve_segments: Vec<IntegralQuadraticCurveSegment>,
    pub integral_cubic_curve_segments: Vec<IntegralCubicCurveSegment>,
    pub rational_quadratic_curve_segments: Vec<RationalQuadraticCurveSegment>,
    pub rational_cubic_curve_segments: Vec<RationalCubicCurveSegment>,
    pub segement_types: Vec<SegmentType>,
}

impl PathBuilder {
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

    pub fn push_segment(&mut self, segment: Segment) {
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
    }
}
