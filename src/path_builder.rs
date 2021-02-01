pub struct LineSegment {
    pub control_points: [glam::Vec2; 1],
}

pub struct QuadraticCurveSegment {
    pub control_points: [glam::Vec2; 2],
}

pub struct CubicCurveSegment {
    pub control_points: [glam::Vec2; 3],
}

pub struct RationalQuadraticCurveSegment {
    pub first_weight: f32,
    pub control_points: [glam::Vec3; 2],
}

pub struct RationalCubicCurveSegment {
    pub first_weight: f32,
    pub control_points: [glam::Vec3; 3],
}

pub enum Segment {
    Line(LineSegment),
    QuadraticCurve(QuadraticCurveSegment),
    CubicCurve(CubicCurveSegment),
    RationalQuadraticCurve(RationalQuadraticCurveSegment),
    RationalCubicCurve(RationalCubicCurveSegment),
}

impl From<LineSegment> for Segment {
    fn from(segment: LineSegment) -> Self {
        Self::Line(segment)
    }
}

impl From<QuadraticCurveSegment> for Segment {
    fn from(segment: QuadraticCurveSegment) -> Self {
        Self::QuadraticCurve(segment)
    }
}

impl From<CubicCurveSegment> for Segment {
    fn from(segment: CubicCurveSegment) -> Self {
        Self::CubicCurve(segment)
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
    QuadraticCurve,
    CubicCurve,
    RationalQuadraticCurve,
    RationalCubicCurve,
}

#[derive(Default)]
pub struct PathBuilder {
    pub anchor: glam::Vec2,
    pub line_segments: Vec<LineSegment>,
    pub quadratic_curve_segments: Vec<QuadraticCurveSegment>,
    pub cubic_curve_segments: Vec<CubicCurveSegment>,
    pub rational_quadratic_curve_segments: Vec<RationalQuadraticCurveSegment>,
    pub rational_cubic_curve_segments: Vec<RationalCubicCurveSegment>,
    pub segement_types: Vec<SegmentType>,
}

impl PathBuilder {
    pub fn push_line(&mut self, segment: LineSegment) {
        self.line_segments.push(segment);
        self.segement_types.push(SegmentType::Line);
    }

    pub fn push_quadratic_curve(&mut self, segment: QuadraticCurveSegment) {
        self.quadratic_curve_segments.push(segment);
        self.segement_types.push(SegmentType::QuadraticCurve);
    }

    pub fn push_cubic_curve(&mut self, segment: CubicCurveSegment) {
        self.cubic_curve_segments.push(segment);
        self.segement_types.push(SegmentType::CubicCurve);
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
            Segment::QuadraticCurve(segment) => {
                self.push_quadratic_curve(segment);
            }
            Segment::CubicCurve(segment) => {
                self.push_cubic_curve(segment);
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
