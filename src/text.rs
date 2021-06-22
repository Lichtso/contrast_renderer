//! (Optional) Working with [font faces](ttf_parser::Face), converting glyphs and text to [Path]s

use crate::{
    path::{IntegralCubicCurveSegment, IntegralQuadraticCurveSegment, LineSegment, Path},
    safe_float::SafeFloat,
};
use geometric_algebra::ppga2d;

/// Heap allocated font with a closed lifetime.
pub struct Font {
    _backing_store: Vec<u8>,
    parsed_face: [u8; std::mem::size_of::<ttf_parser::Face>()],
}

impl Font {
    /// Load a TTF font face.
    pub fn new(font_data: &[u8]) -> Self {
        let backing_store = font_data.to_vec();
        let parsed_face = unsafe { std::mem::transmute(ttf_parser::Face::from_slice(&backing_store, 0).unwrap()) };
        Self {
            _backing_store: backing_store,
            parsed_face,
        }
    }
    /// Get the parsed font face.
    pub fn face(&self) -> ttf_parser::Face {
        unsafe { std::mem::transmute(self.parsed_face) }
    }
}

#[derive(Default)]
struct OutlineBuilder {
    path: Path,
    paths: Vec<Path>,
}

impl ttf_parser::OutlineBuilder for OutlineBuilder {
    fn move_to(&mut self, x: f32, y: f32) {
        self.path.start = [x, y].into();
    }

    fn line_to(&mut self, x: f32, y: f32) {
        self.path.push_line(LineSegment {
            control_points: [[x, y].into()],
        });
    }

    fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
        self.path.push_integral_quadratic_curve(IntegralQuadraticCurveSegment {
            control_points: [[x1, y1].into(), [x, y].into()],
        });
    }

    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
        self.path.push_integral_cubic_curve(IntegralCubicCurveSegment {
            control_points: [[x1, y1].into(), [x2, y2].into(), [x, y].into()],
        });
    }

    fn close(&mut self) {
        let mut path = Path::default();
        std::mem::swap(&mut path, &mut self.path);
        self.paths.push(path);
    }
}

/// Returns the paths of a given glyph in a given font face.
pub fn paths_of_glyph(face: &ttf_parser::Face, glyph_id: ttf_parser::GlyphId) -> Vec<Path> {
    let mut outline_builder = OutlineBuilder::default();
    if let Some(_bounding_box) = face.outline_glyph(glyph_id, &mut outline_builder) {
        outline_builder.paths
    } else {
        Vec::new()
    }
}

/// Defines the axis and direction of text flow.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Orientation {
    /// The text flows from right to left (-X).
    RightToLeft,
    /// The text flows from left to right (+X).
    LeftToRight,
    /// The text flows from top to bottom (-Y).
    TopToBottom,
    /// The text flows from bottom to top (+Y).
    BottomToTop,
}

/// Defines where the origin of the text is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Alignment {
    /// Origin is to the bottom left of the text (assuming [Orientation] is [LeftToRight]).
    Begin,
    /// Origin is on the baseline of the text.
    Baseline,
    /// Origin is at the center of the text.
    Center,
    /// Origin is to the top right of the text (assuming [Orientation] is [LeftToRight]).
    End,
}

/// Defines the geometric layout of a text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Layout {
    /// Font size
    pub size: SafeFloat<f32, 1>,
    /// Axis and direction of text flow
    pub orientation: Orientation,
    /// Alignment along the axis defined by [Orientation]
    pub major_alignment: Alignment,
    /// Alignment along the other axis
    pub minor_alignment: Alignment,
}

/// Arranges a given string to a set of paths (of its glyphs), according to the given alignment and font face.
pub fn paths_of_text(face: &ttf_parser::Face, layout: &Layout, text: &str) -> Vec<Path> {
    let mut glyph_positions = Vec::new();
    {
        let kerning_table = face.kerning_subtables().next();
        let mut major_offset = 0.0;
        let mut prev_glyph_id = None;
        for char in text.chars() {
            let glyph_id = face.glyph_index(char).unwrap();
            if let Some(prev_glyph_id) = prev_glyph_id {
                if let Some(kerning_table) = kerning_table {
                    if let Some(kerning) = kerning_table.glyphs_kerning(prev_glyph_id, glyph_id) {
                        major_offset += kerning as f32;
                    }
                }
            }
            match layout.orientation {
                Orientation::RightToLeft | Orientation::LeftToRight => {
                    glyph_positions.push(([major_offset, 0.0], glyph_id));
                    if let Some(advance_x) = face.glyph_hor_advance(glyph_id) {
                        major_offset += advance_x as f32;
                    }
                }
                Orientation::TopToBottom | Orientation::BottomToTop => {
                    glyph_positions.push(([0.0, major_offset], glyph_id));
                    if let Some(advance_y) = face.glyph_ver_advance(glyph_id) {
                        major_offset += advance_y as f32;
                    }
                }
            }
            prev_glyph_id = Some(glyph_id);
        }
        major_offset *= match layout.major_alignment {
            Alignment::Begin => 0.0,
            Alignment::Baseline | Alignment::Center => -0.5,
            Alignment::End => -1.0,
        };
        let mut minor_offset = -match layout.minor_alignment {
            Alignment::Begin => face.descender() as f32,
            Alignment::Baseline => 0.0,
            Alignment::Center => face.x_height().unwrap() as f32 * 0.5,
            Alignment::End => (face.ascender() + face.descender()) as f32,
        };
        let (swap, mut major_sign, mut minor_sign) = match layout.orientation {
            Orientation::RightToLeft => (false, -1.0, 1.0),
            Orientation::LeftToRight => (false, 1.0, 1.0),
            Orientation::TopToBottom => (true, 1.0, -1.0),
            Orientation::BottomToTop => (true, 1.0, 1.0),
        };
        if swap {
            std::mem::swap(&mut major_offset, &mut minor_offset);
            std::mem::swap(&mut major_sign, &mut minor_sign);
        }
        for (offsets, _glyph_id) in &mut glyph_positions {
            offsets[0] = major_sign * (offsets[0] + major_offset);
            offsets[1] = minor_sign * (offsets[1] + minor_offset);
        }
    }
    let scale = layout.size.unwrap() / face.height() as f32;
    let mut result = Vec::new();
    for ([x, y], glyph_id) in glyph_positions {
        let scalator = ppga2d::Scalar { g0: scale };
        let motor = ppga2d::Motor {
            g0: [1.0, 0.0, -0.5 * y * scale, 0.5 * x * scale].into(),
        };
        let mut paths = paths_of_glyph(&face, glyph_id);
        for path in &mut paths {
            path.transform(&scalator, &motor);
        }
        result.append(&mut paths);
    }
    result
}

/// Calculates the bounding box in which [paths_of_text] fits, including preemptive spacing.
///
/// Texts like "hello" and "bye" use the same space even though the "y" in "bye" extends further downward.
/// This way they are easier to align in the same row.
pub fn half_extent_of_text(face: &ttf_parser::Face, layout: &Layout, text: &str) -> SafeFloat<f32, 2> {
    let kerning_table = face.kerning_subtables().next();
    let mut major_size = 0.0;
    let mut minor_size = (face.ascender() + face.descender()) as f32;
    let mut prev_glyph_id = None;
    for char in text.chars() {
        let glyph_id = face.glyph_index(char).unwrap();
        if let Some(prev_glyph_id) = prev_glyph_id {
            if let Some(kerning_table) = kerning_table {
                if let Some(kerning) = kerning_table.glyphs_kerning(prev_glyph_id, glyph_id) {
                    major_size += kerning as f32;
                }
            }
        }
        match layout.orientation {
            Orientation::RightToLeft | Orientation::LeftToRight => {
                if let Some(advance_x) = face.glyph_hor_advance(glyph_id) {
                    major_size += advance_x as f32;
                }
            }
            Orientation::TopToBottom | Orientation::BottomToTop => {
                if let Some(advance_y) = face.glyph_ver_advance(glyph_id) {
                    major_size += advance_y as f32;
                }
            }
        }
        prev_glyph_id = Some(glyph_id);
    }
    let scale = 0.5 * layout.size.unwrap() / face.height() as f32;
    major_size *= scale;
    minor_size *= scale;
    match layout.orientation {
        Orientation::RightToLeft | Orientation::LeftToRight => [major_size, minor_size].into(),
        Orientation::TopToBottom | Orientation::BottomToTop => [minor_size, major_size].into(),
    }
}
