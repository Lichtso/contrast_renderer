//! (Optional) Working with [font faces](ttf_parser::Face), converting glyphs and text to [Path]s

use crate::path::{IntegralCubicCurveSegment, IntegralQuadraticCurveSegment, LineSegment, Path};
use geometric_algebra::ppga2d;

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

/// Defines where the X-axis origin of the text is.
pub enum HorizontalAlignment {
    /// Origin is to the left of the text (at the beginning).
    Left,
    /// Origin is at the center of the text (at the middle).
    Center,
    /// Origin is to the right of the text (at the end).
    Right,
}

/// Defines where the Y-axis origin of the text is.
pub enum VerticalAlignment {
    /// Origin is at the bottom of the text.
    Bottom,
    /// Origin is at the base line of the text.
    Base,
    /// Origin is at the center line of the text (middle between bottom and top).
    Center,
    /// Origin is at the top of the text.
    Top,
}

/// Arranges a given string to a set of paths (of its glyphs), according to the given alignment and font face.
pub fn paths_of_text(
    face: &ttf_parser::Face,
    horizontal_alignment: HorizontalAlignment,
    vertical_alignment: VerticalAlignment,
    text: &str,
) -> Vec<Path> {
    let mut layout = Vec::new();
    {
        let kerning_table = face.kerning_subtables().next();
        let mut offset_x = 0.0;
        let mut offset_y = 0.0;
        let mut prev_glyph_id = None;
        for char in text.chars() {
            let glyph_id = face.glyph_index(char).unwrap();
            if let Some(prev_glyph_id) = prev_glyph_id {
                if let Some(kerning_table) = kerning_table {
                    if let Some(kerning) = kerning_table.glyphs_kerning(prev_glyph_id, glyph_id) {
                        offset_x += kerning as f32;
                    }
                }
            }
            layout.push(([offset_x, offset_y], glyph_id));
            if let Some(advance_x) = face.glyph_hor_advance(glyph_id) {
                offset_x += advance_x as f32;
            }
            if let Some(advance_y) = face.glyph_ver_advance(glyph_id) {
                offset_y += advance_y as f32;
            }
            prev_glyph_id = Some(glyph_id);
        }
        offset_x *= match horizontal_alignment {
            HorizontalAlignment::Left => 0.0,
            HorizontalAlignment::Center => -0.5,
            HorizontalAlignment::Right => -1.0,
        };
        offset_y = -match vertical_alignment {
            VerticalAlignment::Bottom => face.descender() as f32,
            VerticalAlignment::Base => 0.0,
            VerticalAlignment::Center => face.x_height().unwrap() as f32 * 0.5,
            VerticalAlignment::Top => (face.ascender() + face.descender()) as f32,
        };
        for ([x, y], _glyph_id) in &mut layout {
            *x += offset_x;
            *y += offset_y;
        }
    }
    let mut result = Vec::new();
    for ([x, y], glyph_id) in layout {
        let scalator = ppga2d::Scalar { g0: 1.0 };
        let motor = ppga2d::Motor {
            g0: [1.0, 0.0, -0.5 * y, 0.5 * x].into(),
        };
        let mut paths = paths_of_glyph(&face, glyph_id);
        for path in &mut paths {
            path.transform(&scalator, &motor);
        }
        result.append(&mut paths);
    }
    result
}
