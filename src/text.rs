use crate::path::{IntegralCubicCurveSegment, IntegralQuadraticCurveSegment, LineSegment, Path};

#[derive(Default)]
struct OutlineBuilder {
    path: Path,
    paths: Vec<Path>,
}

impl ttf_parser::OutlineBuilder for OutlineBuilder {
    fn move_to(&mut self, x: f32, y: f32) {
        self.path.start = glam::vec2(x, y);
    }

    fn line_to(&mut self, x: f32, y: f32) {
        self.path.push_line(LineSegment {
            control_points: [glam::vec2(x, y)],
        });
    }

    fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
        self.path.push_integral_quadratic_curve(IntegralQuadraticCurveSegment {
            control_points: [glam::vec2(x1, y1), glam::vec2(x, y)],
        });
    }

    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
        self.path.push_integral_cubic_curve(IntegralCubicCurveSegment {
            control_points: [glam::vec2(x1, y1), glam::vec2(x2, y2), glam::vec2(x, y)],
        });
    }

    fn close(&mut self) {
        let mut path = Path::default();
        std::mem::swap(&mut path, &mut self.path);
        self.paths.push(path);
    }
}

pub fn paths_of_glyph(face: &ttf_parser::Face, glyph_id: ttf_parser::GlyphId) -> Vec<Path> {
    let mut outline_builder = OutlineBuilder::default();
    if let Some(_bounding_box) = face.outline_glyph(glyph_id, &mut outline_builder) {
        outline_builder.paths
    } else {
        Vec::new()
    }
}

pub enum HorizontalAlignment {
    Left,
    Center,
    Right,
}

pub enum VerticalAlignment {
    Bottom,
    Base,
    Center,
    Top,
}

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
        let transform = glam::Mat3::from_scale_angle_translation(glam::vec2(1.0, 1.0), 0.0, glam::vec2(x, y));
        let mut paths = paths_of_glyph(&face, glyph_id);
        for path in &mut paths {
            path.transform(&transform);
        }
        result.append(&mut paths);
    }
    result
}
