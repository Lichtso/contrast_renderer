//! (Optional) Working with [font faces](ttf_parser::Face), converting glyphs and text to [Path]s

use crate::{
    path::{IntegralCubicCurveSegment, IntegralQuadraticCurveSegment, LineSegment, Path},
    safe_float::SafeFloat,
    utils::{aabb_to_convex_polygon, do_convex_polygons_overlap, translate2d},
};
use geometric_algebra::ppga2d;

/// Heap allocated font with a closed lifetime.
pub struct Font {
    _backing_store: std::pin::Pin<Box<[u8]>>,
    parsed_face: ttf_parser::Face<'static>,
}

impl Font {
    /// Load a TTF font face.
    pub fn new(font_data: &[u8]) -> Self {
        let backing_store = std::pin::Pin::new(font_data.to_vec().into_boxed_slice());
        let backing_slice = unsafe { std::mem::transmute::<&[u8], &[u8]>(&backing_store) };
        Self {
            _backing_store: backing_store,
            parsed_face: ttf_parser::Face::from_slice(backing_slice, 0).unwrap(),
        }
    }

    /// Get the parsed font face.
    pub fn face(&self) -> &ttf_parser::Face {
        &self.parsed_face
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

macro_rules! calculate_kerning {
    ($face:expr, $layout:expr, $text:expr, |$glyph_position:ident, $glyph_id:ident| $glyph_position_callback:tt) => {{
        let kerning_table = $face.tables().kern.and_then(|table| table.subtables.into_iter().next());
        let mut major_offset = 0.0;
        let mut prev_glyph_id = None;
        for char in $text.chars() {
            let $glyph_id = $face.glyph_index(char).unwrap();
            if let Some(prev_glyph_id) = prev_glyph_id {
                if let Some(kerning_table) = kerning_table {
                    if let Some(kerning) = kerning_table.glyphs_kerning(prev_glyph_id, $glyph_id) {
                        major_offset += kerning as f32;
                    }
                }
            }
            let $glyph_position = match $layout.orientation {
                Orientation::RightToLeft | Orientation::LeftToRight => {
                    let position = [major_offset, 0.0];
                    if let Some(advance_x) = $face.glyph_hor_advance($glyph_id) {
                        major_offset += advance_x as f32;
                    }
                    position
                }
                Orientation::TopToBottom | Orientation::BottomToTop => {
                    let position = [0.0, major_offset];
                    if let Some(advance_y) = $face.glyph_ver_advance($glyph_id) {
                        major_offset += advance_y as f32;
                    }
                    position
                }
            };
            $glyph_position_callback
            prev_glyph_id = Some($glyph_id);
        }
        major_offset
    }};
}

macro_rules! calculate_aligned_positions {
    ($face:expr, $layout:expr, $text:expr) => {{
        let mut glyph_positions = Vec::new();
        let mut major_offset = calculate_kerning!($face, $layout, $text, |glyph_position, glyph_id| {
            glyph_positions.push((glyph_position, glyph_id));
        });
        major_offset *= match $layout.major_alignment {
            Alignment::Begin => 0.0,
            Alignment::Baseline | Alignment::Center => -0.5,
            Alignment::End => -1.0,
        };
        let mut minor_offset = -match $layout.minor_alignment {
            Alignment::Begin => $face.descender() as f32,
            Alignment::Baseline => 0.0,
            Alignment::Center => $face.x_height().unwrap() as f32 * 0.5,
            Alignment::End => $face.height() as f32,
        };
        let (swap, mut major_sign, mut minor_sign) = match $layout.orientation {
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
        (major_offset, minor_offset, glyph_positions)
    }};
}

/// Arranges a given string to a set of paths (of its glyphs), according to the given alignment and font face.
///
/// If a `clipping_area` convex polygon is given, then glyphs which are completely outside are discarded.
/// Other glyphs will stay at the same position.
pub fn paths_of_text(face: &ttf_parser::Face, layout: &Layout, text: &str, clipping_area: Option<&[ppga2d::Point]>) -> Vec<Path> {
    let (_major_offset, _minor_offset, glyph_positions) = calculate_aligned_positions!(face, layout, text);
    let scale = layout.size.unwrap() / face.height() as f32;
    let mut result = Vec::new();
    for ([x, y], glyph_id) in glyph_positions {
        if let Some(glyph_bounding_box) = face.glyph_bounding_box(glyph_id) {
            if let Some(clipping_area) = clipping_area {
                let aabb = [
                    (glyph_bounding_box.x_min as f32 + x) * scale,
                    (glyph_bounding_box.y_min as f32 + y) * scale,
                    (glyph_bounding_box.x_max as f32 + x) * scale,
                    (glyph_bounding_box.y_max as f32 + y) * scale,
                ];
                if !do_convex_polygons_overlap(&aabb_to_convex_polygon(&aabb), clipping_area) {
                    continue;
                }
            }
        } else {
            continue;
        }
        let scalator = ppga2d::Scalar { g0: scale };
        let motor = translate2d([x * scale, y * scale]);
        let mut paths = paths_of_glyph(face, glyph_id);
        for path in &mut paths {
            path.transform(&scalator, &motor);
        }
        result.append(&mut paths);
    }
    result
}

/// Calculates the bounding box in which [paths_of_text] fits, including preemptive spacing.
///
/// Texts like "hello" and "bye" use the same vertical space even though the "y" in "bye" extends further downward.
/// This way they are easier to align in the same row.
pub fn half_extent_of_text(face: &ttf_parser::Face, layout: &Layout, text: &str) -> SafeFloat<f32, 2> {
    let scale = 0.5 * layout.size.unwrap() / face.height() as f32;
    let major_size = calculate_kerning!(face, layout, text, |_glyph_position, _glyph_id| {}) * scale;
    let minor_size = face.height() as f32 * scale;
    match layout.orientation {
        Orientation::RightToLeft | Orientation::LeftToRight => [major_size, minor_size].into(),
        Orientation::TopToBottom | Orientation::BottomToTop => [minor_size, major_size].into(),
    }
}

/// Calculates, from a geometric position, the placement of the cursor as character index in the text.
pub fn index_of_char_at(face: &ttf_parser::Face, layout: &Layout, text: &str, cursor: ppga2d::Point) -> usize {
    let (_major_offset, _minor_offset, glyph_positions) = calculate_aligned_positions!(face, layout, text);
    let scale = face.height() as f32 / (layout.size.unwrap() * cursor.g0[0]);
    let cursor = [cursor.g0[1] * scale, cursor.g0[2] * scale];
    match glyph_positions.binary_search_by(|([x, _y], glyph_id)| {
        (x + face.glyph_hor_advance(*glyph_id).unwrap_or(0) as f32 * 0.5)
            .partial_cmp(&cursor[0])
            .unwrap()
    }) {
        Ok(index) => index,
        Err(index) => index,
    }
}

/// Calculates the byte offsets of chars in an UTF8 string
pub fn byte_offset_of_char_index(string: &str, char_index: usize) -> usize {
    string.char_indices().nth(char_index).map(|(index, _char)| index).unwrap_or(string.len())
}
