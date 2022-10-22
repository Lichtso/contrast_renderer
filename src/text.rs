//! (Optional) Working with [font faces](ttf_parser::Face), converting glyphs and text to [Path]s

use crate::{
    path::{IntegralCubicCurveSegment, IntegralQuadraticCurveSegment, LineSegment, Path},
    safe_float::SafeFloat,
    utils::{aabb_to_convex_polygon, do_convex_polygons_overlap, translate2d},
};
use geometric_algebra::ppga2d;

/// Heap allocated font with a closed lifetime.
pub struct Font {
    name: String,
    _backing_store: std::pin::Pin<Box<[u8]>>,
    parsed_face: ttf_parser::Face<'static>,
}

impl Font {
    /// Load a TTF font face.
    pub fn new(name: String, font_data: &[u8]) -> Self {
        let backing_store = std::pin::Pin::new(font_data.to_vec().into_boxed_slice());
        let backing_slice = unsafe { std::mem::transmute::<&[u8], &[u8]>(&backing_store) };
        Self {
            name,
            _backing_store: backing_store,
            parsed_face: ttf_parser::Face::from_slice(backing_slice, 0).unwrap(),
        }
    }

    /// Get the name of the font.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get the parsed font face.
    pub fn face(&self) -> &ttf_parser::Face {
        &self.parsed_face
    }
}

impl std::fmt::Debug for Font {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Font({:?})", self.name)
    }
}

impl PartialEq for Font {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Font {}

impl std::hash::Hash for Font {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
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
    /// Origin is to the bottom left of the text (when [Orientation::LeftToRight]).
    Begin,
    /// Origin is on the baseline of the text.
    Baseline,
    /// Origin is at the center of the text.
    Center,
    /// Origin is to the top right of the text (when [Orientation::LeftToRight]).
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

macro_rules! calculate_aligned_positions {
    ($face:expr, $layout:expr, $text:expr) => {{
        let replacement_glyph_id = $face.glyph_index('�');
        let kerning_table = $face.tables().kern.and_then(|table| table.subtables.into_iter().next());
        let (major_axis, sign_x, sign_y) = match $layout.orientation {
            Orientation::RightToLeft => (0, -1, -1),
            Orientation::LeftToRight => (0, 1, -1),
            Orientation::TopToBottom => (1, 1, -1),
            Orientation::BottomToTop => (1, 1, 1),
        };
        let (line_minor_extent, line_gap) = if major_axis == 0 {
            ($face.height() as i64, $face.line_gap() as i64)
        } else {
            ($face.vertical_height().unwrap_or(0) as i64, $face.vertical_line_gap().unwrap_or(0) as i64)
        };
        // let line_count = $text.chars().filter(|(index, char)| char == '\n').fold(1, |counter, _| counter + 1);
        let mut lines = Vec::new();
        let mut line_major_extent = 0;
        let mut extent = [0; 2];
        let mut glyph_positions = Vec::new();
        let mut prev_glyph_id = None;
        let mut index = 0;
        for char in $text.chars() {
            index += 1;
            let mut glyph_position = extent;
            glyph_position[major_axis] = line_major_extent;
            if char == '\n' {
                glyph_positions.push((glyph_position, ttf_parser::GlyphId(0)));
                let mut line_glyph_positions = Vec::new();
                std::mem::swap(&mut glyph_positions, &mut line_glyph_positions);
                lines.push((index, line_glyph_positions));
                extent[major_axis] = extent[major_axis].max(line_major_extent);
                extent[1 - major_axis] += line_minor_extent + line_gap;
                line_major_extent = 0;
                prev_glyph_id = None;
            } else {
                let glyph_id = $face.glyph_index(char).or(replacement_glyph_id).unwrap();
                if let (Some(kerning_table), Some(prev_glyph_id)) = (kerning_table, prev_glyph_id) {
                    if let Some(kerning) = kerning_table.glyphs_kerning(prev_glyph_id, glyph_id) {
                        line_major_extent += kerning as i64;
                    }
                }
                prev_glyph_id = Some(glyph_id);
                let advance = if major_axis == 0 {
                    $face.glyph_hor_advance(glyph_id)
                } else {
                    $face.glyph_ver_advance(glyph_id)
                };
                if let Some(advance) = advance {
                    line_major_extent += advance as i64;
                }
                glyph_positions.push((glyph_position, glyph_id));
            };
        }
        {
            let mut glyph_position = extent;
            glyph_position[major_axis] = line_major_extent;
            glyph_positions.push((glyph_position, ttf_parser::GlyphId(0)));
            lines.push((index + 1, glyph_positions));
            extent[major_axis] = extent[major_axis].max(line_major_extent);
            extent[1 - major_axis] += line_minor_extent;
        }
        let mut offset = [0, 0];
        offset[1 - major_axis] = match $layout.minor_alignment {
            Alignment::Begin => -$face.descender() as i64,
            Alignment::Baseline => 0,
            Alignment::Center => $face.x_height().unwrap() as i64 / 2,
            Alignment::End => -line_minor_extent,
        };
        for (_line_range_end, glyph_positions) in lines.iter_mut() {
            let line_major_extent = glyph_positions.last().unwrap().0[major_axis];
            let mut offset = offset;
            offset[major_axis] = match $layout.major_alignment {
                Alignment::Begin => -extent[major_axis] / 2,
                Alignment::Baseline | Alignment::Center => -line_major_extent / 2,
                Alignment::End => extent[major_axis] / 2 - line_major_extent,
            };
            offset[1 - major_axis] -= (extent[1 - major_axis] - line_minor_extent) / 2;
            for (position, _glyph_id) in glyph_positions.iter_mut() {
                position[0] = sign_x * (position[0] + offset[0]);
                position[1] = sign_y * (position[1] + offset[1]);
            }
        }
        (extent, [sign_x * offset[0], sign_y * offset[1]], lines)
    }};
}

/// Arranges a given string to a set of paths (of its glyphs), according to the given alignment and font face.
///
/// If a `clipping_area` convex polygon is given, then glyphs which are completely outside are discarded.
/// Other glyphs will stay at the same position.
pub fn paths_of_text(face: &ttf_parser::Face, layout: &Layout, text: &str, clipping_area: Option<&[ppga2d::Point]>) -> Vec<Path> {
    let (_extent, _offset, lines) = calculate_aligned_positions!(face, layout, text);
    let scale = layout.size.unwrap() / face.height() as f32;
    let mut result = Vec::new();
    for ([x, y], glyph_id) in lines
        .iter()
        .flat_map(|(_line_range_end, glyph_positions)| glyph_positions[0..glyph_positions.len() - 1].iter())
    {
        if let (Some(clipping_area), Some(glyph_bounding_box)) = (clipping_area, face.glyph_bounding_box(*glyph_id)) {
            let aabb = [
                (glyph_bounding_box.x_min as i64 + x) as f32 * scale,
                (glyph_bounding_box.y_min as i64 + y) as f32 * scale,
                (glyph_bounding_box.x_max as i64 + x) as f32 * scale,
                (glyph_bounding_box.y_max as i64 + y) as f32 * scale,
            ];
            if !do_convex_polygons_overlap(&aabb_to_convex_polygon(&aabb), clipping_area) {
                continue;
            }
        }
        let scalator = ppga2d::Scalar::new(scale);
        let motor = translate2d([*x as f32 * scale, *y as f32 * scale]);
        let mut paths = paths_of_glyph(face, *glyph_id);
        for path in &mut paths {
            path.transform(&scalator, &motor);
        }
        result.append(&mut paths);
    }
    result
}

/// Bounding box of the entire text and glyph positions of the characters separated into lines
pub struct TextGeometry {
    /// Primary flow direction of text
    ///
    /// 0 if horizontal, 1 if vertical.
    pub major_axis: usize,
    /// Half extent of the entire text
    pub half_extent: SafeFloat<f32, 2>,
    /// Line ending character index and glyph positions per line
    ///
    /// The glyph positions include the line break at the end, so there is one more entry than there are printable characters.
    pub lines: Vec<(usize, Vec<SafeFloat<f32, 2>>)>,
}

impl TextGeometry {
    /// Calculates the bounding box in which [paths_of_text] fits, including preemptive spacing.
    ///
    /// Texts like "hello" and "bye" use the same vertical space even though the "y" in "bye" extends further downward.
    /// This way they are easier to align in the same row.
    pub fn new(face: &ttf_parser::Face, layout: &Layout, text: &str) -> Self {
        let major_axis = match layout.orientation {
            Orientation::RightToLeft | Orientation::LeftToRight => 0,
            Orientation::TopToBottom | Orientation::BottomToTop => 1,
        };
        let scale = layout.size.unwrap() / face.height() as f32;
        let (extent, offset, lines) = calculate_aligned_positions!(face, layout, text);
        Self {
            major_axis,
            half_extent: [extent[0] as f32 * scale * 0.5, extent[1] as f32 * scale * 0.5].into(),
            lines: lines
                .iter()
                .map(|(line_range_end, glyph_positions)| {
                    (
                        *line_range_end,
                        glyph_positions
                            .iter()
                            .map(|(position, _glyph_id)| [(position[0] - offset[0]) as f32 * scale, (position[1] - offset[1]) as f32 * scale].into())
                            .collect(),
                    )
                })
                .collect(),
        }
    }

    /// Returns the line index of a given character index
    pub fn line_index_from_char_index(&self, char_index: usize) -> usize {
        self.lines
            .iter()
            .position(|(line_range_end, _glyph_positions)| *line_range_end > char_index)
            .unwrap()
    }

    /// Finds the character index of a given position
    pub fn char_index_from_position(&self, cursor: SafeFloat<f32, 2>) -> usize {
        let cursor = cursor.unwrap();
        let minor_half_extent = self.half_extent.unwrap()[1 - self.major_axis];
        let line_index = ((minor_half_extent - cursor[1 - self.major_axis]) * self.lines.len() as f32 / (minor_half_extent * 2.0))
            .max(0.0)
            .min((self.lines.len() - 1) as f32) as usize;
        let glyph_positions = &self.lines[line_index].1;
        glyph_positions
            .iter()
            .zip(glyph_positions.iter().skip(1))
            .position(|(prev, next)| (prev.unwrap()[self.major_axis] + next.unwrap()[self.major_axis]) * 0.5 > cursor[self.major_axis])
            .unwrap_or(glyph_positions.len() - 1)
            + if line_index == 0 { 0 } else { self.lines[line_index - 1].0 }
    }

    /// Used to jump into the previous or next line
    pub fn advance_char_index_by_line_index(&self, char_index: usize, relative_line_index: isize) -> usize {
        let line_index = self.line_index_from_char_index(char_index);
        if relative_line_index < 0 && line_index == 0 {
            return 0;
        } else if relative_line_index > 0 && line_index == self.lines.len() - 1 {
            return self.lines.last().unwrap().0 - 1;
        }
        let (line_range_end, glyph_positions) = &self.lines[line_index];
        let mut cursor = glyph_positions[char_index + glyph_positions.len() - *line_range_end].unwrap();
        let line_minor_extent = self.half_extent.unwrap()[1 - self.major_axis] * 2.0 / self.lines.len() as f32;
        cursor[1 - self.major_axis] -= line_minor_extent * relative_line_index as f32;
        self.char_index_from_position(cursor.into())
    }
}

/// Calculates the byte offsets of chars in an UTF8 string
pub fn byte_offset_of_char_index(string: &str, char_index: usize) -> usize {
    string.char_indices().nth(char_index).map(|(index, _char)| index).unwrap_or(string.len())
}
