//! Text label
use crate::{
    hash_map, match_option,
    path::Path,
    text::{byte_offset_of_char_index, paths_of_text, Layout, TextGeometry},
    ui::{
        message::{self, Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Rendering, TextInteraction,
    },
    utils::{point_to_vec, translate2d},
};

fn text_selection(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent(false);
                let text_selection_color = match_option!(context.derive_attribute("text_selection_color"), Value::Float4).unwrap();
                rendering
                    .colored_paths
                    .push((text_selection_color, vec![Path::from_rect([0.0, 0.0], half_extent.unwrap())]));
                update_rendering.set_attribute("rendering", Value::Rendering(Box::new(rendering)));
            }
            vec![update_rendering]
        }
        "Reconfigure" => {
            let unaffected = !context.was_attribute_touched(&["half_extent"]);
            if unaffected {
                return Vec::new();
            }
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            Vec::new()
        }
        _ => vec![messenger.clone()],
    }
}

macro_rules! layout {
    ($context:expr) => {
        Layout {
            size: match_option!($context.derive_attribute("font_size"), Value::Float1).unwrap(),
            orientation: match_option!($context.derive_attribute("text_orientation"), Value::TextOrientation).unwrap(),
            major_alignment: match_option!($context.derive_attribute("text_major_alignment"), Value::TextAlignment).unwrap(),
            minor_alignment: match_option!($context.derive_attribute("text_minor_alignment"), Value::TextAlignment).unwrap(),
        }
    };
}

/// Text label
pub fn text_label(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let text_color = match_option!(context.derive_attribute("text_color"), Value::Float4).unwrap();
                let paths = paths_of_text(
                    match_option!(context.derive_attribute("font_face"), Value::TextFont).unwrap().face(),
                    &layout!(context),
                    &match_option!(context.get_attribute("text_content"), Value::TextString).unwrap_or_else(String::default),
                    None, // TODO: Clipping rect
                );
                if !paths.is_empty() {
                    rendering.colored_paths.push((text_color, paths));
                }
                update_rendering.set_attribute("rendering", Value::Rendering(Box::new(rendering)));
            }
            vec![update_rendering]
        }
        "Reconfigure" => {
            if !context.was_attribute_touched(&["text_content", "cursor_a", "cursor_b", "observes", "input_source_entered"]) {
                return Vec::new();
            }
            let entered = if let Value::NodeOrObservableIdentifier(input_source) = context.get_attribute("input_source_entered") {
                context.does_observe(&input_source)
            } else {
                false
            };
            let text_font = match_option!(context.derive_attribute("font_face"), Value::TextFont).unwrap();
            let layout = layout!(context);
            let text_content = match_option!(context.get_attribute("text_content"), Value::TextString).unwrap_or_else(String::default);
            let cursor_a = match_option!(context.get_attribute("cursor_a"), Value::Natural1).unwrap_or(0);
            let cursor_b = match_option!(context.get_attribute("cursor_b"), Value::Natural1).unwrap_or(0);
            let range = cursor_a.min(cursor_b)..cursor_a.max(cursor_b);
            let text_geometry = TextGeometry::new(text_font.face(), &layout, &text_content);
            let half_extent = text_geometry.half_extent.unwrap();
            context.set_attribute("proposed_half_width", Value::Float1(half_extent[0].into()));
            context.set_attribute("proposed_half_height", Value::Float1(half_extent[1].into()));
            let mut surplus_lines = if entered {
                let mut selection_half_extent = [0.0, 0.0];
                selection_half_extent[1 - text_geometry.major_axis] = layout.size.unwrap() * 0.4;
                for (line_index, (line_range_end, glyph_positions)) in text_geometry.lines.iter().enumerate() {
                    let line_range_start = *line_range_end - glyph_positions.len();
                    context.configure_child(
                        NodeOrObservableIdentifier::NamedAndIndexed("selection", line_index),
                        if range.start < *line_range_end && line_range_start <= range.end {
                            Some(|node: &mut Node| {
                                let start_position = glyph_positions[range.start.saturating_sub(line_range_start)].unwrap();
                                let end_position =
                                    glyph_positions[glyph_positions.len() - 1 - (line_range_end - 1).saturating_sub(range.end)].unwrap();
                                let selection_translation =
                                    [(end_position[0] + start_position[0]) * 0.5, (end_position[1] + start_position[1]) * 0.5];
                                selection_half_extent[text_geometry.major_axis] =
                                    (end_position[text_geometry.major_axis] - start_position[text_geometry.major_axis]).abs() * 0.5;
                                if selection_half_extent[text_geometry.major_axis] == 0.0 {
                                    selection_half_extent[text_geometry.major_axis] = layout.size.unwrap() * 0.03;
                                }
                                node.set_messenger_handler(text_selection);
                                node.set_attribute("motor", Value::Motor(translate2d(selection_translation).into()));
                                node.set_attribute("half_extent", Value::Float2(selection_half_extent.into()));
                            })
                        } else {
                            None
                        },
                    );
                }
                text_geometry.lines.len()
            } else {
                0
            };
            while context
                .inspect_child(
                    &NodeOrObservableIdentifier::NamedAndIndexed("selection", surplus_lines),
                    |_node: &Node| (),
                )
                .is_some()
            {
                context.remove_child(NodeOrObservableIdentifier::NamedAndIndexed("selection", surplus_lines), false);
                surplus_lines += 1;
            }
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            Vec::new()
        }
        "PointerInput" => {
            let text_interaction = match_option!(context.get_attribute("text_interaction"), Value::TextInteraction).unwrap_or(TextInteraction::None);
            if text_interaction == TextInteraction::None || messenger.propagation_direction != PropagationDirection::Parent(-1) {
                return vec![messenger.clone()];
            }
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            let text_geometry = TextGeometry::new(
                match_option!(context.derive_attribute("font_face"), Value::TextFont).unwrap().face(),
                &layout!(context),
                &match_option!(context.get_attribute("text_content"), Value::TextString).unwrap_or_else(String::default),
            );
            let index = text_geometry.char_index_from_position(point_to_vec((*input_state.relative_positions.get(&0).unwrap()).into()).into());
            if messenger.get_attribute("changed_pointer") == &Value::InputChannel(0) {
                if let Value::Boolean(pressed) = messenger.get_attribute("pressed_or_released") {
                    if *pressed {
                        context.set_attribute("cursor_a", Value::Natural1(index));
                        context.set_attribute("cursor_b", Value::Natural1(index));
                        let input_source = match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap();
                        let input_source = match_option!(input_source, NodeOrObservableIdentifier::PointerInput).unwrap();
                        context.set_attribute(
                            "input_source_entered",
                            Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::ButtonInput(*input_source)),
                        );
                    }
                    return context.input_focus_self(messenger);
                } else if context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap()) {
                    context.set_attribute("cursor_b", Value::Natural1(index));
                }
            }
            Vec::new()
        }
        "ButtonInput" => {
            let text_interaction = match_option!(context.get_attribute("text_interaction"), Value::TextInteraction).unwrap_or(TextInteraction::None);
            if text_interaction == TextInteraction::None || messenger.get_attribute("changed_key") == &Value::Void {
                return Vec::new();
            }
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            let changed_key = *match_option!(messenger.get_attribute("changed_key"), Value::Character).unwrap();
            if !input_state.pressed_keys.contains(&changed_key) {
                return Vec::new();
            }
            let mut text_content = match_option!(context.get_attribute("text_content"), Value::TextString).unwrap_or_else(String::default);
            let mut cursor_a = match_option!(context.get_attribute("cursor_a"), Value::Natural1).unwrap_or(0);
            let mut cursor_b = match_option!(context.get_attribute("cursor_b"), Value::Natural1).unwrap_or(0);
            let range = cursor_a.min(cursor_b)..cursor_a.max(cursor_b);
            let text_geometry = TextGeometry::new(
                match_option!(context.derive_attribute("font_face"), Value::TextFont).unwrap().face(),
                &layout!(context),
                &text_content,
            );
            match changed_key {
                '⇥' => {
                    if messenger.get_attribute("origin") != &Value::Void {
                        return context.input_focus_self(messenger);
                    } else if input_state.pressed_keys.contains(&'⇧') {
                        if &context.get_attribute("input_source_entered") == messenger.get_attribute("input_source") {
                            context.set_attribute("input_source_entered", Value::Void);
                            if text_interaction != TextInteraction::Editing {
                                return Vec::new();
                            }
                            context.touch_attribute("text_content");
                            if let Value::NodeOrObservableIdentifier(input_field_id) = context.get_attribute("input_field_id") {
                                return vec![Messenger::new(
                                    &message::USER_INPUT,
                                    hash_map! {
                                        "input_state" => messenger.get_attribute("input_state").clone(),
                                        "input_source" => messenger.get_attribute("input_source").clone(),
                                        "input_field_id" => Value::NodeOrObservableIdentifier(input_field_id),
                                        "value" => context.get_attribute("text_content"),
                                    },
                                )];
                            }
                        } else {
                            return vec![context.input_focus_parent_or_child(messenger, None)];
                        }
                    } else {
                        context.set_attribute("input_source_entered", messenger.get_attribute("input_source").clone());
                    }
                    Vec::new()
                }
                '←' | '→' | '↑' | '↓' => {
                    if &context.get_attribute("input_source_entered") != messenger.get_attribute("input_source") {
                        vec![context.redirect_input_focus_navigation_to_parent(messenger)]
                    } else if input_state.pressed_keys.contains(&'⇧') {
                        cursor_a = match changed_key {
                            '←' if cursor_a > 0 => cursor_a - 1,
                            '→' if cursor_a < text_content.chars().count() => cursor_a + 1,
                            '↑' => text_geometry.advance_char_index_by_line_index(cursor_a, -1),
                            '↓' => text_geometry.advance_char_index_by_line_index(cursor_a, 1),
                            _ => {
                                return Vec::new();
                            }
                        };
                        context.set_attribute("cursor_a", Value::Natural1(cursor_a));
                        Vec::new()
                    } else {
                        cursor_a = match changed_key {
                            '←' if range.start > 0 && range.start == range.end => range.start - 1,
                            '←' if range.end > range.start && range.start != range.end => range.start,
                            '→' if range.end < text_content.chars().count() && range.start == range.end => range.end + 1,
                            '→' if range.start < range.end && range.start != range.end => range.end,
                            '↑' => text_geometry.advance_char_index_by_line_index(range.start, -1),
                            '↓' => text_geometry.advance_char_index_by_line_index(range.end, 1),
                            _ => {
                                return Vec::new();
                            }
                        };
                        cursor_b = cursor_a;
                        context.set_attribute("cursor_a", Value::Natural1(cursor_a));
                        context.set_attribute("cursor_b", Value::Natural1(cursor_b));
                        Vec::new()
                    }
                }
                '⌥' | '⎈' | '⇧' | '⌘' => Vec::new(),
                _ if text_interaction == TextInteraction::Editing
                    && &context.get_attribute("input_source_entered") == messenger.get_attribute("input_source") =>
                {
                    if changed_key == '⌫' {
                        if range.start == range.end && range.start > 0 {
                            cursor_a = range.start - 1;
                            cursor_b = cursor_a;
                            text_content.remove(byte_offset_of_char_index(&text_content, cursor_a));
                        } else if range.start < range.end {
                            cursor_a = range.start;
                            cursor_b = cursor_a;
                            text_content.replace_range(
                                byte_offset_of_char_index(&text_content, range.start)..byte_offset_of_char_index(&text_content, range.end),
                                "",
                            );
                        } else {
                            return Vec::new();
                        }
                    } else {
                        let changed_key = match changed_key {
                            '␣' => ' ',
                            '⇥' => '\t',
                            '⏎' => '\n',
                            _ => changed_key,
                        };
                        match range.start {
                            _ if range.start == range.end => {
                                cursor_a = range.start + 1;
                                cursor_b = cursor_a;
                                text_content.insert(byte_offset_of_char_index(&text_content, range.start), changed_key);
                            }
                            _ if range.start < range.end => {
                                cursor_a = range.start + 1;
                                cursor_b = cursor_a;
                                text_content.replace_range(
                                    byte_offset_of_char_index(&text_content, range.start)..byte_offset_of_char_index(&text_content, range.end),
                                    &changed_key.to_string(),
                                );
                            }
                            _ => {
                                return Vec::new();
                            }
                        }
                    }
                    context.set_attribute_privately("text_content", Value::TextString(text_content));
                    context.set_attribute("cursor_a", Value::Natural1(cursor_a));
                    context.set_attribute("cursor_b", Value::Natural1(cursor_b));
                    Vec::new()
                }
                _ => Vec::new(),
            }
        }
        "Defocus" => {
            if &context.get_attribute("input_source_entered") == messenger.get_attribute("input_source") {
                context.set_attribute("input_source_entered", Value::Void);
            }
            context.input_defocus_self(messenger)
        }
        _ => vec![messenger.clone()],
    }
}
