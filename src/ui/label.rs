//! Text label
use crate::{
    hash_map, match_option,
    path::Path,
    text::{byte_offset_of_char_index, half_extent_of_text, index_of_char_at, paths_of_text, Layout},
    ui::{
        message::{self, input_focus_parent_or_child, Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Rendering, TextInteraction,
    },
    utils::translate2d,
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
        _ => Vec::new(),
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
            if !context.was_attribute_touched(&["text_content", "cursor_a", "cursor_b", "text_interaction"]) {
                return Vec::new();
            }
            let text_font = match_option!(context.derive_attribute("font_face"), Value::TextFont).unwrap();
            let layout = layout!(context);
            let text_content = match_option!(context.get_attribute("text_content"), Value::TextString).unwrap_or_else(String::default);
            let cursor_a = match_option!(context.get_attribute("cursor_a"), Value::Natural1).unwrap_or(0);
            let cursor_b = match_option!(context.get_attribute("cursor_b"), Value::Natural1).unwrap_or(0);
            let range = cursor_a.min(cursor_b)..cursor_a.max(cursor_b);
            let half_extent = half_extent_of_text(text_font.face(), &layout, &text_content);
            let text_interaction = match_option!(context.get_attribute("text_interaction"), Value::TextInteraction).unwrap_or(TextInteraction::None);
            context.set_half_extent(half_extent);
            let selection_start_position =
                half_extent_of_text(text_font.face(), &layout, &text_content.chars().take(range.start).collect::<String>()).unwrap()[0] * 2.0
                    - half_extent.unwrap()[0];
            let (selection_translation, selection_half_width) = if cursor_a == cursor_b {
                (selection_start_position, layout.size.unwrap() * 0.03)
            } else {
                let selection_end_position =
                    half_extent_of_text(text_font.face(), &layout, &text_content.chars().take(range.end).collect::<String>()).unwrap()[0] * 2.0
                        - half_extent.unwrap()[0];
                (
                    (selection_end_position + selection_start_position) * 0.5,
                    (selection_end_position - selection_start_position) * 0.5,
                )
            };
            context.configure_child(
                NodeOrObservableIdentifier::Named("selection"),
                if text_interaction == TextInteraction::Editing || (text_interaction == TextInteraction::Selection && cursor_a != cursor_b) {
                    Some(|node: &mut Node| {
                        node.set_messenger_handler(text_selection);
                        node.set_attribute("motor", Value::Float4(translate2d([selection_translation, 0.0]).into()));
                        node.set_attribute("half_extent", Value::Float2([selection_half_width, layout.size.unwrap() * 0.4].into()));
                    })
                } else {
                    None
                },
            );
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            Vec::new()
        }
        "PointerInput" => {
            let text_interaction = match_option!(context.get_attribute("text_interaction"), Value::TextInteraction).unwrap_or(TextInteraction::None);
            if text_interaction == TextInteraction::None || messenger.propagation_direction != PropagationDirection::Parent(-1) {
                return vec![messenger.clone()];
            }
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            let index = index_of_char_at(
                match_option!(context.derive_attribute("font_face"), Value::TextFont).unwrap().face(),
                &layout!(context),
                &match_option!(context.get_attribute("text_content"), Value::TextString).unwrap_or_else(String::default),
                (*input_state.relative_positions.get(&0).unwrap()).into(),
            );
            if messenger.get_attribute("changed_pointer") == &Value::InputChannel(0) {
                if let Value::Boolean(pressed) = messenger.get_attribute("pressed_or_released") {
                    if *pressed {
                        context.set_attribute("cursor_a", Value::Natural1(index));
                        context.set_attribute("cursor_b", Value::Natural1(index));
                    }
                    context.pointer_and_button_input_focus(messenger);
                } else if context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap()) {
                    context.set_attribute("cursor_b", Value::Natural1(index));
                }
            }
            Vec::new()
        }
        "ButtonInput" => {
            let text_interaction = match_option!(context.get_attribute("text_interaction"), Value::TextInteraction).unwrap_or(TextInteraction::None);
            if text_interaction == TextInteraction::None || messenger.get_attribute("changed_keycode") == &Value::Void {
                return Vec::new();
            }
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            let changed_keycode = *match_option!(messenger.get_attribute("changed_keycode"), Value::Character).unwrap();
            if !input_state.pressed_keycodes.contains(&changed_keycode) {
                return Vec::new();
            }
            let mut text_content = match_option!(context.get_attribute("text_content"), Value::TextString).unwrap_or_else(String::default);
            let mut cursor_a = match_option!(context.get_attribute("cursor_a"), Value::Natural1).unwrap_or(0);
            let mut cursor_b = match_option!(context.get_attribute("cursor_b"), Value::Natural1).unwrap_or(0);
            let range = cursor_a.min(cursor_b)..cursor_a.max(cursor_b);
            match changed_keycode {
                '⇥' => {
                    if messenger.get_attribute("origin") != &Value::Void {
                        context.pointer_and_button_input_focus(messenger);
                    } else if input_state.pressed_keycodes.contains(&'⇧') {
                        return vec![input_focus_parent_or_child(messenger, None)];
                    }
                    Vec::new()
                }
                '←' | '→' | '↑' | '↓' => {
                    if input_state.pressed_keycodes.contains(&'⇧') {
                        cursor_a = match changed_keycode {
                            '←' if cursor_a > 0 => cursor_a - 1,
                            '→' if cursor_a < text_content.chars().count() => cursor_a + 1,
                            '↑' => 0,
                            '↓' => text_content.chars().count(),
                            _ => {
                                return Vec::new();
                            }
                        };
                        context.set_attribute("cursor_a", Value::Natural1(cursor_a));
                    } else {
                        cursor_a = match changed_keycode {
                            '←' if range.start > 0 && range.start == range.end => range.start - 1,
                            '←' if range.end > range.start && range.start != range.end => range.start,
                            '→' if range.end < text_content.chars().count() && range.start == range.end => range.end + 1,
                            '→' if range.start < range.end && range.start != range.end => range.end,
                            '↑' => 0,
                            '↓' => text_content.chars().count(),
                            _ => {
                                return Vec::new();
                            }
                        };
                        cursor_b = cursor_a;
                        context.set_attribute("cursor_a", Value::Natural1(cursor_a));
                        context.set_attribute("cursor_b", Value::Natural1(cursor_b));
                    }
                    Vec::new()
                }
                '⌥' | '⎈' | '⇧' | '⌘' => Vec::new(),
                '⏎' => {
                    if text_interaction != TextInteraction::Editing {
                        return Vec::new();
                    }
                    context.touch_attribute("text_content");
                    if let Value::NodeOrObservableIdentifier(input_field_id) = context.get_attribute("input_field_id") {
                        vec![Messenger::new(
                            &message::USER_INPUT,
                            hash_map! {
                                "input_field_id" => Value::NodeOrObservableIdentifier(input_field_id),
                                "value" => context.get_attribute("text_content"),
                            },
                        )]
                    } else {
                        Vec::new()
                    }
                }
                _ => {
                    if text_interaction != TextInteraction::Editing {
                        return Vec::new();
                    }
                    if changed_keycode == '⌫' {
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
                        let changed_keycode = match changed_keycode {
                            '␣' => ' ',
                            '⇥' => '\t',
                            '⏎' => '\n',
                            _ => changed_keycode,
                        };
                        match range.start {
                            _ if range.start == range.end => {
                                cursor_a = range.start + 1;
                                cursor_b = cursor_a;
                                text_content.insert(byte_offset_of_char_index(&text_content, range.start), changed_keycode);
                            }
                            _ if range.start < range.end => {
                                cursor_a = range.start + 1;
                                cursor_b = cursor_a;
                                text_content.replace_range(
                                    byte_offset_of_char_index(&text_content, range.start)..byte_offset_of_char_index(&text_content, range.end),
                                    &changed_keycode.to_string(),
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
            }
        }
        _ => Vec::new(),
    }
}
