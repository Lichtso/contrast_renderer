use crate::{
    hash_map, hash_set, match_option,
    path::Path,
    text::{byte_offset_of_char_index, half_extent_of_text, index_of_char_at, paths_of_text, Layout},
    ui::{
        message::{self, rendering_default_behavior, Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Rendering, TextInteraction,
    },
    utils::translate2d,
};

fn text_selection(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.behavior.label {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent();
                let text_selection_color = match_option!(context.derive_attribute("text_selection_color"), Value::Float4).unwrap();
                rendering
                    .colored_paths
                    .push((text_selection_color, vec![Path::from_rect([0.0, 0.0], half_extent.unwrap())]));
                update_rendering.set_attribute("rendering", Value::Rendering(rendering));
            }
            vec![update_rendering]
        }
        "Render" => rendering_default_behavior(messenger),
        "ConfigurationRequest" => {
            context.set_attribute("is_rendering_dirty", Value::Boolean(true));
            vec![Messenger::new(
                &message::CONFIGURATION_RESPONSE,
                hash_map! {
                    "half_extent" => Value::Float2(context.get_half_extent()),
                },
            )]
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

pub fn text_label(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.behavior.label {
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
                update_rendering.set_attribute("rendering", Value::Rendering(rendering));
            }
            vec![messenger.clone(), update_rendering]
        }
        "Render" => rendering_default_behavior(messenger),
        "ConfigurationRequest" => {
            let text_font = match_option!(context.derive_attribute("font_face"), Value::TextFont).unwrap();
            let layout = layout!(context);
            let text_content = match_option!(context.get_attribute("text_content"), Value::TextString).unwrap_or_else(String::default);
            let cursor_a = match_option!(context.get_attribute("cursor_a"), Value::Natural1).unwrap_or(0);
            let cursor_b = match_option!(context.get_attribute("cursor_b"), Value::Natural1).unwrap_or(0);
            let range = cursor_a.min(cursor_b)..cursor_a.max(cursor_b);
            let half_extent = half_extent_of_text(text_font.face(), &layout, &text_content);
            let text_interaction = match_option!(context.get_attribute("text_interaction"), Value::TextInteraction).unwrap_or(TextInteraction::None);
            let mut result = vec![Messenger::new(
                &message::CONFIGURATION_RESPONSE,
                hash_map! {
                    "half_extent" => Value::Float2(half_extent),
                },
            )];
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
                &mut result,
                NodeOrObservableIdentifier::Named("selection"),
                if text_interaction == TextInteraction::Editing || (text_interaction == TextInteraction::Selection && cursor_a != cursor_b) {
                    Some(|node: &mut Node| {
                        node.set_messenger_handler(text_selection);
                        node.set_motor(translate2d([selection_translation, 0.0]));
                        node.set_half_extent([selection_half_width, layout.size.unwrap() * 0.4].into());
                    })
                } else {
                    None
                },
            );
            context.set_attribute("is_rendering_dirty", Value::Boolean(true));
            result
        }
        "Pointer" => {
            let text_interaction = match_option!(context.get_attribute("text_interaction"), Value::TextInteraction).unwrap_or(TextInteraction::None);
            if text_interaction == TextInteraction::None || messenger.propagation_direction != PropagationDirection::Parent {
                return vec![messenger.clone()];
            }
            let index = index_of_char_at(
                match_option!(context.derive_attribute("font_face"), Value::TextFont).unwrap().face(),
                &layout!(context),
                &match_option!(context.get_attribute("text_content"), Value::TextString).unwrap_or_else(String::default),
                (*match_option!(messenger.get_attribute("relative_position"), Value::Float3).unwrap()).into(),
            );
            match match_option!(messenger.get_attribute("changed_button"), Value::ButtonOrKey).unwrap() {
                0 => {
                    if context.does_observe(match_option!(messenger.get_attribute("device"), Value::NodeOrObservableIdentifier).unwrap()) {
                        context.set_attribute("cursor_b", Value::Natural1(index));
                        vec![Messenger::new(&message::RECONFIGURE, hash_map! {})]
                    } else {
                        Vec::new()
                    }
                }
                2 => {
                    let pressed_buttons = match_option!(messenger.get_attribute("pressed_buttons"), Value::ButtonsOrKeys).unwrap();
                    if pressed_buttons.contains(&2) {
                        context.set_attribute("cursor_a", Value::Natural1(index));
                        context.set_attribute("cursor_b", Value::Natural1(index));
                        vec![
                            Messenger::new(
                                &message::OBSERVE,
                                hash_map! {
                                    "observes" => Value::NodeOrObservableIdentifiers(hash_set!{
                                        *match_option!(messenger.get_attribute("device"), Value::NodeOrObservableIdentifier).unwrap()
                                    }),
                                },
                            ),
                            Messenger::new(&message::RECONFIGURE, hash_map! {}),
                        ]
                    } else {
                        context.set_attribute("cursor_b", Value::Natural1(index));
                        vec![
                            Messenger::new(
                                &message::OBSERVE,
                                hash_map! {
                                    "observes" => Value::NodeOrObservableIdentifiers(hash_set!{}),
                                },
                            ),
                            Messenger::new(&message::RECONFIGURE, hash_map! {}),
                        ]
                    }
                }
                _ => Vec::new(),
            }
        }
        "Key" => {
            let text_interaction = match_option!(context.get_attribute("text_interaction"), Value::TextInteraction).unwrap_or(TextInteraction::None);
            if text_interaction == TextInteraction::None || messenger.get_attribute("changed_keycode") == &Value::Void {
                return Vec::new();
            }
            let pressed_keycodes = match_option!(messenger.get_attribute("pressed_keycodes"), Value::Characters).unwrap();
            let changed_keycode = *match_option!(messenger.get_attribute("changed_keycode"), Value::Character).unwrap();
            if !pressed_keycodes.contains(&changed_keycode) {
                return Vec::new();
            }
            let mut text_content = match_option!(context.get_attribute("text_content"), Value::TextString).unwrap_or_else(String::default);
            let mut cursor_a = match_option!(context.get_attribute("cursor_a"), Value::Natural1).unwrap_or(0);
            let mut cursor_b = match_option!(context.get_attribute("cursor_b"), Value::Natural1).unwrap_or(0);
            let range = cursor_a.min(cursor_b)..cursor_a.max(cursor_b);
            match changed_keycode {
                '←' | '→' | '↑' | '↓' => {
                    if pressed_keycodes.contains(&'⇧') {
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
                    vec![Messenger::new(&message::RECONFIGURE, hash_map! {})]
                }
                '⌥' | '⎈' | '⇧' | '⌘' => Vec::new(),
                '⏎' => {
                    if text_interaction != TextInteraction::Editing {
                        return Vec::new();
                    }
                    vec![Messenger::new(
                        &message::INPUT_VALUE_CHANGED,
                        hash_map! {
                            "child_id" => Value::Void,
                            "new_value" => Value::TextString(text_content),
                        },
                    )]
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
                        if range.start == range.end {
                            cursor_a = range.start + 1;
                            cursor_b = cursor_a;
                            text_content.insert(byte_offset_of_char_index(&text_content, range.start), changed_keycode);
                        } else if range.start < range.end {
                            cursor_a = range.start + 1;
                            cursor_b = cursor_a;
                            text_content.replace_range(
                                byte_offset_of_char_index(&text_content, range.start)..byte_offset_of_char_index(&text_content, range.end),
                                &changed_keycode.to_string(),
                            );
                        } else {
                            return Vec::new();
                        }
                    }
                    context.set_attribute("text_content", Value::TextString(text_content));
                    context.set_attribute("cursor_a", Value::Natural1(cursor_a));
                    context.set_attribute("cursor_b", Value::Natural1(cursor_b));
                    vec![Messenger::new(&message::RECONFIGURE, hash_map! {})]
                }
            }
        }
        _ => Vec::new(),
    }
}
