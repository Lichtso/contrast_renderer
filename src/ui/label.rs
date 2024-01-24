use crate::{
    hash_set, match_option,
    path::Path,
    text::{byte_offset_of_char_index, half_extent_of_text, index_of_char_at, paths_of_text, Layout},
    ui::{
        message::{self, rendering_default_behavior, Messenger},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, TextInteraction,
    },
    utils::translate2d,
};

fn text_selection(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger {
        Messenger::PrepareRendering(message) => {
            println!("text_label PrepareRendering");
            let (_prepare_rendering, mut update_rendering) = context.prepare_rendering_helper(message);
            if let Some(rendering) = &mut update_rendering.rendering {
                let half_extent = context.get_half_extent();
                let text_selection_color = match_option!(context.derive_attribute("text_selection_color"), Value::Float4).unwrap();
                rendering
                    .colored_paths
                    .push((text_selection_color, vec![Path::from_rect([0.0, 0.0], half_extent.unwrap())]));
            }
            vec![Messenger::UpdateRendering(update_rendering)]
        }
        Messenger::Render(message) => rendering_default_behavior(message),
        Messenger::ConfigurationRequest(_message) => {
            println!("text_label ConfigurationRequest");
            context.set_attribute("is_rendering_dirty", Value::Boolean(true));
            vec![Messenger::ConfigurationResponse(message::ConfigurationResponse {
                half_extent: context.get_half_extent(),
            })]
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
    match messenger {
        Messenger::PrepareRendering(message) => {
            println!("text_label PrepareRendering");
            let (prepare_rendering, mut update_rendering) = context.prepare_rendering_helper(message);
            if let Some(rendering) = &mut update_rendering.rendering {
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
            }
            vec![
                Messenger::PrepareRendering(prepare_rendering),
                Messenger::UpdateRendering(update_rendering),
            ]
        }
        Messenger::Render(message) => rendering_default_behavior(message),
        Messenger::ConfigurationRequest(_message) => {
            println!("text_label ConfigurationRequest");
            let text_font = match_option!(context.derive_attribute("font_face"), Value::TextFont).unwrap();
            let layout = layout!(context);
            let text_content = match_option!(context.get_attribute("text_content"), Value::TextString).unwrap_or_else(String::default);
            let cursor_a = match_option!(context.get_attribute("cursor_a"), Value::Natural1).unwrap_or(0);
            let cursor_b = match_option!(context.get_attribute("cursor_b"), Value::Natural1).unwrap_or(0);
            let range = cursor_a.min(cursor_b)..cursor_a.max(cursor_b);
            let half_extent = half_extent_of_text(text_font.face(), &layout, &text_content);
            let text_interaction = match_option!(context.get_attribute("text_interaction"), Value::TextInteraction).unwrap_or(TextInteraction::None);
            let mut result = vec![Messenger::ConfigurationResponse(message::ConfigurationResponse { half_extent })];
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
        Messenger::Pointer(message) => {
            println!("text_label Pointer");
            let text_interaction = match_option!(context.get_attribute("text_interaction"), Value::TextInteraction).unwrap_or(TextInteraction::None);
            if text_interaction == TextInteraction::None || !message.bubbling_up {
                return vec![Messenger::Pointer(message.clone())];
            }
            let index = index_of_char_at(
                match_option!(context.derive_attribute("font_face"), Value::TextFont).unwrap().face(),
                &layout!(context),
                &match_option!(context.get_attribute("text_content"), Value::TextString).unwrap_or_else(String::default),
                message.relative_position,
            );
            match message.changed_button {
                0 => {
                    if context.does_observe(&NodeOrObservableIdentifier::InputDevice(message.device_id)) {
                        context.set_attribute("cursor_b", Value::Natural1(index));
                        vec![Messenger::Reconfigure(message::Reconfigure {})]
                    } else {
                        Vec::new()
                    }
                }
                2 => {
                    if message.pressed_buttons.contains(&2) {
                        context.set_attribute("cursor_a", Value::Natural1(index));
                        context.set_attribute("cursor_b", Value::Natural1(index));
                        vec![
                            Messenger::Observe(message::Observe {
                                observes: hash_set! { NodeOrObservableIdentifier::InputDevice(message.device_id) },
                            }),
                            Messenger::Reconfigure(message::Reconfigure {}),
                        ]
                    } else {
                        context.set_attribute("cursor_b", Value::Natural1(index));
                        vec![
                            Messenger::Observe(message::Observe { observes: hash_set! {} }),
                            Messenger::Reconfigure(message::Reconfigure {}),
                        ]
                    }
                }
                _ => Vec::new(),
            }
        }
        Messenger::Key(message) => {
            println!("text_label Key");
            let text_interaction = match_option!(context.get_attribute("text_interaction"), Value::TextInteraction).unwrap_or(TextInteraction::None);
            if text_interaction == TextInteraction::None
                || message.changed_keycode.is_none()
                || !message.pressed_keycodes.contains(&message.changed_keycode.unwrap())
            {
                return Vec::new();
            }
            let mut text_content = match_option!(context.get_attribute("text_content"), Value::TextString).unwrap_or_else(String::default);
            let mut cursor_a = match_option!(context.get_attribute("cursor_a"), Value::Natural1).unwrap_or(0);
            let mut cursor_b = match_option!(context.get_attribute("cursor_b"), Value::Natural1).unwrap_or(0);
            let range = cursor_a.min(cursor_b)..cursor_a.max(cursor_b);
            let keycode = message.changed_keycode.unwrap();
            match keycode {
                '←' | '→' | '↑' | '↓' => {
                    if message.pressed_keycodes.contains(&'⇧') {
                        cursor_a = match keycode {
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
                        cursor_a = match keycode {
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
                    vec![Messenger::Reconfigure(message::Reconfigure {})]
                }
                '⌥' | '⎈' | '⇧' | '⌘' => Vec::new(),
                '⏎' => {
                    if text_interaction != TextInteraction::Editing {
                        return Vec::new();
                    }
                    vec![Messenger::InputValueChanged(message::InputValueChanged {
                        child_id: NodeOrObservableIdentifier::Named("uninitialized"),
                        new_value: Value::TextString(text_content),
                    })]
                }
                _ => {
                    if text_interaction != TextInteraction::Editing {
                        return Vec::new();
                    }
                    if keycode == '⌫' {
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
                        let keycode = match keycode {
                            '␣' => ' ',
                            '⇥' => '\t',
                            '⏎' => '\n',
                            _ => keycode,
                        };
                        if range.start == range.end {
                            cursor_a = range.start + 1;
                            cursor_b = cursor_a;
                            text_content.insert(byte_offset_of_char_index(&text_content, range.start), keycode);
                        } else if range.start < range.end {
                            cursor_a = range.start + 1;
                            cursor_b = cursor_a;
                            text_content.replace_range(
                                byte_offset_of_char_index(&text_content, range.start)..byte_offset_of_char_index(&text_content, range.end),
                                &keycode.to_string(),
                            );
                        } else {
                            return Vec::new();
                        }
                    }
                    context.set_attribute("text_content", Value::TextString(text_content));
                    context.set_attribute("cursor_a", Value::Natural1(cursor_a));
                    context.set_attribute("cursor_b", Value::Natural1(cursor_b));
                    vec![Messenger::Reconfigure(message::Reconfigure {})]
                }
            }
        }
        _ => Vec::new(),
    }
}
