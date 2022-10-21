//! Range slider
use crate::{
    hash_map, match_option,
    path::Path,
    ui::{
        label::text_label,
        message::{self, Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Orientation, Rendering, TextInteraction,
    },
    utils::translate2d,
};
use geometric_algebra::ppga2d;

fn range_bar(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent(false);
                let bar_color_attribute = if match_option!(context.get_attribute("is_filled"), Value::Boolean).unwrap() {
                    "range_filled_color"
                } else {
                    "range_empty_color"
                };
                let bar_color = match_option!(context.derive_attribute(bar_color_attribute), Value::Float4).unwrap();
                rendering
                    .colored_paths
                    .push((bar_color, vec![Path::from_rect([0.0, 0.0], half_extent.unwrap())]));
                update_rendering.set_attribute("rendering", Value::Rendering(Box::new(rendering)));
            }
            vec![update_rendering]
        }
        "Reconfigure" => {
            if !context.was_attribute_touched(&["half_extent", "is_filled"]) {
                return Vec::new();
            }
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            Vec::new()
        }
        _ => vec![messenger.clone()],
    }
}

/// Range slider
pub fn range(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent(false).unwrap();
                let corner_radius = match_option!(context.derive_attribute("range_corner_radius"), Value::Float1)
                    .unwrap()
                    .unwrap()
                    .min(half_extent[0].min(half_extent[1]));
                rendering.clip_paths = vec![Path::from_rounded_rect([0.0, 0.0], half_extent, corner_radius)];
                update_rendering.set_attribute("rendering", Value::Rendering(Box::new(rendering)));
            }
            vec![update_rendering]
        }
        "Reconfigure" => {
            let mut unaffected = !context.was_attribute_touched(&[
                "half_extent",
                "orientation",
                "numeric_value",
                "numeric_value_range",
                "snap_clamp_function",
                "textual_projection",
            ]);
            let mut messengers = Vec::new();
            if let Value::TextualProjection(textual_projection) = context.get_attribute("textual_projection") {
                let text_content = context.inspect_child(&NodeOrObservableIdentifier::Named("textual"), |node: &Node| {
                    if context.was_attribute_of_child_touched(node, &["text_content"]) {
                        Some(match_option!(node.get_attribute("text_content"), Value::TextString).unwrap())
                    } else {
                        None
                    }
                });
                if let Some(Some(text_content)) = text_content {
                    if let Some(mut new_numeric_value) = (textual_projection.backward)(text_content) {
                        let numeric_value = match_option!(context.get_attribute("numeric_value"), Value::Float1)
                            .map(|value| value.unwrap())
                            .unwrap_or(0.0);
                        let numeric_value_range = match_option!(context.get_attribute("numeric_value_range"), Value::Float2)
                            .map(|value| value.unwrap())
                            .unwrap_or([0.0, 1.0]);
                        if let Value::SnapClampFunction(snap_clamp_function) = context.get_attribute("snap_clamp_function") {
                            new_numeric_value = (snap_clamp_function.handler)(new_numeric_value, &numeric_value_range);
                        }
                        if new_numeric_value != numeric_value {
                            unaffected = false;
                            context.set_attribute("numeric_value", Value::Float1(new_numeric_value.into()));
                            if let Value::NodeOrObservableIdentifier(input_field_id) = context.get_attribute("input_field_id") {
                                messengers.push(Messenger::new(
                                    &message::USER_INPUT,
                                    hash_map! {
                                        "input_state" => messenger.get_attribute("input_state").clone(),
                                        "input_source" => messenger.get_attribute("input_source").clone(),
                                        "input_field_id" => Value::NodeOrObservableIdentifier(input_field_id),
                                        "value" => context.get_attribute("numeric_value"),
                                    },
                                ));
                            }
                        }
                    }
                }
            }
            if unaffected {
                return messengers;
            }
            let half_extent = context.get_half_extent(false).unwrap();
            let axis = match_option!(context.get_attribute("orientation"), Value::Orientation).unwrap_or(Orientation::Horizontal) as usize;
            let numeric_value = match_option!(context.get_attribute("numeric_value"), Value::Float1)
                .map(|value| value.unwrap())
                .unwrap_or(0.0);
            let numeric_value_range = match_option!(context.get_attribute("numeric_value_range"), Value::Float2)
                .map(|value| value.unwrap())
                .unwrap_or([0.0, 1.0]);
            let mut empty_half_extent = half_extent;
            let mut empty_translation = [0.0, 0.0];
            let mut filled_half_extent = empty_half_extent;
            let mut filled_translation = empty_translation;
            let normalized_value = (numeric_value - numeric_value_range[0]) / (numeric_value_range[1] - numeric_value_range[0]);
            let translation = ((normalized_value.abs() + 1.0) % 2.0 - 1.0) * normalized_value.signum();
            filled_half_extent[axis] = translation.abs() * half_extent[axis];
            empty_half_extent[axis] = half_extent[axis] - filled_half_extent[axis];
            empty_translation[axis] = (half_extent[axis] - empty_half_extent[axis]) * translation.signum();
            filled_translation[axis] = (filled_half_extent[axis] - half_extent[axis]) * translation.signum();
            context.configure_child(
                NodeOrObservableIdentifier::Named("empty"),
                Some(&mut |node: &mut Node| {
                    node.set_messenger_handler(range_bar);
                    node.set_attribute("is_filled", Value::Boolean(false));
                    node.set_attribute("motor", Value::Motor(translate2d(empty_translation).into()));
                    node.set_attribute("half_extent", Value::Float2(empty_half_extent.into()));
                }),
            );
            context.configure_child(
                NodeOrObservableIdentifier::Named("filled"),
                Some(&mut |node: &mut Node| {
                    node.set_messenger_handler(range_bar);
                    node.set_attribute("is_filled", Value::Boolean(true));
                    node.set_attribute("motor", Value::Motor(translate2d(filled_translation).into()));
                    node.set_attribute("half_extent", Value::Float2(filled_half_extent.into()));
                }),
            );
            let text_content = if let Value::TextualProjection(textual_projection) = context.get_attribute("textual_projection") {
                Some((textual_projection.forward)(numeric_value))
            } else {
                None
            };
            let text_interaction = if match_option!(context.get_attribute("enable_interaction"), Value::Boolean).unwrap_or(false) {
                TextInteraction::Editing
            } else {
                TextInteraction::None
            };
            context.configure_child(
                NodeOrObservableIdentifier::Named("textual"),
                text_content.map(|text_content| {
                    |node: &mut Node| {
                        node.set_messenger_handler(text_label);
                        node.set_attribute("text_content", Value::TextString(text_content));
                        node.set_attribute("text_interaction", Value::TextInteraction(text_interaction));
                        node.set_attribute_privately("layer_index", Value::Natural1(1));
                    }
                }),
            );
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            messengers
        }
        "PointerInput" => {
            if !match_option!(context.get_attribute("enable_interaction"), Value::Boolean).unwrap_or(false)
                || messenger.propagation_direction != PropagationDirection::Parent(-1)
            {
                return vec![messenger.clone()];
            }
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            if messenger.get_attribute("changed_pointer") == &Value::InputChannel(0) {
                if let Value::Boolean(pressed) = messenger.get_attribute("pressed_or_released") {
                    if *pressed {
                        let numeric_value = context.get_attribute("numeric_value");
                        context.set_attribute("previous_numeric_value", numeric_value);
                        context.set_attribute("pointer_start", Value::Float3(*input_state.absolute_positions.get(&0).unwrap()));
                    } else {
                        context.set_attribute("pointer_start", Value::Void);
                    }
                    return context.input_focus_self(messenger);
                } else if context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap()) {
                    let absolute_position: ppga2d::Point = (*input_state.absolute_positions.get(&0).unwrap()).into();
                    let pointer_start: ppga2d::Point = match_option!(context.get_attribute("pointer_start"), Value::Float3).unwrap().into();
                    let half_extent = context.get_half_extent(false).unwrap();
                    let axis = match_option!(context.get_attribute("orientation"), Value::Orientation).unwrap_or(Orientation::Horizontal) as usize;
                    let numeric_value = match_option!(context.get_attribute("numeric_value"), Value::Float1)
                        .map(|value| value.unwrap())
                        .unwrap_or(0.0);
                    let numeric_value_range = match_option!(context.get_attribute("numeric_value_range"), Value::Float2)
                        .map(|value| value.unwrap())
                        .unwrap_or([0.0, 1.0]);
                    let mut new_numeric_value =
                        (absolute_position - pointer_start)[1 + axis] / half_extent[axis] * 0.5 * (numeric_value_range[1] - numeric_value_range[0])
                            + match_option!(context.get_attribute("previous_numeric_value"), Value::Float1)
                                .unwrap()
                                .unwrap();
                    if let Value::SnapClampFunction(snap_clamp_function) = context.get_attribute("snap_clamp_function") {
                        new_numeric_value = (snap_clamp_function.handler)(new_numeric_value, &numeric_value_range);
                    }
                    if numeric_value != new_numeric_value {
                        context.set_attribute("numeric_value", Value::Float1(new_numeric_value.into()));
                        if let Value::NodeOrObservableIdentifier(input_field_id) = context.get_attribute("input_field_id") {
                            return vec![Messenger::new(
                                &message::USER_INPUT,
                                hash_map! {
                                    "input_state" => messenger.get_attribute("input_state").clone(),
                                    "input_source" => messenger.get_attribute("input_source").clone(),
                                    "input_field_id" => Value::NodeOrObservableIdentifier(input_field_id),
                                    "value" => context.get_attribute("numeric_value"),
                                },
                            )];
                        }
                    }
                }
            }
            Vec::new()
        }
        "ButtonInput" => {
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            let changed_keycode = *match_option!(messenger.get_attribute("changed_keycode"), Value::Character).unwrap();
            if !input_state.pressed_keycodes.contains(&changed_keycode) {
                return Vec::new();
            }
            match changed_keycode {
                '⇥' => {
                    let focus_child_id = if messenger.get_attribute("origin") != &Value::Void {
                        return context.input_focus_self(messenger);
                    } else if input_state.pressed_keycodes.contains(&'⇧') {
                        None
                    } else {
                        Some(NodeOrObservableIdentifier::Named("textual"))
                    };
                    vec![context.input_focus_parent_or_child(messenger, focus_child_id)]
                }
                '←' | '→' | '↑' | '↓' => vec![context.redirect_input_focus_navigation_to_parent(messenger)],
                _ => Vec::new(),
            }
        }
        "Defocus" => context.input_defocus_self(messenger),
        _ => vec![messenger.clone()],
    }
}
