use crate::{
    hash_map, hash_set, match_option,
    path::Path,
    ui::{
        label::text_label,
        message::{self, rendering_default_behavior, Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Orientation, Rendering, TextInteraction,
    },
    utils::translate2d,
};
use geometric_algebra::ppga2d;

fn range_bar(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.behavior.label {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent();
                let bar_color_attribute = if match_option!(context.get_attribute("is_filled"), Value::Boolean).unwrap() {
                    "range_filled_color"
                } else {
                    "range_empty_color"
                };
                let bar_color = match_option!(context.derive_attribute(bar_color_attribute), Value::Float4).unwrap();
                rendering
                    .colored_paths
                    .push((bar_color, vec![Path::from_rect([0.0, 0.0], half_extent.unwrap())]));
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

pub fn range(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.behavior.label {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent();
                rendering.clip_paths = vec![Path::from_rounded_rect(
                    [0.0, 0.0],
                    half_extent.unwrap(),
                    match_option!(context.derive_attribute("range_corner_radius"), Value::Float1)
                        .unwrap()
                        .unwrap(),
                )];
                update_rendering.set_attribute("rendering", Value::Rendering(rendering));
            }
            vec![messenger.clone(), update_rendering]
        }
        "Render" => rendering_default_behavior(messenger),
        "ConfigurationRequest" => {
            let half_extent = context.get_half_extent().unwrap();
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
            let mut result = vec![Messenger::new(
                &message::CONFIGURATION_RESPONSE,
                hash_map! {
                    "half_extent" => Value::Float2(half_extent.into()),
                },
            )];
            context.configure_child(
                &mut result,
                NodeOrObservableIdentifier::Named("empty"),
                Some(&mut |node: &mut Node| {
                    node.set_attribute("is_filled", Value::Boolean(false));
                    node.set_messenger_handler(range_bar);
                    node.set_motor(translate2d(empty_translation));
                    node.set_half_extent(empty_half_extent.into());
                }),
            );
            context.configure_child(
                &mut result,
                NodeOrObservableIdentifier::Named("filled"),
                Some(&mut |node: &mut Node| {
                    node.set_attribute("is_filled", Value::Boolean(true));
                    node.set_messenger_handler(range_bar);
                    node.set_motor(translate2d(filled_translation));
                    node.set_half_extent(filled_half_extent.into());
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
                &mut result,
                NodeOrObservableIdentifier::Named("textual"),
                text_content.map(|text_content| {
                    |node: &mut Node| {
                        node.set_attribute("text_content", Value::TextString(text_content));
                        node.set_attribute("text_interaction", Value::TextInteraction(text_interaction));
                        node.set_messenger_handler(text_label);
                        node.layer_index = 1;
                    }
                }),
            );
            context.set_attribute("is_rendering_dirty", Value::Boolean(true));
            result
        }
        "Pointer" => {
            if !match_option!(context.get_attribute("enable_interaction"), Value::Boolean).unwrap_or(false)
                || messenger.propagation_direction != PropagationDirection::Parent
            {
                return vec![messenger.clone()];
            }
            match match_option!(messenger.get_attribute("changed_button"), Value::ButtonOrKey).unwrap() {
                0 => {
                    if context.does_observe(match_option!(messenger.get_attribute("device"), Value::NodeOrObservableIdentifier).unwrap()) {
                        let absolute_position: ppga2d::Point =
                            (*match_option!(messenger.get_attribute("absolute_position"), Value::Float3).unwrap()).into();
                        let pointer_start: ppga2d::Point = match_option!(context.get_attribute("pointer_start"), Value::Float3).unwrap().into();
                        let half_extent = context.get_half_extent().unwrap();
                        let axis =
                            match_option!(context.get_attribute("orientation"), Value::Orientation).unwrap_or(Orientation::Horizontal) as usize;
                        let numeric_value = match_option!(context.get_attribute("numeric_value"), Value::Float1)
                            .map(|value| value.unwrap())
                            .unwrap_or(0.0);
                        let numeric_value_range = match_option!(context.get_attribute("numeric_value_range"), Value::Float2)
                            .map(|value| value.unwrap())
                            .unwrap_or([0.0, 1.0]);
                        let mut new_numeric_value = (absolute_position - pointer_start).g0[1 + axis] / half_extent[axis]
                            * 0.5
                            * (numeric_value_range[1] - numeric_value_range[0])
                            + match_option!(context.get_attribute("previous_numeric_value"), Value::Float1)
                                .unwrap()
                                .unwrap();
                        if let Value::SnapClampFunction(snap_clamp_function) = context.get_attribute("snap_clamp_function") {
                            new_numeric_value = (snap_clamp_function.handler)(new_numeric_value, &numeric_value_range);
                        }
                        if numeric_value != new_numeric_value {
                            context.set_attribute("numeric_value", Value::Float1(new_numeric_value.into()));
                            context.set_attribute("rendering_is_dirty", Value::Boolean(true));
                            return vec![
                                Messenger::new(
                                    &message::INPUT_VALUE_CHANGED,
                                    hash_map! {
                                        "child_id" => Value::Void,
                                        "new_value" => Value::Float1(new_numeric_value.into()),
                                    },
                                ),
                                Messenger::new(&message::RECONFIGURE, hash_map! {}),
                            ];
                        }
                    }
                    Vec::new()
                }
                2 => {
                    let pressed_buttons = match_option!(messenger.get_attribute("pressed_buttons"), Value::ButtonsOrKeys).unwrap();
                    if pressed_buttons.contains(&2) {
                        let numeric_value = context.get_attribute("numeric_value");
                        context.set_attribute("previous_numeric_value", numeric_value);
                        context.set_attribute("pointer_start", messenger.get_attribute("absolute_position").clone());
                        vec![Messenger::new(
                            &message::OBSERVE,
                            hash_map! {
                                "observes" => Value::NodeOrObservableIdentifiers(hash_set!{
                                    *match_option!(messenger.get_attribute("device"), Value::NodeOrObservableIdentifier).unwrap()
                                }),
                            },
                        )]
                    } else {
                        context.set_attribute("pointer_start", Value::Void);
                        vec![Messenger::new(
                            &message::OBSERVE,
                            hash_map! {
                                "observes" => Value::NodeOrObservableIdentifiers(hash_set!{}),
                            },
                        )]
                    }
                }
                _ => Vec::new(),
            }
        }
        "Key" => {
            vec![messenger.clone()]
        }
        "InputValueChanged" => {
            if let Value::TextualProjection(textual_projection) = context.get_attribute("textual_projection") {
                let text_content = match_option!(messenger.get_attribute("new_value"), Value::TextString).unwrap();
                if let Some(mut new_numeric_value) = (textual_projection.backward)(text_content.clone()) {
                    let numeric_value = match_option!(context.get_attribute("numeric_value"), Value::Float1)
                        .map(|value| value.unwrap())
                        .unwrap_or(0.0);
                    let numeric_value_range = match_option!(context.get_attribute("numeric_value_range"), Value::Float2)
                        .map(|value| value.unwrap())
                        .unwrap_or([0.0, 1.0]);
                    if let Value::SnapClampFunction(snap_clamp_function) = context.get_attribute("snap_clamp_function") {
                        new_numeric_value = (snap_clamp_function.handler)(new_numeric_value, &numeric_value_range);
                    }
                    let mut result = Vec::new();
                    if new_numeric_value != numeric_value {
                        context.set_attribute("numeric_value", Value::Float1(new_numeric_value.into()));
                        context.set_attribute("rendering_is_dirty", Value::Boolean(true));
                        result.push(Messenger::new(
                            &message::INPUT_VALUE_CHANGED,
                            hash_map! {
                                "child_id" => Value::Void,
                                "new_value" => Value::Float1(new_numeric_value.into()),
                            },
                        ));
                    }
                    result.push(Messenger::new(&message::RECONFIGURE, hash_map! {}));
                    return result;
                }
            }
            Vec::new()
        }
        _ => Vec::new(),
    }
}
