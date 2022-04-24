use crate::{
    hash_map, match_option,
    path::Path,
    ui::{
        label::text_label,
        message::{self, pointer_and_button_input_focus, rendering_default_behavior, Messenger, PropagationDirection},
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
        "Render" => rendering_default_behavior(messenger),
        "Reconfigure" => {
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            vec![Messenger::new(&message::CONFIGURED, hash_map! {})]
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
                let half_extent = context.get_half_extent(false);
                rendering.clip_paths = vec![Path::from_rounded_rect(
                    [0.0, 0.0],
                    half_extent.unwrap(),
                    match_option!(context.derive_attribute("range_corner_radius"), Value::Float1)
                        .unwrap()
                        .unwrap(),
                )];
                update_rendering.set_attribute("rendering", Value::Rendering(Box::new(rendering)));
            }
            vec![messenger.clone(), update_rendering]
        }
        "Render" => rendering_default_behavior(messenger),
        "Reconfigure" => {
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
            let mut result = vec![Messenger::new(&message::CONFIGURED, hash_map! {})];
            context.configure_child(
                &mut result,
                NodeOrObservableIdentifier::Named("empty"),
                Some(&mut |node: &mut Node| {
                    node.set_messenger_handler(range_bar);
                    node.set_attribute("is_filled", Value::Boolean(false));
                    node.set_attribute_privately("motor", Value::Float4(translate2d(empty_translation).into()));
                    node.set_attribute("half_extent", Value::Float2(empty_half_extent.into()));
                }),
            );
            context.configure_child(
                &mut result,
                NodeOrObservableIdentifier::Named("filled"),
                Some(&mut |node: &mut Node| {
                    node.set_messenger_handler(range_bar);
                    node.set_attribute("is_filled", Value::Boolean(true));
                    node.set_attribute_privately("motor", Value::Float4(translate2d(filled_translation).into()));
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
                &mut result,
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
            result
        }
        "PointerInput" => {
            if !match_option!(context.get_attribute("enable_interaction"), Value::Boolean).unwrap_or(false)
                || messenger.propagation_direction != PropagationDirection::Parent
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
                    return pointer_and_button_input_focus(messenger);
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
                        return vec![Messenger::new(&message::RECONFIGURE, hash_map! {})];
                    }
                }
            }
            Vec::new()
        }
        "PropertiesChanged" => {
            if match_option!(messenger.get_attribute("attributes"), Value::Attributes)
                .unwrap()
                .contains("text_content")
            {
                if let Value::TextualProjection(textual_projection) = context.get_attribute("textual_projection") {
                    let text_content = context
                        .inspect_child(
                            match_option!(messenger.get_attribute("child_id"), Value::NodeOrObservableIdentifier).unwrap(),
                            |child_node: &Node| match_option!(child_node.get_attribute("text_content"), Value::TextString).unwrap(),
                        )
                        .unwrap();
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
                            context.set_attribute("numeric_value", Value::Float1(new_numeric_value.into()));
                            context.set_attribute("rendering_is_dirty", Value::Boolean(true));
                        }
                        return vec![Messenger::new(&message::RECONFIGURE, hash_map! {})];
                    }
                }
            }
            Vec::new()
        }
        _ => Vec::new(),
    }
}
