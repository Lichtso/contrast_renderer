//! Button
use crate::{
    hash_map, match_option,
    path::Path,
    ui::{
        message::{self, Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Rendering,
    },
};

/// Button
pub fn button(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent(false).unwrap();
                let corner_radius = match_option!(context.derive_attribute("button_corner_radius"), Value::Float1)
                    .unwrap()
                    .unwrap()
                    .min(half_extent[0].min(half_extent[1]));
                let fill_color = match_option!(context.derive_attribute("button_fill_color"), Value::Float4).unwrap();
                let fill_path = Path::from_rounded_rect([0.0, 0.0], half_extent, corner_radius);
                rendering.colored_paths.push((fill_color, vec![fill_path]));
                update_rendering.set_attribute("rendering", Value::Rendering(Box::new(rendering)));
            }
            vec![update_rendering]
        }
        "Reconfigure" => {
            let mut unaffected = !context.was_attribute_touched(&["child_count", "half_extent", "observes"]);
            if let Value::Node(content_node) = context.get_attribute("content") {
                context.add_child(NodeOrObservableIdentifier::Named("content"), content_node, true);
                context.set_attribute("content", Value::Void);
                unaffected = false;
            }
            unaffected &= !context
                .inspect_child(&NodeOrObservableIdentifier::Named("content"), |content| {
                    context.was_attribute_of_child_touched(content, &["proposed_half_extent"])
                })
                .unwrap_or(false);
            if unaffected {
                return Vec::new();
            }
            if let Some(content_half_extent) =
                context.inspect_child(&NodeOrObservableIdentifier::Named("content"), |content| content.get_half_extent(true))
            {
                let padding = match_option!(context.derive_attribute("button_padding"), Value::Float2).unwrap().unwrap();
                let mut half_extent = context.get_half_extent(false).unwrap();
                let mut content_half_extent = content_half_extent.unwrap();
                half_extent[0] = content_half_extent[0] + padding[0];
                half_extent[1] = content_half_extent[1] + padding[1];
                content_half_extent[0] = half_extent[0] - padding[0];
                content_half_extent[1] = half_extent[1] - padding[1];
                context.set_attribute("proposed_half_extent", Value::Float2(half_extent.into()));
                context.configure_child(
                    NodeOrObservableIdentifier::Named("content"),
                    Some(|node: &mut Node| {
                        node.set_attribute("half_extent", Value::Float2(content_half_extent.into()));
                    }),
                );
            }
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            Vec::new()
        }
        "PointerInput" => {
            if !match_option!(context.get_attribute("enable_interaction"), Value::Boolean).unwrap_or(false)
                || messenger.propagation_direction != PropagationDirection::Parent(-1)
            {
                return vec![messenger.clone()];
            }
            let mut messengers = Vec::new();
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            if messenger.get_attribute("changed_pointer") == &Value::InputChannel(0) && *input_state.is_inside_bounds.get(&0).unwrap() {
                if let Value::Boolean(pressed) = messenger.get_attribute("pressed_or_released") {
                    if !*pressed
                        && context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap())
                    {
                        context.touch_attribute("active");
                        if let Value::NodeOrObservableIdentifier(input_field_id) = context.get_attribute("input_field_id") {
                            messengers.push(Messenger::new(
                                &message::USER_INPUT,
                                hash_map! {
                                    "input_state" => messenger.get_attribute("input_state").clone(),
                                    "input_source" => messenger.get_attribute("input_source").clone(),
                                    "input_field_id" => Value::NodeOrObservableIdentifier(input_field_id),
                                },
                            ));
                        }
                    }
                    messengers.append(&mut context.pointer_and_button_input_focus(messenger));
                }
            }
            messengers
        }
        "ButtonInput" => {
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            let changed_keycode = *match_option!(messenger.get_attribute("changed_keycode"), Value::Character).unwrap();
            if !input_state.pressed_keycodes.contains(&changed_keycode) {
                return Vec::new();
            }
            match changed_keycode {
                '⇥' => {
                    if messenger.get_attribute("origin") != &Value::Void {
                        return context.pointer_and_button_input_focus(messenger);
                    } else if input_state.pressed_keycodes.contains(&'⇧') {
                        return vec![context.input_focus_parent_or_child(messenger, None)];
                    }
                    Vec::new()
                }
                '←' | '→' | '↑' | '↓' => vec![context.redirect_input_focus_navigation_to_parent(messenger)],
                '⏎' => {
                    context.touch_attribute("active");
                    if let Value::NodeOrObservableIdentifier(input_field_id) = context.get_attribute("input_field_id") {
                        vec![Messenger::new(
                            &message::USER_INPUT,
                            hash_map! {
                                "input_state" => messenger.get_attribute("input_state").clone(),
                                "input_source" => messenger.get_attribute("input_source").clone(),
                                "input_field_id" => Value::NodeOrObservableIdentifier(input_field_id),
                            },
                        )]
                    } else {
                        Vec::new()
                    }
                }
                _ => Vec::new(),
            }
        }
        _ => Vec::new(),
    }
}
