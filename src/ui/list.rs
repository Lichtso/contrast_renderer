use crate::{
    hash_map, match_option,
    ui::{
        message::{self, focus_parent_or_child, rendering_default_behavior, Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Orientation,
    },
    utils::translate2d,
};

pub fn list(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.behavior.label {
        "PrepareRendering" => {
            vec![messenger.clone()]
        }
        "Render" => rendering_default_behavior(messenger),
        "Reconfigure" => {
            let first_phase = match_option!(context.get_attribute("first_phase"), Value::Boolean).unwrap_or(true);
            context.set_attribute_privately("first_phase", Value::Boolean(!first_phase));
            let mut unaffected =
                first_phase && !context.was_attribute_touched(&["child_count", "half_extent", "proposed_half_extent", "orientation", "reverse"]);
            context.iter_children(|_local_child_id: &NodeOrObservableIdentifier, node: &Node| {
                if node.was_attribute_touched(&["proposed_half_extent"]) {
                    unaffected = false;
                }
            });
            let mut result = Vec::new();
            if unaffected {
                return result;
            }
            let propose_half_extent = context.get_attribute("proposed_half_extent") != Value::Void;
            let margin = match_option!(context.derive_attribute("list_margin"), Value::Float1).unwrap().unwrap();
            let padding = match_option!(context.derive_attribute("list_padding"), Value::Float2).unwrap().unwrap();
            let minor_axis_alignment = context.derive_attribute("list_minor_axis_alignment");
            let reverse = match_option!(context.get_attribute("reverse"), Value::Boolean).unwrap_or(false);
            let major_axis = match_option!(context.get_attribute("orientation"), Value::Orientation).unwrap_or(Orientation::Horizontal) as usize;
            let minor_axis = 1 - major_axis;
            let mut half_extent = [0.0, 0.0];
            let mut weight_sum: f32 = 0.0;
            context.iter_children(|_local_child_id: &NodeOrObservableIdentifier, node: &Node| {
                let weight = match_option!(node.get_attribute("weight"), Value::Float1)
                    .map(|value| value.unwrap())
                    .unwrap_or(0.0)
                    .max(0.0);
                let child_half_extent = node.get_half_extent(true).unwrap();
                half_extent[major_axis] += child_half_extent[major_axis];
                half_extent[minor_axis] = half_extent[minor_axis].max(child_half_extent[minor_axis]);
                weight_sum += weight;
            });
            half_extent[major_axis] += margin * 0.5 * context.get_number_of_children().saturating_sub(1) as f32;
            let major_half_extent_to_distribute = if propose_half_extent {
                0.0
            } else {
                let imposed_half_extent = context.get_half_extent(false).unwrap();
                half_extent[minor_axis] = imposed_half_extent[minor_axis] - padding[major_axis];
                let proposed_major_half_extent = imposed_half_extent[major_axis] - padding[major_axis];
                let mut major_half_extent_to_distribute = (proposed_major_half_extent - half_extent[major_axis]).max(0.0);
                if weight_sum > 0.0 {
                    half_extent[major_axis] = proposed_major_half_extent;
                    major_half_extent_to_distribute /= weight_sum;
                }
                major_half_extent_to_distribute
            };
            let mut major_axis_offset = -half_extent[major_axis];
            if first_phase {
                result.insert(0, Messenger::new(&message::RECONFIGURE, hash_map! {}));
            }
            for child_index in 0..context.get_number_of_children() {
                context.configure_child(
                    NodeOrObservableIdentifier::Indexed(child_index),
                    Some(|node: &mut Node| {
                        let weight = match_option!(node.get_attribute("weight"), Value::Float1)
                            .map(|value| value.unwrap())
                            .unwrap_or(0.0)
                            .max(0.0);
                        let mut child_half_extent = node.get_half_extent(true).unwrap();
                        if first_phase {
                            if minor_axis_alignment == Value::Void {
                                child_half_extent[minor_axis] = half_extent[minor_axis];
                            }
                            node.set_attribute("half_extent", Value::Float2(child_half_extent.into()));
                        } else {
                            let mut translation = [0.0; 2];
                            child_half_extent[major_axis] += major_half_extent_to_distribute * weight;
                            translation[minor_axis] = match minor_axis_alignment {
                                Value::Void => 0.0,
                                Value::Float1(bias) => (half_extent[minor_axis] - child_half_extent[minor_axis]) * bias.unwrap(),
                                _ => panic!("Unexpected value type"),
                            };
                            translation[major_axis] = major_axis_offset + child_half_extent[major_axis];
                            major_axis_offset += child_half_extent[major_axis] * 2.0 + margin;
                            if reverse {
                                translation[major_axis] *= -1.0;
                            }
                            node.set_attribute("motor", Value::Float4(translate2d(translation).into()));
                        }
                    }),
                );
            }
            half_extent[0] += padding[0];
            half_extent[1] += padding[1];
            if !first_phase && propose_half_extent {
                context.set_half_extent(half_extent.into());
            }
            result
        }
        "PointerInput" => {
            vec![messenger.clone()]
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
                        context.pointer_and_button_input_focus(messenger);
                        return Vec::new();
                    } else if input_state.pressed_keycodes.contains(&'⇧') {
                        None
                    } else {
                        Some(NodeOrObservableIdentifier::Indexed(context.get_number_of_children() / 2))
                    };
                    vec![focus_parent_or_child(messenger, focus_child_id)]
                }
                '←' | '→' | '↑' | '↓' => {
                    if let Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::Indexed(child_index)) = messenger.get_attribute("origin") {
                        let direction = if context.get_attribute("orientation") == Value::Orientation(Orientation::Horizontal) {
                            match changed_keycode {
                                '←' => Some(-1),
                                '→' => Some(1),
                                _ => None,
                            }
                        } else {
                            match changed_keycode {
                                '↑' => Some(1),
                                '↓' => Some(-1),
                                _ => None,
                            }
                        };
                        if let Some(direction) = direction {
                            let focus_child_index = *child_index as isize + direction;
                            if focus_child_index >= 0 && focus_child_index < context.get_number_of_children() as isize {
                                return vec![focus_parent_or_child(
                                    messenger,
                                    Some(NodeOrObservableIdentifier::Indexed(focus_child_index as usize)),
                                )];
                            }
                        }
                        Vec::new()
                    } else {
                        let mut messenger = messenger.clone();
                        messenger.propagation_direction = PropagationDirection::Parent;
                        vec![messenger]
                    }
                }
                _ => Vec::new(),
            }
        }
        _ => Vec::new(),
    }
}
