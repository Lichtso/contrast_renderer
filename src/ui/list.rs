//! List
use crate::{
    hash_map, match_option,
    ui::{
        message::{self, Messenger},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Orientation,
    },
    utils::translate2d,
};

fn get_child_count(context: &NodeMessengerContext) -> usize {
    let mut child_count = 0;
    while context
        .inspect_child(&NodeOrObservableIdentifier::Indexed(child_count), |_node: &Node| ())
        .is_some()
    {
        child_count += 1;
    }
    child_count
}

/// List
pub fn list(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "Reconfigure" => {
            let first_phase = match_option!(context.get_attribute("first_phase"), Value::Boolean).unwrap_or(true);
            let mut unaffected = first_phase
                && !context.was_attribute_touched(&[
                    "child_count",
                    "half_extent",
                    "proposed_half_width",
                    "proposed_half_height",
                    "orientation",
                    "reverse",
                ]);
            if let Value::Vec(entries) = context.get_attribute("entries") {
                let mut child_index = 0;
                while context.remove_child(NodeOrObservableIdentifier::Indexed(child_index), true).is_some() {
                    child_index += 1;
                }
                for (child_index, entry) in entries.into_iter().enumerate() {
                    let child_node = match_option!(entry, Value::Node).unwrap();
                    context.add_child(NodeOrObservableIdentifier::Indexed(child_index), child_node, true);
                }
                context.set_attribute("entries", Value::Void);
                unaffected = false;
            }
            context.iter_children(|local_child_id: &NodeOrObservableIdentifier, node: &mut Node| {
                if !matches!(local_child_id, NodeOrObservableIdentifier::Indexed(_)) {
                    return;
                }
                if context.was_attribute_of_child_touched(node, &["proposed_half_width", "proposed_half_height"]) {
                    unaffected = false;
                }
            });
            if unaffected {
                context.set_attribute_privately("first_phase", Value::Boolean(true));
                return Vec::new();
            }
            context.set_attribute_privately("first_phase", Value::Boolean(!first_phase));
            let propose_half_extent = [
                context.get_attribute("proposed_half_width") != Value::Void,
                context.get_attribute("proposed_half_height") != Value::Void,
            ];
            let margin = match_option!(context.derive_attribute("list_margin"), Value::Float1).unwrap().unwrap();
            let padding = match_option!(context.derive_attribute("list_padding"), Value::Float2).unwrap().unwrap();
            let minor_axis_alignment = context.derive_attribute("list_minor_axis_alignment");
            let reverse = matches!(context.get_attribute("reverse"), Value::Boolean(true));
            let major_axis = match_option!(context.get_attribute("orientation"), Value::Orientation).unwrap_or(Orientation::Horizontal) as usize;
            let minor_axis = 1 - major_axis;
            let mut half_extent = [0.0, 0.0];
            let mut weight_sum: f32 = 0.0;
            let mut child_count = 0usize;
            context.iter_children(|local_child_id: &NodeOrObservableIdentifier, node: &mut Node| {
                if !matches!(local_child_id, NodeOrObservableIdentifier::Indexed(_)) {
                    return;
                }
                child_count += 1;
                let weight = match_option!(node.get_attribute("weight"), Value::Float1)
                    .map(|value| value.unwrap())
                    .unwrap_or(0.0)
                    .max(0.0);
                let child_half_extent = node.get_half_extent(true).unwrap();
                half_extent[major_axis] += child_half_extent[major_axis];
                half_extent[minor_axis] = half_extent[minor_axis].max(child_half_extent[minor_axis]);
                weight_sum += weight;
            });
            half_extent[major_axis] += margin * 0.5 * child_count.saturating_sub(1) as f32;
            let major_half_extent_to_distribute = if propose_half_extent[major_axis] {
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
            for child_index in 0..child_count {
                context.configure_child(
                    NodeOrObservableIdentifier::Indexed(child_index),
                    Some(|node: &mut Node| {
                        let weight = match_option!(node.get_attribute("weight"), Value::Float1)
                            .map(|value| value.unwrap())
                            .unwrap_or(0.0)
                            .max(0.0);
                        let mut child_half_extent = node.get_half_extent(true).unwrap();
                        if minor_axis_alignment == Value::Void {
                            child_half_extent[minor_axis] = half_extent[minor_axis];
                        }
                        node.set_attribute("half_extent", Value::Float2(child_half_extent.into()));
                        if !first_phase {
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
                            node.set_attribute("motor", Value::Motor(translate2d(translation).into()));
                        }
                    }),
                );
            }
            if first_phase {
                vec![Messenger::new(&message::RECONFIGURE, hash_map! {})]
            } else {
                if propose_half_extent[0] {
                    half_extent[0] += padding[0];
                    context.set_attribute("proposed_half_width", Value::Float1(half_extent[0].into()));
                }
                if propose_half_extent[1] {
                    half_extent[1] += padding[1];
                    context.set_attribute("proposed_half_height", Value::Float1(half_extent[1].into()));
                }
                Vec::new()
            }
        }
        "AdoptNode" => {
            let content_node = match_option!(messenger.get_attribute("node"), Value::Node).unwrap().clone();
            context.add_child(NodeOrObservableIdentifier::Indexed(get_child_count(context)), content_node, true);
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
                    let focus_child_id = match messenger.get_attribute("origin") {
                        Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::Indexed(_)) => None,
                        Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::Named("parents")) => {
                            Some(NodeOrObservableIdentifier::Indexed(get_child_count(context) / 2))
                        }
                        _ => panic!(),
                    };
                    vec![context.input_focus_parent_or_child(messenger, focus_child_id)]
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
                        if let Some(mut direction) = direction {
                            if matches!(context.get_attribute("reverse"), Value::Boolean(true)) {
                                direction *= -1;
                            }
                            let focus_child_index = *child_index as isize + direction;
                            if focus_child_index >= 0 && focus_child_index < get_child_count(context) as isize {
                                return vec![context
                                    .input_focus_parent_or_child(messenger, Some(NodeOrObservableIdentifier::Indexed(focus_child_index as usize)))];
                            }
                        }
                        Vec::new()
                    } else {
                        vec![context.redirect_input_focus_navigation_to_parent(messenger)]
                    }
                }
                _ => Vec::new(),
            }
        }
        "UserInput" => {
            vec![messenger.clone()]
        }
        _ => vec![messenger.clone()],
    }
}
