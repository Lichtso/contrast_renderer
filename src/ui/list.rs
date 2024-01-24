use crate::{
    match_option,
    ui::{
        message::{self, rendering_default_behavior, Messenger},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Orientation,
    },
    utils::translate2d,
};

pub fn list(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger {
        Messenger::PrepareRendering(message) => {
            println!("list PrepareRendering");
            let (prepare_rendering, _update_rendering) = context.prepare_rendering_helper(message);
            vec![Messenger::PrepareRendering(prepare_rendering)]
        }
        Messenger::Render(message) => rendering_default_behavior(message),
        Messenger::ConfigurationRequest(_message) => {
            println!("list ConfigurationRequest");
            let margin = match_option!(context.derive_attribute("list_margin"), Value::Float1).unwrap().unwrap();
            let padding = match_option!(context.derive_attribute("list_padding"), Value::Float2).unwrap().unwrap();
            let minor_axis_alignment = context.derive_attribute("list_minor_axis_alignment");
            let reverse = match_option!(context.get_attribute("reverse"), Value::Boolean).unwrap_or(false);
            let major_axis = match_option!(context.get_attribute("orientation"), Value::Orientation).unwrap_or(Orientation::Horizontal) as usize;
            let minor_axis = 1 - major_axis;
            let mut half_extent = [0.0, 0.0];
            let mut weight_sum: f32 = 0.0;
            context.iter_children(|_local_child_id: &NodeOrObservableIdentifier, child: &Node| {
                let weight = match_option!(child.properties.get("weight").unwrap(), Value::Float1)
                    .map(|value| value.unwrap())
                    .unwrap_or(0.0)
                    .max(0.0);
                let child_half_extent = child.half_extent.unwrap(); // TODO: child_proposed_half_extent ?
                half_extent[major_axis] += child_half_extent[major_axis];
                half_extent[minor_axis] = half_extent[minor_axis].max(child_half_extent[minor_axis]);
                weight_sum += weight;
            });
            half_extent[major_axis] += margin * 0.5 * context.get_number_of_children().saturating_sub(1) as f32;
            let proposed_half_extent = [
                context.get_attribute("proposed_half_width"),
                context.get_attribute("proposed_half_height"),
            ];
            let major_half_extent_to_distribute = if let Value::Float1(proposed_major_half_extent) = proposed_half_extent[major_axis] {
                let proposed_major_half_extent = proposed_major_half_extent.unwrap() - padding[major_axis];
                let mut major_half_extent_to_distribute = (proposed_major_half_extent - half_extent[major_axis]).max(0.0);
                if weight_sum > 0.0 {
                    half_extent[major_axis] = proposed_major_half_extent;
                    major_half_extent_to_distribute /= weight_sum;
                }
                major_half_extent_to_distribute
            } else {
                0.0
            };
            if let Value::Float1(proposed_half_extent) = proposed_half_extent[minor_axis] {
                half_extent[minor_axis] = proposed_half_extent.unwrap() - padding[major_axis];
            }
            let mut major_axis_offset = -half_extent[major_axis];
            half_extent[0] += padding[0];
            half_extent[1] += padding[1];
            let mut result = vec![Messenger::ConfigurationResponse(message::ConfigurationResponse {
                half_extent: half_extent.into(),
            })];
            for child_index in 0..context.get_number_of_children() {
                let local_child_id = NodeOrObservableIdentifier::Indexed(child_index);
                context.configure_child(
                    &mut result,
                    local_child_id,
                    Some(|node: &mut Node| {
                        let weight = match_option!(node.properties.get("weight").unwrap(), Value::Float1)
                            .map(|value| value.unwrap())
                            .unwrap_or(0.0)
                            .max(0.0);
                        let mut child_half_extent = node.half_extent.unwrap(); // TODO: child_proposed_half_extent ?
                        child_half_extent[major_axis] += major_half_extent_to_distribute * weight;
                        let mut translation = [0.0; 2];
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
                        node.set_motor(translate2d(translation));
                        let mut proposed_half_extent = [None, None];
                        if weight > 0.0 {
                            proposed_half_extent[major_axis] = Some(child_half_extent[major_axis]);
                        }
                        if let Value::Void = minor_axis_alignment {
                            proposed_half_extent[minor_axis] = Some(half_extent[minor_axis]);
                        }
                        /* TODO
                        if let Some(value) = proposed_half_extent[0] {
                            node.properties.insert("proposed_half_width", Value::Float1(value.into()));
                        } else {
                            node.properties.remove("proposed_half_width");
                        }
                        if let Some(value) = proposed_half_extent[1] {
                            node.properties.insert("proposed_half_height", Value::Float1(value.into()));
                        } else {
                            node.properties.remove("proposed_half_height");
                        }*/
                    }),
                );
            }
            result
        }
        Messenger::ChildResized(_message) => {
            println!("list ChildResized");
            vec![Messenger::Reconfigure(message::Reconfigure {})]
        }
        Messenger::Pointer(message) => {
            println!("list Pointer");
            vec![Messenger::Pointer(message.clone())]
        }
        Messenger::Key(message) => {
            println!("list Key");
            vec![Messenger::Key(message.clone())]
        }
        _ => Vec::new(),
    }
}
