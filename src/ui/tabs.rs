//! Tabs
use crate::{
    hash_map, match_option,
    path::{Cap, CurveApproximation, DynamicStrokeOptions, Join, Path, StrokeOptions},
    ui::{
        message::{input_focus_parent_or_child, Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Orientation, Rendering,
    },
    utils::translate2d,
};
use geometric_algebra::{ppga2d, simd::Simd32x4};

/// Tab
pub fn tab(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent(false).unwrap();
                let corner_radius = match_option!(context.derive_attribute("tab_corner_radius"), Value::Float1)
                    .unwrap()
                    .unwrap()
                    .min(half_extent[0].min(half_extent[1]));
                let fill_color = match_option!(context.derive_attribute("tab_fill_color"), Value::Float4).unwrap();
                let fill_path = Path::from_rounded_rect([0.0, 0.0], half_extent, corner_radius);
                rendering.colored_paths.push((fill_color, vec![fill_path]));
                update_rendering.set_attribute("rendering", Value::Rendering(Box::new(rendering)));
            }
            vec![update_rendering]
        }
        "Reconfigure" => {
            let mut unaffected = !context.was_attribute_touched(&["child_count", "half_extent"]);
            if let Value::Node(content_node) = context.get_attribute("content") {
                context.add_child(NodeOrObservableIdentifier::Named("content"), content_node);
                context.set_attribute("content", Value::Void);
                unaffected = false;
            }
            if unaffected {
                return Vec::new();
            }
            let half_extent = context.get_half_extent(false);
            if context.get_number_of_children() == 1 {
                context.configure_child(
                    NodeOrObservableIdentifier::Named("content"),
                    Some(|node: &mut Node| {
                        node.set_attribute("half_extent", Value::Float2(half_extent));
                    }),
                );
            }
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            Vec::new()
        }
        "AdoptNode" => {
            let content_node = match_option!(messenger.get_attribute("node"), Value::Node).unwrap().clone();
            context.add_child(NodeOrObservableIdentifier::Named("content"), content_node);
            context.pointer_and_button_input_focus(messenger);
            Vec::new()
        }
        "PointerInput" => {
            if messenger.propagation_direction == PropagationDirection::Parent(-1) {
                context.pointer_and_button_input_focus(messenger);
            }
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
                        Some(NodeOrObservableIdentifier::Named("content"))
                    };
                    vec![input_focus_parent_or_child(messenger, focus_child_id)]
                }
                '←' | '→' | '↑' | '↓' => {
                    let mut messenger = messenger.clone();
                    messenger.propagation_direction = PropagationDirection::Parent(0);
                    vec![messenger]
                }
                _ => Vec::new(),
            }
        }
        _ => Vec::new(),
    }
}

/// Tab handle
pub fn tab_handle(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent(false);
                let weight: Simd32x4 = [match_option!(context.get_attribute("weight"), Value::Float1)
                    .map(|value| value.unwrap())
                    .unwrap_or(0.0); 4]
                    .into();
                let stroke_width = match_option!(context.derive_attribute("tab_handle_stroke_width"), Value::Float1).unwrap();
                let inactive_fill_color: Simd32x4 = match_option!(context.derive_attribute("tab_handle_inactive_fill_color"), Value::Float4)
                    .unwrap()
                    .unwrap()
                    .into();
                let active_fill_color: Simd32x4 = match_option!(context.derive_attribute("tab_handle_active_fill_color"), Value::Float4)
                    .unwrap()
                    .unwrap()
                    .into();
                let fill_color: [f32; 4] = (inactive_fill_color + (active_fill_color - inactive_fill_color) * weight).into();
                let stroke_color = match_option!(context.derive_attribute("tab_handle_stroke_color"), Value::Float4).unwrap();
                let fill_path = Path::from_ellipse([0.0, 0.0], half_extent.unwrap());
                let mut stroke_path = fill_path.clone();
                stroke_path.stroke_options = Some(StrokeOptions {
                    width: stroke_width,
                    offset: 0.0.into(),
                    miter_clip: 1.0.into(),
                    closed: true,
                    dynamic_stroke_options_group: 0,
                    curve_approximation: CurveApproximation::UniformTangentAngle(0.1.into()),
                });
                rendering.colored_paths.push((fill_color.into(), vec![fill_path]));
                rendering.colored_paths.push((stroke_color, vec![stroke_path]));
                rendering.dynamic_stroke_options = vec![DynamicStrokeOptions::Solid {
                    join: Join::Miter,
                    start: Cap::Butt,
                    end: Cap::Butt,
                }];
                update_rendering.set_attribute("rendering", Value::Rendering(Box::new(rendering)));
            }
            vec![update_rendering]
        }
        "Reconfigure" => {
            if !context.was_attribute_touched(&["half_extent", "weight"]) {
                return Vec::new();
            }
            context.set_half_extent(match_option!(context.derive_attribute("tab_handle_half_extent"), Value::Float2).unwrap());
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            Vec::new()
        }
        "PointerInput" => {
            if messenger.propagation_direction != PropagationDirection::Parent(-1) {
                return vec![messenger.clone()];
            }
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            if messenger.get_attribute("changed_pointer") == &Value::InputChannel(0) {
                if let Value::Boolean(pressed) = messenger.get_attribute("pressed_or_released") {
                    if !*pressed
                        && *input_state.is_inside_bounds.get(&0).unwrap()
                        && context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap())
                    {
                        context.touch_attribute("active");
                    }
                    context.pointer_and_button_input_focus(messenger);
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
                    if messenger.get_attribute("origin") != &Value::Void {
                        context.pointer_and_button_input_focus(messenger);
                    } else if input_state.pressed_keycodes.contains(&'⇧') {
                        return vec![input_focus_parent_or_child(messenger, None)];
                    }
                    Vec::new()
                }
                '←' | '→' | '↑' | '↓' => {
                    let mut messenger = messenger.clone();
                    messenger.propagation_direction = PropagationDirection::Parent(0);
                    vec![messenger]
                }
                '⏎' => {
                    context.touch_attribute("active");
                    Vec::new()
                }
                _ => Vec::new(),
            }
        }
        _ => Vec::new(),
    }
}

/// Tab container
pub fn tab_container(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "Reconfigure" => {
            let mut unaffected = !context.was_attribute_touched(&["child_count", "half_extent", "orientation"]);
            if let Value::Vec(entries) = context.get_attribute("entries") {
                for child_index in 0..context.get_number_of_children() {
                    context.remove_child(NodeOrObservableIdentifier::Indexed(child_index), true);
                }
                let weight = 1.0 / entries.len() as f32;
                for (tab_index, entry) in entries.into_iter().enumerate() {
                    let tab_node = match_option!(entry, Value::Node).unwrap();
                    context.add_child(NodeOrObservableIdentifier::NamedAndIndexed("tab", tab_index), tab_node);
                    let handle_node = Node::new(
                        tab_handle,
                        hash_map! {
                            "weight" => Value::Float1(weight.into()),
                        },
                    );
                    context.add_child(NodeOrObservableIdentifier::NamedAndIndexed("handle", tab_index), handle_node);
                }
                context.set_attribute("entries", Value::Void);
                unaffected = false;
            }
            let mut active = None;
            context.iter_children(|local_child_id: &NodeOrObservableIdentifier, node: &Node| {
                if node.was_attribute_touched(&["weight"]) {
                    unaffected = false;
                }
                if node.was_attribute_touched(&["active"]) {
                    unaffected = false;
                    active = Some(*local_child_id);
                }
            });
            if unaffected {
                return Vec::new();
            }
            if let Some(NodeOrObservableIdentifier::NamedAndIndexed("handle", handle_index)) = active {
                let tab_count = context.get_number_of_children() / 2;
                let start_time = context.get_last_animation_time();
                for child_index in 0..tab_count {
                    let weight = if child_index == handle_index { 1.0 } else { 0.0 };
                    context.configure_child(
                        NodeOrObservableIdentifier::NamedAndIndexed("handle", child_index),
                        Some(|node: &mut Node| {
                            node.set_attribute_animated("weight", Value::Float1(weight.into()), start_time, 5.0);
                        }),
                    );
                }
            }
            let tab_count = context.get_number_of_children() / 2;
            let tab_handle_margin = match_option!(context.derive_attribute("tab_handle_margin"), Value::Float1)
                .unwrap()
                .unwrap();
            let tab_handle_padding = match_option!(context.derive_attribute("tab_handle_padding"), Value::Float1)
                .unwrap()
                .unwrap();
            let tab_handle_half_extent = match_option!(context.derive_attribute("tab_handle_half_extent"), Value::Float2)
                .unwrap()
                .unwrap();
            let margin = match_option!(context.derive_attribute("tabs_margin"), Value::Float1).unwrap().unwrap();
            let padding = match_option!(context.derive_attribute("tabs_padding"), Value::Float2).unwrap().unwrap();
            let major_axis = match_option!(context.get_attribute("orientation"), Value::Orientation).unwrap_or(Orientation::Horizontal) as usize;
            let minor_axis = 1 - major_axis;
            let mut half_extent = context.get_half_extent(false).unwrap();
            half_extent[0] -= padding[0];
            half_extent[1] -= padding[1];
            let mut weights = vec![0.0; tab_count];
            context.iter_children(|local_child_id: &NodeOrObservableIdentifier, node: &Node| {
                let child_index = match local_child_id {
                    NodeOrObservableIdentifier::NamedAndIndexed("handle", child_index) => *child_index,
                    _ => return,
                };
                weights[child_index] = match_option!(node.properties.get("weight").unwrap(), Value::Float1)
                    .map(|value| value.unwrap())
                    .unwrap_or(0.0)
                    .max(0.0);
            });
            let open_tab_count = weights
                .iter()
                .fold(0usize, |counter, weight| if *weight > 0.0 { counter + 1 } else { counter });
            let mut major_axis_offset = -half_extent[major_axis];
            half_extent[major_axis] -= margin * 0.5 * open_tab_count.saturating_sub(1) as f32;
            for (child_index, weight) in weights.iter().enumerate() {
                context.configure_child(
                    NodeOrObservableIdentifier::NamedAndIndexed("tab", child_index),
                    Some(|node: &mut Node| {
                        node.set_attribute("dormant", Value::Boolean(*weight == 0.0));
                        node.set_attribute_privately("layer_index", Value::Natural1(0));
                        let mut child_half_extent = [0.0; 2];
                        child_half_extent[major_axis] = half_extent[major_axis] * weight;
                        child_half_extent[minor_axis] = half_extent[minor_axis];
                        node.set_attribute("half_extent", Value::Float2(child_half_extent.into()));
                        let mut translation = [0.0; 2];
                        translation[major_axis] = major_axis_offset + child_half_extent[major_axis];
                        if *weight > 0.0 {
                            major_axis_offset += child_half_extent[major_axis] * 2.0 + margin;
                        } else {
                            translation[major_axis] -= margin * 0.5;
                        }
                        node.set_attribute("motor", Value::Float4(translate2d(translation).into()));
                    }),
                );
                context.configure_child(
                    NodeOrObservableIdentifier::NamedAndIndexed("handle", child_index),
                    Some(|node: &mut Node| {
                        node.set_attribute_privately("layer_index", Value::Natural1(1));
                        let mut translation = [0.0; 2];
                        translation[major_axis] = (tab_handle_half_extent[major_axis] * 2.0 + tab_handle_margin)
                            * (child_index as f32 - tab_count.saturating_sub(1) as f32 * 0.5);
                        translation[minor_axis] = tab_handle_padding + tab_handle_half_extent[minor_axis] - half_extent[minor_axis];
                        if major_axis == 0 {
                            translation[minor_axis] = -translation[minor_axis];
                        }
                        node.set_attribute("motor", Value::Float4(translate2d(translation).into()));
                    }),
                );
            }
            Vec::new()
        }
        "AdoptNode" => {
            let tab_index = context.get_number_of_children();
            let tab_node = match_option!(messenger.get_attribute("node"), Value::Node).unwrap().clone();
            context.add_child(NodeOrObservableIdentifier::NamedAndIndexed("tab", tab_index), tab_node);
            let handle_node = Node::new(
                tab_handle,
                hash_map! {
                    "weight" => Value::Float1((0.0).into()),
                },
            );
            context.add_child(NodeOrObservableIdentifier::NamedAndIndexed("handle", tab_index), handle_node);
            Vec::new()
        }
        "PointerInput" => {
            if messenger.propagation_direction != PropagationDirection::Parent(-1) {
                return vec![messenger.clone()];
            }
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            if messenger.get_attribute("changed_pointer") == &Value::InputChannel(0) {
                let absolute_position: ppga2d::Point = (*input_state.absolute_positions.get(&0).unwrap()).into();
                let margin = match_option!(context.derive_attribute("tabs_margin"), Value::Float1).unwrap().unwrap();
                let major_axis = match_option!(context.get_attribute("orientation"), Value::Orientation).unwrap_or(Orientation::Horizontal) as usize;
                let motor_factor = if major_axis == 0 { 2.0 } else { -2.0 };
                if let Value::Boolean(pressed) = messenger.get_attribute("pressed_or_released") {
                    if *pressed {
                        let tabs_splitter_width = match_option!(context.derive_attribute("tabs_splitter_width"), Value::Float1)
                            .unwrap()
                            .unwrap();
                        let relative_position: ppga2d::Point = (*input_state.relative_positions.get(&0).unwrap()).into();
                        let tab_count = context.get_number_of_children() / 2;
                        let mut splitter_index = None;
                        for child_index in 1..tab_count {
                            let mut break_the_loop = false;
                            context.inspect_child(&NodeOrObservableIdentifier::NamedAndIndexed("tab", child_index), |node: &Node| {
                                let child_motor: ppga2d::Motor = match_option!(node.get_attribute("motor"), Value::Float4).unwrap().into();
                                let half_extent = node.get_half_extent(false).unwrap()[major_axis];
                                let mut boundary = child_motor.g0[3 - major_axis] * motor_factor;
                                if node.get_attribute("dormant") != Value::Boolean(true) {
                                    boundary -= half_extent + margin * 0.5;
                                }
                                let dist = relative_position.g0[1 + major_axis] - boundary;
                                if dist.abs() < tabs_splitter_width * 0.5 {
                                    break_the_loop = dist < 0.0;
                                    splitter_index = Some(child_index - 1);
                                }
                            });
                            if break_the_loop {
                                break;
                            }
                        }
                        if let Some(splitter_index) = splitter_index {
                            let mut weights = [0.0; 2];
                            let mut half_extents = [0.0; 2];
                            for side in 0..2 {
                                context.inspect_child(
                                    &NodeOrObservableIdentifier::NamedAndIndexed("tab", splitter_index + side),
                                    |node: &Node| {
                                        half_extents[side] = node.get_half_extent(false).unwrap()[major_axis];
                                    },
                                );
                                context.inspect_child(
                                    &NodeOrObservableIdentifier::NamedAndIndexed("handle", splitter_index + side),
                                    |node: &Node| {
                                        weights[side] = match_option!(node.get_attribute("weight"), Value::Float1).unwrap().unwrap();
                                    },
                                );
                            }
                            context.set_attribute("weights", Value::Float2(weights.into()));
                            context.set_attribute("half_extents", Value::Float2(half_extents.into()));
                            context.set_attribute("splitter_index", Value::Natural1(splitter_index));
                            context.set_attribute("pointer_start", Value::Float3(*input_state.absolute_positions.get(&0).unwrap()));
                            context.pointer_and_button_input_focus(messenger);
                        } else {
                            return Vec::new();
                        }
                    } else {
                        if context.get_attribute("pointer_start") != Value::Void {
                            context.pointer_and_button_input_focus(messenger);
                        }
                        context.set_attribute("pointer_start", Value::Void);
                    }
                } else if context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap()) {
                    let mut weights = match_option!(context.get_attribute("weights"), Value::Float2).unwrap().unwrap();
                    let half_extents = match_option!(context.get_attribute("half_extents"), Value::Float2).unwrap().unwrap();
                    let splitter_index = match_option!(context.get_attribute("splitter_index"), Value::Natural1).unwrap();
                    let pointer_start: ppga2d::Point = match_option!(context.get_attribute("pointer_start"), Value::Float3).unwrap().into();
                    let weight_sum = weights[0] + weights[1];
                    let half_extent_sum = half_extents[0] + half_extents[1];
                    let mut diff = (absolute_position - pointer_start).g0[1 + major_axis] / half_extent_sum * weight_sum * 0.5;
                    /*if diff < -weights[0] || diff > weights[1] {
                        let margin = match_option!(context.derive_attribute("tabs_margin"), Value::Float1).unwrap().unwrap();
                        // TODO: Redistribute weight of missing margin
                    }*/
                    diff = diff.max(-weights[0]).min(weights[1]);
                    weights[0] += diff;
                    weights[1] -= diff;
                    for (side, weight) in weights.iter().enumerate() {
                        context.configure_child(
                            NodeOrObservableIdentifier::NamedAndIndexed("handle", splitter_index + side),
                            Some(|node: &mut Node| {
                                node.set_attribute("weight", Value::Float1(weight.into()));
                            }),
                        );
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
            let tab_count = context.get_number_of_children() / 2;
            let mut focus_child_id = None;
            match changed_keycode {
                '⇥' => {
                    if messenger.get_attribute("origin") != &Value::Void {
                        context.pointer_and_button_input_focus(messenger);
                        return Vec::new();
                    } else if input_state.pressed_keycodes.contains(&'⇧') {
                        return vec![input_focus_parent_or_child(messenger, None)];
                    } else {
                        focus_child_id = Some(NodeOrObservableIdentifier::NamedAndIndexed("handle", tab_count / 2));
                    }
                }
                '←' | '→' | '↑' | '↓' => {
                    if let Value::NodeOrObservableIdentifier(child_id) = messenger.get_attribute("origin") {
                        let direction = if context.get_attribute("orientation") == Value::Orientation(Orientation::Horizontal) {
                            match changed_keycode {
                                '←' => 0,
                                '→' => 1,
                                '↑' => 2,
                                '↓' => 3,
                                _ => unreachable!(),
                            }
                        } else {
                            match changed_keycode {
                                '←' => 2,
                                '→' => 3,
                                '↑' => 0,
                                '↓' => 1,
                                _ => unreachable!(),
                            }
                        };
                        fn find_next_open_tab(
                            context: &NodeMessengerContext,
                            tab_count: usize,
                            mut tab_index: usize,
                            direction: isize,
                        ) -> Option<NodeOrObservableIdentifier> {
                            while tab_index as isize + direction >= 0 && tab_index as isize + direction < tab_count as isize {
                                tab_index = (tab_index as isize + direction) as usize;
                                let weight = context
                                    .inspect_child(&NodeOrObservableIdentifier::NamedAndIndexed("handle", tab_index), |node: &Node| {
                                        node.get_attribute("weight")
                                    })
                                    .unwrap();
                                if match_option!(weight, Value::Float1).unwrap().unwrap() > 0.0 {
                                    return Some(NodeOrObservableIdentifier::NamedAndIndexed("tab", tab_index));
                                }
                            }
                            None
                        }
                        focus_child_id = match child_id {
                            NodeOrObservableIdentifier::NamedAndIndexed("handle", tab_index) => match direction {
                                3 => Some(NodeOrObservableIdentifier::NamedAndIndexed("tab", *tab_index)),
                                0 if *tab_index > 0 => Some(NodeOrObservableIdentifier::NamedAndIndexed("handle", *tab_index - 1)),
                                1 if *tab_index + 1 < tab_count => Some(NodeOrObservableIdentifier::NamedAndIndexed("handle", *tab_index + 1)),
                                _ => None,
                            },
                            NodeOrObservableIdentifier::NamedAndIndexed("tab", tab_index) => match direction {
                                2 => Some(NodeOrObservableIdentifier::NamedAndIndexed("handle", *tab_index)),
                                0 => find_next_open_tab(context, tab_count, *tab_index, -1),
                                1 => find_next_open_tab(context, tab_count, *tab_index, 1),
                                _ => None,
                            },
                            _ => None,
                        };
                    } else {
                        let mut messenger = messenger.clone();
                        messenger.propagation_direction = PropagationDirection::Parent(0);
                        return vec![messenger];
                    }
                }
                _ => {}
            }
            if focus_child_id.is_some() {
                vec![input_focus_parent_or_child(messenger, focus_child_id)]
            } else {
                Vec::new()
            }
        }
        _ => Vec::new(),
    }
}
