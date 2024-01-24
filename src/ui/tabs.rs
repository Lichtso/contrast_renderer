//! Tabs
use crate::{
    hash_map, match_option,
    path::{Cap, CurveApproximation, DynamicStrokeOptions, Join, Path, StrokeOptions},
    ui::{
        message::{Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Orientation, Rendering, ANIMATION_FADE_IN_OUT,
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
                context.add_child(NodeOrObservableIdentifier::Named("content"), content_node, true);
                context.set_attribute("content", Value::Void);
                unaffected = false;
            }
            if unaffected {
                return Vec::new();
            }
            let half_extent = context.get_half_extent(false);
            if context
                .inspect_child(&NodeOrObservableIdentifier::Named("content"), |_node: &Node| ())
                .is_some()
            {
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
            context.add_child(NodeOrObservableIdentifier::Named("content"), content_node, true);
            Vec::new()
        }
        "PointerInput" => {
            let mut messengers = vec![messenger.clone()];
            if messenger.propagation_direction == PropagationDirection::Parent(-1) {
                messengers.append(&mut context.input_focus_self(messenger));
            }
            messengers
        }
        "ButtonInput" => {
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            let changed_key = *match_option!(messenger.get_attribute("changed_key"), Value::Character).unwrap();
            if !input_state.pressed_keys.contains(&changed_key) {
                return Vec::new();
            }
            match changed_key {
                '⇥' => {
                    let focus_child_id = if messenger.get_attribute("origin") != &Value::Void {
                        return context.input_focus_self(messenger);
                    } else if input_state.pressed_keys.contains(&'⇧') {
                        None
                    } else {
                        Some(NodeOrObservableIdentifier::Named("content"))
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
            let half_extent = match_option!(context.derive_attribute("tab_handle_half_extent"), Value::Float2)
                .unwrap()
                .unwrap();
            context.set_attribute("proposed_half_width", Value::Float1(half_extent[0].into()));
            context.set_attribute("proposed_half_height", Value::Float1(half_extent[1].into()));
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            Vec::new()
        }
        "PointerInput" => {
            if messenger.propagation_direction != PropagationDirection::Parent(-1) {
                return vec![messenger.clone()];
            }
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            if messenger.get_attribute("changed_pointer") == &Value::InputChannel(0) && *input_state.is_inside_bounds.get(&0).unwrap() {
                if let Value::Boolean(pressed) = messenger.get_attribute("pressed_or_released") {
                    if !*pressed
                        && context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap())
                    {
                        context.touch_attribute("active");
                    }
                    return context.input_focus_self(messenger);
                }
            }
            Vec::new()
        }
        "ButtonInput" => {
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            let changed_key = *match_option!(messenger.get_attribute("changed_key"), Value::Character).unwrap();
            if !input_state.pressed_keys.contains(&changed_key) {
                return Vec::new();
            }
            match changed_key {
                '⇥' => {
                    if messenger.get_attribute("origin") != &Value::Void {
                        return context.input_focus_self(messenger);
                    } else if input_state.pressed_keys.contains(&'⇧') {
                        return vec![context.input_focus_parent_or_child(messenger, None)];
                    } else {
                        context.touch_attribute("active");
                    }
                    Vec::new()
                }
                '←' | '→' | '↑' | '↓' => vec![context.redirect_input_focus_navigation_to_parent(messenger)],
                _ => Vec::new(),
            }
        }
        "Defocus" => context.input_defocus_self(messenger),
        _ => vec![messenger.clone()],
    }
}

fn get_tab_count(context: &NodeMessengerContext) -> usize {
    let mut tab_count = 0;
    while context
        .inspect_child(&NodeOrObservableIdentifier::NamedAndIndexed("handle", tab_count), |_node: &Node| ())
        .is_some()
    {
        tab_count += 1;
    }
    tab_count
}

/// Tab container
pub fn tab_container(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "Reconfigure" => {
            let mut unaffected = !context.was_attribute_touched(&["child_count", "half_extent", "orientation"]);
            if let Value::Vec(entries) = context.get_attribute("entries") {
                let mut tab_index = 0;
                while context
                    .remove_child(NodeOrObservableIdentifier::NamedAndIndexed("tab", tab_index), true)
                    .is_some()
                {
                    assert!(context
                        .remove_child(NodeOrObservableIdentifier::NamedAndIndexed("handle", tab_index), true)
                        .is_some());
                    tab_index += 1;
                }
                let weight = 1.0 / entries.len() as f32;
                for (tab_index, entry) in entries.into_iter().enumerate() {
                    let tab_node = match_option!(entry, Value::Node).unwrap();
                    context.add_child(NodeOrObservableIdentifier::NamedAndIndexed("tab", tab_index), tab_node, true);
                    let handle_node = Node::new(
                        tab_handle,
                        hash_map! {
                            "weight" => Value::Float1(weight.into()),
                        },
                    );
                    context.add_child(NodeOrObservableIdentifier::NamedAndIndexed("handle", tab_index), handle_node, false);
                }
                context.set_attribute("entries", Value::Void);
                unaffected = false;
            }
            let mut active = None;
            context.iter_children(|local_child_id: &NodeOrObservableIdentifier, node: &mut Node| {
                if context.was_attribute_of_child_touched(node, &["weight"]) {
                    unaffected = false;
                }
                if context.was_attribute_of_child_touched(node, &["active"]) {
                    unaffected = false;
                    active = Some(*local_child_id);
                }
            });
            if unaffected {
                return Vec::new();
            }
            let start_time = context.get_last_animation_time();
            let duration = match_option!(context.derive_attribute("tab_animation_duration"), Value::Float1)
                .unwrap()
                .unwrap() as f64;
            let mut tab_count = 0usize;
            let mut open_tab_count = 0usize;
            context.iter_children(|local_child_id: &NodeOrObservableIdentifier, node: &mut Node| {
                if !matches!(local_child_id, NodeOrObservableIdentifier::NamedAndIndexed("handle", _)) {
                    return;
                }
                tab_count += 1;
                if active.is_some() {
                    let weight = if Some(*local_child_id) == active { 1.0 } else { 0.0 };
                    node.set_attribute_animated("weight", Value::Float1(weight.into()), start_time, duration, ANIMATION_FADE_IN_OUT);
                }
                let weight = match_option!(node.get_attribute("weight"), Value::Float1).unwrap().unwrap();
                if weight > 0.0 {
                    open_tab_count += 1;
                }
            });
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
            let mut major_axis_offset = -half_extent[major_axis];
            half_extent[major_axis] -= margin * 0.5 * open_tab_count.saturating_sub(1) as f32;
            for tab_index in 0..tab_count {
                let weight = context
                    .configure_child(
                        NodeOrObservableIdentifier::NamedAndIndexed("handle", tab_index),
                        Some(|node: &mut Node| {
                            node.set_attribute_privately("layer_index", Value::Natural1(1));
                            let mut translation = [0.0; 2];
                            translation[major_axis] = (tab_handle_half_extent[major_axis] * 2.0 + tab_handle_margin)
                                * (tab_index as f32 - tab_count.saturating_sub(1) as f32 * 0.5);
                            translation[minor_axis] = tab_handle_padding + tab_handle_half_extent[minor_axis] - half_extent[minor_axis];
                            if major_axis == 0 {
                                translation[minor_axis] = -translation[minor_axis];
                            }
                            node.set_attribute("motor", Value::Motor(translate2d(translation).into()));
                            match_option!(node.get_attribute("weight"), Value::Float1).unwrap().unwrap()
                        }),
                    )
                    .unwrap();
                context.configure_child(
                    NodeOrObservableIdentifier::NamedAndIndexed("tab", tab_index),
                    Some(|node: &mut Node| {
                        node.set_attribute("dormant", Value::Boolean(weight == 0.0));
                        node.set_attribute_privately("layer_index", Value::Natural1(0));
                        let mut child_half_extent = [0.0; 2];
                        child_half_extent[major_axis] = half_extent[major_axis] * weight;
                        child_half_extent[minor_axis] = half_extent[minor_axis];
                        node.set_attribute("half_extent", Value::Float2(child_half_extent.into()));
                        let mut translation = [0.0; 2];
                        translation[major_axis] = major_axis_offset + child_half_extent[major_axis];
                        if weight > 0.0 {
                            major_axis_offset += child_half_extent[major_axis] * 2.0 + margin;
                        } else {
                            translation[major_axis] -= margin * 0.5;
                        }
                        node.set_attribute("motor", Value::Motor(translate2d(translation).into()));
                    }),
                );
            }
            Vec::new()
        }
        "AdoptNode" => {
            let tab_index = get_tab_count(context);
            let tab_node = match_option!(messenger.get_attribute("node"), Value::Node).unwrap().clone();
            context.add_child(NodeOrObservableIdentifier::NamedAndIndexed("tab", tab_index), tab_node, true);
            let handle_node = Node::new(
                tab_handle,
                hash_map! {
                    "weight" => Value::Float1(0.0.into()),
                },
            );
            context.add_child(NodeOrObservableIdentifier::NamedAndIndexed("handle", tab_index), handle_node, false);
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
                let tab_count = get_tab_count(context);
                if let Value::Boolean(pressed) = messenger.get_attribute("pressed_or_released") {
                    if *pressed {
                        let tabs_splitter_width = match_option!(context.derive_attribute("tabs_splitter_width"), Value::Float1)
                            .unwrap()
                            .unwrap();
                        let relative_position: ppga2d::Point = (*input_state.relative_positions.get(&0).unwrap()).into();
                        let mut splitter_index = None;
                        for child_index in 1..tab_count {
                            let mut break_the_loop = false;
                            context.inspect_child(&NodeOrObservableIdentifier::NamedAndIndexed("tab", child_index), |node: &Node| {
                                let child_motor: ppga2d::Motor = match_option!(node.get_attribute("motor"), Value::Motor).unwrap().into();
                                let half_extent = node.get_half_extent(false).unwrap()[major_axis];
                                let mut boundary = child_motor[3 - major_axis] * motor_factor;
                                if node.get_attribute("dormant") != Value::Boolean(true) {
                                    boundary -= half_extent + margin * 0.5;
                                }
                                let dist = relative_position[1 + major_axis] - boundary;
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
                            for tab_index in 0..tab_count {
                                context.configure_child(
                                    NodeOrObservableIdentifier::NamedAndIndexed("handle", tab_index),
                                    Some(|node: &mut Node| {
                                        node.set_attribute("prev_weight", node.get_attribute("weight"));
                                    }),
                                );
                            }
                            context.set_attribute("splitter_index", Value::Natural1(splitter_index));
                            context.set_attribute("pointer_start", Value::Float3(*input_state.absolute_positions.get(&0).unwrap()));
                            return context.input_focus_self(messenger);
                        } else {
                            return Vec::new();
                        }
                    } else {
                        let focus = context.get_attribute("pointer_start") != Value::Void;
                        context.set_attribute("pointer_start", Value::Void);
                        if focus {
                            return context.input_focus_self(messenger);
                        }
                    }
                } else if context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap()) {
                    let padding = match_option!(context.derive_attribute("tabs_padding"), Value::Float2).unwrap().unwrap();
                    let mut half_extent = context.get_half_extent(false).unwrap();
                    half_extent[0] -= padding[0];
                    half_extent[1] -= padding[1];
                    let mut weights: Vec<f32> = (0..tab_count)
                        .map(|tab_index| {
                            context
                                .inspect_child(&NodeOrObservableIdentifier::NamedAndIndexed("handle", tab_index), |node: &Node| {
                                    match_option!(node.get_attribute("prev_weight"), Value::Float1).unwrap().unwrap()
                                })
                                .unwrap()
                        })
                        .collect();
                    let open_tab_count = weights
                        .iter()
                        .fold(0usize, |counter, weight| if *weight > 0.0 { counter + 1 } else { counter });
                    half_extent[major_axis] -= margin * 0.5 * open_tab_count.saturating_sub(1) as f32;
                    let pointer_start: ppga2d::Point = match_option!(context.get_attribute("pointer_start"), Value::Float3).unwrap().into();
                    let splitter_index = match_option!(context.get_attribute("splitter_index"), Value::Natural1).unwrap();
                    let both_were_open = weights[splitter_index] > 0.0 && weights[splitter_index + 1] > 0.0;
                    let prev_weight_sum = weights[splitter_index] + weights[splitter_index + 1];
                    let weight_to_redistribute = margin * 0.5 / half_extent[major_axis] * (1.0 - prev_weight_sum);
                    let lower_limit = if both_were_open { 0.0 } else { weight_to_redistribute };
                    let diff = (absolute_position - pointer_start)[1 + major_axis] / half_extent[major_axis] * 0.5;
                    // max-min-clamping won't work here because of floating point imprecision
                    let hit_lower_limit = diff <= lower_limit - weights[splitter_index];
                    let hit_upper_limit = diff >= weights[splitter_index + 1];
                    let both_are_open = if hit_lower_limit {
                        weights[splitter_index] = 0.0;
                        weights[splitter_index + 1] = prev_weight_sum;
                        false
                    } else if hit_upper_limit {
                        weights[splitter_index] = prev_weight_sum;
                        weights[splitter_index + 1] = 0.0;
                        false
                    } else {
                        weights[splitter_index] += diff;
                        weights[splitter_index + 1] -= diff;
                        true
                    };
                    if both_are_open != both_were_open {
                        if both_are_open {
                            if hit_lower_limit {
                                weights[splitter_index + 1] += weight_to_redistribute;
                            } else {
                                weights[splitter_index] -= weight_to_redistribute;
                            }
                        } else {
                            let redistribution_index = (weights[splitter_index + 1] > 0.0) as usize;
                            weights[splitter_index + redistribution_index] += weight_to_redistribute;
                        }
                    }
                    let post_weight_sum = weights[splitter_index] + weights[splitter_index + 1];
                    let rescale_factor = (1.0 - post_weight_sum) / (1.0 - prev_weight_sum);
                    for (tab_index, weight) in weights.iter_mut().enumerate() {
                        context.configure_child(
                            NodeOrObservableIdentifier::NamedAndIndexed("handle", tab_index),
                            Some(|node: &mut Node| {
                                if (tab_index < splitter_index || tab_index > splitter_index + 1) && prev_weight_sum < 1.0 {
                                    *weight *= rescale_factor;
                                }
                                node.set_attribute("weight", Value::Float1((*weight).into()));
                            }),
                        );
                    }
                }
            }
            Vec::new()
        }
        "ButtonInput" => {
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            let changed_key = *match_option!(messenger.get_attribute("changed_key"), Value::Character).unwrap();
            if !input_state.pressed_keys.contains(&changed_key) {
                return Vec::new();
            }
            let tab_count = get_tab_count(context);
            let mut focus_child_id = None;
            match changed_key {
                '⇥' => {
                    if messenger.get_attribute("origin") != &Value::Void {
                        return context.input_focus_self(messenger);
                    } else if input_state.pressed_keys.contains(&'⇧') {
                        return vec![context.input_focus_parent_or_child(messenger, None)];
                    } else {
                        focus_child_id = Some(NodeOrObservableIdentifier::NamedAndIndexed("handle", tab_count / 2));
                    }
                }
                '←' | '→' | '↑' | '↓' => {
                    if let Value::NodeOrObservableIdentifier(child_id) = messenger.get_attribute("origin") {
                        let direction = if context.get_attribute("orientation") == Value::Orientation(Orientation::Horizontal) {
                            match changed_key {
                                '←' => 0,
                                '→' => 1,
                                '↑' => 2,
                                '↓' => 3,
                                _ => unreachable!(),
                            }
                        } else {
                            match changed_key {
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
                        return vec![context.redirect_input_focus_navigation_to_parent(messenger)];
                    }
                }
                _ => {}
            }
            if focus_child_id.is_some() {
                vec![context.input_focus_parent_or_child(messenger, focus_child_id)]
            } else {
                Vec::new()
            }
        }
        "Defocus" => context.input_defocus_self(messenger),
        _ => vec![messenger.clone()],
    }
}
