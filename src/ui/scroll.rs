//! Scroll
use crate::{
    match_option,
    path::{Cap, CurveApproximation, DynamicStrokeOptions, Join, Path, StrokeOptions},
    ui::{
        message::{Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Orientation, Rendering, ScrollBarType,
    },
    utils::translate2d,
};
use geometric_algebra::{ppga2d, One};

fn scroll_bar(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent(false).unwrap();
                let corner_radius = match_option!(context.derive_attribute("scroll_bar_corner_radius"), Value::Float1)
                    .unwrap()
                    .unwrap()
                    .min(half_extent[0].min(half_extent[1]));
                let fill_path = Path::from_rounded_rect([0.0, 0.0], half_extent, corner_radius);
                let stroke_width = match_option!(context.derive_attribute("scroll_bar_stroke_width"), Value::Float1).unwrap();
                let fill_color_attribute = if matches!(context.derive_attribute("pointer_start"), Value::Float3(_)) {
                    "scroll_bar_active_fill_color"
                } else {
                    "scroll_bar_fill_color"
                };
                let fill_color = match_option!(context.derive_attribute(fill_color_attribute), Value::Float4).unwrap();
                let stroke_color = match_option!(context.derive_attribute("scroll_bar_stroke_color"), Value::Float4).unwrap();
                let mut stroke_path = fill_path.clone();
                stroke_path.stroke_options = Some(StrokeOptions {
                    width: stroke_width,
                    offset: 0.0.into(),
                    miter_clip: 1.0.into(),
                    closed: true,
                    dynamic_stroke_options_group: 0,
                    curve_approximation: CurveApproximation::UniformTangentAngle(0.1.into()),
                });
                rendering.colored_paths.push((fill_color, vec![fill_path]));
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
            if !context.was_attribute_touched(&["half_extent"]) {
                return Vec::new();
            }
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
                    if *pressed {
                        context.set_attribute("previous_content_motor", context.get_attribute("content_motor"));
                        context.set_attribute("pointer_start", Value::Float3(*input_state.absolute_positions.get(&0).unwrap()));
                    } else {
                        context.set_attribute("pointer_start", Value::Void);
                    }
                    let mut messengers = context.input_focus_self(messenger);
                    messengers.pop();
                    return messengers;
                } else if context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap()) {
                    let absolute_position: ppga2d::Point = (*input_state.absolute_positions.get(&0).unwrap()).into();
                    let pointer_start: ppga2d::Point = match_option!(context.get_attribute("pointer_start"), Value::Float3).unwrap().into();
                    let orientation = match_option!(context.get_attribute("orientation"), Value::Orientation).unwrap();
                    let movement_scale = match_option!(context.get_attribute("movement_scale"), Value::Float1).unwrap().unwrap();
                    let mut content_motor: ppga2d::Motor = match_option!(context.get_attribute("previous_content_motor"), Value::Motor)
                        .unwrap()
                        .into();
                    content_motor[3 - orientation as usize] += if orientation == Orientation::Horizontal { -0.5 } else { 0.5 }
                        * (absolute_position - pointer_start)[1 + orientation as usize]
                        * movement_scale;
                    context.set_attribute("content_motor", Value::Motor(content_motor.into()));
                }
            }
            Vec::new()
        }
        "AxisInput" => {
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            let mut content_motor = match_option!(context.get_attribute("content_motor"), Value::Motor)
                .map(|value| value.into())
                .unwrap_or_else(ppga2d::Motor::one);
            let movement_scale = match_option!(context.get_attribute("movement_scale"), Value::Float1).unwrap().unwrap();
            content_motor = translate2d([
                input_state.axes.get(&0).unwrap().unwrap() * movement_scale,
                input_state.axes.get(&1).unwrap().unwrap() * movement_scale,
            ]) * content_motor;
            context.set_attribute("content_motor", Value::Motor(content_motor.into()));
            Vec::new()
        }
        "Defocus" => context.input_defocus_self(messenger),
        _ => vec![messenger.clone()],
    }
}

/// Scroll
pub fn scroll(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent(false).unwrap();
                let corner_radius = match_option!(context.derive_attribute("scroll_corner_radius"), Value::Float1)
                    .unwrap()
                    .unwrap()
                    .min(half_extent[0].min(half_extent[1]));
                rendering.clip_paths = vec![Path::from_rounded_rect([0.0, 0.0], half_extent, corner_radius)];
                update_rendering.set_attribute("rendering", Value::Rendering(Box::new(rendering)));
            }
            vec![update_rendering]
        }
        "Reconfigure" => {
            let mut unaffected = !context.was_attribute_touched(&["child_count", "half_extent", "content_motor", "content_scale"]);
            if let Value::Node(content_node) = context.get_attribute("content") {
                context.add_child(NodeOrObservableIdentifier::Named("content"), content_node, true);
                context.set_attribute("content", Value::Void);
                unaffected = false;
            }
            let mut content_motor = None;
            context.iter_children(|_local_child_id: &NodeOrObservableIdentifier, node: &mut Node| {
                if context.was_attribute_of_child_touched(node, &["proposed_half_width", "proposed_half_height"]) {
                    unaffected = false;
                }
                if context.was_attribute_of_child_touched(node, &["content_motor"]) {
                    unaffected = false;
                    content_motor = Some(node.get_attribute("content_motor"));
                }
            });
            if unaffected {
                return Vec::new();
            }
            if let Some(content_motor) = content_motor {
                context.set_attribute("content_motor", content_motor);
            }
            if let Some(content_half_extent) = context.inspect_child(&NodeOrObservableIdentifier::Named("content"), |content| {
                content.get_half_extent(true).unwrap()
            }) {
                let half_extent = context.get_half_extent(false).unwrap();
                let mut content_motor = match_option!(context.get_attribute("content_motor"), Value::Motor)
                    .map(|value| value.into())
                    .unwrap_or_else(ppga2d::Motor::one);
                let content_scale = match_option!(context.get_attribute("content_scale"), Value::Float1)
                    .map(|value| value.unwrap())
                    .unwrap_or(1.0);
                let scroll_bar_half_width = match_option!(context.derive_attribute("scroll_bar_corner_radius"), Value::Float1)
                    .unwrap()
                    .unwrap();
                let scroll_bar_half_min_length = match_option!(context.derive_attribute("scroll_bar_half_min_length"), Value::Float1)
                    .unwrap()
                    .unwrap();
                for (orientation, sign, name) in [
                    (Orientation::Horizontal, -1.0, "horizontal_bar"),
                    (Orientation::Vertical, 1.0, "vertical_bar"),
                ] {
                    let axis = orientation as usize;
                    let scroll_bar_type = match_option!(context.get_attribute(name), Value::ScrollBarType).unwrap_or(ScrollBarType::Overflow);
                    let content_half_extent = content_half_extent[axis] * content_scale;
                    let max_translation = (content_half_extent - half_extent[axis]).max(0.0);
                    let content_translation = (sign * -2.0 * content_motor[3 - axis]).clamp(-max_translation, max_translation);
                    if scroll_bar_type != ScrollBarType::Infinite {
                        content_motor[3 - axis] = sign * -0.5 * content_translation;
                    }
                    let content_ratio = if content_half_extent == 0.0 { 1.0 } else { 1.0 / content_half_extent };
                    let mut max_half_extent = half_extent[axis] - 4.0 * scroll_bar_half_width;
                    let mut bar_translation = [0.0, 0.0];
                    let mut bar_half_extent = [0.0, 0.0];
                    bar_half_extent[axis] = max_half_extent * (half_extent[axis] * content_ratio).min(1.0);
                    if bar_half_extent[axis] < scroll_bar_half_min_length {
                        max_half_extent -= scroll_bar_half_min_length - bar_half_extent[axis];
                        bar_half_extent[axis] = scroll_bar_half_min_length;
                    }
                    bar_half_extent[1 - axis] = scroll_bar_half_width;
                    bar_translation[axis] = sign * 2.0 * scroll_bar_half_width - max_half_extent * content_translation * content_ratio;
                    bar_translation[1 - axis] = sign * (half_extent[1 - axis] - 3.0 * scroll_bar_half_width);
                    let possible_movement = max_half_extent - bar_half_extent[axis];
                    let movement_scale = if possible_movement <= 0.0 {
                        0.0
                    } else {
                        max_translation / possible_movement
                    };
                    context.configure_child(
                        NodeOrObservableIdentifier::Named(name),
                        if scroll_bar_type == ScrollBarType::Always
                            || (scroll_bar_type == ScrollBarType::Overflow && half_extent[axis] < content_half_extent)
                        {
                            Some(|node: &mut Node| {
                                node.set_messenger_handler(scroll_bar);
                                node.set_attribute("orientation", Value::Orientation(orientation));
                                node.set_attribute("content_motor", Value::Motor(content_motor.into()));
                                node.set_attribute("movement_scale", Value::Float1(movement_scale.into()));
                                node.set_attribute_privately("layer_index", Value::Natural1(1));
                                node.set_attribute("motor", Value::Motor(translate2d(bar_translation).into()));
                                node.set_attribute("half_extent", Value::Float2(bar_half_extent.into()));
                            })
                        } else {
                            None
                        },
                    );
                }
                context.configure_child(
                    NodeOrObservableIdentifier::Named("content"),
                    Some(|node: &mut Node| {
                        node.set_attribute("motor", Value::Motor(content_motor.into()));
                        node.set_attribute("scale", Value::Float1(content_scale.into()));
                    }),
                );
                context.set_attribute("content_motor", Value::Motor(content_motor.into()));
            }
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            Vec::new()
        }
        "ScrollIntoView" => {
            let own_absolute_motor: ppga2d::Motor = match_option!(context.get_attribute("absolute_motor"), Value::Motor).unwrap().into();
            // let own_absolute_scale = match_option!(context.get_attribute("absolute_scale"), Value::Float1).unwrap().unwrap();
            // let own_half_extent = context.get_half_extent(false);
            let target_absolute_motor: ppga2d::Motor = (*match_option!(messenger.get_attribute("absolute_motor"), Value::Motor).unwrap()).into();
            // let target_absolute_scale = match_option!(messenger.get_attribute("absolute_scale"), Value::Float1).unwrap().unwrap();
            // let target_half_extent = match_option!(messenger.get_attribute("half_extent"), Value::Float2).unwrap().unwrap();
            let content_motor: ppga2d::Motor = match_option!(context.get_attribute("content_motor"), Value::Motor).unwrap().into();
            let mut relative_motor = own_absolute_motor / target_absolute_motor;
            relative_motor[0] = 1.0;
            relative_motor[1] = 0.0;
            context.set_attribute("content_motor", Value::Motor((content_motor * relative_motor).into()));
            Vec::new()
        }
        "AdoptNode" => {
            let content_node = match_option!(messenger.get_attribute("node"), Value::Node).unwrap().clone();
            context.add_child(NodeOrObservableIdentifier::Named("content"), content_node, true);
            Vec::new()
        }
        "PointerInput" => {
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            if match_option!(context.get_attribute("enable_dragging_scroll"), Value::Boolean).unwrap_or(false)
                && messenger.get_attribute("changed_pointer") == &Value::InputChannel(0)
            {
                if let Value::Boolean(pressed) = messenger.get_attribute("pressed_or_released") {
                    if *pressed {
                        context.set_attribute("previous_content_motor", context.get_attribute("content_motor"));
                        context.set_attribute("pointer_start", Value::Float3(*input_state.absolute_positions.get(&0).unwrap()));
                    } else {
                        context.set_attribute("pointer_start", Value::Void);
                    }
                    return context.input_focus_self(messenger);
                } else if context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap()) {
                    let absolute_position: ppga2d::Point = (*input_state.absolute_positions.get(&0).unwrap()).into();
                    let pointer_start: ppga2d::Point = match_option!(context.get_attribute("pointer_start"), Value::Float3).unwrap().into();
                    let mut content_motor = match_option!(context.get_attribute("content_motor"), Value::Motor)
                        .map(|value| value.into())
                        .unwrap_or_else(ppga2d::Motor::one);
                    let scale = match_option!(context.get_attribute("content_scale"), Value::Float1)
                        .map(|value| 1.0 / value.unwrap())
                        .unwrap_or(1.0);
                    let delta = absolute_position - pointer_start;
                    content_motor = translate2d([delta[1] * scale, delta[2] * scale]) * content_motor;
                    context.set_attribute("content_motor", Value::Motor(content_motor.into()));
                }
                Vec::new()
            } else {
                vec![messenger.clone()]
            }
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
                        Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::Named("content")) => None,
                        Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::Named("parents")) => {
                            Some(NodeOrObservableIdentifier::Named("content"))
                        }
                        _ => panic!(),
                    };
                    vec![context.input_focus_parent_or_child(messenger, focus_child_id)]
                }
                '←' | '→' | '↑' | '↓' => vec![context.redirect_input_focus_navigation_to_parent(messenger)],
                _ => Vec::new(),
            }
        }
        _ => vec![messenger.clone()],
    }
}
