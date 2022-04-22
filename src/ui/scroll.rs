use crate::{
    hash_map, match_option,
    path::{Cap, CurveApproximation, DynamicStrokeOptions, Join, Path, StrokeOptions},
    ui::{
        message::{self, pointer_and_button_input_focus, rendering_default_behavior, Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Orientation, Rendering, ScrollBarType,
    },
    utils::translate2d,
};
use geometric_algebra::{ppga2d, One};

fn scroll_bar(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.behavior.label {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent(false);
                let fill_path = Path::from_rounded_rect(
                    [0.0, 0.0],
                    half_extent.unwrap(),
                    match_option!(context.derive_attribute("scroll_bar_corner_radius"), Value::Float1)
                        .unwrap()
                        .unwrap(),
                );
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
        "Render" => rendering_default_behavior(messenger),
        "Reconfigure" => {
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            vec![Messenger::new(&message::CONFIGURED, hash_map! {})]
        }
        "PointerInput" => {
            if messenger.propagation_direction != PropagationDirection::Parent {
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
                    return pointer_and_button_input_focus(messenger);
                } else if context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap()) {
                    let absolute_position: ppga2d::Point = (*input_state.absolute_positions.get(&0).unwrap()).into();
                    let pointer_start: ppga2d::Point = match_option!(context.get_attribute("pointer_start"), Value::Float3).unwrap().into();
                    let orientation = match_option!(context.get_attribute("orientation"), Value::Orientation).unwrap();
                    let movement_scale = match_option!(context.get_attribute("movement_scale"), Value::Float1).unwrap().unwrap();
                    let mut content_motor: ppga2d::Motor = match_option!(context.get_attribute("previous_content_motor"), Value::Float4)
                        .unwrap()
                        .into();
                    content_motor.g0[3 - orientation as usize] += if orientation == Orientation::Horizontal { -0.5 } else { 0.5 }
                        * (absolute_position - pointer_start).g0[1 + orientation as usize]
                        * movement_scale;
                    context.set_attribute("content_motor", Value::Float4(content_motor.into()));
                    return vec![];
                }
            }
            Vec::new()
        }
        _ => Vec::new(),
    }
}

pub fn scroll(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.behavior.label {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent(false);
                rendering.clip_paths = vec![Path::from_rounded_rect(
                    [0.0, 0.0],
                    half_extent.unwrap(),
                    match_option!(context.derive_attribute("scroll_corner_radius"), Value::Float1)
                        .unwrap()
                        .unwrap(),
                )];
                update_rendering.set_attribute("rendering", Value::Rendering(Box::new(rendering)));
            }
            vec![messenger.clone(), update_rendering]
        }
        "Render" => rendering_default_behavior(messenger),
        "Reconfigure" => {
            let mut result = vec![Messenger::new(&message::CONFIGURED, hash_map! {})];
            if let Some(content_half_extent) = context.inspect_child(&NodeOrObservableIdentifier::Named("content"), |content| {
                content.get_half_extent(true).unwrap()
            }) {
                let half_extent = context.get_half_extent(false).unwrap();
                let mut content_motor = match_option!(context.get_attribute("content_motor"), Value::Float4)
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
                    let content_translation = (sign * -2.0 * content_motor.g0[3 - axis]).clamp(-max_translation, max_translation);
                    if scroll_bar_type != ScrollBarType::Infinite {
                        content_motor.g0[3 - axis] = sign * -0.5 * content_translation;
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
                        &mut result,
                        NodeOrObservableIdentifier::Named(name),
                        if scroll_bar_type == ScrollBarType::Always
                            || (scroll_bar_type == ScrollBarType::Overflow && half_extent[axis] < content_half_extent)
                        {
                            Some(|node: &mut Node| {
                                node.set_messenger_handler(scroll_bar);
                                node.set_attribute("orientation", Value::Orientation(orientation));
                                node.set_attribute("content_motor", Value::Float4(content_motor.into()));
                                node.set_attribute("movement_scale", Value::Float1(movement_scale.into()));
                                node.set_attribute_privately("layer_index", Value::Natural1(1));
                                node.set_attribute_privately("motor", Value::Float4(translate2d(bar_translation).into()));
                                node.set_attribute("half_extent", Value::Float2(bar_half_extent.into()));
                            })
                        } else {
                            None
                        },
                    );
                }
                context.configure_child(
                    &mut result,
                    NodeOrObservableIdentifier::Named("content"),
                    Some(|node: &mut Node| {
                        node.set_attribute_privately("motor", Value::Float4(content_motor.into()));
                        node.set_attribute("scale", Value::Float1(content_scale.into()));
                    }),
                );
                context.set_attribute("content_motor", Value::Float4(content_motor.into()));
            }
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            result
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
                    return pointer_and_button_input_focus(messenger);
                } else if context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap()) {
                    let absolute_position: ppga2d::Point = (*input_state.absolute_positions.get(&0).unwrap()).into();
                    let pointer_start: ppga2d::Point = match_option!(context.get_attribute("pointer_start"), Value::Float3).unwrap().into();
                    let mut content_motor = match_option!(context.get_attribute("content_motor"), Value::Float4)
                        .map(|value| value.into())
                        .unwrap_or_else(ppga2d::Motor::one);
                    let scale = match_option!(context.get_attribute("content_scale"), Value::Float1)
                        .map(|value| 1.0 / value.unwrap())
                        .unwrap_or(1.0);
                    let delta = absolute_position - pointer_start;
                    content_motor = translate2d([delta.g0[1] * scale, delta.g0[2] * scale]) * content_motor;
                    context.set_attribute("content_motor", Value::Float4(content_motor.into()));
                    return vec![Messenger::new(&message::RECONFIGURE, hash_map! {})];
                }
            }
            vec![messenger.clone()]
        }
        "AxisInput" => {
            if match_option!(context.get_attribute("enable_stationary_scroll"), Value::Boolean).unwrap_or(false)
                && !context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap())
            {
                let mut content_motor = match_option!(context.get_attribute("content_motor"), Value::Float4)
                    .map(|value| value.into())
                    .unwrap_or_else(ppga2d::Motor::one);
                let scale = match_option!(context.get_attribute("content_scale"), Value::Float1)
                    .map(|value| -1.0 / value.unwrap())
                    .unwrap_or(-1.0);
                let delta: ppga2d::Point = match_option!(context.get_attribute("delta"), Value::Float3).unwrap().into();
                content_motor = translate2d([delta.g0[1] * scale, delta.g0[2] * scale]) * content_motor;
                context.set_attribute("content_motor", Value::Float4(content_motor.into()));
                vec![Messenger::new(&message::RECONFIGURE, hash_map! {})]
            } else {
                vec![messenger.clone()]
            }
        }
        "PropertiesChanged" => {
            if match_option!(messenger.get_attribute("attributes"), Value::Attributes)
                .unwrap()
                .contains("proposed_half_extent")
            {
                return vec![Messenger::new(&message::RECONFIGURE, hash_map! {})];
            }
            if match_option!(messenger.get_attribute("attributes"), Value::Attributes)
                .unwrap()
                .contains("content_motor")
            {
                let content_motor = context
                    .inspect_child(
                        match_option!(messenger.get_attribute("child_id"), Value::NodeOrObservableIdentifier).unwrap(),
                        |child_node: &Node| child_node.get_attribute("content_motor").unwrap().clone(),
                    )
                    .unwrap();
                context.set_attribute("content_motor", content_motor);
                return vec![Messenger::new(&message::RECONFIGURE, hash_map! {})];
            }
            Vec::new()
        }
        _ => Vec::new(),
    }
}
