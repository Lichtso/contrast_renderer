//! Drop down selection with overlay
use crate::{
    hash_map, match_option,
    path::{Cap, CurveApproximation, DynamicStrokeOptions, Join, Path, StrokeOptions},
    ui::{
        button::button,
        label::text_label,
        list::list,
        message,
        message::{Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        overlay::speech_balloon,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Orientation, Rendering,
    },
    utils::translate2d,
};

fn toggle_overlay(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    if context
        .inspect_child(&NodeOrObservableIdentifier::Named("overlay"), |_node: &Node| ())
        .is_some()
    {
        let mut to_overlay = Messenger::new(
            &message::CLOSE_OVERLAY,
            hash_map! {
                "input_source" => messenger.get_attribute("input_source").clone(),
                "input_state" => messenger.get_attribute("input_state").clone(),
            },
        );
        to_overlay.propagation_direction = PropagationDirection::Child(NodeOrObservableIdentifier::Named("overlay"));
        return vec![to_overlay];
    }
    let options = match_option!(context.get_attribute("options"), Value::Vec).unwrap();
    let list_entries = options
        .into_iter()
        .enumerate()
        .map(|(option_index, option_label)| {
            let text_label = Value::Node(Node::new(
                text_label,
                hash_map! {
                    "text_content" => option_label,
                },
            ));
            Value::Node(Node::new(
                button,
                hash_map! {
                    "content" => text_label,
                    "enable_interaction" => Value::Boolean(true),
                    "input_field_id" => Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::Indexed(option_index)),
                    "button_corner_radius" => Value::Float1(0.0.into()),
                    "button_padding" => context.derive_attribute("dropdown_overlay_button_padding"),
                },
            ))
        })
        .collect();
    let list_node = Node::new(
        list,
        hash_map! {
            "entries" => Value::Vec(list_entries),
            "orientation" => Value::Orientation(Orientation::Vertical),
            "reverse" => Value::Boolean(true),
            "list_margin" => context.derive_attribute("dropdown_overlay_list_marging"),
            "list_padding" => context.derive_attribute("dropdown_overlay_list_padding"),
            "list_minor_axis_alignment" => Value::Void,
            "proposed_half_extent" => Value::Float2([0.0, 0.0].into()),
        },
    );
    let overlay_node = Node::new(
        speech_balloon,
        hash_map! {
            "content" => Value::Node(list_node),
            "track_half_extent" => context.get_attribute("half_extent"),
            "track_alignment" => Value::Float1((-1.0).into()),
            "track_offset" => context.derive_attribute("dropdown_button_corner_radius"),
            "speech_balloon_round_top_left" => Value::Boolean(false),
            "speech_balloon_arrow_extent" => Value::Float1(0.0.into()),
        },
    );
    context.add_child(NodeOrObservableIdentifier::Named("overlay"), overlay_node.clone(), false);
    let mut to_overlay_container = Messenger::new(
        &message::ADOPT_NODE,
        hash_map! {
            "node" => Value::Node(overlay_node),
            "input_state" => messenger.get_attribute("input_state").clone(),
            "input_source" => messenger.get_attribute("input_source").clone(),
        },
    );
    to_overlay_container.propagation_direction = PropagationDirection::Observers(NodeOrObservableIdentifier::Named("root"));
    vec![
        context.input_focus_parent_or_child(messenger, Some(NodeOrObservableIdentifier::Named("overlay"))),
        to_overlay_container,
    ]
}

/// Drop down selection with overlay
pub fn dropdown(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent(false).unwrap();
                let triangle_extent = match_option!(context.derive_attribute("dropdown_button_triangle_extent"), Value::Float1)
                    .unwrap()
                    .unwrap();
                let padding = match_option!(context.derive_attribute("dropdown_button_padding"), Value::Float2)
                    .unwrap()
                    .unwrap();
                let corner_radius = match_option!(context.derive_attribute("dropdown_button_corner_radius"), Value::Float1)
                    .unwrap()
                    .unwrap()
                    .min(half_extent[0].min(half_extent[1]));
                let fill_color_attribute = if context
                    .inspect_child(&NodeOrObservableIdentifier::Named("overlay"), |_node: &Node| ())
                    .is_some()
                {
                    "dropdown_button_open_color"
                } else {
                    "dropdown_button_closed_color"
                };
                let fill_color = match_option!(context.derive_attribute(fill_color_attribute), Value::Float4).unwrap();
                let fill_path = Path::from_rounded_rect([0.0, 0.0], half_extent, corner_radius);
                rendering.colored_paths.push((fill_color, vec![fill_path]));
                let mut stroke_path = Path::from_polygon(&[
                    [half_extent[0] - padding[0] - 2.0 * triangle_extent, 0.5 * triangle_extent],
                    [half_extent[0] - padding[0] - triangle_extent, -0.5 * triangle_extent],
                    [half_extent[0] - padding[0], 0.5 * triangle_extent],
                ]);
                stroke_path.stroke_options = Some(StrokeOptions {
                    width: (0.2 * triangle_extent).into(),
                    offset: 0.0.into(),
                    miter_clip: (0.1 * triangle_extent).into(),
                    closed: false,
                    dynamic_stroke_options_group: 0,
                    curve_approximation: CurveApproximation::UniformlySpacedParameters(0),
                });
                rendering.colored_paths.push((
                    match_option!(context.derive_attribute("dropdown_button_stroke_color"), Value::Float4).unwrap(),
                    vec![stroke_path],
                ));
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
            let mut unaffected = !context.was_attribute_touched(&["child_count", "options", "option_index", "half_extent"]);
            unaffected &= !context
                .inspect_child(&NodeOrObservableIdentifier::Named("content"), |content| {
                    context.was_attribute_of_child_touched(content, &["proposed_half_extent"])
                })
                .unwrap_or(false);
            if unaffected {
                return Vec::new();
            }
            let options = match_option!(context.get_attribute("options"), Value::Vec).unwrap();
            let option_index = match_option!(context.get_attribute("option_index"), Value::Natural1).unwrap();
            let triangle_extent = match_option!(context.derive_attribute("dropdown_button_triangle_extent"), Value::Float1)
                .unwrap()
                .unwrap();
            let padding = match_option!(context.derive_attribute("dropdown_button_padding"), Value::Float2)
                .unwrap()
                .unwrap();
            let mut half_extent = context.get_half_extent(false).unwrap();
            context.configure_child(
                NodeOrObservableIdentifier::Named("content"),
                Some(|node: &mut Node| {
                    node.set_messenger_handler(text_label);
                    node.set_attribute("text_content", options.get(option_index).unwrap().clone());
                    node.set_attribute("motor", Value::Motor(translate2d([-1.5 * triangle_extent, 0.0]).into()));
                    half_extent = node.get_half_extent(true).unwrap();
                    half_extent[0] += padding[0] + triangle_extent * 1.5;
                    half_extent[1] += padding[1];
                }),
            );
            if context
                .inspect_child(&NodeOrObservableIdentifier::Named("overlay"), |_node: &Node| ())
                .is_some()
            {
                context.configure_child(
                    NodeOrObservableIdentifier::Named("overlay"),
                    Some(|node: &mut Node| {
                        node.set_attribute("track_half_extent", Value::Float2(half_extent.into()));
                    }),
                );
            }
            context.set_attribute("proposed_half_extent", Value::Float2(half_extent.into()));
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            Vec::new()
        }
        "ScrollIntoView" => {
            vec![messenger.clone()]
        }
        "PointerInput" => {
            if !match_option!(context.get_attribute("enable_interaction"), Value::Boolean).unwrap_or(false)
                || messenger.propagation_direction != PropagationDirection::Parent(-1)
            {
                return vec![messenger.clone()];
            }
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            if messenger.get_attribute("changed_pointer") == &Value::InputChannel(0) && *input_state.is_inside_bounds.get(&0).unwrap() {
                if let Value::Boolean(pressed) = messenger.get_attribute("pressed_or_released") {
                    let action = !*pressed
                        && context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap());
                    let mut messengers = context.pointer_and_button_input_focus(messenger);
                    if action {
                        messengers.append(&mut toggle_overlay(context, messenger));
                    }
                    return messengers;
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
                        return context.pointer_and_button_input_focus(messenger);
                    } else if input_state.pressed_keycodes.contains(&'⇧') {
                        return vec![context.input_focus_parent_or_child(messenger, None)];
                    }
                    Vec::new()
                }
                '←' | '→' | '↑' | '↓' => vec![context.redirect_input_focus_navigation_to_parent(messenger)],
                '⏎' if match_option!(context.get_attribute("enable_interaction"), Value::Boolean).unwrap_or(false) => {
                    toggle_overlay(context, messenger)
                }
                _ => Vec::new(),
            }
        }
        "UserInput" => {
            let input_field_id = match_option!(messenger.get_attribute("input_field_id"), Value::NodeOrObservableIdentifier).unwrap();
            let option_index = match_option!(input_field_id, NodeOrObservableIdentifier::Indexed).unwrap();
            context.set_attribute("option_index", Value::Natural1(*option_index));
            let mut result = Vec::new();
            if let Value::NodeOrObservableIdentifier(input_field_id) = context.get_attribute("input_field_id") {
                result.push(Messenger::new(
                    &message::USER_INPUT,
                    hash_map! {
                        "input_state" => messenger.get_attribute("input_state").clone(),
                        "input_source" => messenger.get_attribute("input_source").clone(),
                        "input_field_id" => Value::NodeOrObservableIdentifier(input_field_id),
                        "value" => context.get_attribute("option_index"),
                    },
                ));
            }
            let mut to_overlay = Messenger::new(
                &message::CLOSE_OVERLAY,
                hash_map! {
                    "input_source" => messenger.get_attribute("input_source").clone(),
                    "input_state" => messenger.get_attribute("input_state").clone(),
                },
            );
            to_overlay.propagation_direction = PropagationDirection::Child(NodeOrObservableIdentifier::Named("overlay"));
            result.push(to_overlay);
            result
        }
        _ => Vec::new(),
    }
}
