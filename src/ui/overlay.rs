//! Overlays
use crate::{
    hash_map, match_option,
    path::{Cap, CurveApproximation, DynamicStrokeOptions, Join, LineSegment, Path, StrokeOptions},
    ui::{
        message::{self, input_focus_parent_or_child, Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Rendering, Side,
    },
    utils::translate2d,
};
use geometric_algebra::{ppga2d, One};

/// Speech balloon overlay
pub fn speech_balloon(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent(false).unwrap();
                let corner_radius = match_option!(context.derive_attribute("speech_balloon_corner_radius"), Value::Float1)
                    .unwrap()
                    .unwrap()
                    .min(half_extent[0].min(half_extent[1]));
                let arrow_extent = match_option!(context.derive_attribute("speech_balloon_arrow_extent"), Value::Float1)
                    .unwrap()
                    .unwrap();
                let arrow_origin = match_option!(context.derive_attribute("speech_balloon_arrow_origin"), Value::Float1)
                    .unwrap()
                    .unwrap();
                let arrow_extent_abs = arrow_extent.abs();
                let arrow_side = match_option!(context.derive_attribute("speech_balloon_arrow_side"), Value::Side).unwrap();
                let vertices = [
                    (
                        Side::Bottom,
                        "speech_balloon_round_bottom_left",
                        [
                            [
                                (arrow_origin + arrow_extent_abs)
                                    .max(-half_extent[0] + corner_radius + 2.0 * arrow_extent_abs)
                                    .min(half_extent[0] - corner_radius),
                                -half_extent[1],
                            ],
                            [arrow_origin, -half_extent[1] - arrow_extent],
                            [
                                (arrow_origin - arrow_extent_abs)
                                    .max(-half_extent[0] + corner_radius)
                                    .min(half_extent[0] - corner_radius - 2.0 * arrow_extent_abs),
                                -half_extent[1],
                            ],
                        ],
                        [
                            [-half_extent[0] + corner_radius, -half_extent[1]],
                            [-half_extent[0], -half_extent[1]],
                            [-half_extent[0], -half_extent[1] + corner_radius],
                        ],
                    ),
                    (
                        Side::Left,
                        "speech_balloon_round_top_left",
                        [
                            [
                                -half_extent[0],
                                (arrow_origin - arrow_extent_abs)
                                    .max(-half_extent[1] + corner_radius)
                                    .min(half_extent[1] - corner_radius - 2.0 * arrow_extent_abs),
                            ],
                            [-half_extent[0] - arrow_extent, arrow_origin],
                            [
                                -half_extent[0],
                                (arrow_origin + arrow_extent_abs)
                                    .max(-half_extent[1] + corner_radius + 2.0 * arrow_extent_abs)
                                    .min(half_extent[1] - corner_radius),
                            ],
                        ],
                        [
                            [-half_extent[0], half_extent[1] - corner_radius],
                            [-half_extent[0], half_extent[1]],
                            [-half_extent[0] + corner_radius, half_extent[1]],
                        ],
                    ),
                    (
                        Side::Top,
                        "speech_balloon_round_top_right",
                        [
                            [
                                (arrow_origin - arrow_extent_abs)
                                    .max(-half_extent[0] + corner_radius)
                                    .min(half_extent[0] - corner_radius - 2.0 * arrow_extent_abs),
                                half_extent[1],
                            ],
                            [arrow_origin, half_extent[1] + arrow_extent],
                            [
                                (arrow_origin + arrow_extent_abs)
                                    .max(-half_extent[0] + corner_radius + 2.0 * arrow_extent_abs)
                                    .min(half_extent[0] - corner_radius),
                                half_extent[1],
                            ],
                        ],
                        [
                            [half_extent[0] - corner_radius, half_extent[1]],
                            [half_extent[0], half_extent[1]],
                            [half_extent[0], half_extent[1] - corner_radius],
                        ],
                    ),
                    (
                        Side::Right,
                        "speech_balloon_round_bottom_right",
                        [
                            [
                                half_extent[0],
                                (arrow_origin + arrow_extent_abs)
                                    .max(-half_extent[1] + corner_radius + 2.0 * arrow_extent_abs)
                                    .min(half_extent[1] - corner_radius),
                            ],
                            [half_extent[0] + arrow_extent, arrow_origin],
                            [
                                half_extent[0],
                                (arrow_origin - arrow_extent_abs)
                                    .max(-half_extent[1] + corner_radius)
                                    .min(half_extent[1] - corner_radius - 2.0 * arrow_extent_abs),
                            ],
                        ],
                        [
                            [half_extent[0], -half_extent[1] + corner_radius],
                            [half_extent[0], -half_extent[1]],
                            [half_extent[0] - corner_radius, -half_extent[1]],
                        ],
                    ),
                ];
                let mut fill_path = Path {
                    start: vertices[3].3[2].into(),
                    ..Path::default()
                };
                for (side, corner_name, arrow, corner) in vertices.iter() {
                    if arrow_side == *side {
                        for arrow_vertex in arrow.iter() {
                            fill_path.push_line(LineSegment {
                                control_points: [arrow_vertex.into()],
                            });
                        }
                    }
                    if match_option!(context.derive_attribute(corner_name), Value::Boolean).unwrap() {
                        fill_path.push_line(LineSegment {
                            control_points: [corner[0].into()],
                        });
                        fill_path.push_quarter_ellipse(corner[1], corner[2]);
                    } else {
                        fill_path.push_line(LineSegment {
                            control_points: [corner[1].into()],
                        });
                        if corner_name == &"speech_balloon_round_bottom_right" {
                            fill_path.start = vertices[3].3[1].into();
                        }
                    }
                }
                let stroke_width = match_option!(context.derive_attribute("speech_balloon_stroke_width"), Value::Float1).unwrap();
                let fill_color = match_option!(context.derive_attribute("speech_balloon_fill_color"), Value::Float4).unwrap();
                let stroke_color = match_option!(context.derive_attribute("speech_balloon_stroke_color"), Value::Float4).unwrap();
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
            let mut unaffected = !context.was_attribute_touched(&["child_count", "content_node", "track_node", "track_half_extent", "track_motor"]);
            context.iter_children(|_local_child_id: &NodeOrObservableIdentifier, node: &Node| {
                if node.was_attribute_touched(&["proposed_half_extent"]) {
                    unaffected = false;
                }
            });
            if let Value::Node(content_node) = context.get_attribute("content_node") {
                context.add_child(NodeOrObservableIdentifier::Named("content"), content_node);
                context.set_attribute("content_node", Value::Void);
                unaffected = false;
            }
            if unaffected {
                return Vec::new();
            }
            if let Some(content_half_extent) =
                context.inspect_child(&NodeOrObservableIdentifier::Named("content"), |content| content.get_half_extent(true))
            {
                let mut half_extent = content_half_extent.unwrap();
                let padding = match_option!(context.derive_attribute("speech_balloon_stroke_width"), Value::Float1)
                    .unwrap()
                    .unwrap()
                    * 0.5;
                half_extent[0] += padding;
                half_extent[1] += padding;
                context.set_half_extent(half_extent.into());
            }
            let half_extent = context.get_half_extent(false).unwrap();
            let arrow_side = match_option!(context.derive_attribute("speech_balloon_arrow_side"), Value::Side).unwrap();
            let track_motor: ppga2d::Motor = if let Value::Natural1(global_node_id) = context.get_attribute("track_node") {
                context.configure_observe(NodeOrObservableIdentifier::NodeAttribute(global_node_id, "half_extent"), true, false);
                context.configure_observe(NodeOrObservableIdentifier::NodeAttribute(global_node_id, "absolute_motor"), true, false);
                context.configure_observe(NodeOrObservableIdentifier::NodeAttribute(global_node_id, "dormant"), true, false);
                context.configure_observe(NodeOrObservableIdentifier::NodeAttribute(global_node_id, "parents"), true, false);
                let track_half_extent = match_option!(context.get_attribute("track_half_extent"), Value::Float2)
                    .map(|value| value.into())
                    .unwrap_or([0.0, 0.0]);
                if let Value::Float1(track_alignment) = context.get_attribute("track_alignment") {
                    let track_alignment = track_alignment.unwrap()
                        * match arrow_side {
                            Side::Bottom | Side::Top => half_extent[0] - track_half_extent[0],
                            Side::Left | Side::Right => half_extent[1] - track_half_extent[1],
                            _ => panic!(),
                        };
                    context.set_attribute("speech_balloon_arrow_origin", Value::Float1(track_alignment.into()));
                }
                let mut track_motor = match_option!(context.get_attribute("track_motor"), Value::Float4)
                    .map(|value| value.into())
                    .unwrap_or_else(ppga2d::Motor::one);
                track_motor.g0[0] = 1.0;
                track_motor.g0[1] = 0.0;
                track_motor
            } else {
                ppga2d::Motor::one()
            };
            let arrow_extent = match_option!(context.derive_attribute("speech_balloon_arrow_extent"), Value::Float1)
                .unwrap()
                .unwrap();
            let arrow_origin = match_option!(context.derive_attribute("speech_balloon_arrow_origin"), Value::Float1)
                .unwrap()
                .unwrap();
            let translation = match arrow_side {
                Side::Bottom => [-arrow_origin, half_extent[1] + arrow_extent],
                Side::Left => [half_extent[0] + arrow_extent, -arrow_origin],
                Side::Top => [-arrow_origin, -half_extent[1] - arrow_extent],
                Side::Right => [-half_extent[0] - arrow_extent, -arrow_origin],
                _ => panic!(),
            };
            context.set_attribute("motor", Value::Float4((track_motor * translate2d(translation)).into()));
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            Vec::new()
        }
        "PropertyChanged" => {
            match match_option!(messenger.get_attribute("attribute"), Value::Attribute).unwrap() {
                &"half_extent" => {
                    context.set_attribute("track_half_extent", messenger.get_attribute("value").clone());
                }
                &"absolute_motor" => {
                    context.set_attribute("track_motor", messenger.get_attribute("value").clone());
                }
                &"dormant" | &"parents" => {
                    return vec![Messenger::new(
                        &message::CLOSE_OVERLAY,
                        hash_map! {
                            "overlay_index" => context.get_attribute("overlay_index"),
                        },
                    )];
                }
                _ => unreachable!(),
            }
            Vec::new()
        }
        "PointerInput" => {
            if messenger.propagation_direction != PropagationDirection::Parent(-1) {
                return vec![messenger.clone()];
            }
            if messenger.get_attribute("changed_pointer") == &Value::InputChannel(0) {
                if let Value::Boolean(_pressed) = messenger.get_attribute("pressed_or_released") {
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
                        Vec::new()
                    } else if input_state.pressed_keycodes.contains(&'⇧') {
                        vec![
                            Messenger::new(
                                &message::CLOSE_OVERLAY,
                                hash_map! {
                                    "overlay_index" => context.get_attribute("overlay_index"),
                                },
                            ),
                            input_focus_parent_or_child(messenger, None),
                        ]
                    } else {
                        let focus_child_id = Some(NodeOrObservableIdentifier::Named("content"));
                        vec![input_focus_parent_or_child(messenger, focus_child_id)]
                    }
                }
                _ => Vec::new(),
            }
        }
        _ => Vec::new(),
    }
}

/// Overlay container
pub fn overlay_container(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "Reconfigure" => {
            let mut unaffected = !context.was_attribute_touched(&["child_count"]);
            context.iter_children(|local_child_id: &NodeOrObservableIdentifier, node: &Node| {
                if local_child_id == &NodeOrObservableIdentifier::Named("content") && node.was_attribute_touched(&["proposed_half_extent"]) {
                    unaffected = false;
                }
            });
            if unaffected {
                return Vec::new();
            }
            if let Some(content_half_extent) =
                context.inspect_child(&NodeOrObservableIdentifier::Named("content"), |content| content.get_half_extent(true))
            {
                context.set_half_extent(content_half_extent);
                for overlay_index in 0..context.get_number_of_children() - 1 {
                    context.configure_child(
                        NodeOrObservableIdentifier::NamedAndIndexed("overlay", overlay_index),
                        Some(|node: &mut Node| {
                            node.set_attribute("layer_index", Value::Natural1(1));
                        }),
                    );
                }
            }
            Vec::new()
        }
        "OpenOverlay" => {
            let overlay_index = context.get_number_of_children() - 1;
            let overlay_node = match_option!(messenger.get_attribute("overlay_node"), Value::Node).unwrap().clone();
            let mut borrowed_node = overlay_node.borrow_mut();
            borrowed_node.set_attribute("track_node", Value::Natural1(messenger.get_source_node_id()));
            borrowed_node.set_attribute("overlay_index", Value::Natural1(overlay_index));
            drop(borrowed_node);
            let child_id = NodeOrObservableIdentifier::NamedAndIndexed("overlay", overlay_index);
            context.add_child(child_id, overlay_node);
            vec![input_focus_parent_or_child(messenger, Some(child_id))]
        }
        "CloseOverlay" => {
            let overlay_index = *match_option!(messenger.get_attribute("overlay_index"), Value::Natural1).unwrap();
            for overlay_index in overlay_index..context.get_number_of_children() {
                context.remove_child(NodeOrObservableIdentifier::NamedAndIndexed("overlay", overlay_index), true);
            }
            Vec::new()
        }
        "PointerInput" => {
            vec![messenger.clone()]
        }
        _ => Vec::new(),
    }
}
