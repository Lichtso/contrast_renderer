//! Overlays
use crate::{
    hash_map, match_option,
    path::{Cap, CurveApproximation, DynamicStrokeOptions, Join, LineSegment, Path, StrokeOptions},
    ui::{
        message::{self, Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Node, NodeOrObservableIdentifier, Rendering, Side,
    },
    utils::translate2d,
};

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
                    if arrow_side == *side && arrow_extent_abs != 0.0 {
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
            let mut unaffected = !context.was_attribute_touched(&["parents", "child_count", "track_half_extent"]);
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
            let mut half_extent = context.get_half_extent(false).unwrap();
            if let Some(content_half_extent) =
                context.inspect_child(&NodeOrObservableIdentifier::Named("content"), |content| content.get_half_extent(true))
            {
                half_extent = content_half_extent.unwrap();
                let padding = match_option!(context.derive_attribute("speech_balloon_stroke_width"), Value::Float1)
                    .unwrap()
                    .unwrap()
                    * 0.5;
                half_extent[0] += padding;
                half_extent[1] += padding;
                context.set_attribute("proposed_half_extent", Value::Float2(half_extent.into()));
                half_extent[0] -= padding;
                half_extent[1] -= padding;
            }
            let track_half_extent = match_option!(context.derive_attribute("track_half_extent"), Value::Float2)
                .map(|half_extent| half_extent.into())
                .unwrap_or([0.0, 0.0]);
            let arrow_side = match_option!(context.derive_attribute("speech_balloon_arrow_side"), Value::Side).unwrap();
            let mut arrow_extent = match_option!(context.derive_attribute("speech_balloon_arrow_extent"), Value::Float1)
                .unwrap()
                .unwrap();
            if arrow_extent == 0.0 {
                arrow_extent = track_half_extent[(arrow_side as usize) / 2];
            }
            if let Value::Float1(track_alignment) = context.get_attribute("track_alignment") {
                let track_alignment = track_alignment.unwrap()
                    * match arrow_side {
                        Side::Bottom | Side::Top => half_extent[0] - track_half_extent[0],
                        Side::Left | Side::Right => half_extent[1] - track_half_extent[1],
                        _ => panic!(),
                    };
                context.set_attribute("speech_balloon_arrow_origin", Value::Float1(track_alignment.into()));
            }
            let track_offset = match_option!(context.get_attribute("track_offset"), Value::Float1)
                .map(|track_offset| track_offset.into())
                .unwrap_or(0.0);
            let arrow_origin = match_option!(context.derive_attribute("speech_balloon_arrow_origin"), Value::Float1)
                .unwrap()
                .unwrap();
            let translation = match arrow_side {
                Side::Left => [half_extent[0] + arrow_extent - track_offset, -arrow_origin],
                Side::Right => [-half_extent[0] - arrow_extent + track_offset, -arrow_origin],
                Side::Bottom => [-arrow_origin, half_extent[1] + arrow_extent - track_offset],
                Side::Top => [-arrow_origin, -half_extent[1] - arrow_extent + track_offset],
                _ => panic!(),
            };
            context.set_attribute("motor", Value::Motor(translate2d(translation).into()));
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            Vec::new()
        }
        "AdoptNode" => {
            let content_node = match_option!(messenger.get_attribute("node"), Value::Node).unwrap().clone();
            context.add_child(NodeOrObservableIdentifier::Named("content"), content_node, true);
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
                    let focus_child_id = if input_state.pressed_keycodes.contains(&'⇧') {
                        None
                    } else {
                        Some(NodeOrObservableIdentifier::Named("content"))
                    };
                    vec![context.input_focus_parent_or_child(messenger, focus_child_id)]
                }
                _ => Vec::new(),
            }
        }
        "TraceOverlay" => {
            let mut cursor_count = match_option!(context.get_attribute("cursor_count"), Value::Natural1).unwrap() as isize;
            cursor_count += *match_option!(messenger.get_attribute("cursor_increment"), Value::Integer1).unwrap();
            let mut result = Vec::new();
            if cursor_count <= 0 {
                result.push(Messenger::new(
                    &message::CLOSE_OVERLAY,
                    hash_map! {
                        "overlay_id" => context.get_attribute("overlay_id"),
                    },
                ));
            } else {
                context.set_attribute("cursor_count", Value::Natural1(cursor_count as usize));
            }
            result.push(messenger.clone());
            result
        }
        _ => vec![messenger.clone()],
    }
}

/// Navigation cursor overlay
pub fn navigation_cursor(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let radius = match_option!(context.derive_attribute("navigation_cursor_radius"), Value::Float1)
                    .unwrap()
                    .unwrap();
                let arrow_extent = match_option!(context.derive_attribute("navigation_cursor_arrow_extent"), Value::Float1)
                    .unwrap()
                    .unwrap();
                let arrow_side = match_option!(context.derive_attribute("navigation_cursor_arrow_side"), Value::Side).unwrap();
                let angle = (radius / (radius + arrow_extent)).acos();
                let vertices = match arrow_side {
                    Side::Left => {
                        let end = [-radius * angle.cos(), -radius * angle.sin()];
                        [[-(radius + arrow_extent), 0.0].into(), [end[0], -end[1]].into(), end.into()]
                    }
                    Side::Right => {
                        let end = [radius * angle.cos(), radius * angle.sin()];
                        [[radius + arrow_extent, 0.0].into(), [end[0], -end[1]].into(), end.into()]
                    }
                    Side::Bottom => {
                        let end = [radius * angle.sin(), -radius * angle.cos()];
                        [[0.0, -(radius + arrow_extent)].into(), [-end[0], end[1]].into(), end.into()]
                    }
                    Side::Top => {
                        let end = [-radius * angle.sin(), radius * angle.cos()];
                        [[0.0, radius + arrow_extent].into(), [-end[0], end[1]].into(), end.into()]
                    }
                    _ => panic!(),
                };
                let mut fill_path = Path {
                    start: vertices[0],
                    ..Path::default()
                };
                fill_path.push_line(LineSegment {
                    control_points: [vertices[1]],
                });
                fill_path.push_elliptical_arc([radius, radius], 0.0, true, true, vertices[2].unwrap());
                let stroke_width = match_option!(context.derive_attribute("navigation_cursor_stroke_width"), Value::Float1).unwrap();
                let fill_color = match_option!(context.derive_attribute("navigation_cursor_fill_color"), Value::Float4).unwrap();
                let stroke_color = match_option!(context.derive_attribute("navigation_cursor_stroke_color"), Value::Float4).unwrap();
                let mut stroke_path = fill_path.clone();
                stroke_path.stroke_options = Some(StrokeOptions {
                    width: stroke_width,
                    offset: 0.0.into(),
                    miter_clip: 1.0.into(),
                    closed: true,
                    dynamic_stroke_options_group: 0,
                    curve_approximation: CurveApproximation::UniformlySpacedParameters(6),
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
            let mut unaffected = !context.was_attribute_touched(&["parents", "child_count", "track_half_extent", "dormant_parent_count"]);
            if match_option!(context.derive_attribute("dormant_parent_count"), Value::Natural1).unwrap_or(0) > 0 {
                let overlay_id = context.get_attribute("overlay_id");
                let input_source = match overlay_id {
                    Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::NamedAndIndexed("cursor", input_source)) => input_source,
                    _ => panic!(),
                };
                let mut defocus = Messenger::new(
                    &message::DEFOCUS,
                    hash_map! {
                        "input_source" => Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::ButtonInput(input_source)),
                    },
                );
                defocus.propagation_direction = PropagationDirection::Observers(NodeOrObservableIdentifier::ButtonInput(input_source));
                return vec![
                    defocus,
                    Messenger::new(
                        &message::CLOSE_OVERLAY,
                        hash_map! {
                            "overlay_id" => overlay_id,
                        },
                    ),
                ];
            }
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
            let radius = match_option!(context.derive_attribute("navigation_cursor_radius"), Value::Float1)
                .unwrap()
                .unwrap();
            let half_extent = [radius, radius];
            context.set_attribute("proposed_half_extent", Value::Float2(half_extent.into()));
            let arrow_side = match_option!(context.derive_attribute("navigation_cursor_arrow_side"), Value::Side).unwrap();
            let arrow_extent = match_option!(context.derive_attribute("navigation_cursor_arrow_extent"), Value::Float1)
                .unwrap()
                .unwrap();
            let translation = arrow_extent + radius;
            let translation = match arrow_side {
                Side::Left => [translation, 0.0],
                Side::Right => [-translation, 0.0],
                Side::Bottom => [0.0, translation],
                Side::Top => [0.0, -translation],
                _ => panic!(),
            };
            context.set_attribute("motor", Value::Motor(translate2d(translation).into()));
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            Vec::new()
        }
        _ => vec![messenger.clone()],
    }
}

#[derive(PartialEq, Eq)]
struct LocalChildIdToLayerIndex {
    local_child_id: NodeOrObservableIdentifier,
}

impl LocalChildIdToLayerIndex {
    fn category(&self) -> usize {
        match self.local_child_id {
            NodeOrObservableIdentifier::Named("content") => 0,
            NodeOrObservableIdentifier::Indexed2D(_input_source, _overlay_index) => 1,
            NodeOrObservableIdentifier::NamedAndIndexed("cursor", _input_source) => 2,
            _ => panic!(),
        }
    }

    fn x(&self) -> usize {
        match self.local_child_id {
            NodeOrObservableIdentifier::Named("content") => 0,
            NodeOrObservableIdentifier::Indexed2D(x, _y) => x,
            NodeOrObservableIdentifier::NamedAndIndexed("cursor", input_source) => input_source,
            _ => panic!(),
        }
    }

    fn y(&self) -> usize {
        match self.local_child_id {
            NodeOrObservableIdentifier::Named("content") => 0,
            NodeOrObservableIdentifier::Indexed2D(_x, y) => y,
            NodeOrObservableIdentifier::NamedAndIndexed("cursor", _input_source) => 0,
            _ => panic!(),
        }
    }
}

impl Ord for LocalChildIdToLayerIndex {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .category()
            .cmp(&self.category())
            .then_with(|| other.x().cmp(&self.x()))
            .then_with(|| other.y().cmp(&self.y()))
    }
}

impl PartialOrd for LocalChildIdToLayerIndex {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

use std::collections::BinaryHeap;

/// Overlay container
pub fn overlay_container(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.get_kind() {
        "Reconfigure" => {
            let mut unaffected = !context.was_attribute_touched(&["child_count"]);
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
                context.set_attribute("proposed_half_extent", Value::Float2(content_half_extent));
            }
            let mut queue: BinaryHeap<LocalChildIdToLayerIndex> = BinaryHeap::new();
            context.iter_children(|local_child_id: &NodeOrObservableIdentifier, _node: &mut Node| {
                queue.push(LocalChildIdToLayerIndex {
                    local_child_id: *local_child_id,
                });
            });
            let mut layer_index = 0;
            while let Some(entry) = queue.pop() {
                context.configure_child(
                    entry.local_child_id,
                    Some(|node: &mut Node| {
                        node.set_attribute("layer_index", Value::Natural1(layer_index));
                    }),
                );
                layer_index += 1;
            }
            Vec::new()
        }
        "AdoptNode" => {
            let overlay_node = match_option!(messenger.get_attribute("node"), Value::Node);
            let local_child_id = if let Some(new_cursor) = match_option!(messenger.get_attribute("new_cursor"), Value::Boolean) {
                let input_source = *match messenger.get_attribute("input_source") {
                    Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::ButtonInput(input_source)) => input_source,
                    Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::AxisInput(input_source)) => input_source,
                    Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::PointerInput(input_source)) => input_source,
                    _ => panic!(),
                };
                let local_child_id = NodeOrObservableIdentifier::NamedAndIndexed("cursor", input_source);
                if *new_cursor {
                    context.remove_child(local_child_id, true);
                    let mut borrowed_node = overlay_node.as_ref().unwrap().borrow_mut();
                    borrowed_node.set_attribute("overlay_id", Value::NodeOrObservableIdentifier(local_child_id));
                    drop(borrowed_node);
                }
                local_child_id
            } else {
                let mut overlay_index = 0;
                while context
                    .inspect_child(&NodeOrObservableIdentifier::Indexed2D(overlay_index, 0), |_node: &Node| ())
                    .is_some()
                {
                    overlay_index += 1;
                }
                let local_child_id = NodeOrObservableIdentifier::Indexed2D(overlay_index, 0);
                let mut borrowed_node = overlay_node.as_ref().unwrap().borrow_mut();
                borrowed_node.set_attribute("cursor_count", Value::Natural1(0));
                borrowed_node.set_attribute("overlay_id", Value::NodeOrObservableIdentifier(local_child_id));
                drop(borrowed_node);
                local_child_id
            };
            if let Some(overlay_node) = overlay_node {
                context.add_child(local_child_id, overlay_node.clone(), false);
            }
            Vec::new()
        }
        "CloseOverlay" => {
            let local_child_id = *match_option!(messenger.get_attribute("overlay_id"), Value::NodeOrObservableIdentifier).unwrap();
            context.remove_child(local_child_id, true);
            Vec::new()
        }
        _ => vec![messenger.clone()],
    }
}
