use crate::{
    match_option,
    path::{Cap, CurveApproximation, DynamicStrokeOptions, Join, Path, StrokeOptions},
    ui::{
        message::{rendering_default_behavior, Messenger, PropagationDirection},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        Rendering,
    },
};

pub fn checkbox(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger.behavior.label {
        "PrepareRendering" => {
            let mut update_rendering = context.update_rendering_helper(messenger);
            if update_rendering.get_attribute("rendering") != &Value::Void {
                let mut rendering = Rendering::default();
                let half_extent = context.get_half_extent(false).unwrap();
                let corner_radius = match_option!(context.derive_attribute("checkbox_corner_radius"), Value::Float1)
                    .unwrap()
                    .unwrap()
                    .min(half_extent[0].min(half_extent[1]));
                let is_checked = match_option!(context.get_attribute("is_checked"), Value::Boolean).unwrap_or(false);
                let fill_color_attribute = if is_checked {
                    "checkbox_checked_color"
                } else {
                    "checkbox_unchecked_color"
                };
                let fill_color = match_option!(context.derive_attribute(fill_color_attribute), Value::Float4).unwrap();
                let fill_path = Path::from_rounded_rect([0.0, 0.0], half_extent, corner_radius);
                rendering.colored_paths.push((fill_color, vec![fill_path]));
                if is_checked {
                    let shorter_edge = half_extent[0].min(half_extent[1]);
                    let mut stroke_path = Path::from_polygon(&[
                        [-0.5 * shorter_edge, 0.4 * shorter_edge],
                        [0.0, -0.3 * shorter_edge],
                        [0.8 * shorter_edge, 1.5 * shorter_edge],
                    ]);
                    stroke_path.stroke_options = Some(StrokeOptions {
                        width: (0.2 * shorter_edge).into(),
                        offset: 0.0.into(),
                        miter_clip: (0.1 * shorter_edge).into(),
                        closed: false,
                        dynamic_stroke_options_group: 0,
                        curve_approximation: CurveApproximation::UniformlySpacedParameters(0),
                    });
                    rendering.colored_paths.push((
                        match_option!(context.derive_attribute("checkbox_checkmark_color"), Value::Float4).unwrap(),
                        vec![stroke_path],
                    ));
                    rendering.dynamic_stroke_options = vec![DynamicStrokeOptions::Solid {
                        join: Join::Miter,
                        start: Cap::Butt,
                        end: Cap::Butt,
                    }];
                }
                update_rendering.set_attribute("rendering", Value::Rendering(Box::new(rendering)));
            }
            vec![update_rendering]
        }
        "Render" => rendering_default_behavior(messenger),
        "Reconfigure" => {
            if !context.was_attribute_touched(&["is_checked"]) {
                return Vec::new();
            }
            context.set_half_extent(match_option!(context.derive_attribute("ckeckbox_half_extent"), Value::Float2).unwrap());
            context.set_attribute_privately("is_rendering_dirty", Value::Boolean(true));
            Vec::new()
        }
        "PointerInput" => {
            if !match_option!(context.get_attribute("enable_interaction"), Value::Boolean).unwrap_or(false)
                || messenger.propagation_direction != PropagationDirection::Parent
            {
                return vec![messenger.clone()];
            }
            let input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap();
            if messenger.get_attribute("changed_pointer") == &Value::InputChannel(0) {
                if let Value::Boolean(pressed) = messenger.get_attribute("pressed_or_released") {
                    if !*pressed
                        && *input_state.is_inside_bounds.get(&0).unwrap()
                        && context.does_observe(match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap())
                    {
                        let is_checked = !match_option!(context.get_attribute("is_checked"), Value::Boolean).unwrap_or(false);
                        context.set_attribute("is_checked", Value::Boolean(is_checked));
                    }
                    context.pointer_and_button_input_focus(messenger);
                }
            }
            Vec::new()
        }
        _ => Vec::new(),
    }
}
