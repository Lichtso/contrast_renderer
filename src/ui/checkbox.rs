use crate::{
    hash_set, match_option,
    path::{Cap, CurveApproximation, DynamicStrokeOptions, Join, Path, StrokeOptions},
    ui::{
        message::{self, rendering_default_behavior, Messenger},
        node_hierarchy::NodeMessengerContext,
        wrapped_values::Value,
        NodeOrObservableIdentifier,
    },
};

pub fn checkbox(context: &mut NodeMessengerContext, messenger: &Messenger) -> Vec<Messenger> {
    match messenger {
        Messenger::PrepareRendering(message) => {
            println!("checkbox PrepareRendering");
            let (_prepare_rendering, mut update_rendering) = context.prepare_rendering_helper(message);
            if let Some(rendering) = &mut update_rendering.rendering {
                let half_extent = context.get_half_extent().unwrap();
                let is_checked = match_option!(context.get_attribute("is_checked"), Value::Boolean).unwrap_or(false);
                let fill_color_attribute = if is_checked {
                    "checkbox_checked_color"
                } else {
                    "checkbox_unchecked_color"
                };
                let fill_color = match_option!(context.derive_attribute(fill_color_attribute), Value::Float4).unwrap();
                let fill_path = Path::from_rounded_rect(
                    [0.0, 0.0],
                    half_extent,
                    match_option!(context.derive_attribute("checkbox_corner_radius"), Value::Float1)
                        .unwrap()
                        .unwrap(),
                );
                rendering.colored_paths.push((fill_color, vec![fill_path]));
                if is_checked {
                    let mut stroke_path = Path::from_polygon(&[
                        [-0.5 * half_extent[0], 0.4 * half_extent[1]],
                        [0.0, -0.3 * half_extent[1]],
                        [0.8 * half_extent[0], 1.5 * half_extent[1]],
                    ]);
                    stroke_path.stroke_options = Some(StrokeOptions {
                        width: (0.2 * half_extent[0]).into(),
                        offset: 0.0.into(),
                        miter_clip: (0.1 * half_extent[0]).into(),
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
            }
            vec![Messenger::UpdateRendering(update_rendering)]
        }
        Messenger::Render(message) => rendering_default_behavior(message),
        Messenger::ConfigurationRequest(_message) => {
            println!("checkbox ConfigurationRequest");
            context.set_attribute("is_rendering_dirty", Value::Boolean(true));
            vec![Messenger::ConfigurationResponse(message::ConfigurationResponse {
                half_extent: context.get_half_extent(),
            })]
        }
        Messenger::Pointer(message) => {
            println!("checkbox Pointer");
            if !match_option!(context.get_attribute("enable_interaction"), Value::Boolean).unwrap_or(false) || !message.bubbling_up {
                return vec![Messenger::Pointer(message.clone())];
            }
            if message.changed_button == 2 {
                if message.pressed_buttons.contains(&2) {
                    vec![Messenger::Observe(message::Observe {
                        observes: hash_set! { NodeOrObservableIdentifier::InputDevice(message.device_id) },
                    })]
                } else {
                    let mut result = vec![Messenger::Observe(message::Observe { observes: hash_set! {} })];
                    if message.is_inside_bounds && context.does_observe(&NodeOrObservableIdentifier::InputDevice(message.device_id)) {
                        let is_checked = !match_option!(context.get_attribute("is_checked"), Value::Boolean).unwrap_or(false);
                        context.set_attribute("is_checked", Value::Boolean(is_checked));
                        result.push(Messenger::InputValueChanged(message::InputValueChanged {
                            child_id: NodeOrObservableIdentifier::Named("uninitialized"),
                            new_value: Value::Boolean(is_checked),
                        }));
                        result.push(Messenger::Reconfigure(message::Reconfigure {}));
                    }
                    result
                }
            } else {
                Vec::new()
            }
        }
        _ => Vec::new(),
    }
}
