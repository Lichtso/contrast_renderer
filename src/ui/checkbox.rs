use crate::{
    hash_map, hash_set, match_option,
    path::{Cap, CurveApproximation, DynamicStrokeOptions, Join, Path, StrokeOptions},
    ui::{
        message::{self, rendering_default_behavior, Messenger, PropagationDirection},
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
                update_rendering.set_attribute("rendering", Value::Rendering(rendering));
            }
            vec![update_rendering]
        }
        "Render" => rendering_default_behavior(messenger),
        "ConfigurationRequest" => {
            context.set_attribute("is_rendering_dirty", Value::Boolean(true));
            vec![Messenger::new(
                &message::CONFIGURATION_RESPONSE,
                hash_map! {
                    "half_extent" => Value::Float2(context.get_half_extent()),
                },
            )]
        }
        "Pointer" => {
            if !match_option!(context.get_attribute("enable_interaction"), Value::Boolean).unwrap_or(false)
                || messenger.propagation_direction != PropagationDirection::Parent
            {
                return vec![messenger.clone()];
            }
            let pressed_buttons = match_option!(messenger.get_attribute("pressed_buttons"), Value::ButtonsOrKeys).unwrap();
            if messenger.get_attribute("changed_button") == &Value::ButtonOrKey(2) {
                if pressed_buttons.contains(&2) {
                    vec![Messenger::new(
                        &message::OBSERVE,
                        hash_map! {
                            "observes" => Value::NodeOrObservableIdentifiers(hash_set!{
                                *match_option!(messenger.get_attribute("device"), Value::NodeOrObservableIdentifier).unwrap()
                            }),
                        },
                    )]
                } else {
                    let mut result = vec![Messenger::new(
                        &message::OBSERVE,
                        hash_map! {
                            "observes" => Value::NodeOrObservableIdentifiers(hash_set!{}),
                        },
                    )];
                    if messenger.get_attribute("is_inside_bounds") == &Value::Boolean(true)
                        && context.does_observe(match_option!(messenger.get_attribute("device"), Value::NodeOrObservableIdentifier).unwrap())
                    {
                        let is_checked = !match_option!(context.get_attribute("is_checked"), Value::Boolean).unwrap_or(false);
                        context.set_attribute("is_checked", Value::Boolean(is_checked));
                        result.push(Messenger::new(
                            &message::INPUT_VALUE_CHANGED,
                            hash_map! {
                                "child_id" => Value::Void,
                                "new_value" => Value::Boolean(is_checked),
                            },
                        ));
                        result.push(Messenger::new(&message::RECONFIGURE, hash_map! {}));
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
