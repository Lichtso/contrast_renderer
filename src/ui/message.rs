//! User Interface Events

use crate::{
    error::Error,
    hash_map, match_option,
    safe_float::SafeFloat,
    ui::{wrapped_values::Value, InputState, Node, NodeOrObservableIdentifier, Orientation},
};
use geometric_algebra::{ppga2d, Dual, Inverse, One, SquaredMagnitude, Transformation, Zero};
use std::{collections::HashMap, hash::Hash};

/// How to route a [Messenger].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PropagationDirection {
    /// No propagation should ever be attemted.
    None,
    /// Sends the [Messenger] to the framework.
    Return,
    /// Propagates to parent.
    Parent,
    /// Propagates to a specific child.
    Child(NodeOrObservableIdentifier),
    /// Propagates to all children of the parent except for the sender.
    Siblings,
    /// Propagates to children.
    Children,
    /// Propagates to observers.
    Observers(NodeOrObservableIdentifier),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChangeLayoutDirection {
    MoveLeft,
    MoveRight,
    MoveUp,
    MoveDown,
    Split(Orientation),
    Merge,
}

pub type GetCapturedObservable = fn(&Messenger) -> Option<NodeOrObservableIdentifier>;
pub type DoReflect = fn(&mut Messenger) -> bool;
pub type UpdateAtNodeEdge = fn(&mut Messenger, SafeFloat<f32, 2>, &Node, Option<NodeOrObservableIdentifier>) -> (bool, bool);
pub type ResetAtNodeEdge = fn(&mut Messenger, bool);

const GET_CAPTURED_OBSERVABLE: GetCapturedObservable = |_messenger| None;
const DO_REFLECT: DoReflect = |_messenger| false;
const UPDATE_AT_NODE_EDGE: UpdateAtNodeEdge = |_messenger, _half_extent, _node, _from_child_to_parent| (true, false);
const RESET_AT_NODE_EDGE: ResetAtNodeEdge = |_messenger, _to_absolute_position| {};

/// Messenger Trait
#[derive(Clone, Copy)]
pub struct MessengerBehavior {
    /// Label for debugging
    pub label: &'static str,
    /// [PropagationDirection] used when creating a new [Messenger]
    pub default_propagation_direction: PropagationDirection,
    /// Returns an [NodeOrObservableIdentifier] if this [Messenger] could capture one
    pub get_captured_observable: GetCapturedObservable,
    /// Reflects the propagation direction of this [Messenger] from [PropagationDirection::Children] to [PropagationDirection::Parent]
    pub do_reflect: DoReflect,
    /// Updates the message when this [Messenger] carries it over an edge from one [Node] to an adjacent one
    pub update_at_node_edge: UpdateAtNodeEdge,
    /// Resets the changes made by update_at_node_edge so that they can be performed again for a different child node
    pub reset_at_node_edge: ResetAtNodeEdge,
}

/// The [Messenger] carries messages between [Node]s
#[derive(Clone)]
pub struct Messenger {
    /// Messenger instance properties
    pub properties: HashMap<&'static str, Value>,
    /// Messenger trait
    pub behavior: &'static MessengerBehavior,
    /// Where this Messenger is heading to
    pub propagation_direction: PropagationDirection,
}

impl std::fmt::Debug for Messenger {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug_struct = f.debug_struct(self.behavior.label);
        debug_struct.field("propagation_direction", &self.propagation_direction);
        for (attribute, value) in self.properties.iter() {
            debug_struct.field(attribute, value);
        }
        debug_struct.finish()
    }
}

impl Messenger {
    pub fn new(behavior: &'static MessengerBehavior, properties: HashMap<&'static str, Value>) -> Self {
        Self {
            properties,
            behavior,
            propagation_direction: behavior.default_propagation_direction,
        }
    }

    pub fn get_attribute(&self, attribute: &'static str) -> &Value {
        self.properties.get(attribute).unwrap_or(&Value::Void)
    }

    pub fn get_attribute_mut(&mut self, attribute: &'static str) -> &mut Value {
        self.properties.get_mut(attribute).unwrap()
    }

    pub fn set_attribute(&mut self, attribute: &'static str, value: Value) {
        self.properties.insert(attribute, value);
    }
}

pub fn rendering_default_behavior(_messenger: &Messenger) -> Vec<Messenger> {
    vec![
        Messenger::new(&RENDER_UNCLIP, HashMap::new()),
        Messenger::new(&RENDER, HashMap::new()),
        Messenger::new(&RENDER_AND_CLIP, HashMap::new()),
    ]
}

pub fn focus_parent_or_child(messenger: &Messenger, child_id: Option<NodeOrObservableIdentifier>) -> Messenger {
    let mut input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap().clone();
    if child_id.is_some() {
        input_state.pressed_keycodes.remove(&'⇧');
    } else {
        input_state.pressed_keycodes.insert('⇧');
    }
    input_state.pressed_keycodes.insert('⇥');
    let mut messenger = Messenger::new(
        &BUTTON_INPUT,
        hash_map! {
            "input_source" => messenger.get_attribute("input_source").clone(),
            "input_state" => Value::InputState(input_state),
            "changed_keycode" => Value::Character('⇥'),
        },
    );
    if let Some(child_id) = child_id {
        messenger.propagation_direction = PropagationDirection::Child(child_id);
    } else {
        messenger.propagation_direction = PropagationDirection::Parent;
    }
    messenger
}

/// The node should reevaluate itself and answer with Configured and ConfigureChild
pub const RECONFIGURE: MessengerBehavior = MessengerBehavior {
    label: "Reconfigure",
    default_propagation_direction: PropagationDirection::Return,
    get_captured_observable: GET_CAPTURED_OBSERVABLE,
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// The node should prepare for the following Render [Messenger], respond with a UpdateRendering [Messenger] and direct this [Messenger] to its children
pub const PREPARE_RENDERING: MessengerBehavior = MessengerBehavior {
    label: "PrepareRendering",
    default_propagation_direction: PropagationDirection::Children,
    get_captured_observable: GET_CAPTURED_OBSERVABLE,
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// Lets the framework know the new transform of the node, optionally also a new clip shape and new colored shapes
pub const UPDATE_RENDERING: MessengerBehavior = MessengerBehavior {
    label: "UpdateRendering",
    default_propagation_direction: PropagationDirection::Return,
    get_captured_observable: GET_CAPTURED_OBSERVABLE,
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// The node should render itself and then direct this [Messenger] to its children
pub const RENDER: MessengerBehavior = MessengerBehavior {
    label: "Render",
    default_propagation_direction: PropagationDirection::Children,
    get_captured_observable: GET_CAPTURED_OBSERVABLE,
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// Have the framework render and clip the node
pub const RENDER_AND_CLIP: MessengerBehavior = MessengerBehavior {
    label: "RenderAndClip",
    default_propagation_direction: PropagationDirection::Return,
    get_captured_observable: GET_CAPTURED_OBSERVABLE,
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// Have the framework undo the clipping of the node
pub const RENDER_UNCLIP: MessengerBehavior = MessengerBehavior {
    label: "RenderUnclip",
    default_propagation_direction: PropagationDirection::Return,
    get_captured_observable: GET_CAPTURED_OBSERVABLE,
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// Send for button and key press or release
pub const BUTTON_INPUT: MessengerBehavior = MessengerBehavior {
    label: "ButtonInput",
    default_propagation_direction: PropagationDirection::None,
    get_captured_observable: |messenger| Some(*match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap()),
    do_reflect: DO_REFLECT,
    update_at_node_edge: |messenger, _half_extent, _child_node, from_child_to_parent| {
        if messenger.propagation_direction != PropagationDirection::None {
            if let Some(local_id) = from_child_to_parent {
                messenger.properties.insert("origin", Value::NodeOrObservableIdentifier(local_id));
            } else {
                messenger
                    .properties
                    .insert("origin", Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::Named("parent")));
            }
        }
        (true, false)
    },
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// Send for scroll wheel or joystick movements
pub const AXIS_INPUT: MessengerBehavior = MessengerBehavior {
    label: "AxisInput",
    default_propagation_direction: PropagationDirection::None,
    get_captured_observable: |messenger| Some(*match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap()),
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// Send for mouse, trackpad, touch or pen movements
pub const POINTER_INPUT: MessengerBehavior = MessengerBehavior {
    label: "PointerInput",
    default_propagation_direction: PropagationDirection::Children,
    get_captured_observable: |messenger| Some(*match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap()),
    do_reflect: |messenger| {
        if messenger.propagation_direction == PropagationDirection::Parent {
            return false;
        }
        messenger.propagation_direction = PropagationDirection::Parent;
        true
    },
    update_at_node_edge: |messenger, half_extent, child_node, from_child_to_parent| {
        let (mut motor, mut scale) = (
            child_node
                .properties
                .get("motor")
                .map(|value| (*match_option!(value, Value::Float4).unwrap()).into())
                .unwrap_or_else(ppga2d::Motor::one),
            child_node
                .properties
                .get("scale")
                .map(|value| match_option!(value, Value::Float1).unwrap().into())
                .unwrap_or(1.0),
        );
        if from_child_to_parent.is_none() {
            motor = motor.inverse();
            scale = 1.0 / scale;
        }
        let changed_pointer = match_option!(*messenger.get_attribute("changed_pointer"), Value::InputChannel).unwrap();
        let input_state = match_option!(messenger.get_attribute_mut("input_state"), Value::InputState).unwrap();
        let relative_position = *input_state.relative_positions.get(&changed_pointer).unwrap();
        input_state.relative_positions_in_parent.insert(changed_pointer, relative_position);
        let mut relative_position: ppga2d::Point = relative_position.into();
        relative_position.g0[1] *= scale;
        relative_position.g0[2] *= scale;
        relative_position = motor.transformation(relative_position);
        let half_extent = half_extent.unwrap();
        let is_inside_bounds = relative_position.g0[1].abs() <= half_extent[0] && relative_position.g0[2].abs() <= half_extent[1];
        input_state.is_inside_bounds.insert(changed_pointer, is_inside_bounds);
        input_state.relative_positions.insert(changed_pointer, relative_position.into());
        (is_inside_bounds, is_inside_bounds)
    },
    reset_at_node_edge: |messenger, to_absolute_position| {
        let changed_pointer = match_option!(*messenger.get_attribute("changed_pointer"), Value::InputChannel).unwrap();
        let input_state = match_option!(messenger.get_attribute_mut("input_state"), Value::InputState).unwrap();
        let relative_position = if to_absolute_position {
            *input_state.absolute_positions.get(&changed_pointer).unwrap()
        } else {
            *input_state.relative_positions_in_parent.get(&changed_pointer).unwrap()
        };
        input_state.relative_positions.insert(changed_pointer, relative_position);
    },
};

/// Sets a scroll nodes content motor
pub const SCROLL_TO: MessengerBehavior = MessengerBehavior {
    label: "ScrollTo",
    default_propagation_direction: PropagationDirection::Parent,
    get_captured_observable: GET_CAPTURED_OBSERVABLE,
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// Send when a property of a node changes
pub const PROPERTY_CHANGED: MessengerBehavior = MessengerBehavior {
    label: "PropertyChanged",
    default_propagation_direction: PropagationDirection::None,
    get_captured_observable: GET_CAPTURED_OBSERVABLE,
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

#[cfg(feature = "winit")]
pub struct WinitEventTranslator {
    pub viewport_size: ppga2d::Point,
    pub modifiers: Vec<char>,
    pub keymap: HashMap<u32, char>,
    pub input_sources: HashMap<usize, InputState>,
    pub mouse_positions: HashMap<winit::event::DeviceId, ppga2d::Point>,
    pub source_by_device: HashMap<winit::event::DeviceId, usize>,
    pub record_keymap: bool,
    last_scancode: u32,
}

#[cfg(feature = "winit")]
impl Default for WinitEventTranslator {
    fn default() -> Self {
        Self {
            viewport_size: ppga2d::Point::zero(),
            modifiers: Vec::new(),
            keymap: HashMap::new(),
            input_sources: hash_map! {
                0 => InputState::default(),
            },
            mouse_positions: HashMap::default(),
            source_by_device: HashMap::default(),
            record_keymap: false,
            last_scancode: 0,
        }
    }
}

#[cfg(feature = "winit")]
impl WinitEventTranslator {
    /// Loads a keymap (usually from a file)
    pub fn load_keymap(&mut self, file_content: &str) -> Result<(), Error> {
        self.keymap = HashMap::new();
        let mut byte_index = 0;
        while byte_index < file_content.len() {
            if !file_content[byte_index..].starts_with("0x") {
                return Err(Error::Syntax(byte_index));
            }
            byte_index += 2;
            let scancode_len = &file_content[byte_index..].find(": ").ok_or(Error::Syntax(byte_index))?;
            let scancode = u32::from_str_radix(&file_content[byte_index..byte_index + scancode_len], 16).map_err(|_| Error::Syntax(byte_index))? << 8;
            byte_index += scancode_len + 2;
            loop {
                let mut length = None;
                let mut primed = false;
                for (index, character) in file_content[byte_index..].char_indices() {
                    match character {
                        ';' => {
                            primed = true;
                        }
                        ' ' | '\n' if primed => {
                            length = Some(index - 1);
                            break;
                        }
                        _ => {
                            primed = false;
                        }
                    }
                }
                let mut modifiers_code = 0;
                let length = length.ok_or(Error::Syntax(byte_index))?;
                let modifiers_and_symbol = file_content[byte_index..byte_index + length].chars();
                byte_index += length + 2;
                let character_count = modifiers_and_symbol.clone().count();
                if character_count > 2 {
                    if modifiers_and_symbol.clone().nth_back(1) != Some(' ') {
                        return Err(Error::Syntax(byte_index));
                    }
                    for modifier in modifiers_and_symbol.clone().take(character_count - 2) {
                        let modifier_index = if let Some(index) = self.modifiers.iter().position(|value| *value == modifier) {
                            index
                        } else {
                            self.modifiers.push(modifier);
                            self.modifiers.len() - 1
                        };
                        modifiers_code |= 1 << modifier_index;
                    }
                } else if character_count == 0 {
                    return Err(Error::Syntax(byte_index));
                }
                self.keymap.insert(scancode | modifiers_code, modifiers_and_symbol.last().unwrap());
                if file_content[byte_index - 1..].starts_with('\n') {
                    break;
                }
            }
        }
        Ok(())
    }

    /// Saves a keymap (usually to a file)
    ///
    /// For debugging use `save_keymap(&mut std::io::stdout().lock()).unwrap();`
    pub fn save_keymap<W: std::io::Write>(&self, output: &mut W) -> std::io::Result<()> {
        for scancode in 0..255 {
            let mut is_used = false;
            for modifier_mask in 0..(1 << self.modifiers.len()) {
                is_used |= self.keymap.contains_key(&(scancode << 8 | modifier_mask));
            }
            if !is_used {
                continue;
            }
            write!(output, "0x{:02X}:", scancode)?;
            for modifier_mask in 0..(1 << self.modifiers.len()) {
                if let Some(character) = self.keymap.get(&(scancode << 8 | modifier_mask)) {
                    if modifier_mask != 0 {
                        output.write_all(b" ")?;
                    }
                    for (modifier_index, modifier) in self.modifiers.iter().enumerate() {
                        if modifier_mask & (1 << modifier_index) != 0 {
                            write!(output, "{}", modifier)?;
                        }
                    }
                    write!(output, " {};", character)?;
                }
            }
            output.write_all(b"\n")?;
        }
        Ok(())
    }

    /// Translates winit events to [Messenger]s which can be sent to the root nodes of a [NodeHierarchy]
    pub fn translate(&mut self, event: winit::event::WindowEvent) -> Vec<Messenger> {
        fn translate_keycode(keymap: &HashMap<u32, char>, pressed_modifiers: u32, scancode: u32) -> Option<char> {
            keymap.get(&(scancode << 8 | pressed_modifiers)).cloned()
        }
        fn pointer_moved(input_state: &mut InputState, input_source: usize, changed_pointer: usize, current_position: ppga2d::Point) -> Messenger {
            input_state.absolute_positions.insert(changed_pointer, current_position.into());
            input_state.relative_positions.insert(changed_pointer, current_position.into());
            Messenger::new(
                &POINTER_INPUT,
                hash_map! {
                    "input_source" => Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::PointerInput(input_source)),
                    "input_state" => Value::InputState(Box::new(input_state.clone())),
                    "changed_pointer" => Value::InputChannel(changed_pointer),
                },
            )
        }
        match event {
            winit::event::WindowEvent::ReceivedCharacter(character) => {
                if self.record_keymap {
                    self.keymap.insert(self.last_scancode, character);
                }
                vec![]
            }
            winit::event::WindowEvent::KeyboardInput {
                device_id,
                input: winit::event::KeyboardInput { scancode, state, .. },
                ..
            } => {
                let input_source = *self.source_by_device.entry(device_id).or_insert(0);
                let input_state = self.input_sources.entry(input_source).or_insert_with(InputState::default);
                let keymap = &self.keymap;
                if state == winit::event::ElementState::Pressed {
                    input_state.pressed_scancodes.insert(scancode as usize);
                } else {
                    input_state.pressed_scancodes.remove(&(scancode as usize));
                }
                let pressed_keycodes_without_modifiers = input_state
                    .pressed_scancodes
                    .iter()
                    .filter_map(|scancode| translate_keycode(keymap, 0, *scancode as u32))
                    .collect::<Vec<_>>();
                let modifiers = self.modifiers.iter();
                let pressed_modifiers = pressed_keycodes_without_modifiers.iter().fold(0, |accumulator, keycode| {
                    (if let Some(index) = modifiers.clone().position(|value| value == keycode) {
                        1 << index
                    } else {
                        0
                    } | accumulator)
                });
                input_state.pressed_keycodes = input_state
                    .pressed_scancodes
                    .iter()
                    .filter_map(|scancode| translate_keycode(keymap, pressed_modifiers, *scancode as u32))
                    .collect();
                self.last_scancode = scancode << 8 | pressed_modifiers;
                vec![Messenger::new(
                    &BUTTON_INPUT,
                    hash_map! {
                        "input_source" => Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::ButtonInput(input_source)),
                        "input_state" => Value::InputState(Box::new(input_state.clone())),
                        "changed_scancode" => Value::InputChannel(scancode as usize),
                        "changed_keycode" => if let Some(changed_keycode) = translate_keycode(keymap, pressed_modifiers, scancode) {
                            Value::Character(changed_keycode)
                        } else {
                            Value::Void
                        },
                    },
                )]
            }
            winit::event::WindowEvent::MouseInput {
                device_id, state, button, ..
            } => {
                let input_source = *self.source_by_device.entry(device_id).or_insert(0);
                let input_state = self.input_sources.entry(input_source).or_insert_with(InputState::default);
                input_state.relative_positions.clear();
                input_state.relative_positions_in_parent.clear();
                input_state.is_inside_bounds.clear();
                let changed_pointer = match button {
                    winit::event::MouseButton::Left => 0,                  // '⇖'
                    winit::event::MouseButton::Middle => 1,                // '⇑'
                    winit::event::MouseButton::Right => 2,                 // '⇗'
                    winit::event::MouseButton::Other(i) => 3 + i as usize, // '⇔'
                };
                let current_position = self.mouse_positions.get(&device_id).unwrap();
                let mut result = vec![pointer_moved(input_state, input_source, changed_pointer, *current_position)];
                result[0].set_attribute("pressed_or_released", Value::Boolean(state == winit::event::ElementState::Pressed));
                if state == winit::event::ElementState::Released {
                    input_state.absolute_positions.remove(&changed_pointer);
                }
                result
            }
            winit::event::WindowEvent::CursorMoved { device_id, position, .. } => {
                let input_source = *self.source_by_device.entry(device_id).or_insert(0);
                let input_state = self.input_sources.entry(input_source).or_insert_with(InputState::default);
                input_state.relative_positions.clear();
                input_state.relative_positions_in_parent.clear();
                input_state.is_inside_bounds.clear();
                let current_position = ppga2d::Point {
                    g0: [
                        1.0,
                        position.x as f32 - self.viewport_size.g0[1] * 0.5,
                        self.viewport_size.g0[2] * 0.5 - position.y as f32,
                    ]
                    .into(),
                };
                let previous_position: ppga2d::Point = self.mouse_positions.get(&device_id).cloned().unwrap_or(current_position);
                self.mouse_positions.insert(device_id, current_position);
                let delta = current_position - previous_position;
                if delta.dual().squared_magnitude().g0 == 0.0 {
                    return Vec::new();
                }
                let mut result = Vec::new();
                let pointers: Vec<usize> = input_state.absolute_positions.keys().cloned().collect();
                for changed_pointer in pointers {
                    result.push(pointer_moved(input_state, input_source, changed_pointer, current_position));
                }
                result
            }
            winit::event::WindowEvent::MouseWheel { device_id, delta, .. } => {
                let input_source = *self.source_by_device.entry(device_id).or_insert(0);
                let input_state = self.input_sources.entry(input_source).or_insert_with(InputState::default);
                let (_delta_x, delta_y) = match delta {
                    winit::event::MouseScrollDelta::LineDelta(x, y) => (x, y),
                    winit::event::MouseScrollDelta::PixelDelta(delta) => (delta.x as f32, delta.y as f32),
                };
                // input_state.axes.insert(0, delta_x.into());
                input_state.axes.insert(1, delta_y.into());
                vec![Messenger::new(
                    &AXIS_INPUT,
                    hash_map! {
                        "input_source" => Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::AxisInput(input_source)),
                        "input_state" => Value::InputState(Box::new(input_state.clone())),
                        "changed_axis" => Value::InputChannel(1),
                    },
                )]
            }
            _ => Vec::new(),
        }
    }
}
