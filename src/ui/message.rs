//! User Interface Events

use crate::{
    error::Error,
    hash_map, match_option,
    ui::{wrapped_values::Value, Node, NodeOrObservableIdentifier, Orientation},
};
use geometric_algebra::{ppga2d, Dual, Inverse, SquaredMagnitude, Transformation, Zero};
use std::{
    collections::{hash_map::DefaultHasher, HashMap, HashSet},
    hash::{Hash, Hasher},
};

/// How to route a [Messenger].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PropagationDirection {
    /// No propagation should ever be attemted.
    None,
    /// Sends the [Messenger] to the framework.
    Return,
    /// Propagates to parent.
    Parent,
    /// Propagates to all children of the parent except for the last sender.
    Siblings,
    /// Propagates to children.
    Children,
    /// Propagates to observers.
    Observers(NodeOrObservableIdentifier),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FocusNavigationDirection {
    Left,
    Right,
    Up,
    Down,
    In,
    Out,
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

/// Messenger Trait
#[derive(Clone, Copy)]
pub struct MessengerBehavior {
    /// Label for debugging
    pub label: &'static str,
    /// [PropagationDirection] used when creating a new [Messenger]
    pub default_propagation_direction: PropagationDirection,
    /// Returns an [NodeOrObservableIdentifier] if this [Messenger] could capture one
    pub get_captured_observable: fn(&Messenger) -> Option<NodeOrObservableIdentifier>,
    /// Reflects the propagation direction of this [Messenger] from [PropagationDirection::Children] to [PropagationDirection::Parent]
    pub do_reflect: fn(&mut Messenger) -> bool,
    /// Updates the message when this [Messenger] carries it over an edge from one [Node] to an adjacent one
    pub update_at_node_edge: fn(&mut Messenger, &Node, Option<NodeOrObservableIdentifier>) -> (bool, bool),
    /// Resets the changes made by update_at_node_edge so that they can be performed again for a different child node
    pub reset_at_node_edge: fn(&mut Messenger),
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

    pub fn set_attribute(&mut self, attribute: &'static str, value: Value) {
        self.properties.insert(attribute, value);
    }
}

pub fn rendering_default_behavior(_messager: &Messenger) -> Vec<Messenger> {
    vec![
        Messenger::new(&RENDER_UNCLIP, HashMap::new()),
        Messenger::new(&RENDER, HashMap::new()),
        Messenger::new(&RENDER_AND_CLIP, HashMap::new()),
    ]
}

const GET_CAPTURED_OBSERVABLE: fn(&Messenger) -> Option<NodeOrObservableIdentifier> = |_messenger| None;
const DO_REFLECT: fn(&mut Messenger) -> bool = |_messenger| false;
const UPDATE_AT_NODE_EDGE: fn(&mut Messenger, &Node, Option<NodeOrObservableIdentifier>) -> (bool, bool) =
    |_messenger, _node, _from_child_to_parent| (true, false);
const RESET_AT_NODE_EDGE: fn(&mut Messenger) = |_messenger| {};

/// Triggers the framework to send a new ConfigurationRequest
pub const RECONFIGURE: MessengerBehavior = MessengerBehavior {
    label: "Reconfigure",
    default_propagation_direction: PropagationDirection::Return,
    get_captured_observable: GET_CAPTURED_OBSERVABLE,
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// The node should reevaluate itself and answer with ConfigurationResponse and ConfigureChild
pub const CONFIGURATION_REQUEST: MessengerBehavior = MessengerBehavior {
    label: "ConfigurationRequest",
    default_propagation_direction: PropagationDirection::None,
    get_captured_observable: GET_CAPTURED_OBSERVABLE,
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// Lets the framework know the new half extent of the node
pub const CONFIGURATION_RESPONSE: MessengerBehavior = MessengerBehavior {
    label: "ConfigurationResponse",
    default_propagation_direction: PropagationDirection::Return,
    get_captured_observable: GET_CAPTURED_OBSERVABLE,
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// Triggers the framework to send a new ConfigurationRequest to a child
pub const CONFIGURE_CHILD: MessengerBehavior = MessengerBehavior {
    label: "ConfigureChild",
    default_propagation_direction: PropagationDirection::Return,
    get_captured_observable: GET_CAPTURED_OBSERVABLE,
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// Lets the parent know that a child changed its half extent
pub const CHILD_RESIZED: MessengerBehavior = MessengerBehavior {
    label: "ChildResized",
    default_propagation_direction: PropagationDirection::Parent,
    get_captured_observable: GET_CAPTURED_OBSERVABLE,
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// Lets the framework know the new set of observables to be observed by the node
pub const OBSERVE: MessengerBehavior = MessengerBehavior {
    label: "Observe",
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
    update_at_node_edge: |messenger, node, _from_child_to_parent| {
        let mut motor: ppga2d::Motor = match_option!(*messenger.get_attribute("motor"), Value::Float4).unwrap().into();
        let mut scale: f32 = match_option!(*messenger.get_attribute("scale"), Value::Float1).unwrap().into();
        let mut opacity: f32 = match_option!(*messenger.get_attribute("opacity"), Value::Float1).unwrap().into();
        messenger.properties.insert("motor_in_parent", Value::Float4(motor.into()));
        messenger.properties.insert("scale_in_parent", Value::Float1(scale.into()));
        messenger.properties.insert("opacity_in_parent", Value::Float1(opacity.into()));
        motor *= node.motor;
        scale *= node.scale.unwrap();
        opacity *= node.opacity.unwrap(); // TODO: Group Opacity
        messenger.properties.insert("motor", Value::Float4(motor.into()));
        messenger.properties.insert("scale", Value::Float1(scale.into()));
        messenger.properties.insert("opacity", Value::Float1(opacity.into()));
        (true, false)
    },
    reset_at_node_edge: |messenger| {
        messenger.properties.insert("motor", messenger.get_attribute("motor_in_parent").clone());
        messenger.properties.insert("scale", messenger.get_attribute("scale_in_parent").clone());
        messenger
            .properties
            .insert("opacity", messenger.get_attribute("opacity_in_parent").clone());
    },
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

/// Send for every key press or release on a keyboard
pub const KEY: MessengerBehavior = MessengerBehavior {
    label: "Key",
    default_propagation_direction: PropagationDirection::Children,
    get_captured_observable: |messenger| Some(*match_option!(messenger.get_attribute("device"), Value::NodeOrObservableIdentifier).unwrap()),
    do_reflect: DO_REFLECT,
    update_at_node_edge: UPDATE_AT_NODE_EDGE,
    reset_at_node_edge: RESET_AT_NODE_EDGE,
};

/// Send for pointer move, press, release or scrolling of a mouse, pen, trackpad or touch surface
pub const POINTER: MessengerBehavior = MessengerBehavior {
    label: "Pointer",
    default_propagation_direction: PropagationDirection::Children,
    get_captured_observable: |messenger| Some(*match_option!(messenger.get_attribute("device"), Value::NodeOrObservableIdentifier).unwrap()),
    do_reflect: |messenger| {
        if messenger.propagation_direction == PropagationDirection::Parent {
            return false;
        }
        messenger.propagation_direction = PropagationDirection::Parent;
        true
    },
    update_at_node_edge: |messenger, node, from_child_to_parent| {
        let (motor, scale) = if from_child_to_parent.is_none() {
            (node.motor.inverse(), 1.0 / node.scale.unwrap())
        } else {
            (node.motor, node.scale.unwrap())
        };
        let mut relative_position: ppga2d::Point = match_option!(*messenger.get_attribute("relative_position"), Value::Float3)
            .unwrap()
            .into();
        messenger
            .properties
            .insert("relative_position_in_parent", Value::Float3(relative_position.into()));
        relative_position.g0[1] *= scale;
        relative_position.g0[2] *= scale;
        relative_position = motor.transformation(relative_position);
        let half_extent = &node.half_extent.unwrap();
        let is_inside_bounds = relative_position.g0[1].abs() <= half_extent[0] && relative_position.g0[2].abs() <= half_extent[1];
        messenger.properties.insert("is_inside_bounds", Value::Boolean(is_inside_bounds));
        if is_inside_bounds {
            messenger.properties.insert("relative_position", Value::Float3(relative_position.into()));
        }
        (is_inside_bounds, is_inside_bounds)
    },
    reset_at_node_edge: |messenger| {
        messenger
            .properties
            .insert("relative_position", messenger.get_attribute("relative_position_in_parent").clone());
    },
};

/// Lets the parent know that the user entered a new input or otherwise interacted with a child node
pub const INPUT_VALUE_CHANGED: MessengerBehavior = MessengerBehavior {
    label: "InputValueChanged",
    default_propagation_direction: PropagationDirection::Parent,
    get_captured_observable: GET_CAPTURED_OBSERVABLE,
    do_reflect: DO_REFLECT,
    update_at_node_edge: |messenger, _node, from_child_to_parent| {
        if let Some(local_id) = from_child_to_parent {
            messenger.properties.insert("child_id", Value::NodeOrObservableIdentifier(local_id));
        }
        (true, false)
    },
    reset_at_node_edge: RESET_AT_NODE_EDGE,
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

pub struct KeyboardState {
    pub pressed_scancodes: HashSet<u32>,
}

pub struct PointerState {
    pub pressed: HashSet<u32>,
    pub previous_position: ppga2d::Point,
    pub current_position: ppga2d::Point,
}

#[cfg(feature = "winit")]
pub struct WinitEventTranslator {
    pub viewport_size: ppga2d::Point,
    pub modifiers: Vec<char>,
    pub keymap: HashMap<u32, char>,
    pub keyboards: HashMap<usize, KeyboardState>,
    pub pointers: HashMap<usize, PointerState>,
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
            keyboards: HashMap::default(),
            pointers: HashMap::default(),
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

    fn translate_keycode(&self, pressed_modifiers: u32, scancode: u32) -> Option<char> {
        self.keymap.get(&(scancode << 8 | pressed_modifiers)).cloned()
    }

    /// Translates winit events to [Messenger]s which can be sent to the root nodes of a [NodeHierarchy]
    pub fn translate(&mut self, event: winit::event::WindowEvent) -> Vec<Messenger> {
        fn hash_device_id(device_id: winit::event::DeviceId) -> usize {
            let mut hasher = DefaultHasher::new();
            device_id.hash(&mut hasher);
            hasher.finish() as usize
        }
        fn pointer_button(button: winit::event::MouseButton) -> u32 {
            match button {
                winit::event::MouseButton::Left => 2,
                winit::event::MouseButton::Middle => 3,
                winit::event::MouseButton::Right => 4,
                winit::event::MouseButton::Other(i) => 5 + i as u32,
            }
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
                let device_id = hash_device_id(device_id);
                let cached_keyboard = self.keyboards.entry(device_id).or_insert_with(|| KeyboardState {
                    pressed_scancodes: HashSet::new(),
                });
                if state == winit::event::ElementState::Pressed {
                    cached_keyboard.pressed_scancodes.insert(scancode);
                } else {
                    cached_keyboard.pressed_scancodes.remove(&scancode);
                }
                let pressed_scancodes = cached_keyboard.pressed_scancodes.clone();
                let pressed_keycodes_without_modifiers = pressed_scancodes
                    .iter()
                    .filter_map(|scancode| self.translate_keycode(0, *scancode))
                    .collect::<Vec<_>>();
                let pressed_modifiers = pressed_keycodes_without_modifiers.iter().fold(0, |accumulator, keycode| {
                    (if let Some(index) = self.modifiers.iter().position(|value| value == keycode) {
                        1 << index
                    } else {
                        0
                    } | accumulator)
                });
                let pressed_keycodes = pressed_scancodes
                    .iter()
                    .filter_map(|scancode| self.translate_keycode(pressed_modifiers, *scancode))
                    .collect();
                self.last_scancode = scancode << 8 | pressed_modifiers;
                vec![Messenger::new(
                    &KEY,
                    hash_map! {
                        "device" => Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::InputDevice(device_id)),
                        "pressed_scancodes" => Value::ButtonsOrKeys(pressed_scancodes),
                        "changed_scancode" => Value::ButtonOrKey(scancode),
                        "pressed_keycodes" => Value::Characters(pressed_keycodes),
                        "changed_keycode" => if let Some(changed_keycode) = self.translate_keycode(pressed_modifiers, scancode) {
                            Value::Character(changed_keycode)
                        } else {
                            Value::Void
                        },
                    },
                )]
            }
            winit::event::WindowEvent::MouseInput {
                device_id, state, button, ..
            } if button == winit::event::MouseButton::Left => {
                let device_id = hash_device_id(device_id);
                self.pointers
                    .get_mut(&device_id)
                    .map(|cached_pointer| {
                        if state == winit::event::ElementState::Pressed {
                            cached_pointer.pressed.insert(pointer_button(button));
                        } else {
                            cached_pointer.pressed.remove(&pointer_button(button));
                        }
                        vec![Messenger::new(
                            &POINTER,
                            hash_map! {
                                "device" => Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::InputDevice(device_id)),
                                "pressed_buttons" => Value::ButtonsOrKeys(cached_pointer.pressed.clone()),
                                "changed_button" => Value::ButtonOrKey(pointer_button(button)),
                                "absolute_position" => Value::Float3(cached_pointer.current_position.into()),
                                "relative_position" => Value::Float3(cached_pointer.current_position.into()),
                                "relative_position_in_parent" => Value::Float3(cached_pointer.current_position.into()),
                                "delta" => Value::Float3((cached_pointer.current_position - cached_pointer.previous_position).into()),
                                "is_inside_bounds" => Value::Boolean(true),
                            },
                        )]
                    })
                    .unwrap_or_else(Vec::new)
            }
            winit::event::WindowEvent::CursorMoved { device_id, position, .. } => {
                let device_id = hash_device_id(device_id);
                let cached_pointer = self.pointers.entry(device_id).or_insert_with(|| PointerState {
                    pressed: HashSet::new(),
                    previous_position: ppga2d::Point::zero(),
                    current_position: ppga2d::Point::zero(),
                });
                cached_pointer.previous_position = cached_pointer.current_position;
                cached_pointer.current_position = ppga2d::Point {
                    g0: [
                        1.0,
                        position.x as f32 - self.viewport_size.g0[1] * 0.5,
                        self.viewport_size.g0[2] * 0.5 - position.y as f32,
                    ]
                    .into(),
                };
                let delta = cached_pointer.current_position - cached_pointer.previous_position;
                if delta.dual().squared_magnitude().g0 > 0.0 {
                    vec![Messenger::new(
                        &POINTER,
                        hash_map! {
                            "device" => Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::InputDevice(device_id)),
                            "pressed_buttons" => Value::ButtonsOrKeys(cached_pointer.pressed.clone()),
                            "changed_button" => Value::ButtonOrKey(0),
                            "absolute_position" => Value::Float3(cached_pointer.current_position.into()),
                            "relative_position" => Value::Float3(cached_pointer.current_position.into()),
                            "relative_position_in_parent" => Value::Float3(cached_pointer.current_position.into()),
                            "delta" => Value::Float3(delta.into()),
                            "is_inside_bounds" => Value::Boolean(true),
                        },
                    )]
                } else {
                    Vec::new()
                }
            }
            winit::event::WindowEvent::MouseWheel { device_id, delta, .. } => {
                let device_id = hash_device_id(device_id);
                let delta = match delta {
                    winit::event::MouseScrollDelta::LineDelta(x, y) => ppga2d::Point { g0: [0.0, x, y].into() },
                    winit::event::MouseScrollDelta::PixelDelta(delta) => ppga2d::Point {
                        g0: [0.0, delta.x as f32, delta.y as f32].into(),
                    },
                };
                self.pointers
                    .get(&device_id)
                    .map(|cached_pointer| {
                        vec![Messenger::new(
                            &POINTER,
                            hash_map! {
                                "device" => Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::InputDevice(device_id)),
                                "pressed_buttons" => Value::ButtonsOrKeys(cached_pointer.pressed.clone()),
                                "changed_button" => Value::ButtonOrKey(1),
                                "absolute_position" => Value::Float3(cached_pointer.current_position.into()),
                                "relative_position" => Value::Float3(cached_pointer.current_position.into()),
                                "relative_position_in_parent" => Value::Float3(cached_pointer.current_position.into()),
                                "delta" => Value::Float3(delta.into()),
                                "is_inside_bounds" => Value::Boolean(true),
                            },
                        )]
                    })
                    .unwrap_or_else(Vec::new)
            }
            _ => Vec::new(),
        }
    }
}
