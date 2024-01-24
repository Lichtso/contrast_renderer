//! User Interface Events

use crate::{
    error::Error,
    safe_float::SafeFloat,
    ui::{wrapped_values::Value, Node, NodeOrObservableIdentifier, Orientation, Rendering},
};
use geometric_algebra::{ppga2d, ppga3d, Dual, Inverse, SquaredMagnitude, Transformation, Zero};
use std::{
    cell::RefCell,
    collections::{hash_map::DefaultHasher, HashMap, HashSet},
    hash::{Hash, Hasher},
    rc::Rc,
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

macro_rules! define_messages {
    ($($message_kind:ident $(<$($a:lifetime)? $(,)? $($user_provided_identifier:ident)?>)? { $($name:ident : $type:ty),* $(,)? })*) => {
        $(
        #[derive(Clone)]
        pub struct $message_kind $(<$($a,)? $($user_provided_identifier:)?>)? {
            $(pub $name : $type),*
        }
        )*

        #[derive(Clone)]
        pub enum Messenger {
            // UserDefined(&'static str),
            $($message_kind($message_kind $(<$($a,)? $($user_provided_identifier,)?>)?),)*
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum MessengerKind {
            $($message_kind,)*
        }

        impl Messenger {
            pub fn get_kind(&self) -> MessengerKind {
                match self {
                    $(Self::$message_kind(_) => MessengerKind::$message_kind,)*
                }
            }
        }
    };
}

define_messages!(
    Reconfigure {}
    ConfigurationRequest {
        // TODO: clipping_rect: Option<[SafeFloat<f32, 2>; 2]>,
    }
    ConfigurationResponse {
        half_extent: SafeFloat<f32, 2>,
    }
    ConfigureChild {
        id: NodeOrObservableIdentifier,
        node: Option<Rc<RefCell<Node>>>,
    }
    ChildResized {}
    Observe {
        observes: HashSet<NodeOrObservableIdentifier>,
    }
    PrepareRendering {
        motor: ppga2d::Motor,
        scale: f32,
        opacity: f32,
        motor_in_parent: ppga2d::Motor,
        scale_in_parent: f32,
        opacity_in_parent: f32,
    }
    UpdateRendering {
        model_matrix: [ppga3d::Point; 4],
        rendering: Option<Rendering>,
    }
    Render {}
    RenderAndClip {}
    RenderUnclip {}
    Key {
        device_id: usize,
        pressed_scancodes: HashSet<u32>,
        changed_scancode: u32,
        pressed_keycodes: HashSet<char>,
        changed_keycode: Option<char>,
    }
    Pointer {
        bubbling_up: bool,
        device_id: usize,
        pressed_buttons: HashSet<u32>,
        changed_button: u32,
        absolute_position: ppga2d::Point,
        relative_position: ppga2d::Point,
        relative_position_in_parent: ppga2d::Point,
        delta: ppga2d::Point,
        is_inside_bounds: bool,
    }
    /*IsSelected { selected: bool }
    GetSelection { result: Vec<Rc<RefCell<Node>>> }
    SetSelected { selected: bool }
    InvertSelection { selected: bool }
    SetFocus { focused: bool }
    MoveFocusIntoView {}
    FocusNavigation { direction: FocusNavigationDirection }
    ChangeLayout { direction: ChangeLayoutDirection }
    BecomeToolbarContext {}
    Close {}
    Action {}*/
    InputValueChanged {
        child_id: NodeOrObservableIdentifier,
        new_value: Value,
    }
    ScrollTo {
        content_motor: ppga2d::Motor,
    }
);

pub fn captured_observable_for_messenger(messenger: &Messenger) -> Option<NodeOrObservableIdentifier> {
    match messenger {
        Messenger::Key(message) => Some(NodeOrObservableIdentifier::InputDevice(message.device_id)),
        Messenger::Pointer(message) => Some(NodeOrObservableIdentifier::InputDevice(message.device_id)),
        _ => None,
    }
}

pub fn reflect_messenger(messenger: &mut Messenger) -> bool {
    match messenger {
        Messenger::Pointer(message) => {
            if message.bubbling_up {
                return false;
            }
            message.bubbling_up = true;
            true
        }
        _ => false,
    }
}

pub fn update_messenger_at_node_edge(
    node: &Node,
    messenger: &mut Messenger,
    from_child_to_parent: Option<NodeOrObservableIdentifier>,
) -> (bool, bool) {
    match messenger {
        Messenger::PrepareRendering(message) => {
            message.motor *= node.motor;
            message.scale *= node.scale.unwrap();
            message.opacity *= node.opacity.unwrap(); // TODO: Group Opacity
        }
        Messenger::Pointer(message) => {
            let (motor, scale) = if from_child_to_parent.is_none() {
                (node.motor.inverse(), 1.0 / node.scale.unwrap())
            } else {
                (node.motor, node.scale.unwrap())
            };
            message.relative_position_in_parent = message.relative_position;
            let mut relative_position = message.relative_position_in_parent;
            relative_position.g0[1] *= scale;
            relative_position.g0[2] *= scale;
            relative_position = motor.transformation(relative_position);
            let half_extent = &node.half_extent.unwrap();
            message.is_inside_bounds = relative_position.g0[1].abs() <= half_extent[0] && relative_position.g0[2].abs() <= half_extent[1];
            if message.is_inside_bounds {
                message.relative_position = relative_position;
            }
            return (message.is_inside_bounds, message.is_inside_bounds);
        }
        Messenger::InputValueChanged(message) => {
            if let Some(local_id) = from_child_to_parent {
                message.child_id = local_id;
            }
        }
        _ => {}
    }
    (true, false)
}

pub fn reset_messenger_at_node_edge(messenger: &mut Messenger) {
    match messenger {
        Messenger::PrepareRendering(message) => {
            message.motor = message.motor_in_parent;
            message.scale = message.scale_in_parent;
            message.opacity = message.opacity_in_parent;
        }
        Messenger::Pointer(message) => {
            message.relative_position = message.relative_position_in_parent;
        }
        _ => {}
    }
}

pub fn propagation_direction_of_messenger(messenger: &Messenger) -> PropagationDirection {
    match messenger {
        Messenger::Reconfigure(_) => PropagationDirection::Return,
        Messenger::ConfigurationRequest(_) => PropagationDirection::None,
        Messenger::ConfigurationResponse(_) => PropagationDirection::Return,
        Messenger::ConfigureChild(_) => PropagationDirection::Return,
        Messenger::ChildResized(_) => PropagationDirection::Parent,
        Messenger::Observe(_) => PropagationDirection::Return,
        Messenger::PrepareRendering(_) => PropagationDirection::Children,
        Messenger::UpdateRendering(_) => PropagationDirection::Return,
        Messenger::Render(_) => PropagationDirection::Children,
        Messenger::RenderAndClip(_) => PropagationDirection::Return,
        Messenger::RenderUnclip(_) => PropagationDirection::Return,
        Messenger::Key(_) => PropagationDirection::Children,
        Messenger::Pointer(message) => {
            if message.bubbling_up {
                PropagationDirection::Parent
            } else {
                PropagationDirection::Children
            }
        }
        /*Messenger::IsSelected(_) => PropagationDirection::None,
        Messenger::GetSelection(_) => PropagationDirection::None,
        Messenger::SetSelected(_) => PropagationDirection::Children,
        Messenger::InvertSelection(_) => PropagationDirection::Children,
        Messenger::SetFocus(_) => PropagationDirection::None,
        Messenger::MoveFocusIntoView(_) => PropagationDirection::None,
        Messenger::FocusNavigation(_) => PropagationDirection::Children,
        Messenger::ChangeLayout(_) => PropagationDirection::None,
        Messenger::BecomeToolbarContext(_) => PropagationDirection::None,
        Messenger::Close(_) => PropagationDirection::None,
        Messenger::Action(_) => PropagationDirection::None,*/
        Messenger::InputValueChanged(_) => PropagationDirection::Parent,
        Messenger::ScrollTo(_) => PropagationDirection::Parent,
    }
}

pub fn rendering_default_behavior(_message: &Render) -> Vec<Messenger> {
    vec![
        Messenger::RenderUnclip(RenderUnclip {}),
        Messenger::Render(Render {}),
        Messenger::RenderAndClip(RenderAndClip {}),
    ]
}

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
                let changed_keycode = self.translate_keycode(pressed_modifiers, scancode);
                self.last_scancode = scancode << 8 | pressed_modifiers;
                vec![Messenger::Key(Key {
                    device_id,
                    pressed_scancodes,
                    changed_scancode: scancode,
                    pressed_keycodes,
                    changed_keycode,
                })]
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
                        vec![Messenger::Pointer(Pointer {
                            bubbling_up: false,
                            device_id,
                            pressed_buttons: cached_pointer.pressed.clone(),
                            changed_button: pointer_button(button),
                            absolute_position: cached_pointer.current_position,
                            relative_position: cached_pointer.current_position,
                            relative_position_in_parent: cached_pointer.current_position,
                            delta: cached_pointer.current_position - cached_pointer.previous_position,
                            is_inside_bounds: true,
                        })]
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
                    vec![Messenger::Pointer(Pointer {
                        bubbling_up: false,
                        device_id,
                        pressed_buttons: cached_pointer.pressed.clone(),
                        changed_button: 0,
                        absolute_position: cached_pointer.current_position,
                        relative_position: cached_pointer.current_position,
                        relative_position_in_parent: cached_pointer.current_position,
                        delta,
                        is_inside_bounds: true,
                    })]
                } else {
                    Vec::new()
                }
            }
            winit::event::WindowEvent::MouseWheel { device_id, delta, .. } => {
                let device_id = hash_device_id(device_id);
                self.pointers
                    .get(&device_id)
                    .map(|cached_pointer| {
                        vec![Messenger::Pointer(Pointer {
                            bubbling_up: false,
                            device_id,
                            pressed_buttons: cached_pointer.pressed.clone(),
                            changed_button: 1,
                            absolute_position: cached_pointer.current_position,
                            relative_position: cached_pointer.current_position,
                            relative_position_in_parent: cached_pointer.current_position,
                            delta: match delta {
                                winit::event::MouseScrollDelta::LineDelta(x, y) => ppga2d::Point { g0: [0.0, x, y].into() },
                                winit::event::MouseScrollDelta::PixelDelta(delta) => ppga2d::Point {
                                    g0: [0.0, delta.x as f32, delta.y as f32].into(),
                                },
                            },
                            is_inside_bounds: true,
                        })]
                    })
                    .unwrap_or_else(Vec::new)
            }
            _ => Vec::new(),
        }
    }
}
