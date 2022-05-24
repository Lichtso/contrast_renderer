use crate::{
    match_option,
    path::{DynamicStrokeOptions, Path},
    renderer::Shape,
    safe_float::SafeFloat,
};
use message::Messenger;
use node_hierarchy::NodeMessengerContext;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
    rc::Rc,
};
use wrapped_values::Value;

pub mod checkbox;
pub mod label;
pub mod list;
pub mod message;
pub mod node_hierarchy;
pub mod range;
pub mod renderer;
pub mod scroll;
pub mod speech_balloon;
pub mod tabs;
pub mod wrapped_values;

pub type GlobalNodeIdentifier = usize;

/// Children are matched by this identifier when the parent reconfigures them.
///
/// Useful for retaining internal state and tracking animations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeOrObservableIdentifier {
    /// Useful for static sets of children
    Named(&'static str),
    /// Useful for dynamic lists
    Indexed(usize),
    /// Useful for dynamic tables
    Indexed2D(usize),
    /// Useful for multiple dynamic lists
    NamedAndIndexed(&'static str, usize),
    /// Captured button input source
    ButtonInput(usize),
    /// Captured axis input source
    AxisInput(usize),
    /// Captured pointer input source
    PointerInput(usize),
    /// Property of a specific node
    NodeAttribute(GlobalNodeIdentifier, &'static str),
}

/// Geometric orientation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Orientation {
    /// X axis
    Horizontal = 0,
    /// Y axis
    Vertical = 1,
    // Z axis
    Lateral = 2,
}

/// Geometric side.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Side {
    // -X
    Left,
    // +X
    Right,
    // -Y
    Bottom,
    // +Y
    Top,
    // -Z
    Back,
    // +Z
    Front,
}

/// Scroll bar behavior per axis.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScrollBarType {
    /// Always show
    Always,
    /// Only show if the content is too big
    Overflow,
    /// Never show but don't limit movement
    Never,
    /// Never show and don't limit movement
    Infinite,
}

/// User interaction with a text label.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TextInteraction {
    /// Read only
    None,
    /// Modify text selection
    Selection,
    /// Modify text selection and content
    Editing,
}

/// Converts user inputs between text and numeric values.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TextualProjection {
    /// From numeric value to text
    pub forward: fn(f32) -> String,
    /// From text to numeric value
    pub backward: fn(String) -> Option<f32>,
}

/// Snap and clamp numeric value in a given range.
#[derive(Clone)]
pub struct SnapClampFunction {
    pub handler: fn(f32, &[f32; 2]) -> f32,
}

impl std::fmt::Debug for SnapClampFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let handler: fn(f32, &'static [f32; 2]) -> f32 = self.handler;
        f.debug_struct("SnapClampFunction").field("handler", &handler).finish()
    }
}

impl PartialEq for SnapClampFunction {
    fn eq(&self, other: &Self) -> bool {
        self.handler as fn(f32, &'static [f32; 2]) -> f32 == other.handler as fn(f32, &'static [f32; 2]) -> f32
    }
}

impl std::hash::Hash for SnapClampFunction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.handler as fn(f32, &'static [f32; 2]) -> f32).hash(state);
    }
}

impl Eq for SnapClampFunction {}

/// Groups multiple input devices of a user into one
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct InputState {
    /// Global identifier
    pub id: usize,
    /// Key, button
    pub pressed_scancodes: HashSet<usize>,
    /// Key, button as unicode character
    pub pressed_keycodes: HashSet<char>,
    /// Scroll wheel, joystick
    pub axes: HashMap<usize, SafeFloat<f32, 1>>,
    /// Mouse, touch, pen absolute positions
    pub absolute_positions: HashMap<usize, SafeFloat<f32, 3>>,
    /// Mouse, touch, pen relative to the current node
    pub relative_positions: HashMap<usize, SafeFloat<f32, 3>>,
    /// Mouse, touch, pen relative to the parent node
    pub relative_positions_in_parent: HashMap<usize, SafeFloat<f32, 3>>,
    /// Mouse, touch, pen relative which are inside the current node
    pub is_inside_bounds: HashMap<usize, bool>,
}

/// Defines the shape, colors and clipping of a [Node].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Rendering {
    /// Clipping geometry
    pub clip_paths: Vec<Path>,
    /// Fill and stroke geometry and colors
    pub colored_paths: Vec<(SafeFloat<f32, 4>, Vec<Path>)>,
    /// Stroke options
    pub dynamic_stroke_options: Vec<DynamicStrokeOptions>,
}

/// Property animation key frame
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnimationFrame {
    /// Timestamp in seconds when this frame will reach its end
    pub timestamp: SafeFloat<f64, 1>,
    /// 1D cubic integral bezier curve to remap the time parameter
    ///
    /// y = c[0] (1-x)^3 + c[1] x (1-x)^2 + c[2] x^2 (1-x) + c[3] x^3
    pub interpolation_control_points: SafeFloat<f32, 4>,
    /// The value that will be reached at the end of this frame
    pub value: Value,
}

impl AnimationFrame {
    pub fn interpolate(&self, previous_frame: &Self, current_time: f64) -> Value {
        let t = ((self.timestamp.unwrap() - current_time) / (self.timestamp.unwrap() - previous_frame.timestamp.unwrap())) as f32;
        let s = 1.0 - t;
        let c = self.interpolation_control_points.unwrap();
        let factor = t * t * t * c[0] + s * t * t * c[1] + s * s * t * c[2] + s * s * s * c[3];
        match self.value {
            Value::Natural1(destination_value) => {
                let source_value = match_option!(previous_frame.value, Value::Natural1).unwrap();
                Value::Natural1((factor * (destination_value - source_value) as f32) as usize + source_value)
            }
            Value::Integer1(destination_value) => {
                let source_value = match_option!(previous_frame.value, Value::Integer1).unwrap();
                Value::Integer1((factor * (destination_value - source_value) as f32) as isize + source_value)
            }
            Value::Float1(destination_value) => {
                let source_value = match_option!(previous_frame.value, Value::Float1).unwrap().unwrap();
                Value::Float1((factor * (destination_value.unwrap() - source_value) + source_value).into())
            }
            Value::Float2(destination_value) => {
                let source_value = match_option!(previous_frame.value, Value::Float2).unwrap().unwrap();
                let destination_value = destination_value.unwrap();
                Value::Float2(
                    [
                        factor * (destination_value[0] - source_value[0]) + source_value[0],
                        factor * (destination_value[1] - source_value[1]) + source_value[1],
                    ]
                    .into(),
                )
            }
            Value::Float3(destination_value) => {
                let source_value = match_option!(previous_frame.value, Value::Float3).unwrap().unwrap();
                let destination_value = destination_value.unwrap();
                Value::Float3(
                    [
                        factor * (destination_value[0] - source_value[0]) + source_value[0],
                        factor * (destination_value[1] - source_value[1]) + source_value[1],
                        factor * (destination_value[2] - source_value[2]) + source_value[2],
                    ]
                    .into(),
                )
            }
            Value::Float4(destination_value) => {
                let source_value = match_option!(previous_frame.value, Value::Float4).unwrap().unwrap();
                let destination_value = destination_value.unwrap();
                Value::Float4(
                    [
                        factor * (destination_value[0] - source_value[0]) + source_value[0],
                        factor * (destination_value[1] - source_value[1]) + source_value[1],
                        factor * (destination_value[2] - source_value[2]) + source_value[2],
                        factor * (destination_value[3] - source_value[3]) + source_value[3],
                    ]
                    .into(),
                )
            }
            _ => panic!("Interpolation of this type is not supported"),
        }
    }
}

/// Trait of a node, which defines its behavior
pub type MessengerHandler = for<'a> fn(context: &mut NodeMessengerContext, message: &Messenger) -> Vec<Messenger>;

/// Common body of all nodes.
pub struct Node {
    messenger_handler: MessengerHandler,
    properties: HashMap<&'static str, Value>,
    property_animations: HashMap<&'static str, Vec<AnimationFrame>>,
    in_touched_attributes: HashSet<&'static str>,
    out_touched_attributes: HashSet<&'static str>,

    global_id: GlobalNodeIdentifier,
    local_id: NodeOrObservableIdentifier,
    observes: HashSet<NodeOrObservableIdentifier>,
    children: HashMap<NodeOrObservableIdentifier, GlobalNodeIdentifier>,
    parent: Option<GlobalNodeIdentifier>,

    nesting_depth: usize,
    ordered_children: Vec<GlobalNodeIdentifier>,
    in_reconfigure_queue: bool,
    colored_shapes: Vec<(SafeFloat<f32, 4>, Shape)>,
    clip_shape: Option<Box<Shape>>,
    colored_shapes_instance: u32,
    clip_shape_instance: u32,
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node").field("properties", &self.properties).finish()
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.global_id == other.global_id
    }
}

impl Eq for Node {}

impl Default for Node {
    fn default() -> Self {
        Self {
            messenger_handler: |_context: &mut NodeMessengerContext, _message: &Messenger| panic!(),
            properties: HashMap::new(),
            property_animations: HashMap::new(),
            in_touched_attributes: HashSet::new(),
            out_touched_attributes: HashSet::new(),

            global_id: 0,
            local_id: NodeOrObservableIdentifier::Named("uninitialized"),
            observes: HashSet::new(),
            children: HashMap::new(),
            parent: None,

            nesting_depth: 0,
            ordered_children: Vec::new(),
            in_reconfigure_queue: false,
            colored_shapes: Vec::new(),
            clip_shape: None,
            colored_shapes_instance: 0,
            clip_shape_instance: 0,
        }
    }
}

impl Node {
    pub fn new(messenger_handler: MessengerHandler, properties: HashMap<&'static str, Value>) -> Rc<RefCell<Self>> {
        let in_touched_attributes = properties.keys().cloned().collect();
        Rc::new(RefCell::new(Self {
            messenger_handler,
            properties,
            in_touched_attributes,
            ..Node::default()
        }))
    }

    pub(crate) fn advance_property_animations(&mut self, current_time: f64) -> bool {
        let properties = &mut self.properties;
        let in_touched_attributes = &mut self.in_touched_attributes;
        let mut result = false;
        self.property_animations.retain(|attribute, key_frames| {
            let retain_from_index = key_frames
                .iter()
                .position(|key_frame| current_time < key_frame.timestamp.unwrap())
                .unwrap_or(key_frames.len());
            if retain_from_index >= 2 {
                key_frames.drain(0..retain_from_index - 1);
            }
            if key_frames.is_empty() {
                false
            } else if key_frames.len() == 1 {
                result |= true;
                properties.insert(attribute, key_frames[0].value.clone());
                in_touched_attributes.insert(attribute);
                false
            } else {
                result |= true;
                properties.insert(attribute, key_frames[1].interpolate(&key_frames[0], current_time));
                in_touched_attributes.insert(attribute);
                true
            }
        });
        result
    }

    pub fn set_messenger_handler(&mut self, messenger_handler: MessengerHandler) -> bool {
        if std::ptr::eq(self.messenger_handler as *const u8, messenger_handler as *const u8) {
            return false;
        }
        self.messenger_handler = messenger_handler;
        self.in_touched_attributes.insert("messenger_handler");
        true
    }

    pub fn was_attribute_touched(&self, attributes: &[&'static str]) -> bool {
        for attribute in attributes {
            if self.out_touched_attributes.contains(attribute) {
                return true;
            }
        }
        false
    }

    pub fn touch_attribute(&mut self, attribute: &'static str) {
        self.in_touched_attributes.insert(attribute);
    }

    pub fn get_attribute(&self, attribute: &'static str) -> Value {
        self.properties.get(attribute).cloned().unwrap_or(Value::Void)
    }

    pub fn set_attribute_privately(&mut self, attribute: &'static str, value: Value) {
        self.properties.insert(attribute, value);
    }

    pub fn set_attribute(&mut self, attribute: &'static str, value: Value) -> bool {
        if self.properties.get(attribute) == Some(&value) {
            return false;
        }
        self.properties.insert(attribute, value);
        self.in_touched_attributes.insert(attribute);
        true
    }

    pub fn set_attribute_animated(&mut self, attribute: &'static str, value: Value, start_time: f64, duration: f64) -> bool {
        if self.properties.get(attribute) == Some(&value) {
            return false;
        }
        self.property_animations.insert(
            attribute,
            vec![
                AnimationFrame {
                    timestamp: start_time.into(),
                    interpolation_control_points: [0.0, 0.0, 0.0, 0.0].into(),
                    value: self.get_attribute(attribute),
                },
                AnimationFrame {
                    timestamp: (start_time + duration).into(),
                    interpolation_control_points: [0.0, 0.0, 3.0, 1.0].into(),
                    value,
                },
            ],
        );
        true
    }

    pub fn get_half_extent(&self, proposed: bool) -> SafeFloat<f32, 2> {
        let value = if proposed { self.properties.get("proposed_half_extent") } else { None };
        value
            .or_else(|| self.properties.get("half_extent"))
            .and_then(|value| match_option!(value, Value::Float2).cloned())
            .unwrap_or_else(|| [0.0; 2].into())
    }
}
