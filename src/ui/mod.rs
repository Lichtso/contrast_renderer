//! (Optional) [Node]s and [Messenger]s to build user interfaces

use crate::{
    match_option,
    path::{DynamicStrokeOptions, Path},
    renderer::Shape,
    safe_float::SafeFloat,
    utils::{rotate2d, rotation2d, translate2d, translation2d},
};
use geometric_algebra::{ppga2d, GeometricProduct, GeometricQuotient};
use message::Messenger;
use node_hierarchy::NodeMessengerContext;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
    rc::Rc,
};
use wrapped_values::Value;

pub mod button;
pub mod checkbox;
pub mod dropdown;
pub mod label;
pub mod list;
pub mod message;
pub mod node_hierarchy;
pub mod overlay;
pub mod range;
pub mod renderer;
pub mod scroll;
pub mod tabs;
pub mod wrapped_values;

/// Identifies a [Node] in a [node_hierarchy::NodeHierarchy]
pub type GlobalNodeIdentifier = usize;

/// Children are matched by this identifier when the parent reconfigures them
///
/// Useful for retaining internal state and tracking animations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeOrObservableIdentifier {
    /// Useful for static sets of children
    Named(&'static str),
    /// Useful for dynamic lists
    Indexed(usize),
    /// Useful for dynamic tables
    Indexed2D(usize, usize),
    /// Useful for multiple dynamic lists
    NamedAndIndexed(&'static str, usize),
    /// Captured button input source
    ButtonInput(usize),
    /// Captured axis input source
    AxisInput(usize),
    /// Captured pointer input source
    PointerInput(usize),
}

/// Geometric orientation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Orientation {
    /// X axis
    Horizontal = 0,
    /// Y axis
    Vertical = 1,
    /// Z axis
    Lateral = 2,
}

/// Geometric side
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Side {
    /// -X
    Left,
    /// +X
    Right,
    /// -Y
    Bottom,
    /// +Y
    Top,
    /// -Z
    Back,
    /// +Z
    Front,
}

/// Scroll bar behavior per axis
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

/// User interaction with a text label
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TextInteraction {
    /// Read only
    None,
    /// Modify text selection
    Selection,
    /// Modify text selection and content
    Editing,
}

/// Converts user inputs between text and numeric values
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TextualProjection {
    /// From numeric value to text
    pub forward: fn(f32) -> String,
    /// From text to numeric value
    pub backward: fn(String) -> Option<f32>,
}

/// Snap and clamp numeric value in a given range
#[derive(Clone)]
pub struct SnapClampFunction {
    /// First parameter is the value the second the range to clamp it to
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
    #[cfg(feature = "winit")]
    pressed_logical_keys: HashSet<winit::keyboard::Key>,
    /// Key, button
    pub pressed_keys: HashSet<char>,
    /// Scroll wheel, joystick
    pub axes: HashMap<usize, SafeFloat<f32, 1>>,
    /// Mouse, touch, pen absolute positions
    pub absolute_positions: HashMap<usize, SafeFloat<f32, 3>>,
    /// Mouse, touch, pen relative to the current node
    pub relative_positions: HashMap<usize, SafeFloat<f32, 3>>,
    /// Mouse, touch, pen relative which are inside the current node
    pub is_inside_bounds: HashMap<usize, bool>,
}

/// Defines the shape, colors and clipping of a [Node]
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
    /// `y = c[0] (1-x)^3 + c[1] x (1-x)^2 + c[2] x^2 (1-x) + c[3] x^3`
    pub interpolation_control_points: SafeFloat<f32, 4>,
    /// The value that will be reached at the end of this frame
    pub value: Value,
}

impl AnimationFrame {
    /// Returns an interpolated [Value]
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
            Value::Motor(destination_value) => {
                let source_value: ppga2d::Motor = match_option!(previous_frame.value, Value::Motor).unwrap().into();
                let destination_value: ppga2d::Motor = destination_value.into();
                let quotient_value = destination_value.geometric_quotient(source_value);
                // let interpolated_value = quotient_value.powf(factor) * source_value;
                let source_translation = translation2d(source_value);
                let destination_translation = translation2d(destination_value);
                let interpolated_value = rotate2d(factor * rotation2d(quotient_value) + rotation2d(source_value)).geometric_product(translate2d([
                    factor * (destination_translation[0] - source_translation[0]) + source_translation[0],
                    factor * (destination_translation[1] - source_translation[1]) + source_translation[1],
                ]));
                Value::Motor(interpolated_value.into())
            }
            _ => panic!("Interpolation of this type is not supported"),
        }
    }
}

/// Smoothstep `interpolation_control_points`
pub const ANIMATION_FADE_IN_OUT: [f32; 4] = [0.0, 0.0, 3.0, 1.0];

/// Trait of a [Node], which defines its behavior
pub type MessengerHandler = for<'a> fn(context: &mut NodeMessengerContext, message: &Messenger) -> Vec<Messenger>;

/// An user interface node
pub struct Node {
    messenger_handler: MessengerHandler,
    properties: HashMap<&'static str, Value>,
    property_animations: HashMap<&'static str, Vec<AnimationFrame>>,
    touched_attributes: HashSet<&'static str>,

    parents: Vec<(NodeOrObservableIdentifier, GlobalNodeIdentifier, HashSet<&'static str>)>,
    nesting_depth: usize,
    children: HashMap<NodeOrObservableIdentifier, GlobalNodeIdentifier>,
    ordered_children: Vec<GlobalNodeIdentifier>,

    global_id: GlobalNodeIdentifier,
    in_reconfigure_queue: bool,
    observes: HashSet<NodeOrObservableIdentifier>,
    colored_shapes: Vec<(SafeFloat<f32, 4>, Shape)>,
    alpha_cover_shape: Option<Box<Shape>>,
    clip_shape: Option<Box<Shape>>,
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
            touched_attributes: HashSet::new(),

            parents: Vec::new(),
            nesting_depth: 0,
            children: HashMap::new(),
            ordered_children: Vec::new(),

            global_id: 0,
            in_reconfigure_queue: false,
            observes: HashSet::new(),
            colored_shapes: Vec::new(),
            alpha_cover_shape: None,
            clip_shape: None,
        }
    }
}

impl Node {
    /// Creates a new [Node]
    pub fn new(messenger_handler: MessengerHandler, mut properties: HashMap<&'static str, Value>) -> Rc<RefCell<Self>> {
        let touched_attributes = properties.keys().cloned().collect();
        properties.insert("dormant", Value::Boolean(false));
        Rc::new(RefCell::new(Self {
            messenger_handler,
            properties,
            touched_attributes,
            ..Node::default()
        }))
    }

    pub(crate) fn advance_property_animations(&mut self, current_time: f64) -> bool {
        let properties = &mut self.properties;
        let touched_attributes = &mut self.touched_attributes;
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
                touched_attributes.insert(attribute);
                false
            } else {
                result |= true;
                properties.insert(attribute, key_frames[1].interpolate(&key_frames[0], current_time));
                touched_attributes.insert(attribute);
                true
            }
        });
        result
    }

    /// Changes the [MessengerHandler]
    pub fn set_messenger_handler(&mut self, messenger_handler: MessengerHandler) -> bool {
        if std::ptr::eq(self.messenger_handler as *const u8, messenger_handler as *const u8) {
            return false;
        }
        self.messenger_handler = messenger_handler;
        self.touched_attributes.insert("messenger_handler");
        true
    }

    /// Sets the given `attribute` to be touched
    pub fn touch_attribute(&mut self, attribute: &'static str) {
        self.touched_attributes.insert(attribute);
    }

    /// Get the [Value] of the property by the given `attribute`
    pub fn get_attribute(&self, attribute: &'static str) -> Value {
        self.properties.get(attribute).cloned().unwrap_or(Value::Void)
    }

    /// Set the [Value] of the property by the given `attribute` without touching it
    pub fn set_attribute_privately(&mut self, attribute: &'static str, value: Value) {
        self.properties.insert(attribute, value);
    }

    /// Set the [Value] of the property by the given `attribute` and touches it
    pub fn set_attribute(&mut self, attribute: &'static str, value: Value) -> bool {
        if self.properties.get(attribute) == Some(&value) {
            return false;
        }
        self.properties.insert(attribute, value);
        self.touched_attributes.insert(attribute);
        true
    }

    /// Have the [Value] of the property by the given `attribute` be animated
    pub fn set_attribute_animated(
        &mut self,
        attribute: &'static str,
        value: Value,
        start_time: f64,
        duration: f64,
        interpolation_control_points: [f32; 4],
    ) -> bool {
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
                    interpolation_control_points: interpolation_control_points.into(),
                    value,
                },
            ],
        );
        true
    }

    /// Optionally gets "proposed_half_width" and "proposed_half_height" first. If it is not available returns "half_extent".
    pub fn get_half_extent(&self, prioritize_proposed: bool) -> SafeFloat<f32, 2> {
        let proposed_half_width = self
            .properties
            .get("proposed_half_width")
            .and_then(|value| match_option!(value, Value::Float1))
            .map(|safe_float| safe_float.unwrap());
        let proposed_half_height = self
            .properties
            .get("proposed_half_height")
            .and_then(|value| match_option!(value, Value::Float1))
            .map(|safe_float| safe_float.unwrap());
        let half_extent = self
            .properties
            .get("half_extent")
            .and_then(|value| match_option!(value, Value::Float2))
            .map(|safe_float| safe_float.unwrap());
        let half_width = half_extent.map(|half_extent| half_extent[0]);
        let half_height = half_extent.map(|half_extent| half_extent[1]);
        if prioritize_proposed {
            [
                proposed_half_width.or(half_width).unwrap_or(0.0),
                proposed_half_height.or(half_height).unwrap_or(0.0),
            ]
        } else {
            [
                half_width.or(proposed_half_width).unwrap_or(0.0),
                half_height.or(proposed_half_height).unwrap_or(0.0),
            ]
        }
        .into()
    }
}
