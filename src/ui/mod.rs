use crate::{
    path::{DynamicStrokeOptions, Path},
    renderer::Shape,
    safe_float::SafeFloat,
};
use geometric_algebra::{ppga2d, One};
use message::Messenger;
use node_hierarchy::NodeMessengerContext;
use std::{collections::HashMap, collections::HashSet, hash::Hash};
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
    /// Captured input device id
    InputDevice(usize),
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

/// Trait of a node, which defines its behavior
pub type MessengerHandler = for<'a> fn(context: &mut NodeMessengerContext, message: &Messenger) -> Vec<Messenger>;

/// Common body of all nodes.
pub struct Node {
    /// Node instance properties
    pub properties: HashMap<&'static str, Value>,
    /// Node trait
    pub messenger_handler: MessengerHandler,
    /// If `true` the node will be retained but not handle messages.
    pub dormant: bool,
    /// Position and rotation relative to parent node.
    pub motor: ppga2d::Motor,
    /// Scale relative to parent node.
    pub scale: SafeFloat<f32, 1>,
    /// Bounding box for message handling.
    pub half_extent: SafeFloat<f32, 2>,
    /// If 1.0 the node will be rendered as is, if 0.0 it will be invisible.
    pub opacity: SafeFloat<f32, 1>,
    /// Defines the order among overlapping siblings.
    pub layer_index: usize,

    global_id: GlobalNodeIdentifier,
    local_id: NodeOrObservableIdentifier,
    observes: HashSet<NodeOrObservableIdentifier>,
    children: HashMap<NodeOrObservableIdentifier, GlobalNodeIdentifier>,
    parent: Option<GlobalNodeIdentifier>,

    ordered_children: Vec<GlobalNodeIdentifier>,
    needs_reconfiguration: bool,
    configuration_in_process: bool,
    colored_shapes: Vec<(SafeFloat<f32, 4>, Shape)>,
    clip_shape: Option<Box<Shape>>,
    colored_shapes_instance: u32,
    clip_shape_instance: u32,
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node")
            .field("properties", &self.properties)
            .field("dormant", &self.dormant)
            .field("motor", &self.motor)
            .field("scale", &self.scale)
            .field("half_extent", &self.half_extent)
            .field("opacity", &self.opacity)
            .field("layer_index", &self.layer_index)
            .finish()
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
            properties: HashMap::new(),
            messenger_handler: |_context: &mut NodeMessengerContext, _message: &Messenger| panic!(),
            dormant: false,
            motor: ppga2d::Motor::one(),
            scale: 1.0.into(),
            half_extent: [0.0; 2].into(),
            opacity: 1.0.into(),
            layer_index: 0,

            global_id: 0,
            local_id: NodeOrObservableIdentifier::Named("uninitialized"),
            observes: HashSet::new(),
            children: HashMap::new(),
            parent: None,

            ordered_children: Vec::new(),
            needs_reconfiguration: true,
            configuration_in_process: false,
            colored_shapes: Vec::new(),
            clip_shape: None,
            colored_shapes_instance: 0,
            clip_shape_instance: 0,
        }
    }
}

impl Node {
    pub fn set_attribute(&mut self, attribute: &'static str, value: Value) -> bool {
        if self.properties.get(attribute) == Some(&value) {
            return false;
        }
        self.properties.insert(attribute, value);
        self.needs_reconfiguration = true;
        true
    }

    pub fn set_messenger_handler(&mut self, messenger_handler: MessengerHandler) -> bool {
        if std::ptr::eq(self.messenger_handler as *const u8, messenger_handler as *const u8) {
            return false;
        }
        self.messenger_handler = messenger_handler;
        self.needs_reconfiguration = true;
        true
    }

    pub fn set_dormant(&mut self, dormant: bool) -> bool {
        if self.dormant == dormant {
            return false;
        }
        self.dormant = dormant;
        self.needs_reconfiguration = !dormant;
        true
    }

    pub fn set_motor(&mut self, motor: ppga2d::Motor) -> bool {
        if unsafe { self.motor.g0.f32x4 == motor.g0.f32x4 } {
            return false;
        }
        self.motor = motor;
        self.needs_reconfiguration = true;
        true
    }

    pub fn set_scale(&mut self, scale: SafeFloat<f32, 1>) -> bool {
        if self.scale == scale {
            return false;
        }
        self.scale = scale;
        self.needs_reconfiguration = true;
        true
    }

    pub fn set_half_extent(&mut self, half_extent: SafeFloat<f32, 2>) -> bool {
        if self.half_extent == half_extent {
            return false;
        }
        self.half_extent = half_extent;
        self.needs_reconfiguration = true;
        true
    }
}
