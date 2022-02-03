use crate::{
    safe_float::SafeFloat,
    text,
    ui::{
        InputState, Node, NodeOrObservableIdentifier, Orientation, Rendering, ScrollBarType, Side, SnapClampFunction, TextInteraction,
        TextualProjection,
    },
};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Void,
    Boolean(bool),
    Natural1(usize),
    Integer1(isize),
    Float1(SafeFloat<f32, 1>),
    Float2(SafeFloat<f32, 2>),
    Float3(SafeFloat<f32, 3>),
    Float4(SafeFloat<f32, 4>),
    Float4x4([SafeFloat<f32, 4>; 4]),
    Data(Vec<u8>),
    Character(char),
    TextString(String),
    TextFont(Rc<text::Font>),
    TextAlignment(text::Alignment),
    TextOrientation(text::Orientation),
    TextInteraction(TextInteraction),
    TextualProjection(TextualProjection),
    SnapClampFunction(SnapClampFunction),
    Orientation(Orientation),
    Side(Side),
    ScrollBarType(ScrollBarType),
    InputChannel(usize),
    InputState(Box<InputState>),
    Rendering(Box<Rendering>),
    NodeOrObservableIdentifier(NodeOrObservableIdentifier),
    NodeOrObservableIdentifiers(HashSet<NodeOrObservableIdentifier>),
    Node(Rc<RefCell<Node>>),
    Map(HashMap<&'static str, Value>),
    Vec(Vec<Value>),
}

impl Default for Value {
    fn default() -> Self {
        Self::Void
    }
}
