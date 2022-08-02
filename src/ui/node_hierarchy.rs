//! Scene graph of ui [Node]s

use crate::{
    hash_map, match_option, renderer,
    safe_float::SafeFloat,
    ui::{
        message::{self, Messenger, PropagationDirection},
        renderer::Renderer,
        wrapped_values::Value,
        GlobalNodeIdentifier, Node, NodeOrObservableIdentifier,
    },
    utils::{matrix_multiplication, motor2d_to_motor3d, motor3d_to_mat4},
};
use geometric_algebra::{ppga2d, One};
use std::{
    cell::RefCell,
    collections::{hash_map, BinaryHeap, HashMap, HashSet},
    ops::Range,
    rc::Rc,
};

macro_rules! reconfigure_node {
    ($node_hierarchy:expr, $node:expr) => {
        if !$node.in_reconfigure_queue {
            $node.in_reconfigure_queue = true;
            $node_hierarchy.reconfigure_queue.push(ReconfigurePriority {
                nesting_depth: $node.nesting_depth,
                global_node_id: $node.global_id,
            });
        }
    };
}

/// Used to interact with the `self` [Node] inside a (MessengerHandler)[crate::ui::MessengerHandler]
pub struct NodeMessengerContext<'a> {
    global_node_id: GlobalNodeIdentifier,
    node_hierarchy: &'a mut NodeHierarchy,
}

impl<'a> NodeMessengerContext<'a> {
    fn notify_dormant_recursively(&mut self, global_node_id: GlobalNodeIdentifier, increase: bool) {
        let node = self.node_hierarchy.nodes.get(&global_node_id).unwrap().borrow();
        let ordered_children = node.ordered_children.clone(); // TODO: Efficency
        drop(node);
        for global_child_id in ordered_children.iter() {
            let mut node = self.node_hierarchy.nodes.get(global_child_id).unwrap().borrow_mut();
            let mut dormant_parent_count = match_option!(node.get_attribute("dormant_parent_count"), Value::Natural1).unwrap_or(0);
            if increase {
                dormant_parent_count += 1;
            } else {
                dormant_parent_count -= 1;
            }
            node.set_attribute("dormant_parent_count", Value::Natural1(dormant_parent_count));
            reconfigure_node!(self.node_hierarchy, node);
            drop(node);
            self.notify_dormant_recursively(*global_child_id, increase);
        }
    }

    fn invoke_handler(&mut self, messenger: &mut Messenger) -> bool {
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        let messenger_handler = node.messenger_handler;
        if messenger.behavior.label == "Reconfigure" {
            node.in_reconfigure_queue = false;
        }
        let is_dormant = node.get_attribute("dormant") == Value::Boolean(true);
        if node.touched_attributes.contains("dormant") {
            node.touched_attributes.remove("dormant");
            drop(node);
            self.notify_dormant_recursively(self.global_node_id, is_dormant);
        } else {
            drop(node);
        }
        if is_dormant {
            return true;
        }
        let mut messengers = messenger_handler(self, messenger);
        for messenger in messengers.iter_mut() {
            messenger.source_node_id = self.global_node_id;
        }
        let reflect = messengers.is_empty();
        self.node_hierarchy.messenger_stack.append(&mut messengers);
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        if messenger.behavior.label == "Reconfigure" {
            let absolute_motor_changed = node.touched_attributes.contains(&"absolute_motor");
            let absolute_scale_changed = node.touched_attributes.contains(&"absolute_scale");
            let absolute_motor: ppga2d::Motor = match_option!(node.get_attribute("absolute_motor"), Value::Motor)
                .map(|value| value.into())
                .unwrap_or_else(ppga2d::Motor::one);
            let absolute_scale: f32 = match_option!(node.get_attribute("absolute_scale"), Value::Float1)
                .map(|value| value.into())
                .unwrap_or(1.0);
            let mut children_order_changed = node.touched_attributes.contains(&"child_count");
            for global_child_id in node.children.values() {
                let mut child_node = self.node_hierarchy.nodes.get(global_child_id).unwrap().borrow_mut();
                let parent_index = child_node
                    .parents
                    .iter()
                    .position(|(_local_child_id, global_parent_id, _touched_attributes)| *global_parent_id == self.global_node_id)
                    .unwrap();
                let touched_attributes = &child_node.parents[parent_index].2;
                let parents_changed = touched_attributes.contains(&"parents");
                if parents_changed || touched_attributes.contains(&"layer_index") {
                    children_order_changed = true;
                }
                let absolute_motor_changed = parents_changed || absolute_motor_changed || touched_attributes.contains(&"motor");
                let absolute_scale_changed = parents_changed || absolute_scale_changed || touched_attributes.contains(&"scale");
                if parent_index == 0 && absolute_motor_changed {
                    let child_motor = match_option!(child_node.get_attribute("motor"), Value::Motor)
                        .map(|value| value.into())
                        .unwrap_or_else(ppga2d::Motor::one);
                    child_node.set_attribute("absolute_motor", Value::Motor((child_motor * absolute_motor).into()));
                }
                if parent_index == 0 && absolute_scale_changed {
                    let child_scale = match_option!(child_node.get_attribute("scale"), Value::Float1)
                        .map(|value| value.into())
                        .unwrap_or(1.0);
                    child_node.set_attribute("absolute_scale", Value::Float1((child_scale * absolute_scale).into()));
                }
                child_node.parents[parent_index].2.clear();
            }
            if children_order_changed {
                let mut ordered_children = node
                    .children
                    .iter()
                    .map(|(_local_child_id, global_child_id)| {
                        let child_node = self.node_hierarchy.nodes.get(global_child_id).unwrap().borrow();
                        let layer_index = child_node
                            .properties
                            .get("layer_index")
                            .map(|value| *match_option!(value, Value::Natural1).unwrap())
                            .unwrap_or(0);
                        (layer_index, *global_child_id)
                    })
                    .collect::<Vec<(usize, GlobalNodeIdentifier)>>();
                ordered_children.sort_by_key(|entry| entry.0);
                node.ordered_children = ordered_children
                    .into_iter()
                    .map(|(_layer_index, global_child_id)| global_child_id)
                    .collect();
            }
            if !node.touched_attributes.is_empty() {
                let mut touched_attributes_of_child = HashSet::new();
                std::mem::swap(&mut node.touched_attributes, &mut touched_attributes_of_child);
                for (_local_child_id, global_parent_id, touched_attributes) in node.parents.iter_mut() {
                    *touched_attributes = touched_attributes_of_child.clone();
                    let mut parent_node = self.node_hierarchy.nodes.get(global_parent_id).unwrap().borrow_mut();
                    // println!("parent={} touched_attributes={:?} ", global_parent_id, node.out_touched_attributes);
                    reconfigure_node!(self.node_hierarchy, parent_node);
                }
            }
        } else if !node.touched_attributes.is_empty() {
            // println!("self={} touched_attributes={:?} ", self.global_node_id, node.out_touched_attributes);
            reconfigure_node!(self.node_hierarchy, node);
        }
        for global_child_id in node.children.values() {
            let mut child_node = self.node_hierarchy.nodes.get(global_child_id).unwrap().borrow_mut();
            if !child_node.touched_attributes.is_empty() {
                // println!("child={} touched_attributes={:?} ", global_child_id, child_node.in_touched_attributes);
                reconfigure_node!(self.node_hierarchy, child_node);
            }
        }
        reflect
    }

    /// Similar to [NodeMessengerContext::get_attribute] but falls back to the parents and eventually the [NodeHierarchy::theme_properties]
    pub fn derive_attribute(&self, attribute: &'static str) -> Value {
        let mut global_node_id = self.global_node_id;
        loop {
            let node = self.node_hierarchy.nodes.get(&global_node_id).unwrap().borrow();
            if let Some(value) = node.properties.get(attribute) {
                return value.clone();
            }
            if let Some((_local_child_id, global_parent_id, _touched_attributes)) = node.parents.first() {
                global_node_id = *global_parent_id;
            } else {
                return self.node_hierarchy.theme_properties.get(attribute).cloned().unwrap_or(Value::Void);
            }
        }
    }

    /// Returns true if the child node touched one of the given `attributes`
    pub fn was_attribute_of_child_touched(&self, node: &Node, attributes: &[&'static str]) -> bool {
        for (_local_child_id, global_parent_id, touched_attributes) in node.parents.iter() {
            if *global_parent_id == self.global_node_id {
                for attribute in attributes {
                    if touched_attributes.contains(attribute) {
                        return true;
                    }
                }
                return false;
            }
        }
        panic!();
    }

    /// Returns true if one of the given `attributes` was touched by either the parent, an animation or the node itself
    pub fn was_attribute_touched(&self, attributes: &[&'static str]) -> bool {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        for attribute in attributes {
            if node.touched_attributes.contains(attribute) {
                return true;
            }
        }
        false
    }

    /// Sets the given `attribute` to be touched
    pub fn touch_attribute(&mut self, attribute: &'static str) {
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        node.touch_attribute(attribute);
    }

    /// Get the [Value] of the property by the given `attribute`
    pub fn get_attribute(&self, attribute: &'static str) -> Value {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.get_attribute(attribute)
    }

    /// Set the [Value] of the property by the given `attribute` without touching it
    pub fn set_attribute_privately(&mut self, attribute: &'static str, value: Value) {
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        node.set_attribute_privately(attribute, value);
    }

    /// Set the [Value] of the property by the given `attribute` and touches it
    pub fn set_attribute(&mut self, attribute: &'static str, value: Value) -> bool {
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        node.set_attribute(attribute, value)
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
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        node.set_attribute_animated(attribute, value, start_time, duration, interpolation_control_points)
    }

    /// Optionally gets "proposed_half_extent" first, and if it is not available returns "half_extent"
    pub fn get_half_extent(&self, proposed: bool) -> SafeFloat<f32, 2> {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.get_half_extent(proposed)
    }

    /// Returns the timestamp passed to the last call of [NodeHierarchy::advance_property_animations]
    pub fn get_last_animation_time(&self) -> f64 {
        self.node_hierarchy.last_animation_time
    }

    /// Returns true if this [Node] observes the given `observable`
    pub fn does_observe(&self, observable: &NodeOrObservableIdentifier) -> bool {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.observes.contains(observable)
    }

    /// Returns how many parent [Node]s this [Node] has
    pub fn get_number_of_parents(&self) -> usize {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.parents.len()
    }

    /// Iterates the parent [Node]s of this [Node]
    pub fn iter_parents<F: FnMut(usize, &Node)>(&self, mut callback: F) {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        for (parent_index, (_local_child_id, global_parent_id, _touched_attributes)) in node.parents.iter().enumerate() {
            let parent_node = self.node_hierarchy.nodes.get(global_parent_id).unwrap().borrow();
            callback(parent_index, &parent_node);
        }
    }

    /// Iterates the child [Node]s of this [Node]
    pub fn iter_children<F: FnMut(&NodeOrObservableIdentifier, &mut Node)>(&self, mut callback: F) {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        for (local_child_id, global_child_id) in node.children.iter() {
            let mut child_node = self.node_hierarchy.nodes.get(global_child_id).unwrap().borrow_mut();
            callback(local_child_id, &mut child_node);
        }
    }

    /// Used to read properties of a specific parent [Node]
    pub fn inspect_parent<R, F: FnOnce(&Node) -> R>(&self, parent_index: usize, callback: F) -> Option<R> {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.parents
            .get(parent_index)
            .map(|(_local_child_id, global_parent_id, _touched_attributes)| {
                callback(&self.node_hierarchy.nodes.get(global_parent_id).unwrap().borrow())
            })
    }

    /// Used to read properties of a specific child [Node]
    pub fn inspect_child<R, F: FnOnce(&Node) -> R>(&self, local_child_id: &NodeOrObservableIdentifier, callback: F) -> Option<R> {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.children
            .get(local_child_id)
            .map(|global_child_id| callback(&self.node_hierarchy.nodes.get(global_child_id).unwrap().borrow()))
    }

    /// Adds and links a given child [Node]
    pub fn add_child(&mut self, local_child_id: NodeOrObservableIdentifier, child_node: Rc<RefCell<Node>>, unlink_from_all_parents: bool) {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        let global_child_id = node.children.get(&local_child_id).cloned();
        drop(node);
        if let Some(global_child_id) = global_child_id {
            self.node_hierarchy.unlink_node(global_child_id, None);
        }
        if unlink_from_all_parents {
            let mut node = child_node.borrow_mut();
            for local_and_global_ids in node.parents.iter() {
                if let Some(parent) = self.node_hierarchy.nodes.get(&local_and_global_ids.1) {
                    let mut parent = parent.borrow_mut();
                    parent.touch_attribute("child_count");
                    parent.children.remove(&local_and_global_ids.0);
                    reconfigure_node!(self.node_hierarchy, parent);
                }
            }
            node.parents.clear();
        }
        let _global_child_id = self.node_hierarchy.link_node(child_node, Some((local_child_id, self.global_node_id)));
    }

    /// Removes and unlinks a given child [Node]
    pub fn remove_child(&mut self, local_child_id: NodeOrObservableIdentifier, unlink_from_all_parents: bool) -> Option<Rc<RefCell<Node>>> {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        let global_child_id = node.children.get(&local_child_id).cloned();
        drop(node);
        let global_parent_id = if unlink_from_all_parents { None } else { Some(self.global_node_id) };
        global_child_id.and_then(|global_child_id| self.node_hierarchy.unlink_node(global_child_id, global_parent_id))
    }

    /// Returns the child [Node] given by `local_child_id`
    pub fn get_child(&self, local_child_id: &NodeOrObservableIdentifier) -> Option<Rc<RefCell<Node>>> {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.children
            .get(local_child_id)
            .map(|global_child_id| self.node_hierarchy.nodes.get(global_child_id).unwrap().clone())
    }

    /// Adds, modifies or removes a child [Node]
    ///
    /// If `callback` is `Some` the child [Node] is added if it did not exist before.
    /// Otherwise, if `callback` is `None` the child [Node] is removed if it did exist.
    pub fn configure_child<R, F: FnOnce(&mut Node) -> R>(&mut self, local_child_id: NodeOrObservableIdentifier, callback: Option<F>) -> Option<R> {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        if let Some(global_child_id) = node.children.get(&local_child_id).cloned() {
            drop(node);
            if let Some(callback) = callback {
                let mut child_node = self.node_hierarchy.nodes.get(&global_child_id).unwrap().borrow_mut();
                Some(callback(&mut child_node))
            } else {
                self.remove_child(local_child_id, false);
                None
            }
        } else if let Some(callback) = callback {
            drop(node);
            let mut child_node = Node::default();
            let result = callback(&mut child_node);
            self.add_child(local_child_id, Rc::new(RefCell::new(child_node)), false);
            Some(result)
        } else {
            None
        }
    }

    /// Starts or stops observing an observable
    ///
    /// If `want` is `true` the observable is registered if it was not already.
    /// Otherwise, if `want` is `false` the observable is unregistered if it was not already.
    /// If `unsubscribe_others` is `true` all other obervers of the given `oberverables` are unregistered.
    pub fn configure_observe(&mut self, observable: NodeOrObservableIdentifier, want: bool, unsubscribe_others: bool) {
        if unsubscribe_others {
            self.node_hierarchy.unsubscribe_observers(observable);
        }
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        match node.observes.contains(&observable) {
            true if !want => {
                node.observes.remove(&observable);
                node.touched_attributes.insert("observes");
                drop(node);
                self.node_hierarchy.unsubscribe_observer(self.global_node_id, observable);
            }
            false if want => {
                node.observes.insert(observable);
                node.touched_attributes.insert("observes");
                drop(node);
                self.node_hierarchy.subscribe_observer(self.global_node_id, observable);
            }
            _ => {}
        }
    }

    /// Helper to focus this [Node]
    pub fn pointer_and_button_input_focus(&mut self, messenger: &Messenger) -> Vec<Messenger> {
        let input_source = *match messenger.get_attribute("input_source") {
            Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::ButtonInput(input_source)) => input_source,
            Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::AxisInput(input_source)) => input_source,
            Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::PointerInput(input_source)) => input_source,
            _ => panic!(),
        };
        self.configure_observe(NodeOrObservableIdentifier::ButtonInput(input_source), true, true);
        if messenger.get_attribute("pressed_or_released") == &Value::Boolean(true) {
            self.configure_observe(NodeOrObservableIdentifier::PointerInput(input_source), true, true);
        } else {
            self.configure_observe(NodeOrObservableIdentifier::PointerInput(input_source), false, false);
        }
        Vec::new()
    }

    /// Helper send the focus to the parent [Node] or a child [Node]
    pub fn input_focus_parent_or_child(&self, messenger: &Messenger, child_id: Option<NodeOrObservableIdentifier>) -> Messenger {
        let mut input_state = match_option!(messenger.get_attribute("input_state"), Value::InputState).unwrap().clone();
        if child_id.is_some() {
            input_state.pressed_keycodes.remove(&'⇧');
        } else {
            input_state.pressed_keycodes.insert('⇧');
        }
        input_state.pressed_keycodes.insert('⇥');
        let mut messenger = Messenger::new(
            &message::BUTTON_INPUT,
            hash_map! {
                "input_source" => messenger.get_attribute("input_source").clone(),
                "input_state" => Value::InputState(input_state),
                "changed_keycode" => Value::Character('⇥'),
            },
        );
        if let Some(child_id) = child_id {
            messenger.propagation_direction = PropagationDirection::Child(child_id);
        } else {
            messenger.propagation_direction = PropagationDirection::Parent(0);
        }
        messenger
    }

    /// Helper redirect focus navigation to the parent [Node]
    pub fn redirect_input_focus_navigation_to_parent(&self, messenger: &Messenger) -> Messenger {
        let mut messenger = messenger.clone();
        messenger.propagation_direction = PropagationDirection::Parent(0);
        messenger
    }

    /// Helper to handle the "PrepareRendering" message
    pub fn update_rendering_helper(&mut self, _prepare_rendering: &Messenger) -> Messenger {
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        let change_rendering = node.get_attribute("is_rendering_dirty") == Value::Boolean(true);
        node.set_attribute_privately("is_rendering_dirty", Value::Boolean(false));
        Messenger::new(
            &message::UPDATE_RENDERING,
            hash_map! {
                "rendering" => if change_rendering { Value::Boolean(true) } else { Value::Void },
            },
        )
    }
}

#[derive(PartialEq, Eq)]
struct ReconfigurePriority {
    nesting_depth: usize,
    global_node_id: usize,
}

impl Ord for ReconfigurePriority {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .nesting_depth
            .cmp(&self.nesting_depth)
            .then_with(|| self.global_node_id.cmp(&other.global_node_id))
    }
}

impl PartialOrd for ReconfigurePriority {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// The hierarchy of UI nodes
#[derive(Default)]
pub struct NodeHierarchy {
    next_node_id: GlobalNodeIdentifier,
    nodes: HashMap<GlobalNodeIdentifier, Rc<RefCell<Node>>>,
    messenger_stack: Vec<Messenger>,
    reconfigure_queue: BinaryHeap<ReconfigurePriority>,
    observer_channels: HashMap<NodeOrObservableIdentifier, HashSet<GlobalNodeIdentifier>>,
    last_animation_time: f64,
    /// Global properties accessible to all [Node]s
    pub theme_properties: HashMap<&'static str, Value>,
}

impl NodeHierarchy {
    /// Links the given [Node] to its parent, then returns its [GlobalNodeIdentifier]
    pub fn link_node(
        &mut self,
        node: Rc<RefCell<Node>>,
        parent_link: Option<(NodeOrObservableIdentifier, GlobalNodeIdentifier)>,
    ) -> GlobalNodeIdentifier {
        let mut borrowed_node = node.borrow_mut();
        let was_created = borrowed_node.global_id == 0;
        if was_created {
            self.next_node_id += 1;
            borrowed_node.global_id = self.next_node_id;
        }
        if let Some((local_child_id, global_parent_id)) = parent_link {
            if !borrowed_node
                .parents
                .iter()
                .any(|local_and_global_ids| local_and_global_ids.1 == global_parent_id)
            {
                let mut parent = self.nodes.get(&global_parent_id).unwrap().borrow_mut();
                parent.touch_attribute("child_count");
                parent.children.insert(local_child_id, borrowed_node.global_id);
                reconfigure_node!(self, parent);
                borrowed_node.touch_attribute("parents");
                borrowed_node.parents.push((local_child_id, global_parent_id, HashSet::new()));
                borrowed_node.nesting_depth = borrowed_node.nesting_depth.max(parent.nesting_depth + 1);
            }
        }
        if borrowed_node.parents.is_empty() {
            borrowed_node.observes.insert(NodeOrObservableIdentifier::Named("root"));
        } else {
            borrowed_node.observes.remove(&NodeOrObservableIdentifier::Named("root"));
        }
        reconfigure_node!(self, borrowed_node);
        if parent_link.is_none() {
            for oberverable in borrowed_node.observes.iter() {
                self.subscribe_observer(borrowed_node.global_id, *oberverable);
            }
        }
        let global_node_id = borrowed_node.global_id;
        drop(borrowed_node);
        if was_created {
            assert!(self.nodes.insert(global_node_id, node).is_none());
        }
        global_node_id
    }

    /// Unlinks the given [Node] from the given parent or all parents if no specific one is provided
    ///
    /// Returns the [Node] and deletes it together with its children if the [Node] has no more parents remaining
    pub fn unlink_node(&mut self, global_node_id: GlobalNodeIdentifier, global_parent_id: Option<GlobalNodeIdentifier>) -> Option<Rc<RefCell<Node>>> {
        let mut node = self.nodes.get(&global_node_id).unwrap().borrow_mut();
        for local_and_global_ids in node.parents.iter() {
            if global_parent_id.is_some() && global_parent_id != Some(local_and_global_ids.1) {
                continue;
            }
            if let Some(parent) = self.nodes.get(&local_and_global_ids.1) {
                let mut parent = parent.borrow_mut();
                parent.touch_attribute("child_count");
                parent.children.remove(&local_and_global_ids.0);
                reconfigure_node!(self, parent);
            }
        }
        node.touch_attribute("parents");
        if let Some(global_parent_id) = global_parent_id {
            let parent_index = node
                .parents
                .iter()
                .position(|local_and_global_ids| local_and_global_ids.1 == global_parent_id)
                .unwrap();
            node.parents.remove(parent_index);
        } else {
            node.parents.clear();
        }
        if !node.parents.is_empty() {
            reconfigure_node!(self, node);
            return None;
        }
        drop(node);
        let node = self.nodes.remove(&global_node_id).unwrap();
        let borrowed_node = node.borrow();
        for oberverable in borrowed_node.observes.iter() {
            self.unsubscribe_observer(borrowed_node.global_id, *oberverable);
        }
        for (_local_child_id, global_child_id) in borrowed_node.children.iter() {
            self.unlink_node(*global_child_id, Some(global_node_id));
        }
        drop(borrowed_node);
        Some(node)
    }

    fn subscribe_observer(&mut self, global_node_id: GlobalNodeIdentifier, observable: NodeOrObservableIdentifier) {
        self.observer_channels
            .entry(observable)
            .or_insert_with(HashSet::new)
            .insert(global_node_id);
    }

    fn unsubscribe_observer(&mut self, global_node_id: GlobalNodeIdentifier, observable: NodeOrObservableIdentifier) {
        match self.observer_channels.entry(observable) {
            hash_map::Entry::Occupied(mut entry) => {
                entry.get_mut().remove(&global_node_id);
                if entry.get().is_empty() {
                    entry.remove_entry();
                }
            }
            hash_map::Entry::Vacant(_entry) => {}
        }
    }

    /// Unsubscribes all observers of the given observable
    pub fn unsubscribe_observers(&mut self, observable: NodeOrObservableIdentifier) {
        match self.observer_channels.entry(observable) {
            hash_map::Entry::Occupied(entry) => {
                for global_node_id in entry.get() {
                    let mut node = self.nodes.get(global_node_id).unwrap().borrow_mut();
                    node.observes.remove(&observable);
                    node.touched_attributes.insert("observes");
                    reconfigure_node!(self, node);
                }
                entry.remove_entry();
            }
            hash_map::Entry::Vacant(_entry) => {}
        }
    }

    /// Sends a [Messenger] to a set of UI nodes observing the given observable.
    pub fn notify_observers(&mut self, observable: NodeOrObservableIdentifier, mut messenger: Messenger) {
        messenger.propagation_direction = PropagationDirection::Observers(observable);
        self.messenger_stack.push(messenger);
    }

    fn invoke_handler(&mut self, global_node_id: GlobalNodeIdentifier, messenger: &mut Messenger) -> bool {
        NodeMessengerContext {
            global_node_id,
            node_hierarchy: self,
        }
        .invoke_handler(messenger)
    }

    /// Internally processes all outstanding [Messenger]s
    pub fn process_messengers(&mut self) {
        while let Some(ReconfigurePriority { global_node_id, .. }) = self.reconfigure_queue.pop() {
            let mut messenger = Messenger::new(&message::RECONFIGURE, hash_map! {});
            messenger.source_node_id = global_node_id;
            self.messenger_stack.push(messenger);
        }
        while let Some(mut messenger) = self.messenger_stack.pop() {
            println!(
                "process_messenger {} {} {:?}",
                messenger.behavior.label, messenger.source_node_id, messenger.propagation_direction
            );
            match messenger.propagation_direction {
                PropagationDirection::None => panic!(),
                PropagationDirection::Itself => {
                    self.invoke_handler(messenger.source_node_id, &mut messenger);
                }
                PropagationDirection::Parent(mut parent_index) => {
                    let node = self.nodes.get(&messenger.source_node_id).unwrap().borrow();
                    if parent_index < 0 {
                        parent_index += node.parents.len() as isize;
                    }
                    if let Some((local_child_id, global_parent_id, _touched_attributes)) = node.parents.get(parent_index as usize).cloned() {
                        drop(node);
                        let node = self.nodes.get(&global_parent_id).unwrap().borrow();
                        let (invoke, stop) = (messenger.behavior.update_at_node_edge)(&mut messenger, &node, Some(local_child_id));
                        drop(node);
                        if invoke {
                            self.invoke_handler(global_parent_id, &mut messenger);
                        }
                        if stop {
                            break;
                        }
                    }
                }
                PropagationDirection::Child(local_child_id) => {
                    let node = self.nodes.get(&messenger.source_node_id).unwrap().borrow();
                    let global_child_id = node.children.get(&local_child_id).cloned();
                    drop(node);
                    let mut reflect = true;
                    if let Some(global_child_id) = global_child_id {
                        let node = self.nodes.get(&global_child_id).unwrap().borrow();
                        let (invoke, _stop) = (messenger.behavior.update_at_node_edge)(&mut messenger, &node, None);
                        drop(node);
                        if invoke {
                            reflect &= self.invoke_handler(global_child_id, &mut messenger);
                        }
                    }
                    if reflect {
                        let node = self.nodes.get(&messenger.source_node_id).unwrap().borrow();
                        if (messenger.behavior.do_reflect)(&mut messenger, Some(&node)) {
                            drop(node);
                            self.invoke_handler(messenger.source_node_id, &mut messenger);
                        }
                    }
                }
                PropagationDirection::Siblings => {
                    let parents = self.nodes.get(&messenger.source_node_id).unwrap().borrow().parents.clone(); // TODO: Efficency
                    for (local_child_id, global_parent_id, _touched_attributes) in parents {
                        let node = self.nodes.get(&global_parent_id).unwrap().borrow();
                        (messenger.behavior.update_at_node_edge)(&mut messenger, &node, Some(local_child_id));
                        let ordered_children = node.ordered_children.clone(); // TODO: Efficency
                        drop(node);
                        for global_child_id in ordered_children.iter() {
                            if *global_child_id == messenger.source_node_id {
                                continue;
                            }
                            let node = self.nodes.get(global_child_id).unwrap().borrow();
                            let (invoke, stop) = (messenger.behavior.update_at_node_edge)(&mut messenger, &node, None);
                            drop(node);
                            if invoke {
                                self.invoke_handler(*global_child_id, &mut messenger);
                            }
                            if stop {
                                break;
                            }
                        }
                    }
                }
                PropagationDirection::Children => {
                    let ordered_children = self.nodes.get(&messenger.source_node_id).unwrap().borrow().ordered_children.clone(); // TODO: Efficency
                    let mut reflect = true;
                    for global_child_id in ordered_children.iter().rev() {
                        let node = self.nodes.get(global_child_id).unwrap().borrow();
                        let (invoke, stop) = (messenger.behavior.update_at_node_edge)(&mut messenger, &node, None);
                        drop(node);
                        if invoke {
                            reflect &= self.invoke_handler(*global_child_id, &mut messenger);
                        }
                        if stop {
                            break;
                        }
                    }
                    if reflect {
                        let node = self.nodes.get(&messenger.source_node_id).unwrap().borrow();
                        if (messenger.behavior.do_reflect)(&mut messenger, Some(&node)) {
                            drop(node);
                            self.invoke_handler(messenger.source_node_id, &mut messenger);
                        }
                    }
                }
                PropagationDirection::Observers(observable) => {
                    messenger.propagation_direction = messenger.behavior.default_propagation_direction;
                    let captured_observable = (messenger.behavior.get_captured_observable)(&messenger);
                    let (is_captured_observable, global_node_ids) = if let Some((is_captured_observable, observers)) = captured_observable
                        .and_then(|captured_observable| self.observer_channels.get(&captured_observable).map(|observers| (true, observers)))
                        .or_else(|| self.observer_channels.get(&observable).map(|observers| (false, observers)))
                    {
                        (is_captured_observable, observers.iter().cloned().collect::<Vec<_>>())
                    } else {
                        continue;
                    };
                    if is_captured_observable {
                        (messenger.behavior.do_reflect)(&mut messenger, None);
                    }
                    for global_node_id in global_node_ids.iter() {
                        let mut invoke_handler = true;
                        let mut parents_path = vec![*global_node_id];
                        {
                            let mut global_node_id = *global_node_id;
                            while let Some((_local_child_id, global_parent_id, _touched_attributes)) =
                                self.nodes.get(&global_node_id).unwrap().borrow().parents.last()
                            {
                                global_node_id = *global_parent_id;
                                parents_path.push(global_node_id);
                            }
                            for global_node_id in parents_path.iter().rev() {
                                let node = self.nodes.get(global_node_id).unwrap().borrow();
                                let (invoke, _stop) = (messenger.behavior.update_at_node_edge)(&mut messenger, &node, None);
                                if !invoke && !is_captured_observable {
                                    invoke_handler = false;
                                    break;
                                }
                            }
                        }
                        if invoke_handler || is_captured_observable {
                            self.invoke_handler(*global_node_id, &mut messenger);
                        }
                    }
                }
            }
            while let Some(ReconfigurePriority { global_node_id, .. }) = self.reconfigure_queue.pop() {
                let mut messenger = Messenger::new(&message::RECONFIGURE, hash_map! {});
                messenger.source_node_id = global_node_id;
                self.messenger_stack.push(messenger);
            }
        }
    }

    fn instanciate_node_rendering(
        &mut self,
        renderer: &mut Renderer,
        global_node_id: GlobalNodeIdentifier,
        global_parent_id: Option<GlobalNodeIdentifier>,
        layer_range: Range<usize>,
        clip_depth: u8,
        alpha_layer: u8,
    ) {
        let node = self.nodes.get(&global_node_id).unwrap().borrow();
        if (global_parent_id.is_some() && Some(node.parents.last().unwrap().1) != global_parent_id)
            || node.get_attribute("dormant") == Value::Boolean(true)
            || node.get_attribute("opacity") == Value::Float1(0.0.into())
        {
            return;
        }
        let model_matrix = match_option!(node.get_attribute("model_matrix"), Value::Float4x4)
            .map(|model_matrix| {
                [
                    model_matrix[0].into(),
                    model_matrix[1].into(),
                    model_matrix[2].into(),
                    model_matrix[3].into(),
                ]
            })
            .unwrap_or_else(|| {
                let motor: ppga2d::Motor = match_option!(node.get_attribute("absolute_motor"), Value::Motor)
                    .map(|value| value.into())
                    .unwrap_or_else(ppga2d::Motor::one);
                let scale: f32 = match_option!(node.get_attribute("absolute_scale"), Value::Float1)
                    .map(|value| value.into())
                    .unwrap_or(1.0);
                let motor3d = motor2d_to_motor3d(&motor);
                let mut model_matrix = motor3d_to_mat4(&motor3d);
                model_matrix[0].g0[0] *= scale;
                model_matrix[1].g0[1] *= scale;
                model_matrix[2].g0[2] *= scale;
                model_matrix[3].g0[0] *= scale;
                model_matrix[3].g0[1] *= scale;
                model_matrix[3].g0[2] *= scale;
                model_matrix
            });
        let model_projection_matrix = matrix_multiplication(&renderer.projection_matrix, &model_matrix);
        renderer.instanciate_node(&node, layer_range, clip_depth, alpha_layer, &model_projection_matrix);
    }

    #[allow(clippy::too_many_arguments)]
    fn prepare_node_rendering(
        &mut self,
        contrast_renderer: &renderer::Renderer,
        renderer: &mut Renderer,
        device: &wgpu::Device,
        global_node_id: GlobalNodeIdentifier,
        global_parent_id: Option<GlobalNodeIdentifier>,
        parent_layer_range: &mut Range<usize>,
        mut clip_depth: u8,
        mut alpha_layer: u8,
    ) {
        let node = self.nodes.get(&global_node_id).unwrap().borrow();
        let opacity: f32 = match_option!(node.get_attribute("opacity"), Value::Float1)
            .map(|value| value.into())
            .unwrap_or(1.0);
        if (global_parent_id.is_some() && Some(node.parents.last().unwrap().1) != global_parent_id)
            || node.get_attribute("dormant") == Value::Boolean(true)
            || opacity == 0.0
        {
            return;
        }
        drop(node);
        let mut messenger = Messenger::new(&message::PREPARE_RENDERING, hash_map! {});
        messenger.source_node_id = global_node_id;
        if !self.invoke_handler(global_node_id, &mut messenger) {
            let messenger = self.messenger_stack.pop().unwrap();
            assert_eq!(messenger.behavior.label, "UpdateRendering");
            if let Value::Rendering(rendering) = messenger.get_attribute("rendering") {
                let mut node = self.nodes.get(&global_node_id).unwrap().borrow_mut();
                renderer.set_node_rendering(contrast_renderer, device, &mut node, rendering).unwrap();
            }
        }
        let node = self.nodes.get(&global_node_id).unwrap().borrow();
        let mut additional_layers = 0;
        if opacity < 1.0 {
            additional_layers = 1;
            alpha_layer += 1;
        }
        if node.clip_shape.is_some() {
            additional_layers = 1;
            clip_depth += 1;
        }
        let mut child_layer_range = parent_layer_range.start + additional_layers + node.colored_shapes.len()..0;
        child_layer_range.end = child_layer_range.start;
        let mut prev_child_index = 0;
        let mut prev_layer_index = 0;
        let ordered_children = node.ordered_children.clone(); // TODO: Efficency
        drop(node);
        for (child_index, global_child_id) in ordered_children.iter().enumerate() {
            let child_node = self.nodes.get(global_child_id).unwrap().borrow();
            let layer_index = match_option!(child_node.get_attribute("layer_index"), Value::Natural1).unwrap_or(0);
            drop(child_node);
            if prev_layer_index != layer_index {
                prev_layer_index = layer_index;
                for global_child_id in ordered_children[prev_child_index..child_index].iter() {
                    self.instanciate_node_rendering(
                        renderer,
                        *global_child_id,
                        Some(global_node_id),
                        child_layer_range.clone(),
                        clip_depth,
                        alpha_layer,
                    );
                }
                prev_child_index = child_index;
                child_layer_range.start = child_layer_range.end;
            }
            self.prepare_node_rendering(
                contrast_renderer,
                renderer,
                device,
                *global_child_id,
                Some(global_node_id),
                &mut child_layer_range,
                clip_depth,
                alpha_layer,
            );
        }
        for global_child_id in ordered_children[prev_child_index..].iter() {
            self.instanciate_node_rendering(
                renderer,
                *global_child_id,
                Some(global_node_id),
                child_layer_range.clone(),
                clip_depth,
                alpha_layer,
            );
        }
        parent_layer_range.end = parent_layer_range.end.max(child_layer_range.end + additional_layers);
    }

    /// Preparation step to be called before [Renderer::encode_commands]
    ///
    /// `current_time` is measured in seconds.
    pub fn prepare_rendering(
        &mut self,
        contrast_renderer: &renderer::Renderer,
        renderer: &mut Renderer,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        current_time: f64,
    ) {
        self.last_animation_time = current_time;
        for node in self.nodes.values() {
            let mut node = node.borrow_mut();
            if node.advance_property_animations(current_time) {
                reconfigure_node!(self, node);
            }
        }
        self.process_messengers();
        renderer.reset_buffers();
        let roots = self.observer_channels.get(&NodeOrObservableIdentifier::Named("root")).unwrap().clone();
        let mut layer_range = 0..0;
        for global_node_id in roots {
            self.prepare_node_rendering(contrast_renderer, renderer, device, global_node_id, None, &mut layer_range, 0, 0);
            self.instanciate_node_rendering(renderer, global_node_id, None, layer_range.clone(), 0, 0);
            layer_range.start = layer_range.end;
        }
        renderer.update_instance_buffers(device, queue);
    }
}
