//! Scene graph of ui [Node]s

use crate::{
    hash_map, hash_set, match_option,
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
    ($self:expr, $node:expr) => {
        if !$node.in_reconfigure_queue {
            $node.in_reconfigure_queue = true;
            $self.reconfigure_queue.push(ReconfigurePriority {
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
    fn invoke_handler(&mut self, messenger: &mut Messenger) -> bool {
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        if node.get_attribute("dormant") == Value::Boolean(true) {
            node.in_reconfigure_queue = false;
            return true;
        }
        let messenger_handler = node.messenger_handler;
        drop(node);
        let mut messengers = messenger_handler(self, messenger)
            .into_iter()
            .map(|messenger| (self.global_node_id, messenger))
            .collect::<Vec<_>>();
        let reflect = messengers.is_empty();
        self.node_hierarchy.messenger_stack.append(&mut messengers);
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        if messenger.behavior.label == "Reconfigure" {
            node.out_touched_attributes = &node.in_touched_attributes | &node.out_touched_attributes;
            node.in_reconfigure_queue = false;
            let absolute_motor_changed = node.was_attribute_touched(&["absolute_motor"]);
            let absolute_motor: ppga2d::Motor = match_option!(node.get_attribute("absolute_motor"), Value::Float4)
                .map(|value| value.into())
                .unwrap_or_else(ppga2d::Motor::one);
            let absolute_scale_changed = node.was_attribute_touched(&["absolute_scale"]);
            let absolute_scale: f32 = match_option!(node.get_attribute("absolute_scale"), Value::Float1)
                .map(|value| value.into())
                .unwrap_or(1.0);
            let absolute_opacity_changed = node.was_attribute_touched(&["absolute_opacity"]);
            let absolute_opacity: f32 = match_option!(node.get_attribute("absolute_opacity"), Value::Float1)
                .map(|value| value.into())
                .unwrap_or(1.0);
            let mut children_order_changed = node.was_attribute_touched(&["child_count"]);
            for global_child_id in node.children.values() {
                let mut child_node = self.node_hierarchy.nodes.get(global_child_id).unwrap().borrow_mut();
                if absolute_motor_changed || child_node.was_attribute_touched(&["motor", "parent"]) {
                    let child_motor = match_option!(child_node.get_attribute("motor"), Value::Float4)
                        .map(|value| value.into())
                        .unwrap_or_else(ppga2d::Motor::one);
                    child_node.set_attribute("absolute_motor", Value::Float4((child_motor * absolute_motor).into()));
                }
                if absolute_scale_changed || child_node.was_attribute_touched(&["scale", "parent"]) {
                    let child_scale = match_option!(child_node.get_attribute("scale"), Value::Float1)
                        .map(|value| value.into())
                        .unwrap_or(1.0);
                    child_node.set_attribute("absolute_scale", Value::Float1((child_scale * absolute_scale).into()));
                }
                if absolute_opacity_changed || child_node.was_attribute_touched(&["opacity", "parent"]) {
                    let child_opacity = match_option!(child_node.get_attribute("opacity"), Value::Float1)
                        .map(|value| value.into())
                        .unwrap_or(1.0);
                    child_node.set_attribute("absolute_opacity", Value::Float1((child_opacity * absolute_opacity).into()));
                }
                if child_node.was_attribute_touched(&["layer_index"]) {
                    children_order_changed = true;
                }
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
            node.in_touched_attributes.clear();
            for attribute in node.out_touched_attributes.iter() {
                let observable = NodeOrObservableIdentifier::NodeAttribute(self.global_node_id, attribute);
                if self.node_hierarchy.observer_channels.contains_key(&observable) {
                    let mut messenger = Messenger::new(
                        &message::PROPERTY_CHANGED,
                        hash_map! {
                            "attribute" => Value::Attribute(attribute),
                            "value" => node.get_attribute(attribute),
                        },
                    );
                    messenger.propagation_direction = PropagationDirection::Observers(observable);
                    self.node_hierarchy.messenger_stack.push((0, messenger));
                }
            }
        } else if !node.out_touched_attributes.is_empty() {
            // println!("self={} touched_attributes={:?} ", self.global_node_id, node.out_touched_attributes);
            reconfigure_node!(self.node_hierarchy, node);
            node.in_touched_attributes = &node.in_touched_attributes | &node.out_touched_attributes;
        }
        if let Some(global_parent_id) = node.parent {
            if !node.out_touched_attributes.is_empty() {
                let mut parent_node = self.node_hierarchy.nodes.get(&global_parent_id).unwrap().borrow_mut();
                // println!("parent={} touched_attributes={:?} ", global_parent_id, node.out_touched_attributes);
                reconfigure_node!(self.node_hierarchy, parent_node);
            }
        } else {
            node.out_touched_attributes.clear();
        }
        for global_child_id in node.children.values() {
            let mut child_node = self.node_hierarchy.nodes.get(global_child_id).unwrap().borrow_mut();
            if !child_node.in_touched_attributes.is_empty() {
                // println!("child={} touched_attributes={:?} ", global_child_id, child_node.in_touched_attributes);
                reconfigure_node!(self.node_hierarchy, child_node);
            }
            child_node.out_touched_attributes.clear();
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
            if let Some(global_parent_id) = node.parent {
                global_node_id = global_parent_id;
            } else {
                return self.node_hierarchy.theme_properties.get(attribute).cloned().unwrap_or(Value::Void);
            }
        }
    }

    /// Returns true if one of the given `attributes` was touched by either the parent, an animation or its self
    pub fn was_attribute_touched(&self, attributes: &[&'static str]) -> bool {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        for attribute in attributes {
            if node.in_touched_attributes.contains(attribute) {
                return true;
            }
        }
        false
    }

    /// Lets the parent know that the given `attribute` was touched
    pub fn touch_attribute(&mut self, attribute: &'static str) {
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        node.out_touched_attributes.insert(attribute);
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
        if node.properties.get(attribute) == Some(&value) {
            return false;
        }
        node.properties.insert(attribute, value);
        node.out_touched_attributes.insert(attribute);
        true
    }

    /// Have the [Value] of the property by the given `attribute` be animated
    pub fn set_attribute_animated(&mut self, attribute: &'static str, value: Value, start_time: f64, duration: f64) -> bool {
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        node.set_attribute_animated(attribute, value, start_time, duration)
    }

    /// Sets "proposed_half_extent" and "half_extent"
    pub fn set_half_extent(&mut self, half_extent: SafeFloat<f32, 2>) {
        self.set_attribute("half_extent", Value::Float2(half_extent));
        self.set_attribute("proposed_half_extent", Value::Float2(half_extent));
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

    /// Returns how many child [Node]s this [Node] has
    pub fn get_number_of_children(&self) -> usize {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.children.len()
    }

    /// Iterates the child [Node]s of this [Node]
    pub fn iter_children<F: FnMut(&NodeOrObservableIdentifier, &Node)>(&self, mut callback: F) {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        for (local_child_id, global_child_id) in node.children.iter() {
            let child_node = self.node_hierarchy.nodes.get(global_child_id).unwrap().borrow();
            callback(local_child_id, &child_node);
        }
    }

    /// Used to read properties of a specific child [Node]
    pub fn inspect_child<R, F: FnOnce(&Node) -> R>(&self, local_child_id: &NodeOrObservableIdentifier, callback: F) -> Option<R> {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.children
            .get(local_child_id)
            .map(|global_child_id| callback(&self.node_hierarchy.nodes.get(global_child_id).unwrap().borrow()))
    }

    /// Adds and links a given child [Node]
    pub fn add_child(&mut self, local_child_id: NodeOrObservableIdentifier, child_node: Rc<RefCell<Node>>) {
        let _global_child_id = self.node_hierarchy.insert_node(child_node, Some((self.global_node_id, local_child_id)));
    }

    /// Removes and unlinks a given child [Node]
    pub fn remove_child(&mut self, local_child_id: NodeOrObservableIdentifier) -> Option<Rc<RefCell<Node>>> {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        let global_child_id = node.children.get(&local_child_id).cloned();
        drop(node);
        global_child_id.map(|global_child_id| self.node_hierarchy.delete_node(global_child_id))
    }

    /// Adds, modifies or removes a child [Node]
    ///
    /// If `callback` is `Some` the child [Node] is added if it did not exist before.
    /// Otherwise, if `callback` is `None` the child [Node] is removed if it did exist.
    pub fn configure_child<F: FnOnce(&mut Node)>(&mut self, local_child_id: NodeOrObservableIdentifier, callback: Option<F>) {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        if let Some(global_child_id) = node.children.get(&local_child_id).cloned() {
            drop(node);
            if let Some(callback) = callback {
                let mut child_node = self.node_hierarchy.nodes.get(&global_child_id).unwrap().borrow_mut();
                callback(&mut child_node);
            } else {
                self.remove_child(local_child_id);
            }
        } else if let Some(callback) = callback {
            drop(node);
            let mut child_node = Node::default();
            callback(&mut child_node);
            self.add_child(local_child_id, Rc::new(RefCell::new(child_node)));
        }
    }

    /// Sets the set of `oberverables` this [Node] should be oberving
    ///
    /// If `unsubscribe_others` is `true` all other obervers of the given `oberverables` are unregistered.
    pub fn observe(&mut self, mut observables: HashSet<NodeOrObservableIdentifier>, unsubscribe_others: bool) {
        if unsubscribe_others {
            self.node_hierarchy.unsubscribe_observers(observables.iter());
        }
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        if node.parent.is_none() {
            observables.insert(NodeOrObservableIdentifier::Named("root"));
        }
        drop(node);
        self.node_hierarchy.unsubscribe_observer(self.global_node_id, Some(&observables));
        self.node_hierarchy.subscribe_observer(self.global_node_id, observables.iter());
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        node.observes = observables;
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

    /// Helper to focus this [Node]
    pub fn pointer_and_button_input_focus(&mut self, messenger: &Messenger) {
        let input_source = *match messenger.get_attribute("input_source") {
            Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::ButtonInput(input_source)) => input_source,
            Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::AxisInput(input_source)) => input_source,
            Value::NodeOrObservableIdentifier(NodeOrObservableIdentifier::PointerInput(input_source)) => input_source,
            _ => panic!(),
        };
        if messenger.get_attribute("pressed_or_released") == &Value::Boolean(true) {
            self.observe(
                hash_set! {NodeOrObservableIdentifier::PointerInput(input_source), NodeOrObservableIdentifier::ButtonInput(input_source)},
                true,
            );
        } else {
            self.observe(hash_set! {NodeOrObservableIdentifier::ButtonInput(input_source)}, true);
        }
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
    messenger_stack: Vec<(GlobalNodeIdentifier, Messenger)>,
    reconfigure_queue: BinaryHeap<ReconfigurePriority>,
    observer_channels: HashMap<NodeOrObservableIdentifier, HashSet<GlobalNodeIdentifier>>,
    last_animation_time: f64,
    /// Global properties accessible to all [Node]s
    pub theme_properties: HashMap<&'static str, Value>,
}

impl NodeHierarchy {
    /// Inserts and links the given [Node], then returns its [GlobalNodeIdentifier]
    pub fn insert_node(
        &mut self,
        node: Rc<RefCell<Node>>,
        parent_link: Option<(GlobalNodeIdentifier, NodeOrObservableIdentifier)>,
    ) -> GlobalNodeIdentifier {
        let global_node_id = self.next_node_id;
        self.next_node_id += 1;
        let mut borrowed_node = node.borrow_mut();
        borrowed_node.global_id = global_node_id;
        assert!(borrowed_node.parent.is_none());
        if let Some((global_parent_id, local_child_id)) = parent_link {
            let mut parent = self.nodes.get(&global_parent_id).unwrap().borrow_mut();
            parent.touch_attribute("child_count");
            parent.children.insert(local_child_id, global_node_id);
            borrowed_node.local_id = local_child_id;
            borrowed_node.parent = Some(global_parent_id);
            borrowed_node.nesting_depth = parent.nesting_depth + 1;
            borrowed_node.touch_attribute("parent");
        } else {
            borrowed_node.local_id = NodeOrObservableIdentifier::Named("root");
            borrowed_node.observes.insert(NodeOrObservableIdentifier::Named("root"));
        }
        self.subscribe_observer(global_node_id, borrowed_node.observes.iter());
        reconfigure_node!(self, borrowed_node);
        drop(borrowed_node);
        self.nodes.insert(global_node_id, node);
        global_node_id
    }

    /// Removes and unlinks the given [Node], then returns it
    pub fn delete_node(&mut self, global_node_id: GlobalNodeIdentifier) -> Rc<RefCell<Node>> {
        self.unsubscribe_observer(global_node_id, None);
        let mut node = self.nodes.get(&global_node_id).unwrap().borrow_mut();
        if let Some(global_parent_id) = node.parent {
            node.touch_attribute("parent");
            let local_child_id = node.local_id;
            drop(node);
            let mut parent = self.nodes.get(&global_parent_id).unwrap().borrow_mut();
            parent.touch_attribute("child_count");
            parent.children.remove(&local_child_id);
        } else {
            drop(node);
        }
        let node = self.nodes.remove(&global_node_id).unwrap();
        for (_local_child_id, global_child_id) in node.borrow().children.iter() {
            self.delete_node(*global_child_id);
        }
        node
    }

    fn subscribe_observer<'a, I: IntoIterator<Item = &'a NodeOrObservableIdentifier>>(
        &mut self,
        global_node_id: GlobalNodeIdentifier,
        observables: I,
    ) {
        for observable in observables {
            self.observer_channels
                .entry(*observable)
                .or_insert_with(HashSet::new)
                .insert(global_node_id);
        }
    }

    fn unsubscribe_observer(&mut self, global_node_id: GlobalNodeIdentifier, except_observables: Option<&HashSet<NodeOrObservableIdentifier>>) {
        let node = self.nodes.get(&global_node_id).unwrap().borrow();
        for observable in node.observes.iter() {
            if except_observables.map(|observables| observables.contains(observable)).unwrap_or(false) {
                continue;
            }
            match self.observer_channels.entry(*observable) {
                hash_map::Entry::Occupied(mut entry) => {
                    entry.get_mut().remove(&global_node_id);
                    if entry.get().is_empty() {
                        entry.remove_entry();
                    }
                }
                hash_map::Entry::Vacant(_entry) => {}
            }
        }
    }

    /// Unsubscribes all observers of the given observables
    pub fn unsubscribe_observers<'a, I: IntoIterator<Item = &'a NodeOrObservableIdentifier>>(&mut self, observables: I) {
        for observable in observables {
            match self.observer_channels.entry(*observable) {
                hash_map::Entry::Occupied(entry) => {
                    for global_node_id in entry.get() {
                        let mut node = self.nodes.get(global_node_id).unwrap().borrow_mut();
                        node.observes.remove(observable);
                    }
                    entry.remove_entry();
                }
                hash_map::Entry::Vacant(_entry) => {}
            }
        }
    }

    /// Sends a [Messenger] to a set of UI nodes observing the given observable.
    pub fn notify_observers(&mut self, observable: NodeOrObservableIdentifier, mut messenger: Messenger) {
        messenger.propagation_direction = PropagationDirection::Observers(observable);
        self.messenger_stack.push((0, messenger));
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
            let messenger = Messenger::new(&message::RECONFIGURE, hash_map! {});
            self.messenger_stack.push((global_node_id, messenger));
        }
        while let Some((global_node_id, mut messenger)) = self.messenger_stack.pop() {
            println!(
                "process_messenger global_node_id={} {} {:?}",
                global_node_id, messenger.behavior.label, messenger.propagation_direction
            );
            match messenger.propagation_direction {
                PropagationDirection::None => panic!(),
                PropagationDirection::Itself => {
                    self.invoke_handler(global_node_id, &mut messenger);
                }
                PropagationDirection::Parent => {
                    let node = self.nodes.get(&global_node_id).unwrap().borrow();
                    let global_parent_id = node.parent;
                    let local_child_id = node.local_id;
                    if let Some(global_parent_id) = global_parent_id {
                        let half_extent = self.nodes.get(&global_parent_id).unwrap().borrow().get_half_extent(false);
                        let (invoke, _stop) = (messenger.behavior.update_at_node_edge)(&mut messenger, half_extent, &node, Some(local_child_id));
                        drop(node);
                        if invoke {
                            self.invoke_handler(global_parent_id, &mut messenger);
                        }
                    }
                }
                PropagationDirection::Child(local_child_id) => {
                    let node = self.nodes.get(&global_node_id).unwrap().borrow();
                    let global_child_id = node.children.get(&local_child_id).cloned();
                    drop(node);
                    let mut reflect = true;
                    if let Some(global_child_id) = global_child_id {
                        let node = self.nodes.get(&global_child_id).unwrap().borrow();
                        let (invoke, stop) = (messenger.behavior.update_at_node_edge)(&mut messenger, node.get_half_extent(false), &node, None);
                        drop(node);
                        if invoke {
                            reflect &= self.invoke_handler(global_child_id, &mut messenger);
                        }
                        if !stop {
                            (messenger.behavior.reset_at_node_edge)(&mut messenger, false);
                        }
                    }
                    if reflect && (messenger.behavior.do_reflect)(&mut messenger) {
                        self.invoke_handler(global_node_id, &mut messenger);
                    }
                }
                PropagationDirection::Siblings => {
                    let node = self.nodes.get(&global_node_id).unwrap().borrow();
                    let global_parent_id = node.parent;
                    let local_child_id = node.local_id;
                    if let Some(global_parent_id) = global_parent_id {
                        let half_extent = self.nodes.get(&global_parent_id).unwrap().borrow().get_half_extent(false);
                        (messenger.behavior.update_at_node_edge)(&mut messenger, half_extent, &node, Some(local_child_id));
                        let ordered_children = node.ordered_children.clone(); // TODO: Efficency
                        drop(node);
                        for global_child_id in ordered_children.iter() {
                            if *global_child_id == global_node_id {
                                continue;
                            }
                            let node = self.nodes.get(global_child_id).unwrap().borrow();
                            let (invoke, stop) = (messenger.behavior.update_at_node_edge)(&mut messenger, node.get_half_extent(false), &node, None);
                            drop(node);
                            if invoke {
                                self.invoke_handler(*global_child_id, &mut messenger);
                            }
                            if stop {
                                break;
                            }
                            (messenger.behavior.reset_at_node_edge)(&mut messenger, false);
                        }
                    }
                }
                PropagationDirection::Children => {
                    let node = self.nodes.get(&global_node_id).unwrap().borrow();
                    let ordered_children = node.ordered_children.clone(); // TODO: Efficency
                    drop(node);
                    let mut reflect = true;
                    for global_child_id in ordered_children.iter().rev() {
                        let node = self.nodes.get(global_child_id).unwrap().borrow();
                        let (invoke, stop) = (messenger.behavior.update_at_node_edge)(&mut messenger, node.get_half_extent(false), &node, None);
                        drop(node);
                        if invoke {
                            reflect &= self.invoke_handler(*global_child_id, &mut messenger);
                        }
                        if stop {
                            break;
                        }
                        (messenger.behavior.reset_at_node_edge)(&mut messenger, false);
                    }
                    if reflect && (messenger.behavior.do_reflect)(&mut messenger) {
                        self.invoke_handler(global_node_id, &mut messenger);
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
                        (messenger.behavior.do_reflect)(&mut messenger);
                    }
                    for global_node_id in global_node_ids.iter() {
                        let mut invoke_handler = true;
                        let mut parents_path = vec![*global_node_id];
                        {
                            let mut global_node_id = *global_node_id;
                            while let Some(global_parent_id) = self.nodes.get(&global_node_id).unwrap().borrow().parent {
                                global_node_id = global_parent_id;
                                parents_path.push(global_parent_id);
                            }
                            for global_node_id in parents_path.iter().rev() {
                                let node = self.nodes.get(global_node_id).unwrap().borrow();
                                let (invoke, _stop) =
                                    (messenger.behavior.update_at_node_edge)(&mut messenger, node.get_half_extent(false), &node, None);
                                if !invoke && !is_captured_observable {
                                    invoke_handler = false;
                                    break;
                                }
                            }
                        }
                        if invoke_handler || is_captured_observable {
                            self.invoke_handler(*global_node_id, &mut messenger);
                        }
                        (messenger.behavior.reset_at_node_edge)(&mut messenger, true);
                    }
                }
            }
            while let Some(ReconfigurePriority { global_node_id, .. }) = self.reconfigure_queue.pop() {
                let messenger = Messenger::new(&message::RECONFIGURE, hash_map! {});
                self.messenger_stack.push((global_node_id, messenger));
            }
        }
    }

    fn instanciate_node_rendering(
        &mut self,
        renderer: &mut Renderer,
        global_node_id: GlobalNodeIdentifier,
        layer_range: Range<usize>,
        clip_depth: u8,
    ) {
        let node = self.nodes.get(&global_node_id).unwrap().borrow();
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
                let motor: ppga2d::Motor = match_option!(node.get_attribute("absolute_motor"), Value::Float4)
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
        renderer.instanciate_node(&node, layer_range, clip_depth, &model_projection_matrix);
    }

    fn prepare_node_rendering(
        &mut self,
        renderer: &mut Renderer,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        global_node_id: GlobalNodeIdentifier,
        parent_layer_range: &mut Range<usize>,
        mut clip_depth: u8,
    ) {
        let node = self.nodes.get(&global_node_id).unwrap().borrow();
        if node.get_attribute("dormant") == Value::Boolean(true) || node.get_attribute("absolute_opacity") == Value::Float1(0.0.into()) {
            return;
        }
        drop(node);

        let mut messenger = Messenger::new(&message::PREPARE_RENDERING, hash_map! {});
        if !self.invoke_handler(global_node_id, &mut messenger) {
            let messenger = self.messenger_stack.pop().unwrap().1;
            assert_eq!(messenger.behavior.label, "UpdateRendering");
            if let Value::Rendering(rendering) = messenger.get_attribute("rendering") {
                let mut node = self.nodes.get(&global_node_id).unwrap().borrow_mut();
                renderer.set_node_rendering(device, queue, &mut node, rendering).unwrap();
            }
        }

        let node = self.nodes.get(&global_node_id).unwrap().borrow();
        let has_clip_shape = node.clip_shape.is_some();
        let mut child_layer_range = parent_layer_range.start + node.colored_shapes.len()..0;
        if has_clip_shape {
            child_layer_range.start += 1;
            clip_depth += 1;
        }
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
                    self.instanciate_node_rendering(renderer, *global_child_id, child_layer_range.clone(), clip_depth);
                }
                prev_child_index = child_index;
                child_layer_range.start = child_layer_range.end;
            }
            self.prepare_node_rendering(renderer, device, queue, *global_child_id, &mut child_layer_range, clip_depth);
        }
        for global_child_id in ordered_children[prev_child_index..].iter() {
            self.instanciate_node_rendering(renderer, *global_child_id, child_layer_range.clone(), clip_depth);
        }
        parent_layer_range.end = parent_layer_range.end.max(child_layer_range.end + if has_clip_shape { 1 } else { 0 });
    }

    /// Preparation step to be called before [Renderer::encode_commands]
    ///
    /// `current_time` is measured in seconds.
    pub fn prepare_rendering(&mut self, renderer: &mut Renderer, device: &wgpu::Device, queue: &wgpu::Queue, current_time: f64) {
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
            self.prepare_node_rendering(renderer, device, queue, global_node_id, &mut layer_range, 0);
            self.instanciate_node_rendering(renderer, global_node_id, layer_range.clone(), 0);
            layer_range.start = layer_range.end;
        }
        renderer.update_instance_buffers(device, queue);
    }
}
