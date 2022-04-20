use crate::{
    hash_map, match_option,
    safe_float::SafeFloat,
    ui::{
        message::{self, Messenger, PropagationDirection},
        renderer::{FrameContext, Renderer},
        wrapped_values::Value,
        GlobalNodeIdentifier, Node, NodeOrObservableIdentifier,
    },
    utils::{matrix_multiplication, motor2d_to_motor3d, motor3d_to_mat4},
};
use geometric_algebra::{ppga2d, One};
use std::{
    cell::RefCell,
    collections::{hash_map, HashMap, HashSet},
    rc::Rc,
};

/*#[derive(PartialEq, Eq)]
struct NodeLayer {
    index: usize,
    global_node_id: GlobalNodeIdentifier,
}
impl PartialOrd for NodeLayer {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for NodeLayer {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}
let mut children = node
    .children
    .iter()
    .map(|(_local_child_id, global_child_id)| NodeLayer { index: self.nodes.get(global_child_id).unwrap().borrow().layer_index, global_node_id: *global_child_id })
    .collect::<std::collections::BinaryHeap<NodeLayer>>();*/

pub struct NodeMessengerContext<'a> {
    global_node_id: GlobalNodeIdentifier,
    nodes: &'a mut HashMap<GlobalNodeIdentifier, Rc<RefCell<Node>>>,
    theme_properties: &'a HashMap<&'static str, Value>,
}

impl<'a> NodeMessengerContext<'a> {
    fn invoke_handler(&mut self, messenger_stack: &mut Vec<(GlobalNodeIdentifier, Messenger)>, messenger: &mut Messenger) -> bool {
        let node = self.nodes.get(&self.global_node_id).unwrap().borrow();
        let dormant = node
            .properties
            .get("dormant")
            .map(|value| *match_option!(value, Value::Boolean).unwrap())
            .unwrap_or(false);
        if dormant {
            return true;
        }
        let local_id = node.local_id;
        let messenger_handler = node.messenger_handler;
        drop(node);
        let mut messengers = messenger_handler(self, messenger);
        let reflect = messengers.is_empty();
        let mut node = self.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        if !node.touched_properties.is_empty() {
            let mut touched_properties = HashSet::new();
            std::mem::swap(&mut touched_properties, &mut node.touched_properties);
            messengers.push(Messenger::new(
                &message::PROPERTIES_CHANGED,
                hash_map! {
                    "child_id" => Value::NodeOrObservableIdentifier(local_id),
                    "attributes" => Value::Attributes(touched_properties),
                },
            ));
        }
        messenger_stack.append(
            &mut messengers
                .into_iter()
                .map(|messenger| (self.global_node_id, messenger))
                .collect::<Vec<_>>(),
        );
        reflect
    }

    pub fn derive_attribute(&self, attribute: &'static str) -> Value {
        let mut global_node_id = self.global_node_id;
        loop {
            let node = self.nodes.get(&global_node_id).unwrap().borrow();
            if let Some(value) = node.properties.get(attribute) {
                return value.clone();
            }
            if let Some(global_parent_id) = node.parent {
                global_node_id = global_parent_id;
            } else {
                return self.theme_properties.get(attribute).cloned().unwrap_or(Value::Void);
            }
        }
    }

    pub fn get_attribute(&self, attribute: &'static str) -> Value {
        let node = self.nodes.get(&self.global_node_id).unwrap().borrow();
        node.properties.get(attribute).cloned().unwrap_or(Value::Void)
    }

    pub fn set_attribute_privately(&mut self, attribute: &'static str, value: Value) {
        let mut node = self.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        node.properties.insert(attribute, value);
    }

    pub fn set_attribute(&mut self, attribute: &'static str, value: Value) -> bool {
        let mut node = self.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        if node.properties.get(attribute) == Some(&value) {
            return false;
        }
        node.properties.insert(attribute, value);
        node.touched_properties.insert(attribute);
        true
    }

    pub fn touch_attribute(&mut self, attribute: &'static str) {
        let mut node = self.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        node.touched_properties.insert(attribute);
    }

    pub fn set_half_extent(&mut self, half_extent: SafeFloat<f32, 2>) {
        self.set_attribute("half_extent", Value::Float2(half_extent));
        self.set_attribute("proposed_half_extent", Value::Float2(half_extent));
    }

    pub fn get_half_extent(&self, proposed: bool) -> SafeFloat<f32, 2> {
        let node = self.nodes.get(&self.global_node_id).unwrap().borrow();
        node.get_half_extent(proposed)
    }

    pub fn does_observe(&self, observable: &NodeOrObservableIdentifier) -> bool {
        let node = self.nodes.get(&self.global_node_id).unwrap().borrow();
        node.observes.contains(observable)
    }

    pub fn get_number_of_children(&self) -> usize {
        let node = self.nodes.get(&self.global_node_id).unwrap().borrow();
        node.children.len()
    }

    pub fn iter_children<F: FnMut(&NodeOrObservableIdentifier, &Node)>(&self, mut callback: F) {
        let node = self.nodes.get(&self.global_node_id).unwrap().borrow();
        for (local_child_id, global_child_id) in node.children.iter() {
            let child_node = self.nodes.get(global_child_id).unwrap().borrow();
            callback(local_child_id, &child_node);
        }
    }

    pub fn inspect_child<R, F: FnOnce(&Node) -> R>(&self, local_child_id: &NodeOrObservableIdentifier, callback: F) -> Option<R> {
        let node = self.nodes.get(&self.global_node_id).unwrap().borrow();
        node.children
            .get(local_child_id)
            .map(|global_child_id| callback(&self.nodes.get(global_child_id).unwrap().borrow()))
    }

    pub fn configure_child<F: FnOnce(&mut Node)>(
        &self,
        messengers: &mut Vec<Messenger>,
        local_child_id: NodeOrObservableIdentifier,
        callback: Option<F>,
    ) {
        let node = self.nodes.get(&self.global_node_id).unwrap().borrow();
        let child_node = if let Some(global_child_id) = node.children.get(&local_child_id) {
            let child_node_rc = self.nodes.get(global_child_id).unwrap();
            if let Some(callback) = callback {
                let mut child_node = child_node_rc.borrow_mut();
                callback(&mut child_node);
                if child_node.touched_properties.is_empty() {
                    return;
                }
                child_node_rc.clone()
            } else {
                messengers.push(Messenger::new(
                    &message::CONFIGURE_CHILD,
                    hash_map! {
                        "local_id" => Value::NodeOrObservableIdentifier(local_child_id),
                        "node" => Value::Void,
                    },
                ));
                return;
            }
        } else if let Some(callback) = callback {
            let mut child_node = Node::default();
            callback(&mut child_node);
            Rc::new(RefCell::new(child_node))
        } else {
            return;
        };
        messengers.push(Messenger::new(
            &message::CONFIGURE_CHILD,
            hash_map! {
                "local_id" => Value::NodeOrObservableIdentifier(local_child_id),
                "node" => Value::Node(child_node),
            },
        ));
    }

    pub fn update_rendering_helper(&mut self, prepare_rendering: &Messenger) -> Messenger {
        let motor: ppga2d::Motor = (*match_option!(prepare_rendering.get_attribute("motor"), Value::Float4).unwrap()).into();
        let scale: f32 = match_option!(prepare_rendering.get_attribute("scale"), Value::Float1).unwrap().unwrap();
        let change_rendering = self.get_attribute("is_rendering_dirty") == Value::Boolean(true);
        self.set_attribute_privately("is_rendering_dirty", Value::Boolean(false));
        let motor3d = motor2d_to_motor3d(&motor);
        let mut model_matrix = motor3d_to_mat4(&motor3d);
        model_matrix[0].g0[0] *= scale;
        model_matrix[1].g0[1] *= scale;
        model_matrix[2].g0[2] *= scale;
        model_matrix[3].g0[0] *= scale;
        model_matrix[3].g0[1] *= scale;
        model_matrix[3].g0[2] *= scale;
        Messenger::new(
            &message::UPDATE_RENDERING,
            hash_map! {
                "model_matrix" => Value::Float4x4([
                    model_matrix[0].into(),
                    model_matrix[1].into(),
                    model_matrix[2].into(),
                    model_matrix[3].into(),
                ]),
                "rendering" => if change_rendering { Value::Boolean(true) } else { Value::Void },
            },
        )
    }
}

/// The hierarchy of UI nodes.
#[derive(Default)]
pub struct NodeHierarchy {
    next_node_id: GlobalNodeIdentifier,
    nodes: HashMap<GlobalNodeIdentifier, Rc<RefCell<Node>>>,
    observer_channels: HashMap<NodeOrObservableIdentifier, HashSet<GlobalNodeIdentifier>>,
    pub theme_properties: HashMap<&'static str, Value>,
}

impl NodeHierarchy {
    fn insert_node(
        &mut self,
        parent_link: Option<(GlobalNodeIdentifier, NodeOrObservableIdentifier)>,
        node: Rc<RefCell<Node>>,
    ) -> GlobalNodeIdentifier {
        let global_node_id = self.next_node_id;
        self.next_node_id += 1;
        let mut borrowed_node = node.borrow_mut();
        borrowed_node.global_id = global_node_id;
        assert!(borrowed_node.parent.is_none());
        if let Some((global_parent_id, local_child_id)) = parent_link {
            borrowed_node.local_id = local_child_id;
            borrowed_node.parent = Some(global_parent_id);
            let mut parent = self.nodes.get(&global_parent_id).unwrap().borrow_mut();
            parent.children.insert(local_child_id, global_node_id);
        } else {
            borrowed_node.local_id = NodeOrObservableIdentifier::Named("root");
            borrowed_node.observes.insert(NodeOrObservableIdentifier::Named("root"));
        }
        for observable_topic in borrowed_node.observes.iter() {
            self.observer_channels
                .entry(*observable_topic)
                .or_insert_with(HashSet::new)
                .insert(global_node_id);
        }
        drop(borrowed_node);
        self.nodes.insert(global_node_id, node);
        global_node_id
    }

    pub fn insert_and_configure_node(
        &mut self,
        parent_link: Option<(GlobalNodeIdentifier, NodeOrObservableIdentifier)>,
        node: Node,
    ) -> GlobalNodeIdentifier {
        let global_node_id = self.insert_node(parent_link, Rc::new(RefCell::new(node)));
        self.process_messengers(
            vec![(global_node_id, Messenger::new(&message::RECONFIGURE, hash_map! {}))],
            None,
            None,
            None,
            None,
        );
        global_node_id
    }

    pub fn delete_node(&mut self, global_node_id: GlobalNodeIdentifier) {
        let node = self.nodes.get(&global_node_id).unwrap().borrow();
        for observable in node.observes.iter() {
            self.observer_channels.get_mut(observable).unwrap().remove(&global_node_id);
        }
        if let Some(global_parent_id) = node.parent {
            let mut parent = self.nodes.get(&global_parent_id).unwrap().borrow_mut();
            parent.children.remove(&node.local_id);
        }
        drop(node);
        let node = self.nodes.remove(&global_node_id).unwrap();
        for (_local_child_id, global_child_id) in node.borrow().children.iter() {
            self.delete_node(*global_child_id);
        }
    }

    /// Sends a [Messenger] to a set of UI nodes observing the given observable.
    pub fn notify_observers(
        &mut self,
        renderer: Option<&mut Renderer>,
        device: Option<&wgpu::Device>,
        queue: Option<&wgpu::Queue>,
        frame_context: Option<&mut FrameContext>,
        observable: NodeOrObservableIdentifier,
        mut messenger: Messenger,
    ) {
        messenger.propagation_direction = PropagationDirection::Observers(observable);
        self.process_messengers(vec![(0, messenger)], renderer, device, queue, frame_context);
    }

    fn process_messengers(
        &mut self,
        mut messenger_stack: Vec<(GlobalNodeIdentifier, Messenger)>,
        mut renderer: Option<&mut Renderer>,
        device: Option<&wgpu::Device>,
        queue: Option<&wgpu::Queue>,
        mut frame_context: Option<&mut FrameContext>,
    ) {
        while let Some((global_node_id, mut messenger)) = messenger_stack.pop() {
            println!(
                "process_messenger {} {} {:?}",
                global_node_id, messenger.behavior.label, messenger.propagation_direction
            );
            match messenger.propagation_direction {
                PropagationDirection::None => panic!(),
                PropagationDirection::Return => match messenger.behavior.label {
                    "Reconfigure" => {
                        let mut node = self.nodes.get(&global_node_id).unwrap().borrow_mut();
                        if node.configuration_in_process {
                            continue;
                        }
                        node.configuration_in_process = true;
                        let mut touched_properties = HashSet::new();
                        std::mem::swap(&mut touched_properties, &mut node.touched_properties);
                        let mut messenger = Messenger::new(
                            &message::RECONFIGURE,
                            hash_map! {
                                "attributes" => Value::Attributes(touched_properties),
                            },
                        );
                        drop(node);
                        NodeMessengerContext {
                            global_node_id,
                            nodes: &mut self.nodes,
                            theme_properties: &self.theme_properties,
                        }
                        .invoke_handler(&mut messenger_stack, &mut messenger);
                    }
                    "Configured" => {
                        let mut node = self.nodes.get(&global_node_id).unwrap().borrow_mut();
                        node.configuration_in_process = false;
                        let mut ordered_children = node
                            .children
                            .iter()
                            .filter(|(_local_child_id, global_child_id)| **global_child_id != global_node_id)
                            .map(|(_local_child_id, global_child_id)| {
                                let child_node = self.nodes.get(global_child_id).unwrap().borrow();
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
                    "ConfigureChild" => {
                        let node = self.nodes.get(&global_node_id).unwrap().borrow();
                        let local_child_id = *match_option!(messenger.get_attribute("local_id"), Value::NodeOrObservableIdentifier).unwrap();
                        let global_child_id = node.children.get(&local_child_id).cloned();
                        drop(node);
                        if let Some(mut global_child_id) = global_child_id {
                            if let Value::Node(new_child_node) = messenger.get_attribute("node") {
                                if !Rc::ptr_eq(new_child_node, self.nodes.get(&global_child_id).unwrap()) {
                                    self.delete_node(global_child_id);
                                    global_child_id = self.insert_node(Some((global_node_id, local_child_id)), new_child_node.clone());
                                }
                                messenger_stack.push((global_child_id, Messenger::new(&message::RECONFIGURE, hash_map! {})));
                            } else {
                                self.delete_node(global_child_id);
                            }
                        } else if let Value::Node(new_child_node) = messenger.get_attribute("node") {
                            let global_child_id = self.insert_node(Some((global_node_id, local_child_id)), new_child_node.clone());
                            messenger_stack.push((global_child_id, Messenger::new(&message::RECONFIGURE, hash_map! {})));
                        } else {
                            panic!("Can not delete what does not exist");
                        }
                    }
                    "Observe" => {
                        let mut node = self.nodes.get(&global_node_id).unwrap().borrow_mut();
                        let mut observables = match_option!(messenger.get_attribute("observables"), Value::NodeOrObservableIdentifiers)
                            .unwrap()
                            .clone();
                        if node.parent.is_none() {
                            observables.insert(NodeOrObservableIdentifier::Named("root"));
                        }
                        for observable in node.observes.iter() {
                            if !observables.contains(observable) {
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
                        for observable in observables.iter() {
                            self.observer_channels
                                .entry(*observable)
                                .or_insert_with(HashSet::new)
                                .insert(global_node_id);
                        }
                        node.observes = observables;
                    }
                    "UnsubscribeObservers" => {
                        let observables = match_option!(messenger.get_attribute("observables"), Value::NodeOrObservableIdentifiers)
                            .unwrap()
                            .clone();
                        for observable in observables {
                            match self.observer_channels.entry(observable) {
                                hash_map::Entry::Occupied(entry) => {
                                    for global_node_id in entry.get() {
                                        let mut node = self.nodes.get(global_node_id).unwrap().borrow_mut();
                                        node.observes.remove(&observable);
                                    }
                                    entry.remove_entry();
                                }
                                hash_map::Entry::Vacant(_entry) => {}
                            }
                        }
                    }
                    "UpdateRendering" => {
                        let mut node = self.nodes.get(&global_node_id).unwrap().borrow_mut();
                        if let Value::Rendering(rendering) = messenger.get_attribute("rendering") {
                            renderer
                                .as_ref()
                                .unwrap()
                                .set_node_rendering(device.as_ref().unwrap(), queue.as_ref().unwrap(), &mut node, rendering)
                                .unwrap();
                        }
                        let model_matrix = match_option!(messenger.get_attribute("model_matrix"), Value::Float4x4).unwrap();
                        let model_matrix = [
                            model_matrix[0].into(),
                            model_matrix[1].into(),
                            model_matrix[2].into(),
                            model_matrix[3].into(),
                        ];
                        let model_projection_matrix = matrix_multiplication(&renderer.as_ref().unwrap().projection_matrix, &model_matrix);
                        renderer.as_mut().unwrap().instanciate_node(&mut node, &model_projection_matrix);
                    }
                    "RenderAndClip" => {
                        let node = self.nodes.get(&global_node_id).unwrap().borrow();
                        renderer.as_mut().unwrap().render_node(frame_context.as_mut().unwrap(), &node);
                        renderer.as_mut().unwrap().push_clipping_of_node(frame_context.as_mut().unwrap(), &node);
                    }
                    "RenderUnclip" => {
                        let node = self.nodes.get(&global_node_id).unwrap().borrow();
                        renderer.as_mut().unwrap().pop_clipping_of_node(frame_context.as_mut().unwrap(), &node);
                    }
                    _ => {
                        panic!();
                    }
                },
                PropagationDirection::Parent => {
                    let node = self.nodes.get(&global_node_id).unwrap().borrow();
                    let global_parent_id = node.parent;
                    let local_child_id = node.local_id;
                    drop(node);
                    if let Some(global_parent_id) = global_parent_id {
                        let node = self.nodes.get(&global_parent_id).unwrap().borrow();
                        let (invoke, _stop) = (messenger.behavior.update_at_node_edge)(&mut messenger, &node, Some(local_child_id));
                        drop(node);
                        if invoke {
                            NodeMessengerContext {
                                global_node_id: global_parent_id,
                                nodes: &mut self.nodes,
                                theme_properties: &self.theme_properties,
                            }
                            .invoke_handler(&mut messenger_stack, &mut messenger);
                        }
                    }
                }
                PropagationDirection::Siblings => {
                    let node = self.nodes.get(&global_node_id).unwrap().borrow();
                    let global_parent_id = node.parent;
                    let local_child_id = node.local_id;
                    drop(node);
                    if let Some(global_parent_id) = global_parent_id {
                        let node = self.nodes.get(&global_parent_id).unwrap().borrow();
                        (messenger.behavior.update_at_node_edge)(&mut messenger, &node, Some(local_child_id));
                        let ordered_children = node.ordered_children.clone(); // TODO: Efficency
                        drop(node);
                        for global_child_id in ordered_children.iter() {
                            if *global_child_id == global_node_id {
                                continue;
                            }
                            let node = self.nodes.get(global_child_id).unwrap().borrow();
                            let (invoke, stop) = (messenger.behavior.update_at_node_edge)(&mut messenger, &node, None);
                            drop(node);
                            if invoke {
                                NodeMessengerContext {
                                    global_node_id: *global_child_id,
                                    nodes: &mut self.nodes,
                                    theme_properties: &self.theme_properties,
                                }
                                .invoke_handler(&mut messenger_stack, &mut messenger);
                                (messenger.behavior.reset_at_node_edge)(&mut messenger);
                            }
                            if stop {
                                break;
                            }
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
                        let (invoke, stop) = (messenger.behavior.update_at_node_edge)(&mut messenger, &node, None);
                        drop(node);
                        if invoke {
                            reflect &= NodeMessengerContext {
                                global_node_id: *global_child_id,
                                nodes: &mut self.nodes,
                                theme_properties: &self.theme_properties,
                            }
                            .invoke_handler(&mut messenger_stack, &mut messenger);
                            (messenger.behavior.reset_at_node_edge)(&mut messenger);
                        }
                        if stop {
                            break;
                        }
                    }
                    if reflect && (messenger.behavior.do_reflect)(&mut messenger) {
                        NodeMessengerContext {
                            global_node_id,
                            nodes: &mut self.nodes,
                            theme_properties: &self.theme_properties,
                        }
                        .invoke_handler(&mut messenger_stack, &mut messenger);
                    }
                }
                PropagationDirection::Observers(observable) => {
                    messenger.propagation_direction = messenger.behavior.default_propagation_direction;
                    let captured_observable = (messenger.behavior.get_captured_observable)(&messenger);
                    let global_node_ids = if let Some((is_captured_observable, observers)) = captured_observable
                        .and_then(|captured_observable| self.observer_channels.get(&captured_observable).map(|observers| (true, observers)))
                        .or_else(|| self.observer_channels.get(&observable).map(|observers| (false, observers)))
                    {
                        if is_captured_observable {
                            (messenger.behavior.do_reflect)(&mut messenger);
                        }
                        observers.iter().cloned().collect::<Vec<_>>()
                    } else {
                        continue;
                    };
                    for global_node_id in global_node_ids.iter() {
                        {
                            let mut global_node_id = *global_node_id;
                            let mut parents_path = vec![global_node_id];
                            while let Some(global_parent_id) = self.nodes.get(&global_node_id).unwrap().borrow().parent {
                                global_node_id = global_parent_id;
                                parents_path.push(global_parent_id);
                            }
                            for global_node_id in parents_path.iter().rev() {
                                let node = self.nodes.get(global_node_id).unwrap().borrow();
                                (messenger.behavior.update_at_node_edge)(&mut messenger, &node, None);
                            }
                        }
                        NodeMessengerContext {
                            global_node_id: *global_node_id,
                            nodes: &mut self.nodes,
                            theme_properties: &self.theme_properties,
                        }
                        .invoke_handler(&mut messenger_stack, &mut messenger);
                    }
                }
            }
        }
    }

    /// Preparation step to be called before [render].
    pub fn prepare_rendering(&mut self, renderer: &mut Renderer, device: &wgpu::Device, queue: &wgpu::Queue) {
        renderer.reset_rendering();
        let messenger = Messenger::new(
            &message::PREPARE_RENDERING,
            hash_map! {
                "motor" => Value::Float4(ppga2d::Motor::one().into()),
                "scale" => Value::Float1(1.0.into()),
                "opacity" => Value::Float1(1.0.into()),
                "motor_in_parent" => Value::Float4(ppga2d::Motor::one().into()),
                "scale_in_parent" => Value::Float1(1.0.into()),
                "opacity_in_parent" => Value::Float1(1.0.into()),
            },
        );
        self.notify_observers(
            Some(renderer),
            Some(device),
            Some(queue),
            None,
            NodeOrObservableIdentifier::Named("root"),
            messenger,
        );
        renderer.prepare_rendering(device, queue);
    }

    /// Renders UI nodes.
    pub fn render(&mut self, renderer: &mut Renderer, frame_context: &mut FrameContext) {
        let messenger = Messenger::new(&message::RENDER, hash_map! {});
        self.notify_observers(
            Some(renderer),
            None,
            None,
            Some(frame_context),
            NodeOrObservableIdentifier::Named("root"),
            messenger,
        );
    }

    /// Advances all animations to the provided timestamp (measured in seconds).
    pub fn advance_property_animations(&mut self, current_time: f64) {
        let mut messenger_stack = Vec::new();
        for (global_node_id, node) in self.nodes.iter() {
            let mut node = node.borrow_mut();
            if node.advance_property_animations(current_time) {
                messenger_stack.push((*global_node_id, Messenger::new(&message::RECONFIGURE, hash_map! {})));
            }
        }
        self.process_messengers(messenger_stack, None, None, None, None);
    }
}
