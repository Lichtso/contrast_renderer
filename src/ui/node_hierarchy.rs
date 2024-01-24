use crate::{
    safe_float::SafeFloat,
    ui::{
        message::{
            self, captured_observable_for_messenger, propagation_direction_of_messenger, reflect_messenger, reset_messenger_at_node_edge,
            update_messenger_at_node_edge, Messenger, PropagationDirection,
        },
        renderer::Renderer,
        wrapped_values::Value,
        GlobalNodeIdentifier, Node, NodeOrObservableIdentifier, Rendering,
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

pub struct NodeMessengerContext<'a, 'b> {
    global_node_id: GlobalNodeIdentifier,
    nodes: &'a mut HashMap<GlobalNodeIdentifier, Rc<RefCell<Node>>>,
    theme_properties: &'a HashMap<&'static str, Value>,
    _renderer: &'a Option<&'a mut Renderer<'b>>,
}

impl<'a, 'b> NodeMessengerContext<'a, 'b> {
    fn invoke_handler(
        global_node_id: GlobalNodeIdentifier,
        nodes: &'a mut HashMap<GlobalNodeIdentifier, Rc<RefCell<Node>>>,
        theme_properties: &'a HashMap<&'static str, Value>,
        _renderer: &'a Option<&'a mut Renderer<'b>>,
        messenger_stack: &mut Vec<(GlobalNodeIdentifier, Messenger)>,
        messenger: &mut Messenger,
    ) -> bool {
        let node = nodes.get(&global_node_id).unwrap().borrow();
        if node.dormant {
            return true;
        }
        let messenger_handler = node.messenger_handler;
        drop(node);
        let mut context = Self {
            global_node_id,
            nodes,
            theme_properties,
            _renderer,
        };
        let messengers = messenger_handler(&mut context, messenger);
        let reflect = messengers.is_empty();
        messenger_stack.append(&mut messengers.into_iter().map(|messenger| (global_node_id, messenger)).collect::<Vec<_>>());
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

    pub fn set_attribute(&mut self, attribute: &'static str, value: Value) {
        let mut node = self.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        node.properties.insert(attribute, value);
    }

    pub fn get_half_extent(&self) -> SafeFloat<f32, 2> {
        let node = self.nodes.get(&self.global_node_id).unwrap().borrow();
        node.half_extent
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
                child_node.needs_reconfiguration = true;
                callback(&mut child_node);
                if child_node.needs_reconfiguration {
                    child_node_rc.clone()
                } else {
                    return;
                }
            } else {
                messengers.push(Messenger::ConfigureChild(message::ConfigureChild {
                    id: local_child_id,
                    node: None,
                }));
                return;
            }
        } else if let Some(callback) = callback {
            let mut child_node = Node::default();
            callback(&mut child_node);
            Rc::new(RefCell::new(child_node))
        } else {
            return;
        };
        messengers.push(Messenger::ConfigureChild(message::ConfigureChild {
            id: local_child_id,
            node: Some(child_node),
        }));
    }

    pub fn prepare_rendering_helper(
        &mut self,
        prepare_rendering: &message::PrepareRendering,
    ) -> (message::PrepareRendering, message::UpdateRendering) {
        let change_rendering = self.get_attribute("is_rendering_dirty") == Value::Boolean(true);
        self.set_attribute("is_rendering_dirty", Value::Boolean(false));
        let motor3d = motor2d_to_motor3d(&prepare_rendering.motor);
        let mut model_matrix = motor3d_to_mat4(&motor3d);
        model_matrix[0].g0[0] *= prepare_rendering.scale;
        model_matrix[1].g0[1] *= prepare_rendering.scale;
        model_matrix[2].g0[2] *= prepare_rendering.scale;
        model_matrix[3].g0[0] *= prepare_rendering.scale;
        model_matrix[3].g0[1] *= prepare_rendering.scale;
        model_matrix[3].g0[2] *= prepare_rendering.scale;
        (
            message::PrepareRendering {
                motor: prepare_rendering.motor,
                scale: prepare_rendering.scale,
                opacity: prepare_rendering.opacity,
                motor_in_parent: prepare_rendering.motor,
                scale_in_parent: prepare_rendering.scale,
                opacity_in_parent: prepare_rendering.opacity,
            },
            message::UpdateRendering {
                model_matrix,
                rendering: if change_rendering { Some(Rendering::default()) } else { None },
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
        self.process_messengers(vec![(global_node_id, Messenger::Reconfigure(message::Reconfigure {}))], None, None);
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
    pub fn notify_observers<'a>(
        &mut self,
        renderer: Option<&mut Renderer<'a>>,
        encoder: Option<&mut wgpu::CommandEncoder>,
        observable: NodeOrObservableIdentifier,
        mut messenger: Messenger,
    ) {
        let mut messenger_stack = Vec::new();
        let captured_observable = captured_observable_for_messenger(&messenger);
        println!("notify_observers {:?} {:?} {:?}", captured_observable, observable, messenger.get_kind());
        let global_node_ids = if let Some((is_captured_observable, observers)) = captured_observable
            .and_then(|captured_observable| self.observer_channels.get(&captured_observable).map(|observers| (true, observers)))
            .or_else(|| self.observer_channels.get(&observable).map(|observers| (false, observers)))
        {
            if is_captured_observable {
                reflect_messenger(&mut messenger);
            }
            observers.iter().cloned().collect::<Vec<_>>()
        } else {
            return;
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
                    update_messenger_at_node_edge(&node, &mut messenger, None);
                }
            }
            NodeMessengerContext::invoke_handler(
                *global_node_id,
                &mut self.nodes,
                &self.theme_properties,
                &renderer,
                &mut messenger_stack,
                &mut messenger,
            );
        }
        self.process_messengers(messenger_stack, renderer, encoder);
    }

    pub fn process_messengers<'a>(
        &mut self,
        mut messenger_stack: Vec<(GlobalNodeIdentifier, Messenger)>,
        mut renderer: Option<&mut Renderer<'a>>,
        mut encoder: Option<&mut wgpu::CommandEncoder>,
    ) {
        while let Some((global_node_id, mut messenger)) = messenger_stack.pop() {
            match propagation_direction_of_messenger(&messenger) {
                PropagationDirection::None => panic!(),
                PropagationDirection::Return => match messenger {
                    Messenger::Reconfigure(_message) => {
                        let mut node = self.nodes.get(&global_node_id).unwrap().borrow_mut();
                        if node.configuration_in_process {
                            continue;
                        }
                        node.configuration_in_process = true;
                        let mut messenger = Messenger::ConfigurationRequest(message::ConfigurationRequest {});
                        drop(node);
                        NodeMessengerContext::invoke_handler(
                            global_node_id,
                            &mut self.nodes,
                            &self.theme_properties,
                            &None,
                            &mut messenger_stack,
                            &mut messenger,
                        );
                    }
                    Messenger::ConfigurationResponse(message) => {
                        let mut node = self.nodes.get(&global_node_id).unwrap().borrow_mut();
                        node.needs_reconfiguration = false;
                        node.configuration_in_process = false;
                        if node.half_extent != message.half_extent {
                            node.half_extent = message.half_extent;
                            messenger_stack.push((global_node_id, Messenger::ChildResized(message::ChildResized {})));
                        }
                        let mut ordered_children = node
                            .children
                            .iter()
                            .filter(|(_local_child_id, global_child_id)| **global_child_id != global_node_id)
                            .map(|(_local_child_id, global_child_id)| {
                                (self.nodes.get(global_child_id).unwrap().borrow().layer_index, *global_child_id)
                            })
                            .collect::<Vec<(usize, GlobalNodeIdentifier)>>();
                        ordered_children.sort_by_key(|entry| entry.0);
                        node.ordered_children = ordered_children
                            .into_iter()
                            .map(|(_layer_index, global_child_id)| global_child_id)
                            .collect();
                    }
                    Messenger::ConfigureChild(message) => {
                        let node = self.nodes.get(&global_node_id).unwrap().borrow();
                        let global_child_id = node.children.get(&message.id).cloned();
                        drop(node);
                        if let Some(mut global_child_id) = global_child_id {
                            if let Some(new_child_node) = message.node {
                                if !Rc::ptr_eq(&new_child_node, self.nodes.get(&global_child_id).unwrap()) {
                                    self.delete_node(global_child_id);
                                    global_child_id = self.insert_node(Some((global_node_id, message.id)), new_child_node);
                                }
                                messenger_stack.push((global_child_id, Messenger::Reconfigure(message::Reconfigure {})));
                            } else {
                                self.delete_node(global_child_id);
                            }
                        } else if let Some(new_child_node) = message.node {
                            let global_child_id = self.insert_node(Some((global_node_id, message.id)), new_child_node);
                            messenger_stack.push((global_child_id, Messenger::Reconfigure(message::Reconfigure {})));
                        } else {
                            panic!("Can not delete what does not exist");
                        }
                    }
                    Messenger::Observe(mut message) => {
                        let mut node = self.nodes.get(&global_node_id).unwrap().borrow_mut();
                        if node.parent.is_none() {
                            message.observes.insert(NodeOrObservableIdentifier::Named("root"));
                        }
                        for observable_topic in node.observes.iter() {
                            if !message.observes.contains(observable_topic) {
                                match self.observer_channels.entry(*observable_topic) {
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
                        for observable_topic in message.observes.iter() {
                            self.observer_channels
                                .entry(*observable_topic)
                                .or_insert_with(HashSet::new)
                                .insert(global_node_id);
                        }
                        node.observes = message.observes;
                    }
                    Messenger::UpdateRendering(message) => {
                        let mut node = self.nodes.get(&global_node_id).unwrap().borrow_mut();
                        if let Some(rendering) = &message.rendering {
                            renderer.as_ref().unwrap().set_node_rendering(&mut node, rendering).unwrap();
                        }
                        let model_projection_matrix = matrix_multiplication(&renderer.as_ref().unwrap().projection_matrix, &message.model_matrix);
                        renderer.as_mut().unwrap().instanciate_node(&mut node, &model_projection_matrix);
                    }
                    Messenger::RenderAndClip(_message) => {
                        let node = self.nodes.get(&global_node_id).unwrap().borrow();
                        renderer.as_mut().unwrap().render_node(encoder.as_mut().unwrap(), &node);
                        renderer.as_mut().unwrap().push_clipping_of_node(encoder.as_mut().unwrap(), &node);
                    }
                    Messenger::RenderUnclip(_message) => {
                        let node = self.nodes.get(&global_node_id).unwrap().borrow();
                        renderer.as_mut().unwrap().pop_clipping_of_node(encoder.as_mut().unwrap(), &node);
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
                        let (invoke, _stop) = update_messenger_at_node_edge(&node, &mut messenger, Some(local_child_id));
                        drop(node);
                        if invoke {
                            NodeMessengerContext::invoke_handler(
                                global_parent_id,
                                &mut self.nodes,
                                &self.theme_properties,
                                &renderer,
                                &mut messenger_stack,
                                &mut messenger,
                            );
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
                        update_messenger_at_node_edge(&node, &mut messenger, Some(local_child_id));
                        let ordered_children = node.ordered_children.clone(); // TODO: Efficency
                        drop(node);
                        for global_child_id in ordered_children.iter() {
                            if *global_child_id == global_node_id {
                                continue;
                            }
                            let node = self.nodes.get(global_child_id).unwrap().borrow();
                            let (invoke, stop) = update_messenger_at_node_edge(&node, &mut messenger, None);
                            drop(node);
                            if invoke {
                                NodeMessengerContext::invoke_handler(
                                    *global_child_id,
                                    &mut self.nodes,
                                    &self.theme_properties,
                                    &renderer,
                                    &mut messenger_stack,
                                    &mut messenger,
                                );
                                reset_messenger_at_node_edge(&mut messenger);
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
                        let (invoke, stop) = update_messenger_at_node_edge(&node, &mut messenger, None);
                        drop(node);
                        if invoke {
                            reflect &= NodeMessengerContext::invoke_handler(
                                *global_child_id,
                                &mut self.nodes,
                                &self.theme_properties,
                                &renderer,
                                &mut messenger_stack,
                                &mut messenger,
                            );
                            reset_messenger_at_node_edge(&mut messenger);
                        }
                        if stop {
                            break;
                        }
                    }
                    if reflect && reflect_messenger(&mut messenger) {
                        NodeMessengerContext::invoke_handler(
                            global_node_id,
                            &mut self.nodes,
                            &self.theme_properties,
                            &renderer,
                            &mut messenger_stack,
                            &mut messenger,
                        );
                    }
                }
                PropagationDirection::Observers(observable) => {
                    // TODO
                }
            }
        }
    }

    /// Preparation step to be called before [render].
    pub fn prepare_rendering(&mut self, renderer: &mut Renderer) {
        let message = Messenger::PrepareRendering(message::PrepareRendering {
            motor: ppga2d::Motor::one(),
            scale: 1.0,
            opacity: 1.0,
            motor_in_parent: ppga2d::Motor::one(),
            scale_in_parent: 1.0,
            opacity_in_parent: 1.0,
        });
        self.notify_observers(Some(renderer), None, NodeOrObservableIdentifier::Named("root"), message);
    }

    /// Renders UI nodes.
    pub fn render(&mut self, queue: &wgpu::Queue, encoder: &mut wgpu::CommandEncoder, renderer: &mut Renderer) {
        renderer.prepare_rendering(queue);
        let message = Messenger::Render(message::Render {});
        self.notify_observers(Some(renderer), Some(encoder), NodeOrObservableIdentifier::Named("root"), message);
    }
}
