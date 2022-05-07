use crate::{
    hash_map, hash_set, match_option,
    safe_float::SafeFloat,
    ui::{
        message::{self, Messenger, PropagationDirection},
        renderer::{FrameContext, Renderer},
        wrapped_values::Value,
        GlobalNodeIdentifier, MessengerHandler, Node, NodeOrObservableIdentifier,
    },
    utils::{matrix_multiplication, motor2d_to_motor3d, motor3d_to_mat4},
};
use geometric_algebra::{ppga2d, One};
use std::{
    cell::RefCell,
    collections::{hash_map, HashMap, HashSet},
    rc::Rc,
};

pub struct NodeMessengerContext<'a> {
    global_node_id: GlobalNodeIdentifier,
    node_hierarchy: &'a mut NodeHierarchy,
}

impl<'a> NodeMessengerContext<'a> {
    fn invoke_handler(&mut self, messenger_stack: &mut Vec<(GlobalNodeIdentifier, Messenger)>, messenger: &mut Messenger) -> bool {
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        let dormant = node
            .properties
            .get("dormant")
            .map(|value| *match_option!(value, Value::Boolean).unwrap())
            .unwrap_or(false);
        if dormant {
            node.configuration_in_process = false;
            return true;
        }
        let messenger_handler = node.messenger_handler;
        drop(node);
        let mut messengers = messenger_handler(self, messenger)
            .into_iter()
            .map(|messenger| (self.global_node_id, messenger))
            .collect::<Vec<_>>();
        let reflect = messengers.is_empty();
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        if messenger.behavior.label == "Reconfigure" {
            messengers.push((self.global_node_id, Messenger::new(&message::CONFIGURED, hash_map! {})));
        } else if !node.touched_attributes.is_empty() {
            messengers.push((self.global_node_id, Messenger::new(&message::RECONFIGURE, hash_map! {})));
        }
        if !node.touched_attributes.is_empty() {
            if let Some(global_parent_id) = node.parent {
                messengers.push((global_parent_id, Messenger::new(&message::RECONFIGURE, hash_map! {})));
            }
        }
        let mut children_order_changed = node.was_attribute_touched(&["child_count"]);
        for global_child_id in node.children.values() {
            let child_node = self.node_hierarchy.nodes.get(global_child_id).unwrap().borrow();
            if !child_node.touched_attributes.is_empty() {
                messengers.push((*global_child_id, Messenger::new(&message::RECONFIGURE, hash_map! {})));
                if child_node.was_attribute_touched(&["layer_index"]) {
                    children_order_changed = true;
                }
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
        messenger_stack.append(&mut messengers);
        reflect
    }

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

    pub fn was_attribute_touched(&self, attributes: &[&'static str]) -> bool {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.was_attribute_touched(attributes)
    }

    pub fn touch_attribute(&mut self, attribute: &'static str) {
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        node.touch_attribute(attribute);
    }

    pub fn get_attribute(&self, attribute: &'static str) -> Value {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.get_attribute(attribute)
    }

    pub fn set_attribute_privately(&mut self, attribute: &'static str, value: Value) {
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        node.set_attribute_privately(attribute, value);
    }

    pub fn set_attribute(&mut self, attribute: &'static str, value: Value) -> bool {
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        node.set_attribute(attribute, value)
    }

    pub fn set_attribute_animated(&mut self, attribute: &'static str, value: Value, start_time: f64, duration: f64) -> bool {
        let mut node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow_mut();
        node.set_attribute_animated(attribute, value, start_time, duration)
    }

    pub fn set_half_extent(&mut self, half_extent: SafeFloat<f32, 2>) {
        self.set_attribute("half_extent", Value::Float2(half_extent));
        self.set_attribute("proposed_half_extent", Value::Float2(half_extent));
    }

    pub fn get_half_extent(&self, proposed: bool) -> SafeFloat<f32, 2> {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.get_half_extent(proposed)
    }

    pub fn get_last_animation_time(&self) -> f64 {
        self.node_hierarchy.last_animation_time
    }

    pub fn does_observe(&self, observable: &NodeOrObservableIdentifier) -> bool {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.observes.contains(observable)
    }

    pub fn get_number_of_children(&self) -> usize {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.children.len()
    }

    pub fn iter_children<F: FnMut(&NodeOrObservableIdentifier, &Node)>(&self, mut callback: F) {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        for (local_child_id, global_child_id) in node.children.iter() {
            let child_node = self.node_hierarchy.nodes.get(global_child_id).unwrap().borrow();
            callback(local_child_id, &child_node);
        }
    }

    pub fn inspect_child<R, F: FnOnce(&Node) -> R>(&self, local_child_id: &NodeOrObservableIdentifier, callback: F) -> Option<R> {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        node.children
            .get(local_child_id)
            .map(|global_child_id| callback(&self.node_hierarchy.nodes.get(global_child_id).unwrap().borrow()))
    }

    pub fn add_child(&mut self, local_child_id: NodeOrObservableIdentifier, child_node: Rc<RefCell<Node>>) {
        let _global_child_id = self.node_hierarchy.insert_node(Some((self.global_node_id, local_child_id)), child_node);
    }

    pub fn remove_child(&mut self, local_child_id: NodeOrObservableIdentifier) -> Option<Rc<RefCell<Node>>> {
        let node = self.node_hierarchy.nodes.get(&self.global_node_id).unwrap().borrow();
        let global_child_id = node.children.get(&local_child_id).cloned();
        drop(node);
        global_child_id.map(|global_child_id| self.node_hierarchy.delete_node(global_child_id))
    }

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

    pub fn pointer_and_button_input_focus(&mut self, messenger: &Messenger) {
        if let Value::Boolean(pressed) = messenger.get_attribute("pressed_or_released") {
            let pointer_input = *match_option!(messenger.get_attribute("input_source"), Value::NodeOrObservableIdentifier).unwrap();
            let button_input =
                NodeOrObservableIdentifier::ButtonInput(match_option!(pointer_input, NodeOrObservableIdentifier::PointerInput).unwrap());
            if *pressed {
                self.observe(hash_set! {pointer_input, button_input}, true);
            } else {
                self.observe(hash_set! {button_input}, true);
            }
        }
    }
}

/// The hierarchy of UI nodes.
#[derive(Default)]
pub struct NodeHierarchy {
    next_node_id: GlobalNodeIdentifier,
    nodes: HashMap<GlobalNodeIdentifier, Rc<RefCell<Node>>>,
    observer_channels: HashMap<NodeOrObservableIdentifier, HashSet<GlobalNodeIdentifier>>,
    last_animation_time: f64,
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
            borrowed_node.touch_attribute("parent");
            let mut parent = self.nodes.get(&global_parent_id).unwrap().borrow_mut();
            parent.touch_attribute("child_count");
            parent.children.insert(local_child_id, global_node_id);
        } else {
            borrowed_node.local_id = NodeOrObservableIdentifier::Named("root");
            borrowed_node.observes.insert(NodeOrObservableIdentifier::Named("root"));
        }
        self.subscribe_observer(global_node_id, borrowed_node.observes.iter());
        drop(borrowed_node);
        self.nodes.insert(global_node_id, node);
        global_node_id
    }

    pub fn insert_and_configure_node(
        &mut self,
        node: Node,
        parent_link: Option<(GlobalNodeIdentifier, NodeOrObservableIdentifier)>,
    ) -> GlobalNodeIdentifier {
        let global_node_id = self.insert_node(parent_link, Rc::new(RefCell::new(node)));
        let mut messenger_stack = vec![(global_node_id, Messenger::new(&message::RECONFIGURE, hash_map! {}))];
        if let Some((global_parent_id, _local_child_id)) = parent_link {
            messenger_stack.push((global_parent_id, Messenger::new(&message::RECONFIGURE, hash_map! {})));
        }
        self.process_messengers(messenger_stack, None, None, None, None);
        global_node_id
    }

    pub fn create_node(
        &mut self,
        messenger_handler: MessengerHandler,
        properties: HashMap<&'static str, Value>,
        parent_link: Option<(GlobalNodeIdentifier, NodeOrObservableIdentifier)>,
    ) -> GlobalNodeIdentifier {
        let touched_attributes = properties.keys().cloned().collect();
        let node = Node {
            messenger_handler,
            properties,
            touched_attributes,
            ..Node::default()
        };
        self.insert_and_configure_node(node, parent_link)
    }

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

    fn invoke_handler(
        &mut self,
        messenger_stack: &mut Vec<(GlobalNodeIdentifier, Messenger)>,
        global_node_id: GlobalNodeIdentifier,
        messenger: &mut Messenger,
    ) -> bool {
        NodeMessengerContext {
            global_node_id,
            node_hierarchy: self,
        }
        .invoke_handler(messenger_stack, messenger)
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
                        drop(node);
                        let mut messenger = Messenger::new(&message::RECONFIGURE, hash_map! {});
                        self.invoke_handler(&mut messenger_stack, global_node_id, &mut messenger);
                        let mut node = self.nodes.get(&global_node_id).unwrap().borrow_mut();
                        node.touched_attributes.clear();
                    }
                    "Configured" => {
                        let mut node = self.nodes.get(&global_node_id).unwrap().borrow_mut();
                        node.configuration_in_process = false;
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
                            self.invoke_handler(&mut messenger_stack, global_parent_id, &mut messenger);
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
                        let (invoke, _stop) = (messenger.behavior.update_at_node_edge)(&mut messenger, &node, None);
                        drop(node);
                        if invoke {
                            reflect &= self.invoke_handler(&mut messenger_stack, global_child_id, &mut messenger);
                            (messenger.behavior.reset_at_node_edge)(&mut messenger, false);
                        }
                    }
                    if reflect && (messenger.behavior.do_reflect)(&mut messenger) {
                        self.invoke_handler(&mut messenger_stack, global_node_id, &mut messenger);
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
                                self.invoke_handler(&mut messenger_stack, *global_child_id, &mut messenger);
                                (messenger.behavior.reset_at_node_edge)(&mut messenger, false);
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
                            reflect &= self.invoke_handler(&mut messenger_stack, *global_child_id, &mut messenger);
                            (messenger.behavior.reset_at_node_edge)(&mut messenger, false);
                        }
                        if stop {
                            break;
                        }
                    }
                    if reflect && (messenger.behavior.do_reflect)(&mut messenger) {
                        self.invoke_handler(&mut messenger_stack, global_node_id, &mut messenger);
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
                                let (invoke, _stop) = (messenger.behavior.update_at_node_edge)(&mut messenger, &node, None);
                                if !invoke && !is_captured_observable {
                                    invoke_handler = false;
                                    break;
                                }
                            }
                        }
                        if invoke_handler || is_captured_observable {
                            self.invoke_handler(&mut messenger_stack, *global_node_id, &mut messenger);
                        }
                        (messenger.behavior.reset_at_node_edge)(&mut messenger, true);
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
        self.last_animation_time = current_time;
        let mut messenger_stack = Vec::new();
        for (global_node_id, node) in self.nodes.iter() {
            let mut node = node.borrow_mut();
            if node.advance_property_animations(current_time) {
                messenger_stack.push((*global_node_id, Messenger::new(&message::RECONFIGURE, hash_map! {})));
                if let Some(global_parent_id) = node.parent {
                    messenger_stack.push((global_parent_id, Messenger::new(&message::RECONFIGURE, hash_map! {})));
                }
            }
        }
        self.process_messengers(messenger_stack, None, None, None, None);
    }
}
