//! Rendering of ui [Node](crate::ui::Node)s
use crate::{
    concat_buffers,
    error::Error,
    renderer::{self, Buffer, RenderOperation, Shape},
    safe_float::SafeFloat,
    ui::{Node, Rendering},
};
use geometric_algebra::{ppga3d, Zero};
use std::{cmp::Ordering, collections::BinaryHeap, ops::Range, rc::Rc};

struct RenderCommand {
    render_layer: usize,
    operation: RenderOperation,
    clip_depth: u8,
    instance_index: u32,
    shape: &'static Shape,
}

impl Ord for RenderCommand {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .render_layer
            .cmp(&self.render_layer)
            .then_with(|| other.operation.cmp(&self.operation))
    }
}

impl PartialOrd for RenderCommand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for RenderCommand {}

impl PartialEq for RenderCommand {
    fn eq(&self, other: &Self) -> bool {
        self.render_layer == other.render_layer && self.operation == other.operation && self.instance_index == other.instance_index
    }
}

/// Used to render [Node](crate::ui::Node)s of a [NodeHierarchy](crate::ui::node_hierarchy::NodeHierarchy)
pub struct Renderer {
    renderer: Rc<renderer::Renderer>,
    instance_buffers: [Buffer; 2],
    /// This matrix is applied to all transformations in the entire [NodeHierarchy](crate::ui::node_hierarchy::NodeHierarchy)
    pub projection_matrix: [ppga3d::Point; 4],
    command_stream: BinaryHeap<RenderCommand>,
    instance_transform: Vec<[ppga3d::Point; 4]>,
    instance_color: Vec<SafeFloat<f32, 4>>,
}

impl Renderer {
    /// Constructs a new ui [Renderer]
    pub fn new(renderer: Rc<renderer::Renderer>, device: &wgpu::Device) -> Self {
        Renderer {
            renderer,
            instance_buffers: [
                Buffer::new(device, wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST, &[]),
                Buffer::new(device, wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST, &[]),
            ],
            projection_matrix: [ppga3d::Point::zero(); 4],
            command_stream: BinaryHeap::default(),
            instance_transform: Vec::default(),
            instance_color: Vec::default(),
        }
    }

    pub(super) fn set_node_rendering(
        &self,
        device: &wgpu::Device,
        _queue: &wgpu::Queue,
        node: &mut Node,
        rendering: &Rendering,
    ) -> Result<(), Error> {
        node.clip_shape = if rendering.clip_paths.is_empty() {
            None
        } else {
            Some(Box::new(Shape::from_paths(
                device,
                &self.renderer,
                &rendering.dynamic_stroke_options,
                &rendering.clip_paths,
                None,
            )?))
        };
        node.colored_shapes = rendering
            .colored_paths
            .iter()
            .map(|(color, paths)| {
                Shape::from_paths(device, &self.renderer, &rendering.dynamic_stroke_options, paths, None).map(|shape| (*color, shape))
            })
            .collect::<Result<Vec<_>, Error>>()?;
        Ok(())
    }

    pub(super) fn instanciate_node(&mut self, node: &Node, layer_range: Range<usize>, clip_depth: u8, model_projection_matrix: &[ppga3d::Point; 4]) {
        let instance_index = self.instance_transform.len() as u32;
        for (color, _shape) in node.colored_shapes.iter() {
            self.instance_transform.push(*model_projection_matrix);
            self.instance_color.push(*color);
        }
        if let Some(shape) = &node.clip_shape {
            if node.colored_shapes.is_empty() {
                self.instance_transform.push(*model_projection_matrix);
                self.instance_color.push([0.0; 4].into());
            }
            let shape: &Shape = shape;
            // As long as the Shapes are not changed until encode_commands() is called, this is safe.
            let shape: &'static Shape = unsafe { std::mem::transmute(shape) };
            self.command_stream.push(RenderCommand {
                operation: RenderOperation::Stencil,
                render_layer: layer_range.start + node.colored_shapes.len(),
                clip_depth,
                instance_index,
                shape,
            });
            self.command_stream.push(RenderCommand {
                operation: RenderOperation::Clip,
                render_layer: layer_range.start + node.colored_shapes.len(),
                clip_depth: clip_depth + 1,
                instance_index,
                shape,
            });
            self.command_stream.push(RenderCommand {
                operation: RenderOperation::UnClip,
                render_layer: layer_range.end - 1,
                clip_depth,
                instance_index,
                shape,
            });
        }
        for (layer_index, (_color, shape)) in node.colored_shapes.iter().enumerate() {
            // As long as the Shapes are not changed until encode_commands() is called, this is safe.
            let shape: &'static Shape = unsafe { std::mem::transmute(shape) };
            self.command_stream.push(RenderCommand {
                operation: RenderOperation::Stencil,
                render_layer: layer_range.start + layer_index,
                clip_depth,
                instance_index,
                shape,
            });
            self.command_stream.push(RenderCommand {
                operation: RenderOperation::Color,
                render_layer: layer_range.start + layer_index,
                clip_depth,
                instance_index: instance_index + layer_index as u32,
                shape,
            });
        }
    }

    pub(super) fn reset_buffers(&mut self) {
        self.command_stream.clear();
        self.instance_transform.clear();
        self.instance_color.clear();
    }

    pub(super) fn update_instance_buffers(&mut self, device: &wgpu::Device, queue: &wgpu::Queue) {
        self.instance_buffers[0].update(device, queue, &concat_buffers!([&self.instance_transform]).1);
        self.instance_buffers[1].update(device, queue, &concat_buffers!([&self.instance_color]).1);
    }

    /// Encodes the rendering commands with the given attachments
    ///
    /// [NodeHierarchy::prepare_rendering](crate::ui::node_hierarchy::NodeHierarchy::prepare_rendering) needs to be called first.
    pub fn encode_commands<'a, 'b: 'a>(&'b mut self, render_pass: &mut wgpu::RenderPass<'a>) {
        let command_count = self.command_stream.len();
        let mut used_render_passes = 0;
        let mut prev_entry: Option<RenderCommand> = None;
        render_pass.set_vertex_buffer(0, self.instance_buffers[0].buffer.slice(..));
        while let Some(entry) = self.command_stream.pop() {
            if prev_entry.map(|prev_entry| prev_entry.cmp(&entry) != Ordering::Equal).unwrap_or(true) {
                used_render_passes += 1;
                if entry.operation == RenderOperation::Color {
                    render_pass.set_vertex_buffer(1, self.instance_buffers[1].buffer.slice(..));
                }
            }
            self.renderer.set_clip_depth(render_pass, entry.clip_depth as usize).unwrap();
            entry.shape.render(
                &self.renderer,
                render_pass,
                entry.instance_index..entry.instance_index + 1,
                entry.operation,
            );
            prev_entry = Some(entry);
        }
        println!(
            "used_render_passes={}/{} (saved {:.2}%)",
            used_render_passes,
            command_count,
            (1.0 - used_render_passes as f32 / command_count as f32) * 100.0
        );
    }
}
