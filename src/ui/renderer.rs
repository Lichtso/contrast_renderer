use crate::{
    concat_buffers,
    error::Error,
    renderer::{self, Buffer, ClipStack, Shape},
    safe_float::SafeFloat,
    ui::{Node, Rendering},
};
use geometric_algebra::{ppga3d, Zero};

/// Used to render [ui::Node]s.
pub struct Renderer<'a> {
    renderer: &'a renderer::Renderer,
    device: &'a wgpu::Device,
    instance_buffers: [Buffer; 2],
    stencil_render_pass_descriptor: wgpu::RenderPassDescriptor<'a, 'a>,
    pub cover_render_pass_descriptor: wgpu::RenderPassDescriptor<'a, 'a>,
    pub projection_matrix: [ppga3d::Point; 4],
    color_instance_transform: Vec<[ppga3d::Point; 4]>,
    clipping_instance_transform: Vec<[ppga3d::Point; 4]>,
    instance_color: Vec<SafeFloat<f32, 4>>,
    next_color_instance: u32,
    next_clipping_instance: u32,
    clip_stack: ClipStack,
}

impl<'a> Renderer<'a> {
    /// Constructs a new ui [Renderer].
    pub fn new(renderer: &'a renderer::Renderer, device: &'a wgpu::Device) -> Self {
        Renderer {
            renderer,
            device,
            instance_buffers: [
                Buffer::new(device, wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST, &[]),
                Buffer::new(device, wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST, &[]),
            ],
            stencil_render_pass_descriptor: wgpu::RenderPassDescriptor::default(),
            cover_render_pass_descriptor: wgpu::RenderPassDescriptor::default(),
            projection_matrix: [ppga3d::Point::zero(); 4],
            color_instance_transform: Vec::default(),
            clipping_instance_transform: Vec::default(),
            instance_color: Vec::default(),
            next_color_instance: 0,
            next_clipping_instance: 0,
            clip_stack: ClipStack::default(),
        }
    }

    pub(super) fn set_node_rendering(&self, node: &mut Node, rendering: &Rendering) -> Result<(), Error> {
        node.clip_shape = if rendering.clip_paths.is_empty() {
            None
        } else {
            Some(Box::new(Shape::from_paths(
                self.device,
                self.renderer,
                &rendering.dynamic_stroke_options,
                &rendering.clip_paths,
                None,
            )?))
        };
        node.colored_shapes = rendering
            .colored_paths
            .iter()
            .map(|(color, paths)| {
                Shape::from_paths(self.device, self.renderer, &rendering.dynamic_stroke_options, paths, None).map(|shape| (*color, shape))
            })
            .collect::<Result<Vec<_>, Error>>()?;
        Ok(())
    }

    pub(super) fn instanciate_node(&mut self, node: &mut Node, model_projection_matrix: &[ppga3d::Point; 4]) {
        node.colored_shapes_instance = self.next_color_instance;
        self.next_color_instance += node.colored_shapes.len() as u32;
        for (color, _shapes) in node.colored_shapes.iter() {
            self.color_instance_transform.push(*model_projection_matrix);
            self.instance_color.push(*color);
        }
        if node.clip_shape.is_some() {
            node.clip_shape_instance = self.next_clipping_instance;
            self.next_clipping_instance += 1;
            self.clipping_instance_transform.push(*model_projection_matrix);
        }
    }

    pub(super) fn prepare_rendering(&mut self, queue: &wgpu::Queue) {
        self.instance_buffers[0].update(
            self.device,
            queue,
            &concat_buffers!([&self.color_instance_transform, &self.clipping_instance_transform]).1,
        );
        self.instance_buffers[1].update(self.device, queue, &concat_buffers!([&self.instance_color]).1);
        self.stencil_render_pass_descriptor.depth_stencil_attachment = self.cover_render_pass_descriptor.depth_stencil_attachment.clone();
    }

    pub(super) fn render_node(&mut self, encoder: &mut wgpu::CommandEncoder, node: &Node) {
        for (i, (_color, shape)) in node.colored_shapes.iter().enumerate() {
            let instance_range = node.colored_shapes_instance + i as u32..node.colored_shapes_instance + i as u32 + 1;
            {
                let mut render_pass = encoder.begin_render_pass(&self.stencil_render_pass_descriptor);
                render_pass.set_vertex_buffer(0, self.instance_buffers[0].buffer.slice(..));
                shape.render_stencil(self.renderer, &mut render_pass, self.clip_stack.height(), instance_range.clone());
            }
            {
                let mut render_pass = encoder.begin_render_pass(&self.cover_render_pass_descriptor);
                render_pass.set_vertex_buffer(0, self.instance_buffers[0].buffer.slice(..));
                render_pass.set_vertex_buffer(1, self.instance_buffers[1].buffer.slice(..));
                shape.render_cover(self.renderer, &mut render_pass, self.clip_stack.height(), instance_range, true);
            }
        }
    }

    pub(super) fn push_clipping_of_node(&mut self, encoder: &mut wgpu::CommandEncoder, node: &Node) {
        if let Some(clip_shape) = &node.clip_shape {
            let rendering_instance = self.instance_color.len() as u32 + node.clip_shape_instance;
            let mut render_pass = encoder.begin_render_pass(&self.stencil_render_pass_descriptor);
            render_pass.set_vertex_buffer(0, self.instance_buffers[0].buffer.slice(..));
            self.clip_stack
                .push(self.renderer, &mut render_pass, clip_shape, rendering_instance..rendering_instance + 1)
                .unwrap();
        }
    }

    pub(super) fn pop_clipping_of_node(&mut self, encoder: &mut wgpu::CommandEncoder, node: &Node) {
        if let Some(clip_shape) = &node.clip_shape {
            let mut render_pass = encoder.begin_render_pass(&self.stencil_render_pass_descriptor);
            render_pass.set_vertex_buffer(0, self.instance_buffers[0].buffer.slice(..));
            self.clip_stack.pop(self.renderer, &mut render_pass, clip_shape).unwrap();
        }
    }
}
