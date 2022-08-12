use crate::{
    concat_buffers,
    error::Error,
    renderer::{self, Buffer, ClipStack, Shape},
    safe_float::SafeFloat,
    ui::{Node, Rendering},
};
use geometric_algebra::{ppga3d, Zero};
use std::rc::Rc;

/// Used to render a single frame.
pub struct FrameContext<'a> {
    encoder: &'a mut wgpu::CommandEncoder,
    stencil_render_pass_descriptor: wgpu::RenderPassDescriptor<'a, 'a>,
    cover_render_pass_descriptor: wgpu::RenderPassDescriptor<'a, 'a>,
}

impl<'a> FrameContext<'a> {
    /// Constructs a new [FrameContext].
    pub fn new(
        encoder: &'a mut wgpu::CommandEncoder,
        color_attachments: &'a [wgpu::RenderPassColorAttachment<'a>],
        depth_stencil_attachment: wgpu::RenderPassDepthStencilAttachment<'a>,
    ) -> Self {
        Self {
            encoder,
            stencil_render_pass_descriptor: wgpu::RenderPassDescriptor {
                label: None,
                color_attachments: &[],
                depth_stencil_attachment: Some(depth_stencil_attachment.clone()),
            },
            cover_render_pass_descriptor: wgpu::RenderPassDescriptor {
                label: None,
                color_attachments,
                depth_stencil_attachment: Some(depth_stencil_attachment),
            },
        }
    }
}

/// Used to render [ui::Node]s.
pub struct Renderer {
    renderer: Rc<renderer::Renderer>,
    instance_buffers: [Buffer; 2],
    pub projection_matrix: [ppga3d::Point; 4],
    color_instance_transform: Vec<[ppga3d::Point; 4]>,
    clipping_instance_transform: Vec<[ppga3d::Point; 4]>,
    instance_color: Vec<SafeFloat<f32, 4>>,
    clip_stack: ClipStack,
}

impl Renderer {
    /// Constructs a new ui [Renderer].
    pub fn new(renderer: Rc<renderer::Renderer>, device: &wgpu::Device) -> Self {
        Renderer {
            renderer,
            instance_buffers: [
                Buffer::new(device, wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST, &[]),
                Buffer::new(device, wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST, &[]),
            ],
            projection_matrix: [ppga3d::Point::zero(); 4],
            color_instance_transform: Vec::default(),
            clipping_instance_transform: Vec::default(),
            instance_color: Vec::default(),
            clip_stack: ClipStack::default(),
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

    pub(super) fn instanciate_node(&mut self, node: &mut Node, model_projection_matrix: &[ppga3d::Point; 4]) {
        node.colored_shapes_instance = self.color_instance_transform.len() as u32;
        for (color, _shapes) in node.colored_shapes.iter() {
            self.color_instance_transform.push(*model_projection_matrix);
            self.instance_color.push(*color);
        }
        if node.clip_shape.is_some() {
            node.clip_shape_instance = self.clipping_instance_transform.len() as u32;
            self.clipping_instance_transform.push(*model_projection_matrix);
        }
    }

    pub(super) fn reset_rendering(&mut self) {
        self.color_instance_transform.clear();
        self.clipping_instance_transform.clear();
        self.instance_color.clear();
    }

    pub(super) fn prepare_rendering(&mut self, device: &wgpu::Device, queue: &wgpu::Queue) {
        self.instance_buffers[0].update(
            device,
            queue,
            &concat_buffers!([&self.color_instance_transform, &self.clipping_instance_transform]).1,
        );
        self.instance_buffers[1].update(device, queue, &concat_buffers!([&self.instance_color]).1);
    }

    pub(super) fn render_node(&mut self, frame_context: &mut FrameContext, node: &Node) {
        for (i, (_color, shape)) in node.colored_shapes.iter().enumerate() {
            let instance_range = node.colored_shapes_instance + i as u32..node.colored_shapes_instance + i as u32 + 1;
            {
                let mut render_pass = frame_context.encoder.begin_render_pass(&frame_context.stencil_render_pass_descriptor);
                render_pass.set_vertex_buffer(0, self.instance_buffers[0].buffer.slice(..));
                shape.render_stencil(&self.renderer, &mut render_pass, self.clip_stack.height(), instance_range.clone());
            }
            {
                let mut render_pass = frame_context.encoder.begin_render_pass(&frame_context.cover_render_pass_descriptor);
                render_pass.set_vertex_buffer(0, self.instance_buffers[0].buffer.slice(..));
                render_pass.set_vertex_buffer(1, self.instance_buffers[1].buffer.slice(..));
                shape.render_cover(&self.renderer, &mut render_pass, self.clip_stack.height(), instance_range, true);
            }
        }
    }

    pub(super) fn push_clipping_of_node(&mut self, frame_context: &mut FrameContext, node: &Node) {
        if let Some(clip_shape) = &node.clip_shape {
            let rendering_instance = self.instance_color.len() as u32 + node.clip_shape_instance;
            let mut render_pass = frame_context.encoder.begin_render_pass(&frame_context.stencil_render_pass_descriptor);
            render_pass.set_vertex_buffer(0, self.instance_buffers[0].buffer.slice(..));
            self.clip_stack
                .push(&self.renderer, &mut render_pass, clip_shape, rendering_instance..rendering_instance + 1)
                .unwrap();
        }
    }

    pub(super) fn pop_clipping_of_node(&mut self, frame_context: &mut FrameContext, node: &Node) {
        if let Some(clip_shape) = &node.clip_shape {
            let mut render_pass = frame_context.encoder.begin_render_pass(&frame_context.stencil_render_pass_descriptor);
            render_pass.set_vertex_buffer(0, self.instance_buffers[0].buffer.slice(..));
            self.clip_stack.pop(&self.renderer, &mut render_pass, clip_shape).unwrap();
        }
    }
}
