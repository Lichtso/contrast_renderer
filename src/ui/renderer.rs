//! Rendering of ui [Node](crate::ui::Node)s
use crate::{
    concat_buffers,
    error::Error,
    match_option,
    path::Path,
    renderer::{self, Buffer, RenderOperation, Shape},
    safe_float::SafeFloat,
    ui::{Node, Rendering, Value},
};
use geometric_algebra::{ppga3d, Zero};
use std::{cmp::Ordering, collections::BinaryHeap, ops::Range};

struct RenderCommand {
    operation: RenderOperation,
    render_layer: usize,
    alpha_layer: u8,
    clip_depth: u8,
    instance_index: u32,
    shape: &'static Shape,
}

impl Ord for RenderCommand {
    fn cmp(&self, other: &Self) -> Ordering {
        self.render_layer
            .cmp(&other.render_layer)
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
    instance_buffers: [Buffer; 2],
    /// This matrix is applied to all transformations in the entire [NodeHierarchy](crate::ui::node_hierarchy::NodeHierarchy)
    pub projection_matrix: [ppga3d::Point; 4],
    command_stream: BinaryHeap<RenderCommand>,
    instance_transform: Vec<[ppga3d::Point; 4]>,
    instance_color: Vec<SafeFloat<f32, 4>>,
}

impl Renderer {
    /// Constructs a new ui [Renderer]
    pub fn new(device: &wgpu::Device) -> Self {
        Renderer {
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
        contrast_renderer: &renderer::Renderer,
        device: &wgpu::Device,
        node: &mut Node,
        rendering: &Rendering,
    ) -> Result<(), Error> {
        let opacity: f32 = match_option!(node.get_attribute("opacity"), Value::Float1)
            .map(|value| value.into())
            .unwrap_or(1.0);
        node.alpha_cover_shape = if opacity == 1.0 {
            None
        } else {
            let half_extent = node.get_half_extent(false);
            Some(Box::new(Shape::from_paths(
                device,
                contrast_renderer,
                &[],
                &[Path::from_rect([0.0, 0.0], half_extent.into())],
                None,
            )?))
        };
        node.clip_shape = if rendering.clip_paths.is_empty() {
            None
        } else {
            Some(Box::new(Shape::from_paths(
                device,
                contrast_renderer,
                &rendering.dynamic_stroke_options,
                &rendering.clip_paths,
                None,
            )?))
        };
        node.colored_shapes = rendering
            .colored_paths
            .iter()
            .map(|(color, paths)| {
                Shape::from_paths(device, contrast_renderer, &rendering.dynamic_stroke_options, paths, None).map(|shape| (*color, shape))
            })
            .collect::<Result<Vec<_>, Error>>()?;
        Ok(())
    }

    pub(super) fn instanciate_node(
        &mut self,
        node: &Node,
        layer_range: Range<usize>,
        clip_depth: u8,
        alpha_layer: u8,
        model_projection_matrix: &[ppga3d::Point; 4],
    ) {
        let instance_index = self.instance_transform.len() as u32;
        let opacity: f32 = match_option!(node.get_attribute("opacity"), Value::Float1)
            .map(|value| value.into())
            .unwrap_or(1.0);
        if node.alpha_cover_shape.is_some() || (node.colored_shapes.is_empty() && node.clip_shape.is_some()) {
            self.instance_transform.push(*model_projection_matrix);
            self.instance_color.push([0.0, 0.0, 0.0, opacity].into());
        }
        for (layer_index, (color, shape)) in node.colored_shapes.iter().enumerate() {
            self.instance_transform.push(*model_projection_matrix);
            self.instance_color.push(*color);
            // As long as the Shapes are not changed until encode_commands() is called, this is safe.
            let shape: &'static Shape = unsafe { std::mem::transmute(shape) };
            self.command_stream.push(RenderCommand {
                operation: RenderOperation::Stencil,
                render_layer: layer_range.start + layer_index,
                alpha_layer,
                clip_depth,
                instance_index,
                shape,
            });
            self.command_stream.push(RenderCommand {
                operation: RenderOperation::Color,
                render_layer: layer_range.start + layer_index,
                alpha_layer,
                clip_depth,
                instance_index: instance_index + layer_index as u32 + node.alpha_cover_shape.is_some() as u32,
                shape,
            });
        }
        if let Some(shape) = &node.alpha_cover_shape {
            let shape: &Shape = shape;
            // As long as the Shapes are not changed until encode_commands() is called, this is safe.
            let shape: &'static Shape = unsafe { std::mem::transmute(shape) };
            self.command_stream.push(RenderCommand {
                operation: RenderOperation::SaveAlphaContext,
                render_layer: layer_range.end - 1,
                alpha_layer,
                clip_depth,
                instance_index,
                shape,
            });
            self.command_stream.push(RenderCommand {
                operation: RenderOperation::ScaleAlphaContext,
                render_layer: layer_range.end - 1,
                alpha_layer,
                clip_depth,
                instance_index,
                shape,
            });
            self.command_stream.push(RenderCommand {
                operation: RenderOperation::RestoreAlphaContext,
                render_layer: layer_range.start,
                alpha_layer,
                clip_depth,
                instance_index,
                shape,
            });
        }
        if let Some(shape) = &node.clip_shape {
            let shape: &Shape = shape;
            // As long as the Shapes are not changed until encode_commands() is called, this is safe.
            let shape: &'static Shape = unsafe { std::mem::transmute(shape) };
            self.command_stream.push(RenderCommand {
                operation: RenderOperation::Stencil,
                render_layer: layer_range.end - 1,
                alpha_layer,
                clip_depth,
                instance_index,
                shape,
            });
            self.command_stream.push(RenderCommand {
                operation: RenderOperation::Clip,
                render_layer: layer_range.end - 1,
                alpha_layer,
                clip_depth: clip_depth + 1,
                instance_index,
                shape,
            });
            self.command_stream.push(RenderCommand {
                operation: RenderOperation::UnClip,
                render_layer: layer_range.start + node.colored_shapes.len(),
                alpha_layer,
                clip_depth,
                instance_index,
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
        assert_eq!(self.instance_transform.len(), self.instance_color.len());
        self.instance_buffers[0].update(device, queue, &concat_buffers!([&self.instance_transform]).1);
        self.instance_buffers[1].update(device, queue, &concat_buffers!([&self.instance_color]).1);
    }

    /// Encodes the rendering commands with the given attachments
    ///
    /// [NodeHierarchy::prepare_rendering](crate::ui::node_hierarchy::NodeHierarchy::prepare_rendering) needs to be called first.
    pub fn encode_commands<'a>(
        &'a mut self,
        contrast_renderer: &'a renderer::Renderer,
        encoder: &'a mut wgpu::CommandEncoder,
        color_attachment: wgpu::RenderPassColorAttachment<'a>,
        depth_stencil_attachment: wgpu::RenderPassDepthStencilAttachment<'a>,
    ) -> wgpu::RenderPass<'a> {
        let mut prev_entry: Option<RenderCommand> = None;
        let mut render_pass = {
            let encoder: &mut wgpu::CommandEncoder = unsafe { std::mem::transmute(&mut *encoder) };
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: None,
                color_attachments: &[Some(color_attachment.clone())],
                depth_stencil_attachment: Some(depth_stencil_attachment.clone()),
                timestamp_writes: None,
                occlusion_query_set: None,
            });
            render_pass.set_vertex_buffer(0, self.instance_buffers[0].buffer.slice(..));
            render_pass
        };
        while let Some(entry) = self.command_stream.pop() {
            if prev_entry.map(|prev_entry| prev_entry.cmp(&entry) != Ordering::Equal).unwrap_or(true) {
                match entry.operation {
                    RenderOperation::SaveAlphaContext => {
                        drop(render_pass);
                        render_pass = {
                            // The borrow checker does not understand that we already dropped render_pass
                            let encoder: &mut wgpu::CommandEncoder = unsafe { std::mem::transmute(&mut *encoder) };
                            let mut render_pass = contrast_renderer
                                .save_alpha_context(encoder, depth_stencil_attachment.view, entry.alpha_layer as usize)
                                .unwrap();
                            render_pass.set_vertex_buffer(0, self.instance_buffers[0].buffer.slice(..));
                            render_pass
                        };
                    }
                    RenderOperation::ScaleAlphaContext => {
                        drop(render_pass);
                        render_pass = {
                            // The borrow checker does not understand that we already dropped render_pass
                            let encoder: &mut wgpu::CommandEncoder = unsafe { std::mem::transmute(&mut *encoder) };
                            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                                label: None,
                                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                                    view: color_attachment.view,
                                    resolve_target: None,
                                    ops: wgpu::Operations {
                                        load: wgpu::LoadOp::Load,
                                        store: wgpu::StoreOp::Store,
                                    },
                                })],
                                depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachment {
                                    view: depth_stencil_attachment.view,
                                    depth_ops: Some(wgpu::Operations {
                                        load: wgpu::LoadOp::Load,
                                        store: wgpu::StoreOp::Discard,
                                    }),
                                    stencil_ops: Some(wgpu::Operations {
                                        load: wgpu::LoadOp::Load,
                                        store: wgpu::StoreOp::Store,
                                    }),
                                }),
                                timestamp_writes: None,
                                occlusion_query_set: None,
                            });
                            render_pass.set_vertex_buffer(0, self.instance_buffers[0].buffer.slice(..));
                            render_pass
                        };
                    }
                    RenderOperation::RestoreAlphaContext => contrast_renderer
                        .restore_alpha_context(&mut render_pass, entry.alpha_layer as usize)
                        .unwrap(),
                    _ => {}
                }
                if matches!(
                    entry.operation,
                    RenderOperation::Color | RenderOperation::ScaleAlphaContext | RenderOperation::RestoreAlphaContext
                ) {
                    render_pass.set_vertex_buffer(1, self.instance_buffers[1].buffer.slice(..));
                }
            }
            contrast_renderer.set_clip_depth(&mut render_pass, entry.clip_depth as usize).unwrap();
            entry.shape.render(
                contrast_renderer,
                &mut render_pass,
                entry.instance_index..entry.instance_index + 1,
                entry.operation,
            );
            prev_entry = Some(entry);
        }
        render_pass
    }
}
