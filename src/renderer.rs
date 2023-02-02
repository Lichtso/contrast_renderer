//! Rendering of [Path]s bundeled in [Shape]s using a [Renderer]

use crate::{
    error::Error,
    fill::FillBuilder,
    path::{DynamicStrokeOptions, Path, MAX_DASH_INTERVALS},
    safe_float::SafeFloat,
    stroke::StrokeBuilder,
    utils::{transmute_slice, transmute_vec},
    vertex::{triangle_fan_to_strip, Vertex0, Vertex2f, Vertex3f, Vertex4f},
};
use std::ops::Range;
use wgpu::{include_wgsl, util::DeviceExt, vertex_attr_array};

/// Color used in [RenderOperation::Color]
pub type Color = SafeFloat<f32, 4>;

#[derive(Default, Clone)]
#[repr(C)]
struct DynamicStrokeDescriptor {
    gap_start: [f32; MAX_DASH_INTERVALS],
    gap_end: [f32; MAX_DASH_INTERVALS],
    caps: u32,
    count_dashed_join: u32,
    phase: f32,
    _padding: u32,
}

fn convert_dynamic_stroke_options(dynamic_stroke_options: &DynamicStrokeOptions) -> Result<DynamicStrokeDescriptor, Error> {
    match dynamic_stroke_options {
        DynamicStrokeOptions::Dashed { join, pattern, phase } => {
            if pattern.len() > MAX_DASH_INTERVALS {
                return Err(Error::TooManyDashIntervals);
            }
            let mut result = DynamicStrokeDescriptor {
                gap_start: [0.0; MAX_DASH_INTERVALS],
                gap_end: [0.0; MAX_DASH_INTERVALS],
                caps: 0,
                count_dashed_join: ((pattern.len() as u32 - 1) << 3) | 4 | *join as u32,
                phase: phase.unwrap(),
                _padding: 0,
            };
            for (i, dash_interval) in pattern.iter().enumerate() {
                result.gap_start[i] = dash_interval.gap_start.unwrap();
                result.gap_end[i] = dash_interval.gap_end.unwrap();
                result.caps |= (dash_interval.dash_start as u32) << (((i + pattern.len() - 1) % pattern.len()) * 8);
                result.caps |= (dash_interval.dash_end as u32) << (i * 8 + 4);
            }
            Ok(result)
        }
        DynamicStrokeOptions::Solid { join, start, end } => Ok(DynamicStrokeDescriptor {
            gap_start: [0.0; MAX_DASH_INTERVALS],
            gap_end: [0.0; MAX_DASH_INTERVALS],
            caps: *start as u32 | ((*end as u32) << 4),
            count_dashed_join: *join as u32,
            phase: 0.0,
            _padding: 0,
        }),
    }
}

/// A wrapper around [wgpu::Buffer] that can be updated in place.
pub struct Buffer {
    /// Current length of the backing storage in bytes
    pub length: usize,
    /// Parameter of [Buffer::new]
    pub usage: wgpu::BufferUsages,
    /// Allocated backing storage
    pub buffer: wgpu::Buffer,
}

impl Buffer {
    /// Creates a new [Buffer]
    ///
    /// Note: `usage` must include `wgpu::BufferUsages::COPY_DST` so that [Buffer::update] can be used.
    pub fn new(device: &wgpu::Device, usage: wgpu::BufferUsages, data: &[u8]) -> Self {
        Self {
            length: data.len(),
            usage,
            buffer: device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: None,
                contents: data,
                usage,
            }),
        }
    }

    /// Updates an existing buffer in place if possible or reallocates it
    pub fn update(&mut self, device: &wgpu::Device, queue: &wgpu::Queue, data: &[u8]) {
        if data.len() == self.length {
            queue.write_buffer(&self.buffer, 0, data);
        } else {
            *self = Self::new(device, self.usage, data);
        }
    }

    /// Returns a [wgpu::BufferBinding] for a slice of this buffer
    pub fn get_binding<R: std::ops::RangeBounds<u64>>(&self, range: R) -> wgpu::BufferBinding {
        let offset = match range.start_bound() {
            std::ops::Bound::<&u64>::Included(index) => *index,
            std::ops::Bound::<&u64>::Excluded(index) => *index + 1,
            std::ops::Bound::<&u64>::Unbounded => 0,
        }
        .min(self.length as u64);
        let end_offset = match range.end_bound() {
            std::ops::Bound::<&u64>::Included(index) => *index + 1,
            std::ops::Bound::<&u64>::Excluded(index) => *index,
            std::ops::Bound::<&u64>::Unbounded => self.length as u64,
        }
        .min(self.length as u64);
        wgpu::BufferBinding {
            buffer: &self.buffer,
            offset,
            size: wgpu::BufferSize::new(end_offset - offset),
        }
    }
}

/// Concats the given sequence of [Buffer]s and serializes them into a `Vec<u8>`
#[macro_export]
macro_rules! concat_buffers {
    (count: $buffer:expr $(,)?) => {
        1
    };
    (count: $buffer:expr, $($rest:expr),+) => {
        concat_buffers!(count: $($rest),*) + 1
    };
    ([$($buffer:expr),* $(,)?]) => {{
        let buffers = [
            $($crate::utils::transmute_slice::<_, u8>($buffer)),*
        ];
        let mut end_offsets = [0; $crate::concat_buffers!(count: $($buffer),*)];
        let mut buffer_length = 0;
        for (i, buffer) in buffers.iter().enumerate() {
            buffer_length += buffer.len();
            end_offsets[i] = buffer_length;
        }
        let buffer_data = buffers.concat();
        (end_offsets, buffer_data)
    }};
}

/// Which shader to use for rendering a shape
#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum RenderOperation {
    /// Prepare rendering the [Shape]
    Stencil,
    /// Start using the rendered [Shape] as stencil for other [Shape]s
    Clip,
    /// Stop using the rendered [Shape] as stencil for other [Shape]s
    UnClip,
    /// Render the [Shape] as a solid color using alpha blending
    Color,
    /// Start using the rendered [Shape] as opacity group for other [Shape]s
    SaveAlphaContext,
    /// Second step of [RenderOperation::SaveAlphaContext], needs its own [wgpu::RenderPass]
    ScaleAlphaContext,
    /// Stop using the rendered [Shape] as opacity group for other [Shape]s
    RestoreAlphaContext,
}

/// A set of [Path]s which is always rendered together
pub struct Shape {
    vertex_offsets: [usize; 8],
    index_offsets: [usize; 3],
    dynamic_stroke_options_count: usize,
    vertex_buffer: Buffer,
    index_buffer: Buffer,
    stroke_buffer: Buffer,
    stroke_bind_group: Option<wgpu::BindGroup>,
}

impl Shape {
    /// Constructs a [Shape] from a set of [Path]s.
    ///
    /// If there are any stroked [Path]s then `dynamic_stroke_options` must not be empty.
    pub fn from_paths(
        device: &wgpu::Device,
        renderer: &Renderer,
        dynamic_stroke_options: &[DynamicStrokeOptions],
        paths: &[Path],
        existing_shape: Option<(Shape, &wgpu::Queue)>,
    ) -> Result<Self, Error> {
        let mut proto_hull = Vec::new();
        let mut stroke_builder = StrokeBuilder::default();
        let mut fill_builder = FillBuilder::default();
        for path in paths {
            if let Some(stroke_options) = &path.stroke_options {
                if stroke_options.dynamic_stroke_options_group >= dynamic_stroke_options.len() {
                    return Err(Error::DynamicStrokeOptionsIndexOutOfBounds);
                }
                stroke_builder.add_path(&mut proto_hull, path)?;
            } else {
                fill_builder.add_path(&mut proto_hull, path)?;
            }
        }
        let convex_hull = triangle_fan_to_strip(crate::convex_hull::andrew(&proto_hull));
        let (vertex_offsets, vertex_buffer) = concat_buffers!([
            &stroke_builder.line_vertices,
            &stroke_builder.joint_vertices,
            &fill_builder.solid_vertices,
            &fill_builder.integral_quadratic_vertices,
            &fill_builder.integral_cubic_vertices,
            &fill_builder.rational_quadratic_vertices,
            &fill_builder.rational_cubic_vertices,
            &convex_hull,
        ]);
        let (index_offsets, index_buffer) =
            concat_buffers!([&stroke_builder.line_indices, &stroke_builder.joint_indices, &fill_builder.solid_indices]);
        let stroke_buffer = transmute_vec(
            dynamic_stroke_options
                .iter()
                .map(convert_dynamic_stroke_options)
                .collect::<Result<Vec<DynamicStrokeDescriptor>, Error>>()?,
        );
        let (vertex_buffer, index_buffer, stroke_buffer) = if let Some((mut existing_shape, queue)) = existing_shape {
            existing_shape.vertex_buffer.update(device, queue, &vertex_buffer);
            existing_shape.index_buffer.update(device, queue, &index_buffer);
            existing_shape.stroke_buffer.update(device, queue, &stroke_buffer);
            (existing_shape.vertex_buffer, existing_shape.index_buffer, existing_shape.stroke_buffer)
        } else {
            (
                Buffer::new(device, wgpu::BufferUsages::VERTEX, &vertex_buffer),
                Buffer::new(device, wgpu::BufferUsages::INDEX, &index_buffer),
                Buffer::new(device, wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST, &stroke_buffer),
            )
        };
        let stroke_bind_group = if dynamic_stroke_options.is_empty() {
            None
        } else {
            Some(device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: None,
                layout: &renderer.stroke_bind_group_layout,
                entries: &[wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::Buffer(stroke_buffer.get_binding(..)),
                }],
            }))
        };
        Ok(Self {
            vertex_offsets,
            index_offsets,
            dynamic_stroke_options_count: dynamic_stroke_options.len(),
            vertex_buffer,
            index_buffer,
            stroke_buffer,
            stroke_bind_group,
        })
    }

    /// Renderes instances of the [Shape] using the provided `operation` into the given `render_pass`.
    ///
    /// To simply render this [Shape] do:
    /// ```
    ///   shape.render(&renderer, &mut render_pass, instance_indices, RenderOperation::Stencil);
    ///   shape.render(&renderer, &mut render_pass, instance_indices, RenderOperation::Color);
    /// ```
    /// To clip other [Shape]s with this one do:
    /// ```
    ///   shape.render(&renderer, &mut render_pass, instance_indices, RenderOperation::Stencil);
    ///   clip_depth += 1; renderer.set_clip_depth(&mut render_pass, clip_depth);
    ///   shape.render(&renderer, &mut render_pass, instance_indices, RenderOperation::Clip);
    ///   // render what should be clipped
    ///   clip_depth -= 1; renderer.set_clip_depth(&mut render_pass, clip_depth);
    ///   shape.render(&renderer, &mut render_pass, instance_indices, RenderOperation::UnClip);
    /// ```
    pub fn render<'a>(
        &'a self,
        renderer: &'a Renderer,
        render_pass: &mut wgpu::RenderPass<'a>,
        instance_indices: Range<u32>,
        render_operation: RenderOperation,
    ) {
        match render_operation {
            RenderOperation::Stencil => {
                if let Some(stroke_bind_group) = &self.stroke_bind_group {
                    render_pass.set_bind_group(0, stroke_bind_group, &[]);
                    if self.vertex_offsets[0] > 0 {
                        render_pass.set_pipeline(&renderer.stroke_line_pipeline);
                        render_pass.set_vertex_buffer(1, self.vertex_buffer.buffer.slice(0..self.vertex_offsets[0] as u64));
                        render_pass.set_index_buffer(self.index_buffer.buffer.slice(0..self.index_offsets[0] as u64), wgpu::IndexFormat::Uint16);
                        render_pass.draw_indexed(
                            0..(self.index_offsets[0] / std::mem::size_of::<u16>()) as u32,
                            0,
                            instance_indices.clone(),
                        );
                    }
                    let begin_offset = self.vertex_offsets[0];
                    let end_offset = self.vertex_offsets[1];
                    if begin_offset < end_offset {
                        render_pass.set_pipeline(&renderer.stroke_joint_pipeline);
                        render_pass.set_vertex_buffer(1, self.vertex_buffer.buffer.slice(begin_offset as u64..end_offset as u64));
                        render_pass.set_index_buffer(
                            self.index_buffer.buffer.slice(self.index_offsets[0] as u64..self.index_offsets[1] as u64),
                            wgpu::IndexFormat::Uint16,
                        );
                        render_pass.draw_indexed(
                            0..((self.index_offsets[1] - self.index_offsets[0]) / std::mem::size_of::<u16>()) as u32,
                            0,
                            instance_indices.clone(),
                        );
                    }
                }
                let begin_offset = self.vertex_offsets[1];
                let end_offset = self.vertex_offsets[2];
                if begin_offset < end_offset {
                    render_pass.set_pipeline(&renderer.fill_solid_pipeline);
                    render_pass.set_vertex_buffer(1, self.vertex_buffer.buffer.slice(begin_offset as u64..end_offset as u64));
                    render_pass.set_index_buffer(
                        self.index_buffer.buffer.slice(self.index_offsets[1] as u64..self.index_offsets[2] as u64),
                        wgpu::IndexFormat::Uint16,
                    );
                    render_pass.draw_indexed(
                        0..((self.index_offsets[2] - self.index_offsets[1]) / std::mem::size_of::<u16>()) as u32,
                        0,
                        instance_indices.clone(),
                    );
                }
                for (i, (pipeline, vertex_size)) in [
                    (&renderer.fill_integral_quadratic_curve_pipeline, std::mem::size_of::<Vertex2f>()),
                    (&renderer.fill_integral_cubic_curve_pipeline, std::mem::size_of::<Vertex3f>()),
                    (&renderer.fill_rational_quadratic_curve_pipeline, std::mem::size_of::<Vertex3f>()),
                    (&renderer.fill_rational_cubic_curve_pipeline, std::mem::size_of::<Vertex4f>()),
                ]
                .iter()
                .enumerate()
                {
                    let begin_offset = self.vertex_offsets[i + 2];
                    let end_offset = self.vertex_offsets[i + 3];
                    if begin_offset < end_offset {
                        render_pass.set_pipeline(pipeline);
                        render_pass.set_vertex_buffer(1, self.vertex_buffer.buffer.slice(begin_offset as u64..end_offset as u64));
                        render_pass.draw(0..((end_offset - begin_offset) / vertex_size) as u32, instance_indices.clone());
                    }
                }
                return;
            }
            RenderOperation::Clip => render_pass.set_pipeline(&renderer.increment_clip_nesting_counter_pipeline),
            RenderOperation::UnClip => render_pass.set_pipeline(&renderer.decrement_clip_nesting_counter_pipeline),
            RenderOperation::Color => render_pass.set_pipeline(&renderer.color_cover_pipeline),
            RenderOperation::SaveAlphaContext => render_pass.set_pipeline(&renderer.save_alpha_context_cover_pipeline),
            RenderOperation::ScaleAlphaContext => render_pass.set_pipeline(&renderer.scale_alpha_context_cover_pipeline),
            RenderOperation::RestoreAlphaContext => render_pass.set_pipeline(&renderer.restore_alpha_context_cover_pipeline),
        }
        let begin_offset = self.vertex_offsets[6];
        let end_offset = self.vertex_offsets[7];
        render_pass.set_vertex_buffer(
            match render_operation {
                RenderOperation::Color | RenderOperation::ScaleAlphaContext | RenderOperation::RestoreAlphaContext => 2,
                _ => 1,
            },
            self.vertex_buffer.buffer.slice(begin_offset as u64..end_offset as u64),
        );
        render_pass.draw(0..((end_offset - begin_offset) / std::mem::size_of::<Vertex0>()) as u32, instance_indices);
    }

    /// Sets the dash pattern of stroked [Path]s for subsequent stencil rendering calls.
    ///
    /// Call before creating the next [wgpu::RenderPass].
    pub fn set_dynamic_stroke_options(
        &self,
        queue: &wgpu::Queue,
        dynamic_stroke_options_group_index: usize,
        dynamic_stroke_options_group: &DynamicStrokeOptions,
    ) -> Result<(), Error> {
        if dynamic_stroke_options_group_index >= self.dynamic_stroke_options_count {
            return Err(Error::DynamicStrokeOptionsIndexOutOfBounds);
        }
        let data = [convert_dynamic_stroke_options(dynamic_stroke_options_group).unwrap()];
        queue.write_buffer(
            &self.stroke_buffer.buffer,
            (std::mem::size_of::<DynamicStrokeDescriptor>() * dynamic_stroke_options_group_index) as u64,
            transmute_slice(&data),
        );
        Ok(())
    }
}

/// The configurable parameters of [Renderer]
pub struct Configuration {
    /// Defines the blending of the default color cover shader
    pub blending: wgpu::ColorTargetState,
    /// Defines the cull mode of the default color cover shader
    pub cull_mode: Option<wgpu::Face>,
    /// Defines the texture format for the depth and stencil buffer
    pub depth_stencil_format: wgpu::TextureFormat,
    /// Defines the depth compare function of the default color cover shader
    pub depth_compare: wgpu::CompareFunction,
    /// Defines if the default color cover shader writes back into the depth buffer
    pub depth_write_enabled: bool,
    /// Creates a phony color attachment for stencil shaders so that they can be used in the same render pass as the cover shader
    pub color_attachment_in_stencil_pass: bool,
    /// Number of MSAA samples used by the frame buffer
    pub msaa_sample_count: u32,
    /// Up to 2 to the power of `clip_nesting_counter_bits` nested clip [Shape]s are possible
    ///
    /// Wgpu only supports 8 stencil bits so the sum of `clip_nesting_counter_bits` and `winding_counter_bits` can be 8 at most.
    pub clip_nesting_counter_bits: usize,
    /// The winding rule is: Non zero modulo 2 to the power of `winding_counter_bits`
    ///
    /// Thus, setting `winding_counter_bits` to 1 will result in the even-odd winding rule.
    pub winding_counter_bits: usize,
    /// Up to `alpha_layer_count` nested partially transparent layers of [Shape]s are possible
    pub alpha_layer_count: usize,
}

/// The rendering interface for [wgpu]
pub struct Renderer {
    config: Configuration,
    stroke_bind_group_layout: wgpu::BindGroupLayout,
    alpha_context_cover_bind_group_layout: wgpu::BindGroupLayout,
    stroke_line_pipeline: wgpu::RenderPipeline,
    stroke_joint_pipeline: wgpu::RenderPipeline,
    fill_solid_pipeline: wgpu::RenderPipeline,
    fill_integral_quadratic_curve_pipeline: wgpu::RenderPipeline,
    fill_integral_cubic_curve_pipeline: wgpu::RenderPipeline,
    fill_rational_quadratic_curve_pipeline: wgpu::RenderPipeline,
    fill_rational_cubic_curve_pipeline: wgpu::RenderPipeline,
    increment_clip_nesting_counter_pipeline: wgpu::RenderPipeline,
    decrement_clip_nesting_counter_pipeline: wgpu::RenderPipeline,
    color_cover_pipeline: wgpu::RenderPipeline,
    save_alpha_context_cover_pipeline: wgpu::RenderPipeline,
    scale_alpha_context_cover_pipeline: wgpu::RenderPipeline,
    restore_alpha_context_cover_pipeline: wgpu::RenderPipeline,
    save_alpha_context_cover_bind_group: Option<wgpu::BindGroup>,
    save_alpha_context_cover_render_targets: Vec<wgpu::TextureView>,
    restore_alpha_context_cover_bind_groups: Vec<wgpu::BindGroup>,
}

impl Renderer {
    /// Constructs a new [Renderer].
    pub fn new(device: &wgpu::Device, config: Configuration) -> Result<Self, Error> {
        if config.winding_counter_bits == 0 || config.clip_nesting_counter_bits + config.winding_counter_bits > 8 {
            return Err(Error::NumberOfStencilBitsIsUnsupported);
        }

        macro_rules! stencil_descriptor {
            ($compare:ident, $fail:ident, $pass:ident) => {
                wgpu::StencilFaceState {
                    compare: wgpu::CompareFunction::$compare,
                    fail_op: wgpu::StencilOperation::$fail,
                    depth_fail_op: wgpu::StencilOperation::Keep,
                    pass_op: wgpu::StencilOperation::$pass,
                }
            };
        }

        macro_rules! render_pipeline_descriptor {
            ($pipeline_layout:expr,
             $shader_module:expr, $vertex_entry:expr, $fragment_entry:expr,
             $primitive_topology:ident, $primitive_index_format:expr,
             $cull_mode:expr, $depth_compare:expr, $depth_write_enabled:expr,
             $color_states:expr, $stencil_state:expr,
             [$($vertex_buffer:expr),*] $(,)?) => {
                wgpu::RenderPipelineDescriptor {
                    label: None,
                    layout: Some($pipeline_layout),
                    vertex: wgpu::VertexState {
                        module: $shader_module,
                        entry_point: $vertex_entry,
                        buffers: &[wgpu::VertexBufferLayout {
                            array_stride: (16 * 4) as wgpu::BufferAddress,
                            step_mode: wgpu::VertexStepMode::Instance,
                            attributes: &vertex_attr_array![0 => Float32x4, 1 => Float32x4, 2 => Float32x4, 3 => Float32x4],
                        }, $($vertex_buffer,)*],
                    },
                    fragment: Some(wgpu::FragmentState {
                        module: $shader_module,
                        entry_point: $fragment_entry,
                        targets: $color_states,
                    }),
                    primitive: wgpu::PrimitiveState {
                        topology: wgpu::PrimitiveTopology::$primitive_topology,
                        strip_index_format: $primitive_index_format,
                        front_face: wgpu::FrontFace::Ccw,
                        unclipped_depth: false,
                        cull_mode: $cull_mode,
                        conservative: false,
                        polygon_mode: wgpu::PolygonMode::Fill,
                    },
                    depth_stencil: Some(wgpu::DepthStencilState {
                        format: config.depth_stencil_format,
                        depth_write_enabled: $depth_write_enabled,
                        depth_compare: $depth_compare,
                        bias: wgpu::DepthBiasState::default(),
                        stencil: $stencil_state,
                    }),
                    multisample: wgpu::MultisampleState {
                        count: config.msaa_sample_count,
                        mask: !0,
                        alpha_to_coverage_enabled: false,
                    },
                    multiview: None,
                }
            };
        }

        let shader_module = device.create_shader_module(include_wgsl!("shaders.wgsl"));
        let color_instance_buffer_layout = wgpu::VertexBufferLayout {
            array_stride: (4 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Instance,
            attributes: &vertex_attr_array![5 => Float32x4],
        };
        let segment_0f_vertex_buffer_layout = wgpu::VertexBufferLayout {
            array_stride: (2 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &vertex_attr_array![4 => Float32x2],
        };
        let segment_2f_vertex_buffer_layout = wgpu::VertexBufferLayout {
            array_stride: (4 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &vertex_attr_array![4 => Float32x2, 5 => Float32x2],
        };
        let segment_2f1i_vertex_buffer_layout = wgpu::VertexBufferLayout {
            array_stride: (5 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &vertex_attr_array![4 => Float32x2, 5 => Float32x2, 6 => Uint32],
        };
        let segment_3f_vertex_buffer_layout = wgpu::VertexBufferLayout {
            array_stride: (5 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &vertex_attr_array![4 => Float32x2, 5 => Float32x3],
        };
        let segment_3f1i_vertex_buffer_layout = wgpu::VertexBufferLayout {
            array_stride: (6 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &vertex_attr_array![4 => Float32x2, 5 => Float32x3, 6 => Uint32],
        };
        let segment_4f_vertex_buffer_layout = wgpu::VertexBufferLayout {
            array_stride: (6 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &vertex_attr_array![4 => Float32x2, 5 => Float32x4],
        };

        let stroke_bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: None,
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Storage { read_only: true },
                    has_dynamic_offset: false,
                    min_binding_size: wgpu::BufferSize::new(std::mem::size_of::<DynamicStrokeDescriptor>() as u64),
                },
                count: None,
            }],
        });
        let alpha_context_cover_bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: None,
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Texture {
                    sample_type: wgpu::TextureSampleType::Float { filterable: false },
                    view_dimension: wgpu::TextureViewDimension::D2,
                    multisampled: config.msaa_sample_count > 1,
                },
                count: None,
            }],
        });

        let winding_counter_mask = (1 << config.winding_counter_bits) - 1;
        let clip_nesting_counter_mask = ((1 << config.clip_nesting_counter_bits) - 1) << config.winding_counter_bits;
        let phony_color_state = config.color_attachment_in_stencil_pass.then_some(wgpu::ColorTargetState {
            write_mask: wgpu::ColorWrites::empty(),
            ..config.blending
        });
        let stroke_stencil_state = wgpu::StencilState {
            front: stencil_descriptor!(Equal, Keep, IncrementWrap),
            back: stencil_descriptor!(Equal, Keep, IncrementWrap),
            read_mask: clip_nesting_counter_mask | winding_counter_mask,
            write_mask: winding_counter_mask,
        };
        let fill_stencil_state = wgpu::StencilState {
            front: stencil_descriptor!(LessEqual, Keep, IncrementWrap),
            back: stencil_descriptor!(LessEqual, Keep, DecrementWrap),
            read_mask: clip_nesting_counter_mask | winding_counter_mask,
            write_mask: winding_counter_mask,
        };
        let stroke_line_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&stroke_bind_group_layout],
            push_constant_ranges: &[],
        });
        let stencil_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[],
            push_constant_ranges: &[],
        });
        let stroke_line_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stroke_line_pipeline_layout,
            &shader_module,
            "vertex2f1u",
            "stencil_stroke_line",
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            None,
            wgpu::CompareFunction::Always,
            false,
            &[phony_color_state.clone()],
            stroke_stencil_state.clone(),
            [segment_2f1i_vertex_buffer_layout.clone()],
        ));
        let stroke_joint_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stroke_line_pipeline_layout,
            &shader_module,
            "vertex3f1u",
            "stencil_stroke_joint",
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            None,
            wgpu::CompareFunction::Always,
            false,
            &[phony_color_state.clone()],
            stroke_stencil_state,
            [segment_3f1i_vertex_buffer_layout.clone()],
        ));
        let fill_solid_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &shader_module,
            "vertex0",
            "stencil_solid",
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            None,
            wgpu::CompareFunction::Always,
            false,
            &[phony_color_state.clone()],
            fill_stencil_state.clone(),
            [segment_0f_vertex_buffer_layout.clone()],
        ));
        let fill_integral_quadratic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &shader_module,
            "vertex2f",
            "stencil_integral_quadratic_curve",
            TriangleList,
            None,
            None,
            wgpu::CompareFunction::Always,
            false,
            &[phony_color_state.clone()],
            fill_stencil_state.clone(),
            [segment_2f_vertex_buffer_layout],
        ));
        let fill_integral_cubic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &shader_module,
            "vertex3f",
            "stencil_integral_cubic_curve",
            TriangleList,
            None,
            None,
            wgpu::CompareFunction::Always,
            false,
            &[phony_color_state.clone()],
            fill_stencil_state.clone(),
            [segment_3f_vertex_buffer_layout.clone()],
        ));
        let fill_rational_quadratic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &shader_module,
            "vertex3f",
            "stencil_rational_quadratic_curve",
            TriangleList,
            None,
            None,
            wgpu::CompareFunction::Always,
            false,
            &[phony_color_state.clone()],
            fill_stencil_state.clone(),
            [segment_3f_vertex_buffer_layout.clone()],
        ));
        let fill_rational_cubic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &shader_module,
            "vertex4f",
            "stencil_rational_cubic_curve",
            TriangleList,
            None,
            None,
            wgpu::CompareFunction::Always,
            false,
            &[phony_color_state.clone()],
            fill_stencil_state,
            [segment_4f_vertex_buffer_layout],
        ));

        let increment_clip_nesting_counter_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &shader_module,
            "vertex0",
            "stencil_solid",
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            None,
            wgpu::CompareFunction::Always,
            false,
            &[phony_color_state.clone()],
            wgpu::StencilState {
                front: stencil_descriptor!(NotEqual, Keep, Replace),
                back: stencil_descriptor!(NotEqual, Keep, Replace),
                read_mask: winding_counter_mask,
                write_mask: clip_nesting_counter_mask | winding_counter_mask,
            },
            [segment_0f_vertex_buffer_layout.clone()],
        ));
        let decrement_clip_nesting_counter_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &shader_module,
            "vertex0",
            "stencil_solid",
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            None,
            wgpu::CompareFunction::Always,
            false,
            &[phony_color_state],
            wgpu::StencilState {
                front: stencil_descriptor!(Less, Keep, Replace),
                back: stencil_descriptor!(Less, Keep, Replace),
                read_mask: clip_nesting_counter_mask,
                write_mask: clip_nesting_counter_mask | winding_counter_mask,
            },
            [segment_0f_vertex_buffer_layout.clone()],
        ));

        let color_cover_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[],
            push_constant_ranges: &[],
        });
        let color_cover_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &color_cover_pipeline_layout,
            &shader_module,
            "vertex_color",
            "color_cover",
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            config.cull_mode,
            config.depth_compare,
            config.depth_write_enabled,
            &[Some(config.blending.clone())],
            wgpu::StencilState {
                front: stencil_descriptor!(Less, Zero, Zero),
                back: stencil_descriptor!(Less, Zero, Zero),
                read_mask: clip_nesting_counter_mask | winding_counter_mask,
                write_mask: winding_counter_mask,
            },
            [color_instance_buffer_layout.clone(), segment_0f_vertex_buffer_layout.clone()],
        ));

        let alpha_context_cover_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&alpha_context_cover_bind_group_layout],
            push_constant_ranges: &[],
        });
        let alpha_context_cover_stencil_state = wgpu::StencilState {
            front: stencil_descriptor!(LessEqual, Zero, Zero),
            back: stencil_descriptor!(LessEqual, Zero, Zero),
            read_mask: clip_nesting_counter_mask | winding_counter_mask,
            write_mask: 0,
        };
        let save_alpha_context_cover_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &alpha_context_cover_pipeline_layout,
            &shader_module,
            "vertex0",
            if config.msaa_sample_count == 1 {
                "save_alpha_context_cover"
            } else {
                "multisampled_save_alpha_context_cover"
            },
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            None,
            wgpu::CompareFunction::Always,
            false,
            &[Some(wgpu::ColorTargetState {
                format: wgpu::TextureFormat::R8Unorm,
                blend: Some(wgpu::BlendState {
                    color: wgpu::BlendComponent {
                        src_factor: wgpu::BlendFactor::One,
                        dst_factor: wgpu::BlendFactor::Zero,
                        operation: wgpu::BlendOperation::Add,
                    },
                    alpha: wgpu::BlendComponent {
                        src_factor: wgpu::BlendFactor::Zero,
                        dst_factor: wgpu::BlendFactor::Zero,
                        operation: wgpu::BlendOperation::Add,
                    },
                }),
                write_mask: wgpu::ColorWrites::RED,
            })],
            alpha_context_cover_stencil_state.clone(),
            [segment_0f_vertex_buffer_layout.clone()],
        ));
        let scale_alpha_context_cover_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &color_cover_pipeline_layout,
            &shader_module,
            "vertex_color",
            "scale_alpha_context_cover",
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            None,
            wgpu::CompareFunction::Always,
            false,
            &[Some(wgpu::ColorTargetState {
                format: config.blending.format,
                blend: Some(wgpu::BlendState {
                    color: wgpu::BlendComponent {
                        src_factor: wgpu::BlendFactor::Zero,
                        dst_factor: wgpu::BlendFactor::Zero,
                        operation: wgpu::BlendOperation::Add,
                    },
                    alpha: wgpu::BlendComponent {
                        src_factor: wgpu::BlendFactor::One,
                        dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
                        operation: wgpu::BlendOperation::Add,
                    },
                }),
                write_mask: wgpu::ColorWrites::ALPHA,
            })],
            alpha_context_cover_stencil_state.clone(),
            [color_instance_buffer_layout.clone(), segment_0f_vertex_buffer_layout.clone()],
        ));
        let restore_alpha_context_cover_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &alpha_context_cover_pipeline_layout,
            &shader_module,
            "vertex_color",
            if config.msaa_sample_count == 1 {
                "restore_alpha_context_cover"
            } else {
                "multisampled_restore_alpha_context_cover"
            },
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            None,
            wgpu::CompareFunction::Always,
            false,
            &[Some(wgpu::ColorTargetState {
                format: config.blending.format,
                blend: Some(wgpu::BlendState {
                    color: wgpu::BlendComponent {
                        src_factor: wgpu::BlendFactor::Zero,
                        dst_factor: wgpu::BlendFactor::Zero,
                        operation: wgpu::BlendOperation::Add,
                    },
                    alpha: wgpu::BlendComponent {
                        src_factor: wgpu::BlendFactor::One,
                        dst_factor: wgpu::BlendFactor::One,
                        operation: wgpu::BlendOperation::ReverseSubtract,
                    },
                }),
                write_mask: wgpu::ColorWrites::ALPHA,
            })],
            alpha_context_cover_stencil_state,
            [color_instance_buffer_layout, segment_0f_vertex_buffer_layout],
        ));

        Ok(Self {
            config,
            stroke_bind_group_layout,
            alpha_context_cover_bind_group_layout,
            stroke_line_pipeline,
            stroke_joint_pipeline,
            fill_solid_pipeline,
            fill_integral_quadratic_curve_pipeline,
            fill_integral_cubic_curve_pipeline,
            fill_rational_quadratic_curve_pipeline,
            fill_rational_cubic_curve_pipeline,
            increment_clip_nesting_counter_pipeline,
            decrement_clip_nesting_counter_pipeline,
            color_cover_pipeline,
            save_alpha_context_cover_pipeline,
            scale_alpha_context_cover_pipeline,
            restore_alpha_context_cover_pipeline,
            save_alpha_context_cover_bind_group: None,
            save_alpha_context_cover_render_targets: Vec::new(),
            restore_alpha_context_cover_bind_groups: Vec::new(),
        })
    }

    /// Returns the [Configuration]
    pub fn get_config(&self) -> &Configuration {
        &self.config
    }

    /// Call this after initialization and when the viewport / window has been resized.
    pub fn resize_internal_buffers(&mut self, device: &wgpu::Device, viewport_size: wgpu::Extent3d, msaa_frame_view: &wgpu::TextureView) {
        let alpha_context_texture_descriptor = wgpu::TextureDescriptor {
            size: viewport_size,
            mip_level_count: 1,
            sample_count: self.config.msaa_sample_count,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::R8Unorm,
            view_formats: &[wgpu::TextureFormat::R8Unorm],
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::TEXTURE_BINDING,
            label: None,
        };
        self.save_alpha_context_cover_bind_group = Some(device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: &self.alpha_context_cover_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(msaa_frame_view),
            }],
        }));
        (self.save_alpha_context_cover_render_targets, self.restore_alpha_context_cover_bind_groups) = (0..self.config.alpha_layer_count as u32)
            .map(|_layer_index| {
                let alpha_context_texture = device.create_texture(&alpha_context_texture_descriptor);
                let alpha_context_texture_view = alpha_context_texture.create_view(&wgpu::TextureViewDescriptor {
                    dimension: Some(wgpu::TextureViewDimension::D2),
                    ..wgpu::TextureViewDescriptor::default()
                });
                let restore_alpha_context_cover_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
                    label: None,
                    layout: &self.alpha_context_cover_bind_group_layout,
                    entries: &[wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::TextureView(&alpha_context_texture_view),
                    }],
                });
                (alpha_context_texture_view, restore_alpha_context_cover_bind_group)
            })
            .unzip();
    }

    /// Increment before [RenderOperation::Clip] and decrement again before [RenderOperation::UnClip].
    pub fn set_clip_depth(&self, render_pass: &mut wgpu::RenderPass, clip_depth: usize) -> Result<(), Error> {
        if clip_depth >= (1 << self.config.clip_nesting_counter_bits) {
            return Err(Error::ClipStackOverflow);
        }
        render_pass.set_stencil_reference((clip_depth << self.config.winding_counter_bits) as u32);
        Ok(())
    }

    /// Call before [RenderOperation::SaveAlphaContext].
    pub fn save_alpha_context<'a, 'b: 'a>(
        &'a self,
        encoder: &'b mut wgpu::CommandEncoder,
        depth_stencil_texture_view: &'a wgpu::TextureView,
        alpha_layer: usize,
    ) -> Result<wgpu::RenderPass, Error> {
        if alpha_layer >= self.config.alpha_layer_count {
            return Err(Error::TooManyNestedOpacityGroups);
        }
        let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: None,
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: &self.save_alpha_context_cover_render_targets[alpha_layer],
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Load,
                    store: true,
                },
            })],
            depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachment {
                view: depth_stencil_texture_view,
                depth_ops: Some(wgpu::Operations {
                    load: wgpu::LoadOp::Load,
                    store: false,
                }),
                stencil_ops: Some(wgpu::Operations {
                    load: wgpu::LoadOp::Load,
                    store: true,
                }),
            }),
        });
        render_pass.set_bind_group(0, self.save_alpha_context_cover_bind_group.as_ref().unwrap(), &[]);
        Ok(render_pass)
    }

    /// Call before [RenderOperation::RestoreAlphaContext].
    pub fn restore_alpha_context<'a, 'b: 'a>(&'b self, render_pass: &mut wgpu::RenderPass<'a>, alpha_layer: usize) -> Result<(), Error> {
        if alpha_layer >= self.config.alpha_layer_count {
            return Err(Error::TooManyNestedOpacityGroups);
        }
        render_pass.set_bind_group(0, &self.restore_alpha_context_cover_bind_groups[alpha_layer], &[]);
        Ok(())
    }
}
