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
use wgpu::{include_wgsl, util::DeviceExt, vertex_attr_array};

/// Color used in [Renderer::set_solid_color]
pub type Color = SafeFloat<f32, 4>;

#[derive(Default, Clone)]
#[repr(C)]
struct DynamicStrokeDescriptor {
    gap_start: [f32; MAX_DASH_INTERVALS],
    gap_end: [f32; MAX_DASH_INTERVALS],
    caps: u32,
    meta: u32,
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
                meta: ((pattern.len() as u32 - 1) << 3) | 4 | *join as u32,
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
            meta: *join as u32,
            phase: 0.0,
            _padding: 0,
        }),
    }
}

macro_rules! concat_buffers {
    ($device:expr, $usage:ident, $buffer_count:expr, [$($buffer:expr,)*]) => {{
        let mut buffers = [
            $(transmute_vec::<_, u8>($buffer)),*
        ];
        let mut offsets = [0; $buffer_count];
        let mut buffer_length = 0;
        for (i, buffer) in buffers.iter().enumerate() {
            buffer_length += buffer.len();
            offsets[i] = buffer_length;
        }
        let mut buffer_data: Vec<u8> = Vec::with_capacity(buffer_length);
        for mut buffer in &mut buffers {
            buffer_data.append(&mut buffer);
        }
        (
            offsets,
            $device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: None,
                contents: &buffer_data,
                usage: wgpu::BufferUsage::$usage,
            })
        )
    }};
}

/// A set of [Path]s which is always rendered together
pub struct Shape {
    vertex_offsets: [usize; 8],
    index_offsets: [usize; 3],
    stroke_uniform_buffer: Option<wgpu::Buffer>,
    stroke_bind_group: Option<wgpu::BindGroup>,
    dynamic_stroke_options_count: usize,
    vertex_buffer: wgpu::Buffer,
    index_buffer: wgpu::Buffer,
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
    ) -> Result<Self, Error> {
        let max_dynamic_stroke_options_group =
            device.limits().max_uniform_buffer_binding_size as usize / std::mem::size_of::<DynamicStrokeDescriptor>();
        if dynamic_stroke_options.len() > max_dynamic_stroke_options_group {
            return Err(Error::TooManyDynamicStrokeOptionsGroup);
        }
        let mut proto_hull = Vec::new();
        let mut stroke_builder = StrokeBuilder::default();
        let mut fill_builder = FillBuilder::default();
        for path in paths {
            if let Some(stroke_options) = &path.stroke_options {
                if stroke_options.dynamic_stroke_options_group >= dynamic_stroke_options.len() {
                    return Err(Error::DynamicStrokeOptionsIndexOutOfBounds);
                }
                stroke_builder.add_path(&mut proto_hull, &path)?;
            } else {
                fill_builder.add_path(&mut proto_hull, &path)?;
            }
        }
        let (stroke_uniform_buffer, stroke_bind_group) = if dynamic_stroke_options.is_empty() {
            (None, None)
        } else {
            let mut dynamic_stroke_descriptors = vec![DynamicStrokeDescriptor::default(); max_dynamic_stroke_options_group];
            for (i, dynamic_stroke_descriptor) in dynamic_stroke_options.iter().enumerate() {
                dynamic_stroke_descriptors[i] = convert_dynamic_stroke_options(dynamic_stroke_descriptor)?;
            }
            let dynamic_stroke_descriptors = transmute_slice(dynamic_stroke_descriptors.as_slice());
            let stroke_uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: None,
                contents: dynamic_stroke_descriptors,
                usage: wgpu::BufferUsage::UNIFORM | wgpu::BufferUsage::COPY_DST,
            });
            let stroke_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: None,
                layout: &renderer.stroke_bind_group_layout,
                entries: &[wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
                        buffer: &stroke_uniform_buffer,
                        offset: 0,
                        size: wgpu::BufferSize::new(dynamic_stroke_descriptors.len() as u64),
                    }),
                }],
            });
            (Some(stroke_uniform_buffer), Some(stroke_bind_group))
        };
        let convex_hull = crate::convex_hull::andrew(&proto_hull);
        let (vertex_offsets, vertex_buffer) = concat_buffers!(
            device,
            VERTEX,
            8,
            [
                stroke_builder.line_vertices,
                stroke_builder.joint_vertices,
                fill_builder.solid_vertices,
                fill_builder.integral_quadratic_vertices,
                fill_builder.integral_cubic_vertices,
                fill_builder.rational_quadratic_vertices,
                fill_builder.rational_cubic_vertices,
                triangle_fan_to_strip(convex_hull),
            ]
        );
        let (index_offsets, index_buffer) = concat_buffers!(
            device,
            INDEX,
            3,
            [stroke_builder.line_indices, stroke_builder.joint_indices, fill_builder.solid_indices,]
        );
        Ok(Self {
            vertex_offsets,
            index_offsets,
            stroke_uniform_buffer,
            stroke_bind_group,
            dynamic_stroke_options_count: dynamic_stroke_options.len(),
            vertex_buffer,
            index_buffer,
        })
    }

    /// Renderes the [Shape] into the stencil buffer.
    pub fn render_stencil<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>) {
        render_pass.set_bind_group(0, &renderer.transform_bind_group, &[]);
        if let Some(stroke_bind_group) = &self.stroke_bind_group {
            render_pass.set_pipeline(&renderer.stroke_line_pipeline);
            render_pass.set_bind_group(1, stroke_bind_group, &[]);
            render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(0..self.vertex_offsets[0] as u64));
            render_pass.set_index_buffer(self.index_buffer.slice(0..self.index_offsets[0] as u64), wgpu::IndexFormat::Uint16);
            render_pass.draw_indexed(0..(self.index_offsets[0] / std::mem::size_of::<u16>()) as u32, 0, 0..1);
            let begin_offset = self.vertex_offsets[0];
            let end_offset = self.vertex_offsets[1];
            if begin_offset < end_offset {
                render_pass.set_pipeline(&renderer.stroke_joint_pipeline);
                render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(begin_offset as u64..end_offset as u64));
                render_pass.set_index_buffer(
                    self.index_buffer.slice(self.index_offsets[0] as u64..self.index_offsets[1] as u64),
                    wgpu::IndexFormat::Uint16,
                );
                render_pass.draw_indexed(
                    0..((self.index_offsets[1] - self.index_offsets[0]) / std::mem::size_of::<u16>()) as u32,
                    0,
                    0..1,
                );
            }
        }
        let begin_offset = self.vertex_offsets[1];
        let end_offset = self.vertex_offsets[2];
        if begin_offset < end_offset {
            render_pass.set_pipeline(&renderer.fill_solid_pipeline);
            render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(begin_offset as u64..end_offset as u64));
            render_pass.set_index_buffer(
                self.index_buffer.slice(self.index_offsets[1] as u64..self.index_offsets[2] as u64),
                wgpu::IndexFormat::Uint16,
            );
            render_pass.draw_indexed(
                0..((self.index_offsets[2] - self.index_offsets[1]) / std::mem::size_of::<u16>()) as u32,
                0,
                0..1,
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
                render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(begin_offset as u64..end_offset as u64));
                render_pass.draw(0..((end_offset - begin_offset) / vertex_size) as u32, 0..1);
            }
        }
    }

    /// Renderes the [Shape] using a user provided [wgpu::RenderPipeline].
    ///
    /// Requires the [Shape] to be stenciled and [render_pass.set_pipeline()] to be called first.
    fn render_cover<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>, clip_stack_height: usize) {
        let begin_offset = self.vertex_offsets[6];
        let end_offset = self.vertex_offsets[7];
        render_pass.set_stencil_reference((clip_stack_height << renderer.winding_counter_bits) as u32);
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(begin_offset as u64..end_offset as u64));
        render_pass.draw(0..((end_offset - begin_offset) / std::mem::size_of::<Vertex0>()) as u32, 0..1);
    }

    /// Fills the [Shape] with a solid color.
    ///
    /// Requires the [Shape] to be stenciled first.
    pub fn render_color_solid<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>, clip_stack_height: usize) {
        render_pass.set_bind_group(0, &renderer.transform_bind_group, &[]);
        render_pass.set_bind_group(1, &renderer.color_solid_bind_group, &[]);
        render_pass.set_pipeline(&renderer.color_solid_pipeline);
        self.render_cover(renderer, render_pass, clip_stack_height);
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
            self.stroke_uniform_buffer.as_ref().unwrap(),
            (std::mem::size_of::<DynamicStrokeDescriptor>() * dynamic_stroke_options_group_index) as u64,
            transmute_slice(&data),
        );
        Ok(())
    }
}

/// Use [Shape]s as stencil for other [Shape]s
///
/// When using a [ClipStack], color is only rendered inside the logical AND (CSG intersection)
/// of all the [Shape]s on the stack with the [Shape] to be rendered.
#[derive(Default)]
pub struct ClipStack<'a> {
    stack: Vec<&'a Shape>,
}

impl<'b, 'a: 'b> ClipStack<'a> {
    /// The number of clip [Shape]s on the stack.
    pub fn height(&self) -> usize {
        self.stack.len()
    }

    /// Adds a clip [Shape] on top of the stack.
    pub fn push(&mut self, renderer: &'b Renderer, render_pass: &mut wgpu::RenderPass<'b>, shape: &'a Shape) -> Result<(), Error> {
        if self.height() >= (1 << renderer.clip_nesting_counter_bits) {
            return Err(Error::ClipStackOverflow);
        }
        self.stack.push(shape);
        render_pass.set_bind_group(0, &renderer.transform_bind_group, &[]);
        render_pass.set_pipeline(&renderer.increment_clip_nesting_counter_pipeline);
        shape.render_cover(renderer, render_pass, self.height());
        Ok(())
    }

    /// Removes the clip [Shape] at the top of the stack.
    pub fn pop(&mut self, renderer: &'b Renderer, render_pass: &mut wgpu::RenderPass<'b>) -> Result<(), Error> {
        match self.stack.pop() {
            Some(shape) => {
                render_pass.set_bind_group(0, &renderer.transform_bind_group, &[]);
                render_pass.set_pipeline(&renderer.decrement_clip_nesting_counter_pipeline);
                shape.render_cover(renderer, render_pass, self.height());
                Ok(())
            }
            None => Err(Error::ClipStackUnderflow),
        }
    }
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
     $shader_module:expr, $vertex_entry:literal, $fragment_entry:literal,
     $primitive_topology:ident, $primitive_index_format:expr,
     $color_states:expr, $stencil_state:expr,
     $vertex_buffer:expr, $msaa_sample_count:expr $(,)?) => {
        wgpu::RenderPipelineDescriptor {
            label: None,
            layout: Some($pipeline_layout),
            vertex: wgpu::VertexState {
                module: $shader_module,
                entry_point: $vertex_entry,
                buffers: &[$vertex_buffer],
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
                clamp_depth: false,
                cull_mode: None,
                conservative: false,
                polygon_mode: wgpu::PolygonMode::Fill,
            },
            depth_stencil: Some(wgpu::DepthStencilState {
                format: wgpu::TextureFormat::Depth24PlusStencil8,
                depth_write_enabled: false,
                depth_compare: wgpu::CompareFunction::Always,
                bias: wgpu::DepthBiasState::default(),
                stencil: $stencil_state,
            }),
            multisample: wgpu::MultisampleState {
                count: $msaa_sample_count,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
        }
    };
}

/// The rendering interface for [wgpu]
pub struct Renderer {
    winding_counter_bits: usize,
    clip_nesting_counter_bits: usize,
    transform_uniform_buffer: wgpu::Buffer,
    transform_bind_group: wgpu::BindGroup,
    stroke_bind_group_layout: wgpu::BindGroupLayout,
    stroke_line_pipeline: wgpu::RenderPipeline,
    stroke_joint_pipeline: wgpu::RenderPipeline,
    fill_solid_pipeline: wgpu::RenderPipeline,
    fill_integral_quadratic_curve_pipeline: wgpu::RenderPipeline,
    fill_integral_cubic_curve_pipeline: wgpu::RenderPipeline,
    fill_rational_quadratic_curve_pipeline: wgpu::RenderPipeline,
    fill_rational_cubic_curve_pipeline: wgpu::RenderPipeline,
    increment_clip_nesting_counter_pipeline: wgpu::RenderPipeline,
    decrement_clip_nesting_counter_pipeline: wgpu::RenderPipeline,
    color_solid_uniform_buffer: wgpu::Buffer,
    color_solid_bind_group: wgpu::BindGroup,
    color_solid_pipeline: wgpu::RenderPipeline,
}

impl Renderer {
    /// Constructs a new [Renderer].
    ///
    /// A [ClipStack] used with this [Renderer] can contain up to 2 to the power of `clip_nesting_counter_bits` [Shape]s.
    /// The winding rule is: Non zero modulo 2 to the power of `winding_counter_bits`.
    /// Thus, setting `winding_counter_bits` to 1 will result in the even-odd winding rule.
    /// Wgpu only supports 8 stencil bits so the sum of `clip_nesting_counter_bits` and `winding_counter_bits` can be 8 at most.
    pub fn new(
        device: &wgpu::Device,
        blending: wgpu::ColorTargetState,
        msaa_sample_count: u32,
        clip_nesting_counter_bits: usize,
        winding_counter_bits: usize,
    ) -> Result<Self, Error> {
        if winding_counter_bits == 0 || clip_nesting_counter_bits + winding_counter_bits > 8 {
            return Err(Error::NumberOfStencilBitsIsUnsupported);
        }

        let shader_module = device.create_shader_module(&include_wgsl!("shaders.wgsl"));
        let segment_0f_vertex_buffer_descriptor = wgpu::VertexBufferLayout {
            array_stride: (2 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::InputStepMode::Vertex,
            attributes: &vertex_attr_array![0 => Float32x2],
        };
        let segment_2f_vertex_buffer_descriptor = wgpu::VertexBufferLayout {
            array_stride: (4 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::InputStepMode::Vertex,
            attributes: &vertex_attr_array![0 => Float32x2, 1 => Float32x2],
        };
        let segment_2f1i_vertex_buffer_descriptor = wgpu::VertexBufferLayout {
            array_stride: (5 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::InputStepMode::Vertex,
            attributes: &vertex_attr_array![0 => Float32x2, 1 => Float32x2, 2 => Uint32],
        };
        let segment_3f_vertex_buffer_descriptor = wgpu::VertexBufferLayout {
            array_stride: (5 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::InputStepMode::Vertex,
            attributes: &vertex_attr_array![0 => Float32x2, 1 => Float32x3],
        };
        let segment_3f1i_vertex_buffer_descriptor = wgpu::VertexBufferLayout {
            array_stride: (6 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::InputStepMode::Vertex,
            attributes: &vertex_attr_array![0 => Float32x2, 1 => Float32x3, 2 => Uint32],
        };
        let segment_4f_vertex_buffer_descriptor = wgpu::VertexBufferLayout {
            array_stride: (6 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::InputStepMode::Vertex,
            attributes: &vertex_attr_array![0 => Float32x2, 1 => Float32x4],
        };

        const TRANSFORM_UNIFORM_BUFFER_SIZE: usize = std::mem::size_of::<[f32; 4 * 4]>();
        let transform_uniform_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            mapped_at_creation: false,
            size: TRANSFORM_UNIFORM_BUFFER_SIZE as u64,
            usage: wgpu::BufferUsage::UNIFORM | wgpu::BufferUsage::COPY_DST,
        });
        let transform_bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: None,
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStage::VERTEX,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: wgpu::BufferSize::new(TRANSFORM_UNIFORM_BUFFER_SIZE as u64),
                },
                count: None,
            }],
        });
        let transform_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: &transform_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
                    buffer: &transform_uniform_buffer,
                    offset: 0,
                    size: wgpu::BufferSize::new(TRANSFORM_UNIFORM_BUFFER_SIZE as u64),
                }),
            }],
        });

        let winding_counter_mask = (1 << winding_counter_bits) - 1;
        let clip_nesting_counter_mask = ((1 << clip_nesting_counter_bits) - 1) << winding_counter_bits;
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
        let max_dynamic_stroke_options_group =
            device.limits().max_uniform_buffer_binding_size as usize / std::mem::size_of::<DynamicStrokeDescriptor>();
        let stroke_bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: None,
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStage::FRAGMENT,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: wgpu::BufferSize::new(
                        (max_dynamic_stroke_options_group * std::mem::size_of::<DynamicStrokeDescriptor>()) as u64,
                    ),
                },
                count: None,
            }],
        });
        let stroke_line_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&transform_bind_group_layout, &stroke_bind_group_layout],
            push_constant_ranges: &[],
        });
        let stencil_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&transform_bind_group_layout],
            push_constant_ranges: &[],
        });
        let stroke_line_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stroke_line_pipeline_layout,
            &shader_module,
            "vertex2f1u",
            "stencil_stroke_line",
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            &[],
            stroke_stencil_state.clone(),
            segment_2f1i_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let stroke_joint_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stroke_line_pipeline_layout,
            &shader_module,
            "vertex3f1u",
            "stencil_stroke_joint",
            TriangleStrip,
            None,
            &[],
            stroke_stencil_state,
            segment_3f1i_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let fill_solid_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &shader_module,
            "vertex0",
            "stencil_solid",
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            &[],
            fill_stencil_state.clone(),
            segment_0f_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let fill_integral_quadratic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &shader_module,
            "vertex2f",
            "stencil_integral_quadratic_curve",
            TriangleList,
            None,
            &[],
            fill_stencil_state.clone(),
            segment_2f_vertex_buffer_descriptor,
            msaa_sample_count,
        ));
        let fill_integral_cubic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &shader_module,
            "vertex3f",
            "stencil_integral_cubic_curve",
            TriangleList,
            None,
            &[],
            fill_stencil_state.clone(),
            segment_3f_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let fill_rational_quadratic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &shader_module,
            "vertex3f",
            "stencil_rational_quadratic_curve",
            TriangleList,
            None,
            &[],
            fill_stencil_state.clone(),
            segment_3f_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let fill_rational_cubic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &shader_module,
            "vertex4f",
            "stencil_rational_cubic_curve",
            TriangleList,
            None,
            &[],
            fill_stencil_state,
            segment_4f_vertex_buffer_descriptor,
            msaa_sample_count,
        ));

        let increment_clip_nesting_counter_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &shader_module,
            "vertex0",
            "stencil_solid",
            TriangleStrip,
            None,
            &[],
            wgpu::StencilState {
                front: stencil_descriptor!(NotEqual, Keep, Replace),
                back: stencil_descriptor!(NotEqual, Keep, Replace),
                read_mask: winding_counter_mask,
                write_mask: clip_nesting_counter_mask | winding_counter_mask,
            },
            segment_0f_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let decrement_clip_nesting_counter_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &shader_module,
            "vertex0",
            "stencil_solid",
            TriangleStrip,
            None,
            &[],
            wgpu::StencilState {
                front: stencil_descriptor!(Less, Keep, Replace),
                back: stencil_descriptor!(Less, Keep, Replace),
                read_mask: clip_nesting_counter_mask,
                write_mask: clip_nesting_counter_mask | winding_counter_mask,
            },
            segment_0f_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));

        const COLOR_SOLID_UNIFORM_BUFFER_SIZE: usize = std::mem::size_of::<[f32; 4]>();
        let color_solid_uniform_data = Color::from([1.0; 4]);
        let color_solid_uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: transmute_slice(&color_solid_uniform_data.unwrap()),
            usage: wgpu::BufferUsage::UNIFORM | wgpu::BufferUsage::COPY_DST,
        });
        let color_solid_bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: None,
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStage::FRAGMENT,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: wgpu::BufferSize::new(COLOR_SOLID_UNIFORM_BUFFER_SIZE as u64),
                },
                count: None,
            }],
        });
        let color_solid_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: &color_solid_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
                    buffer: &color_solid_uniform_buffer,
                    offset: 0,
                    size: wgpu::BufferSize::new(COLOR_SOLID_UNIFORM_BUFFER_SIZE as u64),
                }),
            }],
        });
        let color_solid_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&transform_bind_group_layout, &color_solid_bind_group_layout],
            push_constant_ranges: &[],
        });
        let color_solid_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &color_solid_pipeline_layout,
            &shader_module,
            "vertex0",
            "fill_solid",
            TriangleStrip,
            None,
            &[blending],
            wgpu::StencilState {
                front: stencil_descriptor!(Less, Zero, Zero),
                back: stencil_descriptor!(Never, Zero, Zero),
                read_mask: clip_nesting_counter_mask | winding_counter_mask,
                write_mask: winding_counter_mask,
            },
            segment_0f_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));

        Ok(Self {
            winding_counter_bits,
            clip_nesting_counter_bits,
            transform_uniform_buffer,
            transform_bind_group,
            stroke_bind_group_layout,
            stroke_line_pipeline,
            stroke_joint_pipeline,
            fill_solid_pipeline,
            fill_integral_quadratic_curve_pipeline,
            fill_integral_cubic_curve_pipeline,
            fill_rational_quadratic_curve_pipeline,
            fill_rational_cubic_curve_pipeline,
            increment_clip_nesting_counter_pipeline,
            decrement_clip_nesting_counter_pipeline,
            color_solid_uniform_buffer,
            color_solid_bind_group,
            color_solid_pipeline,
        })
    }

    /// Set the model view projection matrix for subsequent stencil and color rendering calls.
    ///
    /// Call before creating the next [wgpu::RenderPass].
    pub fn set_transform(&self, queue: &wgpu::Queue, transform: &[f32; 16]) {
        let data = transmute_slice(transform);
        queue.write_buffer(&self.transform_uniform_buffer, 0, &data);
    }

    /// Set the color of subsequent [Shape::render_color_solid] calls.
    ///
    /// Call before creating the next [wgpu::RenderPass].
    pub fn set_solid_color(&self, queue: &wgpu::Queue, color: Color) {
        let color = color.unwrap();
        let data = transmute_slice(&color);
        queue.write_buffer(&self.color_solid_uniform_buffer, 0, &data);
    }
}
