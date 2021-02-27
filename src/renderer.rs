use crate::{
    error::Error,
    fill::FillBuilder,
    path::Path,
    stroke::StrokeBuilder,
    utils::{transmute_slice, transmute_vec},
    vertex::{triangle_fan_to_strip, Vertex0, Vertex2, Vertex3, Vertex4},
};
use wgpu::{include_spirv, util::DeviceExt, vertex_attr_array};

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

/// A set of `Path`s which is always rendered together
pub struct Shape {
    vertex_offsets: [usize; 8],
    index_offsets: [usize; 2],
    vertex_buffer: wgpu::Buffer,
    index_buffer: wgpu::Buffer,
}

impl Shape {
    /// Constructs a `Shape` from a set of `Path`s.
    pub fn from_paths(device: &wgpu::Device, paths: &[Path]) -> Self {
        let mut proto_hull = Vec::new();
        let mut stroke_builder = StrokeBuilder::default();
        let mut fill_builder = FillBuilder::default();
        for path in paths {
            if path.stroke_options.is_some() {
                stroke_builder.add_path(&mut proto_hull, &path);
            } else {
                fill_builder.add_path(&mut proto_hull, &path);
            }
        }
        let convex_hull = crate::convex_hull::andrew(&proto_hull);
        let (vertex_offsets, vertex_buffer) = concat_buffers!(
            device,
            VERTEX,
            8,
            [
                stroke_builder.solid_vertices,
                stroke_builder.quadratic_vertices,
                fill_builder.solid_vertices,
                fill_builder.integral_quadratic_vertices,
                fill_builder.integral_cubic_vertices,
                fill_builder.rational_quadratic_vertices,
                fill_builder.rational_cubic_vertices,
                triangle_fan_to_strip(convex_hull),
            ]
        );
        let (index_offsets, index_buffer) = concat_buffers!(device, INDEX, 2, [stroke_builder.solid_indices, fill_builder.solid_indices,]);
        Self {
            vertex_offsets,
            index_offsets,
            vertex_buffer,
            index_buffer,
        }
    }

    /// Renderes the `Shape` into the stencil buffer.
    pub fn render_stencil<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>) {
        render_pass.set_bind_group(0, &renderer.transform_bind_group, &[]);
        render_pass.set_pipeline(&renderer.stroke_solid_pipeline);
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(0..self.vertex_offsets[0] as u64));
        render_pass.set_index_buffer(self.index_buffer.slice(0..self.index_offsets[0] as u64), wgpu::IndexFormat::Uint16);
        render_pass.draw_indexed(0..(self.index_offsets[0] / std::mem::size_of::<u16>()) as u32, 0, 0..1);
        for (i, (pipeline, vertex_size)) in [(&renderer.stroke_rounding_pipeline, std::mem::size_of::<Vertex3>())].iter().enumerate() {
            let begin_offset = self.vertex_offsets[i];
            let end_offset = self.vertex_offsets[i + 1];
            if begin_offset < end_offset {
                render_pass.set_pipeline(pipeline);
                render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(begin_offset as u64..end_offset as u64));
                render_pass.draw(0..((end_offset - begin_offset) / vertex_size) as u32, 0..1);
            }
        }
        render_pass.set_pipeline(&renderer.fill_solid_pipeline);
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(self.vertex_offsets[1] as u64..self.vertex_offsets[2] as u64));
        render_pass.set_index_buffer(
            self.index_buffer.slice(self.index_offsets[0] as u64..self.index_offsets[1] as u64),
            wgpu::IndexFormat::Uint16,
        );
        render_pass.draw_indexed(
            0..((self.index_offsets[1] - self.index_offsets[0]) / std::mem::size_of::<u16>()) as u32,
            0,
            0..1,
        );
        for (i, (pipeline, vertex_size)) in [
            (&renderer.fill_integral_quadratic_curve_pipeline, std::mem::size_of::<Vertex2>()),
            (&renderer.fill_integral_cubic_curve_pipeline, std::mem::size_of::<Vertex3>()),
            (&renderer.fill_rational_quadratic_curve_pipeline, std::mem::size_of::<Vertex3>()),
            (&renderer.fill_rational_cubic_curve_pipeline, std::mem::size_of::<Vertex4>()),
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

    /// Renderes the `Shape` using a user provided `wgpu::RenderPipeline`.
    ///
    /// Requires the `Shape` to be stenciled and `render_pass.set_pipeline()` to be called first.
    fn render_cover<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>, clip_stack_height: usize) {
        let begin_offset = self.vertex_offsets[6];
        let end_offset = self.vertex_offsets[7];
        render_pass.set_stencil_reference((clip_stack_height << renderer.winding_counter_bits) as u32);
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(begin_offset as u64..end_offset as u64));
        render_pass.draw(0..((end_offset - begin_offset) / std::mem::size_of::<Vertex0>()) as u32, 0..1);
    }

    /// Fills the `Shape` with a solid color.
    ///
    /// Requires the `Shape` to be stenciled first.
    pub fn render_color_solid<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>, clip_stack_height: usize) {
        render_pass.set_bind_group(0, &renderer.transform_bind_group, &[]);
        render_pass.set_bind_group(1, &renderer.color_solid_bind_group, &[]);
        render_pass.set_pipeline(&renderer.color_solid_pipeline);
        self.render_cover(renderer, render_pass, clip_stack_height);
    }
}

/// Use `Shape`s as stencil for other `Shape`s
///
/// When using a `ClipStack`, color is only rendered inside the logical AND (CSG intersection)
/// of all the `Shape`s on the stack with the `Shape` to be rendered.
#[derive(Default)]
pub struct ClipStack<'a> {
    stack: Vec<&'a Shape>,
}

impl<'b, 'a: 'b> ClipStack<'a> {
    /// The number of clip `Shape`s on the stack.
    pub fn height(&self) -> usize {
        self.stack.len()
    }

    /// Adds a clip `Shape` on top of the stack.
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

    /// Removes the clip `Shape` at the top of the stack.
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
     $vertex_module:expr, $fragment_module:expr,
     $primitive_topology:ident, $primitive_index_format:expr,
     $color_states:expr, $stencil_state:expr,
     $vertex_buffer:expr, $msaa_sample_count:expr $(,)?) => {
        wgpu::RenderPipelineDescriptor {
            label: None,
            layout: Some($pipeline_layout),
            vertex: wgpu::VertexState {
                module: $vertex_module,
                entry_point: "main",
                buffers: &[$vertex_buffer],
            },
            fragment: Some(wgpu::FragmentState {
                module: $fragment_module,
                entry_point: "main",
                targets: $color_states,
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::$primitive_topology,
                strip_index_format: $primitive_index_format,
                front_face: wgpu::FrontFace::Ccw,
                cull_mode: wgpu::CullMode::None,
                polygon_mode: wgpu::PolygonMode::Fill,
            },
            depth_stencil: Some(wgpu::DepthStencilState {
                format: wgpu::TextureFormat::Depth24PlusStencil8,
                depth_write_enabled: false,
                depth_compare: wgpu::CompareFunction::Always,
                bias: wgpu::DepthBiasState::default(),
                clamp_depth: false,
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

/// The rendering interface for `wgpu`
pub struct Renderer {
    winding_counter_bits: usize,
    clip_nesting_counter_bits: usize,
    transform_uniform_buffer: wgpu::Buffer,
    transform_bind_group: wgpu::BindGroup,
    stroke_solid_pipeline: wgpu::RenderPipeline,
    stroke_rounding_pipeline: wgpu::RenderPipeline,
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
    /// Constructs a new `Renderer`.
    ///
    /// A `ClipStack` used with this `Renderer` can contain up to 2 to the power of `clip_nesting_counter_bits` `Shape`s.
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

        let segment_0_vertex_module = device.create_shader_module(&include_spirv!("../target/shader_modules/segment0_vert.spv"));
        let segment_3_vertex_module = device.create_shader_module(&include_spirv!("../target/shader_modules/segment3_vert.spv"));
        let stencil_solid_fragment_module = device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_solid_frag.spv"));
        let stencil_rational_quadratic_curve_fragment_module =
            device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_rational_quadratic_curve_frag.spv"));
        let segment_0_vertex_buffer_descriptor = wgpu::VertexBufferLayout {
            array_stride: (2 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::InputStepMode::Vertex,
            attributes: &vertex_attr_array![0 => Float2],
        };
        let segment_2_vertex_buffer_descriptor = wgpu::VertexBufferLayout {
            array_stride: (4 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::InputStepMode::Vertex,
            attributes: &vertex_attr_array![0 => Float2, 1 => Float2],
        };
        let segment_3_vertex_buffer_descriptor = wgpu::VertexBufferLayout {
            array_stride: (5 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::InputStepMode::Vertex,
            attributes: &vertex_attr_array![0 => Float2, 1 => Float3],
        };
        let segment_4_vertex_buffer_descriptor = wgpu::VertexBufferLayout {
            array_stride: (6 * 4) as wgpu::BufferAddress,
            step_mode: wgpu::InputStepMode::Vertex,
            attributes: &vertex_attr_array![0 => Float2, 1 => Float4],
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
            layout: &transform_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Buffer {
                    buffer: &transform_uniform_buffer,
                    offset: 0,
                    size: wgpu::BufferSize::new(TRANSFORM_UNIFORM_BUFFER_SIZE as u64),
                },
            }],
            label: None,
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
        let stencil_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&transform_bind_group_layout],
            push_constant_ranges: &[],
        });
        let stroke_solid_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &stencil_solid_fragment_module,
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            &[],
            stroke_stencil_state.clone(),
            segment_0_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let stroke_rounding_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_3_vertex_module,
            &stencil_rational_quadratic_curve_fragment_module,
            TriangleList,
            None,
            &[],
            stroke_stencil_state,
            segment_3_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let fill_solid_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &stencil_solid_fragment_module,
            TriangleStrip,
            Some(wgpu::IndexFormat::Uint16),
            &[],
            fill_stencil_state.clone(),
            segment_0_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let fill_integral_quadratic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/segment2_vert.spv")),
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_integral_quadratic_curve_frag.spv")),
            TriangleList,
            None,
            &[],
            fill_stencil_state.clone(),
            segment_2_vertex_buffer_descriptor,
            msaa_sample_count,
        ));
        let fill_integral_cubic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_3_vertex_module,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_integral_cubic_curve_frag.spv")),
            TriangleList,
            None,
            &[],
            fill_stencil_state.clone(),
            segment_3_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let fill_rational_quadratic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_3_vertex_module,
            &stencil_rational_quadratic_curve_fragment_module,
            TriangleList,
            None,
            &[],
            fill_stencil_state.clone(),
            segment_3_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let fill_rational_cubic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/segment4_vert.spv")),
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_rational_cubic_curve_frag.spv")),
            TriangleList,
            None,
            &[],
            fill_stencil_state,
            segment_4_vertex_buffer_descriptor,
            msaa_sample_count,
        ));

        let increment_clip_nesting_counter_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &stencil_solid_fragment_module,
            TriangleStrip,
            None,
            &[],
            wgpu::StencilState {
                front: stencil_descriptor!(NotEqual, Keep, Replace),
                back: stencil_descriptor!(NotEqual, Keep, Replace),
                read_mask: winding_counter_mask,
                write_mask: clip_nesting_counter_mask | winding_counter_mask,
            },
            segment_0_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));
        let decrement_clip_nesting_counter_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &stencil_solid_fragment_module,
            TriangleStrip,
            None,
            &[],
            wgpu::StencilState {
                front: stencil_descriptor!(Less, Keep, Replace),
                back: stencil_descriptor!(Less, Keep, Replace),
                read_mask: clip_nesting_counter_mask,
                write_mask: clip_nesting_counter_mask | winding_counter_mask,
            },
            segment_0_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));

        const COLOR_SOLID_UNIFORM_BUFFER_SIZE: usize = std::mem::size_of::<[f32; 4]>();
        let color_solid_uniform_data: [f32; 4] = [1.0, 1.0, 1.0, 1.0];
        let color_solid_uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: transmute_slice(&color_solid_uniform_data),
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
            layout: &color_solid_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Buffer {
                    buffer: &color_solid_uniform_buffer,
                    offset: 0,
                    size: wgpu::BufferSize::new(COLOR_SOLID_UNIFORM_BUFFER_SIZE as u64),
                },
            }],
            label: None,
        });
        let color_solid_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&transform_bind_group_layout, &color_solid_bind_group_layout],
            push_constant_ranges: &[],
        });
        let color_solid_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &color_solid_pipeline_layout,
            &segment_0_vertex_module,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/fill_solid_frag.spv")),
            TriangleStrip,
            None,
            &[blending],
            wgpu::StencilState {
                front: stencil_descriptor!(Less, Zero, Zero),
                back: stencil_descriptor!(Less, Zero, Zero),
                read_mask: clip_nesting_counter_mask | winding_counter_mask,
                write_mask: winding_counter_mask,
            },
            segment_0_vertex_buffer_descriptor.clone(),
            msaa_sample_count,
        ));

        Ok(Self {
            winding_counter_bits,
            clip_nesting_counter_bits,
            transform_uniform_buffer,
            transform_bind_group,
            stroke_solid_pipeline,
            stroke_rounding_pipeline,
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
    /// Call before creating the next `wgpu::RenderPass`.
    pub fn set_transform(&self, queue: &wgpu::Queue, transform: &glam::Mat4) {
        let transform = transform.to_cols_array();
        let data = transmute_slice(&transform);
        queue.write_buffer(&self.transform_uniform_buffer, 0, &data);
    }

    /// Set the color of subsequent `Shape::render_color_solid` calls.
    ///
    /// Call before creating the next `wgpu::RenderPass`.
    pub fn set_solid_color(&self, queue: &wgpu::Queue, color: &[f32; 4]) {
        let data = transmute_slice(color);
        queue.write_buffer(&self.color_solid_uniform_buffer, 0, &data);
    }
}
