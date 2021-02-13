use crate::{error::Error, path::Path};
use wgpu::{include_spirv, util::DeviceExt, vertex_attr_array};

fn triangle_fan_to_strip<T: Copy>(vertices: Vec<T>) -> Vec<T> {
    let gather_indices = (0..vertices.len()).map(|i| if (i & 1) == 0 { i >> 1 } else { vertices.len() - 1 - (i >> 1) });
    let mut result = Vec::with_capacity(vertices.len());
    for src in gather_indices {
        result.push(vertices[src]);
    }
    result
}

pub type Vertex0 = glam::Vec2;
#[derive(Clone, Copy)]
#[repr(packed)]
pub struct Vertex2(pub glam::Vec2, pub glam::Vec2);
#[derive(Clone, Copy)]
#[repr(packed)]
pub struct Vertex3(pub glam::Vec2, pub glam::Vec3);
#[derive(Clone, Copy)]
#[repr(packed)]
pub struct Vertex4(pub glam::Vec2, pub glam::Vec4);

pub struct FillableShape {
    anchor_elements: Vec<usize>,
    offsets: [usize; 6],
    vertex_buffer: wgpu::Buffer,
}

impl FillableShape {
    pub fn new(device: &wgpu::Device, paths: &[Path]) -> Self {
        let mut anchor_elements = Vec::new();
        let mut offsets = [0; 6];
        let mut anchors: Vec<Vertex0> = Vec::new();
        let mut integral_quadratic_curve_elements: Vec<Vertex2> = Vec::new();
        let mut integral_cubic_curve_elements: Vec<Vertex3> = Vec::new();
        let mut rational_quadratic_curve_elements: Vec<Vertex3> = Vec::new();
        let mut rational_cubic_curve_elements: Vec<Vertex4> = Vec::new();
        let mut proto_hull = Vec::new();
        for path in paths {
            anchor_elements.push(path.anchors.len());
            anchors.append(&mut triangle_fan_to_strip(path.anchors.clone()));
            if let Some(triangles) = &path.integral_quadratic_curve_triangles {
                integral_quadratic_curve_elements.append(&mut triangles.clone());
            }
            if let Some(triangles) = &path.integral_cubic_curve_triangles {
                integral_cubic_curve_elements.append(&mut triangles.clone());
            }
            if let Some(triangles) = &path.rational_quadratic_curve_triangles {
                rational_quadratic_curve_elements.append(&mut triangles.clone());
            }
            if let Some(triangles) = &path.rational_cubic_curve_triangles {
                rational_cubic_curve_elements.append(&mut triangles.clone());
            }
            if let Some(path_proto_hull) = &path.proto_hull {
                proto_hull.append(&mut path_proto_hull.clone());
            } else {
                proto_hull.append(&mut path.anchors.clone());
            }
        }
        let convex_hull = crate::convex_hull::andrew(&proto_hull);
        let mut vertex_buffers = [
            crate::utils::transmute_vec::<_, u8>(anchors),
            crate::utils::transmute_vec::<_, u8>(integral_quadratic_curve_elements),
            crate::utils::transmute_vec::<_, u8>(integral_cubic_curve_elements),
            crate::utils::transmute_vec::<_, u8>(rational_quadratic_curve_elements),
            crate::utils::transmute_vec::<_, u8>(rational_cubic_curve_elements),
            crate::utils::transmute_vec::<_, u8>(triangle_fan_to_strip(convex_hull)),
        ];
        let mut vertex_buffer_length = 0;
        for (i, vertex_buffer) in vertex_buffers.iter().enumerate() {
            vertex_buffer_length += vertex_buffer.len();
            offsets[i] = vertex_buffer_length;
        }
        let mut vertex_buffer_data: Vec<u8> = Vec::with_capacity(vertex_buffer_length);
        for mut vertex_buffer in &mut vertex_buffers {
            vertex_buffer_data.append(&mut vertex_buffer);
        }
        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: &vertex_buffer_data,
            usage: wgpu::BufferUsage::VERTEX,
        });
        Self {
            anchor_elements,
            offsets,
            vertex_buffer,
        }
    }

    pub fn render_stencil<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>) {
        render_pass.set_bind_group(0, &renderer.transform_bind_group, &[]);
        render_pass.set_pipeline(&renderer.stencil_solid_pipeline);
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(0..self.offsets[0] as u64));
        let mut begin_index = 0;
        for element_count in &self.anchor_elements {
            let end_index = begin_index + element_count;
            render_pass.draw(begin_index as u32..end_index as u32, 0..1);
            begin_index = end_index;
        }
        for (i, (pipeline, vertex_size)) in [
            (&renderer.stencil_integral_quadratic_curve_pipeline, std::mem::size_of::<Vertex2>()),
            (&renderer.stencil_integral_cubic_curve_pipeline, std::mem::size_of::<Vertex3>()),
            (&renderer.stencil_rational_quadratic_curve_pipeline, std::mem::size_of::<Vertex3>()),
            (&renderer.stencil_rational_cubic_curve_pipeline, std::mem::size_of::<Vertex4>()),
        ]
        .iter()
        .enumerate()
        {
            if self.offsets[i] < self.offsets[i + 1] {
                render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(self.offsets[i] as u64..self.offsets[i + 1] as u64));
                render_pass.set_pipeline(pipeline);
                render_pass.draw(0..((self.offsets[i + 1] - self.offsets[i]) / vertex_size) as u32, 0..1);
            }
        }
    }

    fn render_cover<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>) {
        render_pass.set_bind_group(0, &renderer.transform_bind_group, &[]);
        render_pass.set_bind_group(1, &renderer.fill_solid_bind_group, &[]);
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(self.offsets[4] as u64..self.offsets[5] as u64));
        render_pass.draw(0..((self.offsets[5] - self.offsets[4]) / std::mem::size_of::<Vertex0>()) as u32, 0..1);
    }

    pub fn render_solid_fill<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>, clip_stack_height: usize) {
        render_pass.set_stencil_reference((clip_stack_height << renderer.winding_counter_bits) as u32);
        render_pass.set_pipeline(&renderer.fill_solid_pipeline);
        self.render_cover(renderer, render_pass);
    }
}

#[derive(Default)]
pub struct ClipStack<'a> {
    stack: Vec<&'a FillableShape>,
}

impl<'b, 'a: 'b> ClipStack<'a> {
    pub fn height(&self) -> usize {
        self.stack.len()
    }

    pub fn push(&mut self, renderer: &'b Renderer, render_pass: &mut wgpu::RenderPass<'b>, fillable_shape: &'a FillableShape) -> Result<(), Error> {
        if self.height() >= (1 << renderer.clip_nesting_counter_bits) {
            return Err(Error::ClipStackOverflow);
        }
        self.stack.push(fillable_shape);
        render_pass.set_stencil_reference((self.height() << renderer.winding_counter_bits) as u32);
        render_pass.set_pipeline(&renderer.increment_clip_nesting_counter_pipeline);
        fillable_shape.render_cover(renderer, render_pass);
        Ok(())
    }

    pub fn pop(&mut self, renderer: &'b Renderer, render_pass: &mut wgpu::RenderPass<'b>) -> Result<(), Error> {
        match self.stack.pop() {
            Some(fillable_shape) => {
                render_pass.set_stencil_reference((self.height() << renderer.winding_counter_bits) as u32);
                render_pass.set_pipeline(&renderer.decrement_clip_nesting_counter_pipeline);
                fillable_shape.render_cover(renderer, render_pass);
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
     $primitive_topology:ident, $color_states:expr,
     $stencil_state:expr,
     $vertex_buffer:expr, $sample_count:expr $(,)?) => {
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
                strip_index_format: None,
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
                count: $sample_count,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
        }
    };
}

pub struct Renderer {
    winding_counter_bits: usize,
    clip_nesting_counter_bits: usize,
    transform_uniform_buffer: wgpu::Buffer,
    transform_bind_group: wgpu::BindGroup,
    stencil_solid_pipeline: wgpu::RenderPipeline,
    stencil_integral_quadratic_curve_pipeline: wgpu::RenderPipeline,
    stencil_integral_cubic_curve_pipeline: wgpu::RenderPipeline,
    stencil_rational_quadratic_curve_pipeline: wgpu::RenderPipeline,
    stencil_rational_cubic_curve_pipeline: wgpu::RenderPipeline,
    increment_clip_nesting_counter_pipeline: wgpu::RenderPipeline,
    decrement_clip_nesting_counter_pipeline: wgpu::RenderPipeline,
    fill_solid_uniform_buffer: wgpu::Buffer,
    fill_solid_bind_group: wgpu::BindGroup,
    fill_solid_pipeline: wgpu::RenderPipeline,
}

impl Renderer {
    pub fn new(
        device: &wgpu::Device,
        fill_color_state: wgpu::ColorTargetState,
        sample_count: u32,
        clip_nesting_counter_bits: usize,
        winding_counter_bits: usize,
    ) -> Result<Self, Error> {
        if winding_counter_bits == 0 || clip_nesting_counter_bits + winding_counter_bits > 8 {
            return Err(Error::NumberOfStencilBitsIsUnsupported);
        }

        let segment_0_vertex_module = device.create_shader_module(&include_spirv!("../target/shader_modules/segment0_vert.spv"));
        let segment_3_vertex_module = device.create_shader_module(&include_spirv!("../target/shader_modules/segment3_vert.spv"));
        let stencil_solid_fragment_module = device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_solid_frag.spv"));
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
        let stencil_state = wgpu::StencilState {
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
        let stencil_solid_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &stencil_solid_fragment_module,
            TriangleStrip,
            &[],
            stencil_state.clone(),
            segment_0_vertex_buffer_descriptor.clone(),
            sample_count,
        ));
        let stencil_integral_quadratic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/segment2_vert.spv")),
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_integral_quadratic_curve_frag.spv")),
            TriangleList,
            &[],
            stencil_state.clone(),
            segment_2_vertex_buffer_descriptor,
            sample_count,
        ));
        let stencil_integral_cubic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_3_vertex_module,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_integral_cubic_curve_frag.spv")),
            TriangleList,
            &[],
            stencil_state.clone(),
            segment_3_vertex_buffer_descriptor.clone(),
            sample_count,
        ));
        let stencil_rational_quadratic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_3_vertex_module,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_rational_quadratic_curve_frag.spv")),
            TriangleList,
            &[],
            stencil_state.clone(),
            segment_3_vertex_buffer_descriptor.clone(),
            sample_count,
        ));
        let stencil_rational_cubic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/segment4_vert.spv")),
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_rational_cubic_curve_frag.spv")),
            TriangleList,
            &[],
            stencil_state,
            segment_4_vertex_buffer_descriptor,
            sample_count,
        ));

        let increment_clip_nesting_counter_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &stencil_solid_fragment_module,
            TriangleStrip,
            &[],
            wgpu::StencilState {
                front: stencil_descriptor!(NotEqual, Keep, Replace),
                back: stencil_descriptor!(NotEqual, Keep, Replace),
                read_mask: winding_counter_mask,
                write_mask: clip_nesting_counter_mask | winding_counter_mask,
            },
            segment_0_vertex_buffer_descriptor.clone(),
            sample_count,
        ));
        let decrement_clip_nesting_counter_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &stencil_solid_fragment_module,
            TriangleStrip,
            &[],
            wgpu::StencilState {
                front: stencil_descriptor!(Less, Keep, Replace),
                back: stencil_descriptor!(Less, Keep, Replace),
                read_mask: clip_nesting_counter_mask,
                write_mask: clip_nesting_counter_mask | winding_counter_mask,
            },
            segment_0_vertex_buffer_descriptor.clone(),
            sample_count,
        ));

        const FILL_SOLID_UNIFORM_BUFFER_SIZE: usize = std::mem::size_of::<[f32; 4]>();
        let fill_solid_uniform_data: [f32; 4] = [1.0, 1.0, 1.0, 1.0];
        let fill_solid_uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: crate::utils::transmute_slice(&fill_solid_uniform_data),
            usage: wgpu::BufferUsage::UNIFORM | wgpu::BufferUsage::COPY_DST,
        });
        let fill_solid_bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: None,
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStage::FRAGMENT,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: wgpu::BufferSize::new(FILL_SOLID_UNIFORM_BUFFER_SIZE as u64),
                },
                count: None,
            }],
        });
        let fill_solid_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &fill_solid_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Buffer {
                    buffer: &fill_solid_uniform_buffer,
                    offset: 0,
                    size: wgpu::BufferSize::new(FILL_SOLID_UNIFORM_BUFFER_SIZE as u64),
                },
            }],
            label: None,
        });
        let fill_solid_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&transform_bind_group_layout, &fill_solid_bind_group_layout],
            push_constant_ranges: &[],
        });
        let fill_solid_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &fill_solid_pipeline_layout,
            &segment_0_vertex_module,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/fill_solid_frag.spv")),
            TriangleStrip,
            &[fill_color_state],
            wgpu::StencilState {
                front: stencil_descriptor!(Less, Zero, Zero),
                back: stencil_descriptor!(Less, Zero, Zero),
                read_mask: clip_nesting_counter_mask | winding_counter_mask,
                write_mask: winding_counter_mask,
            },
            segment_0_vertex_buffer_descriptor.clone(),
            sample_count,
        ));

        Ok(Self {
            winding_counter_bits,
            clip_nesting_counter_bits,
            transform_uniform_buffer,
            transform_bind_group,
            stencil_solid_pipeline,
            stencil_integral_quadratic_curve_pipeline,
            stencil_integral_cubic_curve_pipeline,
            stencil_rational_quadratic_curve_pipeline,
            stencil_rational_cubic_curve_pipeline,
            increment_clip_nesting_counter_pipeline,
            decrement_clip_nesting_counter_pipeline,
            fill_solid_uniform_buffer,
            fill_solid_bind_group,
            fill_solid_pipeline,
        })
    }

    pub fn set_transform(&self, queue: &wgpu::Queue, transform: &[[f32; 4]; 4]) {
        let data = crate::utils::transmute_slice(transform);
        queue.write_buffer(&self.transform_uniform_buffer, 0, &data);
    }

    pub fn set_solid_fill_color(&self, queue: &wgpu::Queue, color: &[f32; 4]) {
        let data = crate::utils::transmute_slice(color);
        queue.write_buffer(&self.fill_solid_uniform_buffer, 0, &data);
    }
}
