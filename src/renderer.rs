use crate::path::Path;
use wgpu::{include_spirv, util::DeviceExt, vertex_attr_array};

fn triangle_fan_to_strip<T: Copy>(vertices: Vec<T>) -> Vec<T> {
    let gather_indices = (0..vertices.len()).map(|i| if (i & 1) == 0 { i >> 1 } else { vertices.len() - 1 - (i >> 1) });
    let mut result = Vec::with_capacity(vertices.len());
    for src in gather_indices {
        result.push(vertices[src]);
    }
    result
}

pub struct FillableShape {
    anchor_elements: Vec<u32>,
    cover_elements: u32,
    cover_element_offset: u32,
    quadratic_curve_elements: u32,
    cubic_curve_elements: u32,
    rational_quadratic_curve_elements: u32,
    rational_cubic_curve_elements: u32,
    segment_0_vertex_buffer: wgpu::Buffer,
    segment_2_vertex_buffer: Option<wgpu::Buffer>,
    segment_3_vertex_buffer: Option<wgpu::Buffer>,
    segment_4_vertex_buffer: Option<wgpu::Buffer>,
}

impl FillableShape {
    pub fn new(device: &wgpu::Device, paths: &[Path]) -> Self {
        let mut anchor_elements = Vec::new();
        let mut quadratic_curve_elements = 0;
        let mut cubic_curve_elements = 0;
        let mut rational_quadratic_curve_elements = 0;
        let mut rational_cubic_curve_elements = 0;
        let mut segment_0_vertices: Vec<glam::Vec2> = Vec::new();
        let mut segment_2_vertices: Vec<(glam::Vec2, [f32; 2])> = Vec::new();
        let mut segment_3_vertices: Vec<(glam::Vec2, [f32; 3])> = Vec::new();
        let mut segment_4_vertices: Vec<(glam::Vec2, [f32; 4])> = Vec::new();
        let mut proto_hull = Vec::new();
        for path in paths {
            anchor_elements.push(path.anchors.len() as u32);
            segment_0_vertices.append(&mut triangle_fan_to_strip(path.anchors.clone()));
            if let Some(path_proto_hull) = &path.proto_hull {
                proto_hull.append(&mut path_proto_hull.clone());
            } else {
                proto_hull.append(&mut path.anchors.clone());
            }
            if let Some(triangles) = &path.quadratic_curve_triangles {
                quadratic_curve_elements += triangles.len() as u32;
                segment_2_vertices.append(&mut triangles.clone());
            }
            if let Some(triangles) = &path.cubic_curve_triangles {
                cubic_curve_elements += triangles.len() as u32;
                segment_3_vertices.append(&mut triangles.clone());
            }
        }
        for path in paths {
            if let Some(triangles) = &path.rational_quadratic_curve_triangles {
                rational_quadratic_curve_elements += triangles.len() as u32;
                segment_3_vertices.append(&mut triangles.clone());
            }
            if let Some(triangles) = &path.rational_cubic_curve_triangles {
                rational_cubic_curve_elements += triangles.len() as u32;
                segment_4_vertices.append(&mut triangles.clone());
            }
        }
        let convex_hull = crate::convex_hull::andrew(&proto_hull);
        let cover_elements = convex_hull.len() as u32;
        let cover_element_offset = segment_0_vertices.len() as u32;
        segment_0_vertices.append(&mut triangle_fan_to_strip(convex_hull));
        let segment_0_vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: crate::utils::transmute_slice(&segment_0_vertices),
            usage: wgpu::BufferUsage::VERTEX,
        });
        let segment_2_vertex_buffer = if segment_2_vertices.is_empty() {
            None
        } else {
            Some(device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: None,
                contents: crate::utils::transmute_slice(&segment_2_vertices),
                usage: wgpu::BufferUsage::VERTEX,
            }))
        };
        let segment_3_vertex_buffer = if segment_3_vertices.is_empty() {
            None
        } else {
            Some(device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: None,
                contents: crate::utils::transmute_slice(&segment_3_vertices),
                usage: wgpu::BufferUsage::VERTEX,
            }))
        };
        let segment_4_vertex_buffer = if segment_4_vertices.is_empty() {
            None
        } else {
            Some(device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: None,
                contents: crate::utils::transmute_slice(&segment_4_vertices),
                usage: wgpu::BufferUsage::VERTEX,
            }))
        };
        Self {
            anchor_elements,
            cover_elements,
            cover_element_offset,
            quadratic_curve_elements,
            cubic_curve_elements,
            rational_quadratic_curve_elements,
            rational_cubic_curve_elements,
            segment_0_vertex_buffer,
            segment_2_vertex_buffer,
            segment_3_vertex_buffer,
            segment_4_vertex_buffer,
        }
    }

    pub fn render_stencil<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>) {
        render_pass.set_bind_group(0, &renderer.transform_bind_group, &[]);
        render_pass.set_pipeline(&renderer.stencil_solid_pipeline);
        render_pass.set_vertex_buffer(0, self.segment_0_vertex_buffer.slice(..));
        let mut begin_index = 0;
        for element_count in &self.anchor_elements {
            let end_index = begin_index + element_count;
            render_pass.draw(begin_index..end_index, 0..1);
            begin_index = end_index;
        }
        if let Some(vertex_buffer) = &self.segment_2_vertex_buffer {
            render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
            render_pass.set_pipeline(&renderer.stencil_quadratic_curve_pipeline);
            render_pass.draw(0..self.quadratic_curve_elements, 0..1);
        }
        if let Some(vertex_buffer) = &self.segment_3_vertex_buffer {
            render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
            render_pass.set_pipeline(&renderer.stencil_cubic_curve_pipeline);
            render_pass.draw(0..self.cubic_curve_elements, 0..1);
            render_pass.set_pipeline(&renderer.stencil_rational_quadratic_curve_pipeline);
            render_pass.draw(
                self.cubic_curve_elements..self.cubic_curve_elements + self.rational_quadratic_curve_elements,
                0..1,
            );
        }
        if let Some(vertex_buffer) = &self.segment_4_vertex_buffer {
            render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
            render_pass.set_pipeline(&renderer.stencil_rational_cubic_curve_pipeline);
            render_pass.draw(0..self.rational_cubic_curve_elements, 0..1);
        }
    }

    pub fn render_cover<'a>(&'a self, renderer: &'a Renderer, render_pass: &mut wgpu::RenderPass<'a>) {
        render_pass.set_bind_group(0, &renderer.transform_bind_group, &[]);
        render_pass.set_bind_group(1, &renderer.fill_solid_bind_group, &[]);
        render_pass.set_pipeline(&renderer.fill_solid_pipeline);
        render_pass.set_vertex_buffer(0, self.segment_0_vertex_buffer.slice(..));
        render_pass.draw(self.cover_element_offset..self.cover_element_offset + self.cover_elements, 0..1);
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
     $stencil_front:expr, $stencil_back:expr, $stencil_read_mask:expr,
     $vertex_buffer:expr $(,)?) => {
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
                stencil: wgpu::StencilState {
                    front: $stencil_front,
                    back: $stencil_back,
                    read_mask: $stencil_read_mask,
                    write_mask: !0,
                },
            }),
            multisample: wgpu::MultisampleState {
                count: 1,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
        }
    };
}

pub struct Renderer {
    transform_uniform_buffer: wgpu::Buffer,
    transform_bind_group: wgpu::BindGroup,
    stencil_solid_pipeline: wgpu::RenderPipeline,
    stencil_quadratic_curve_pipeline: wgpu::RenderPipeline,
    stencil_cubic_curve_pipeline: wgpu::RenderPipeline,
    stencil_rational_quadratic_curve_pipeline: wgpu::RenderPipeline,
    stencil_rational_cubic_curve_pipeline: wgpu::RenderPipeline,
    fill_solid_uniform_buffer: wgpu::Buffer,
    fill_solid_bind_group: wgpu::BindGroup,
    fill_solid_pipeline: wgpu::RenderPipeline,
}

impl Renderer {
    pub fn new(device: &wgpu::Device, screen_format: wgpu::TextureFormat) -> Self {
        let segment_0_vertex_module = device.create_shader_module(&include_spirv!("../target/shader_modules/segment0_vert.spv"));
        let segment_3_vertex_module = device.create_shader_module(&include_spirv!("../target/shader_modules/segment3_vert.spv"));
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
        let stencil_front = stencil_descriptor!(Always, Keep, IncrementWrap);
        let stencil_back = stencil_descriptor!(Always, Keep, DecrementWrap);
        let stencil_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: None,
            bind_group_layouts: &[&transform_bind_group_layout],
            push_constant_ranges: &[],
        });
        let stencil_solid_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_0_vertex_module,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_solid_frag.spv")),
            TriangleStrip,
            &[],
            stencil_front.clone(),
            stencil_back.clone(),
            !0,
            segment_0_vertex_buffer_descriptor.clone(),
        ));
        let stencil_quadratic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/segment2_vert.spv")),
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_quadratic_curve_frag.spv")),
            TriangleList,
            &[],
            stencil_front.clone(),
            stencil_back.clone(),
            !0,
            segment_2_vertex_buffer_descriptor,
        ));
        let stencil_cubic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_3_vertex_module,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_cubic_curve_frag.spv")),
            TriangleList,
            &[],
            stencil_front.clone(),
            stencil_back.clone(),
            !0,
            segment_3_vertex_buffer_descriptor.clone(),
        ));
        let stencil_rational_quadratic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &segment_3_vertex_module,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_rational_quadratic_curve_frag.spv")),
            TriangleList,
            &[],
            stencil_front.clone(),
            stencil_back.clone(),
            !0,
            segment_3_vertex_buffer_descriptor.clone(),
        ));
        let stencil_rational_cubic_curve_pipeline = device.create_render_pipeline(&render_pipeline_descriptor!(
            &stencil_pipeline_layout,
            &device.create_shader_module(&include_spirv!("../target/shader_modules/segment4_vert.spv")),
            &device.create_shader_module(&include_spirv!("../target/shader_modules/stencil_rational_cubic_curve_frag.spv")),
            TriangleList,
            &[],
            stencil_front,
            stencil_back,
            !0,
            segment_4_vertex_buffer_descriptor,
        ));

        let fill_color_state = wgpu::ColorTargetState {
            format: screen_format,
            color_blend: wgpu::BlendState {
                src_factor: wgpu::BlendFactor::SrcAlpha,
                dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
                operation: wgpu::BlendOperation::Add,
            },
            alpha_blend: wgpu::BlendState {
                src_factor: wgpu::BlendFactor::SrcAlpha,
                dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
                operation: wgpu::BlendOperation::Add,
            },
            write_mask: wgpu::ColorWrite::ALL,
        };
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
            stencil_descriptor!(NotEqual, Keep, Keep),
            stencil_descriptor!(NotEqual, Keep, Keep),
            1,
            segment_0_vertex_buffer_descriptor.clone(),
        ));

        Self {
            transform_uniform_buffer,
            transform_bind_group,
            stencil_solid_pipeline,
            stencil_quadratic_curve_pipeline,
            stencil_cubic_curve_pipeline,
            stencil_rational_quadratic_curve_pipeline,
            stencil_rational_cubic_curve_pipeline,
            fill_solid_uniform_buffer,
            fill_solid_bind_group,
            fill_solid_pipeline,
        }
    }

    pub fn set_transform(&self, queue: &wgpu::Queue, transform: &[[f32; 4]; 4]) {
        let data = crate::utils::transmute_slice(transform);
        queue.write_buffer(&self.transform_uniform_buffer, 0, &data);
    }

    pub fn set_solid_fill_color(&self, queue: &wgpu::Queue, color: &[f32; 4]) {
        let data = crate::utils::transmute_slice(color);
        queue.write_buffer(&self.fill_solid_uniform_buffer, 0, &data);
    }

    pub fn stencil_render_pass<'a>(encoder: &'a mut wgpu::CommandEncoder, depth_stencil_texture_view: &'a wgpu::TextureView) -> wgpu::RenderPass<'a> {
        encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: None,
            color_attachments: &[],
            depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachmentDescriptor {
                attachment: depth_stencil_texture_view,
                depth_ops: Some(wgpu::Operations {
                    load: wgpu::LoadOp::Clear(0.0),
                    store: true,
                }),
                stencil_ops: Some(wgpu::Operations {
                    load: wgpu::LoadOp::Clear(0),
                    store: true,
                }),
            }),
        })
    }

    pub fn cover_render_pass<'a>(
        encoder: &'a mut wgpu::CommandEncoder,
        depth_stencil_texture_view: &'a wgpu::TextureView,
        render_texture_view: &'a wgpu::TextureView,
    ) -> wgpu::RenderPass<'a> {
        encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: None,
            color_attachments: &[wgpu::RenderPassColorAttachmentDescriptor {
                attachment: render_texture_view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                    store: true,
                },
            }],
            depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachmentDescriptor {
                attachment: depth_stencil_texture_view,
                depth_ops: Some(wgpu::Operations {
                    load: wgpu::LoadOp::Load,
                    store: false,
                }),
                stencil_ops: Some(wgpu::Operations {
                    load: wgpu::LoadOp::Load,
                    store: false,
                }),
            }),
        })
    }
}
