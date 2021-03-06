#[path = "../application_framework.rs"]
mod application_framework;

const MSAA_SAMPLE_COUNT: u32 = 4;

struct Application<'a> {
    depth_stencil_texture_view: Option<wgpu::TextureView>,
    msaa_color_texture_view: Option<wgpu::TextureView>,
    renderer: contrast_renderer::renderer::Renderer,
    dynamic_stroke_options: [contrast_renderer::path::DynamicStrokeOptions<'a>; 1],
    shape: contrast_renderer::renderer::Shape,
    view_size: wgpu::Extent3d,
    view_rotation: glam::Quat,
    view_distance: f32,
}

impl<'a> application_framework::Application for Application<'a> {
    fn new(device: &wgpu::Device, _queue: &mut wgpu::Queue, swap_chain_descriptor: &wgpu::SwapChainDescriptor) -> Self {
        let blending = wgpu::ColorTargetState {
            format: swap_chain_descriptor.format,
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
        let renderer = contrast_renderer::renderer::Renderer::new(&device, blending, MSAA_SAMPLE_COUNT, 4, 4).unwrap();

        let dynamic_stroke_options = [contrast_renderer::path::DynamicStrokeOptions::Dashed {
            join: contrast_renderer::path::Join::Miter,
            pattern: &[contrast_renderer::path::DashInterval {
                gap_start: 3.0,
                gap_end: 4.0,
                dash_start: contrast_renderer::path::Cap::Butt,
                dash_end: contrast_renderer::path::Cap::Butt,
            }],
            phase: 0.0,
        }];

        let data = include_bytes!("fonts/OpenSans-Regular.ttf");
        let face = ttf_parser::Face::from_slice(data, 0).unwrap();
        let mut paths = contrast_renderer::text::paths_of_text(
            &face,
            contrast_renderer::text::HorizontalAlignment::Center,
            contrast_renderer::text::VerticalAlignment::Center,
            "Hello World",
        );
        for path in &mut paths {
            path.transform(&glam::Mat3::from_scale_angle_translation(
                glam::vec2(0.001, 0.001),
                0.0,
                glam::vec2(0.0, 0.0),
            ));
            path.reverse();
        }
        paths.insert(
            0,
            contrast_renderer::path::Path::from_rounded_rect(glam::vec2(0.0, 0.0), glam::vec2(5.8, 1.3), 0.5),
        );
        paths[0].stroke_options = Some(contrast_renderer::path::StrokeOptions {
            width: 0.1,
            offset: 0.0,
            miter_clip: 1.0,
            closed: true,
            dynamic_stroke_options_group: 0,
            curve_approximation: contrast_renderer::path::CurveApproximation::UniformTangentAngle(0.1),
        });
        let shape = contrast_renderer::renderer::Shape::from_paths(&device, &renderer, &dynamic_stroke_options, paths.as_slice()).unwrap();

        Self {
            depth_stencil_texture_view: None,
            msaa_color_texture_view: None,
            renderer,
            dynamic_stroke_options,
            shape,
            view_size: wgpu::Extent3d {
                width: swap_chain_descriptor.width,
                height: swap_chain_descriptor.height,
                depth: 1,
            },
            view_rotation: glam::Quat::default(),
            view_distance: 5.0,
        }
    }

    fn resize(&mut self, device: &wgpu::Device, _queue: &mut wgpu::Queue, swap_chain_descriptor: &wgpu::SwapChainDescriptor) {
        self.view_size = wgpu::Extent3d {
            width: swap_chain_descriptor.width,
            height: swap_chain_descriptor.height,
            depth: 1,
        };
        let depth_stencil_texture_descriptor = wgpu::TextureDescriptor {
            size: self.view_size,
            mip_level_count: 1,
            sample_count: MSAA_SAMPLE_COUNT,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Depth24PlusStencil8,
            usage: wgpu::TextureUsage::RENDER_ATTACHMENT,
            label: None,
        };
        let depth_stencil_texture = device.create_texture(&depth_stencil_texture_descriptor);
        self.depth_stencil_texture_view = Some(depth_stencil_texture.create_view(&wgpu::TextureViewDescriptor {
            dimension: Some(wgpu::TextureViewDimension::D2),
            ..wgpu::TextureViewDescriptor::default()
        }));
        if MSAA_SAMPLE_COUNT > 1 {
            let msaa_color_texture_descriptor = wgpu::TextureDescriptor {
                size: self.view_size,
                mip_level_count: 1,
                sample_count: MSAA_SAMPLE_COUNT,
                dimension: wgpu::TextureDimension::D2,
                format: swap_chain_descriptor.format,
                usage: wgpu::TextureUsage::RENDER_ATTACHMENT,
                label: None,
            };
            let msaa_color_texture = device.create_texture(&msaa_color_texture_descriptor);
            self.msaa_color_texture_view = Some(msaa_color_texture.create_view(&wgpu::TextureViewDescriptor {
                dimension: Some(wgpu::TextureViewDimension::D2),
                ..wgpu::TextureViewDescriptor::default()
            }));
        }
    }

    fn render(&mut self, device: &wgpu::Device, queue: &mut wgpu::Queue, frame: &wgpu::SwapChainTexture, animation_time: f64) {
        match &mut self.dynamic_stroke_options[0] {
            contrast_renderer::path::DynamicStrokeOptions::Dashed { phase, .. } => {
                *phase = animation_time as f32;
            }
            _ => unreachable!(),
        }
        self.shape.set_dynamic_stroke_options(queue, 0, &self.dynamic_stroke_options[0]).unwrap();
        let transform_uniform_data = glam::Mat4::perspective_lh(
            std::f32::consts::PI * 0.5,
            self.view_size.width as f32 / self.view_size.height as f32,
            1.0,
            1000.0,
        ) * glam::Mat4::from_rotation_translation(self.view_rotation, glam::vec3(0.0, 0.0, self.view_distance));
        self.renderer.set_transform(&queue, &transform_uniform_data);
        let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: None,
                color_attachments: &[],
                depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachmentDescriptor {
                    attachment: &self.depth_stencil_texture_view.as_ref().unwrap(),
                    depth_ops: Some(wgpu::Operations {
                        load: wgpu::LoadOp::Clear(0.0),
                        store: false,
                    }),
                    stencil_ops: Some(wgpu::Operations {
                        load: wgpu::LoadOp::Clear(0),
                        store: true,
                    }),
                }),
            });
            self.shape.render_stencil(&self.renderer, &mut render_pass);
        }
        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: None,
                color_attachments: &[wgpu::RenderPassColorAttachmentDescriptor {
                    attachment: if MSAA_SAMPLE_COUNT == 1 {
                        &frame.view
                    } else {
                        &self.msaa_color_texture_view.as_ref().unwrap()
                    },
                    resolve_target: if MSAA_SAMPLE_COUNT == 1 { None } else { Some(&frame.view) },
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: true,
                    },
                }],
                depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachmentDescriptor {
                    attachment: &self.depth_stencil_texture_view.as_ref().unwrap(),
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
            self.shape.render_color_solid(&self.renderer, &mut render_pass, 0);
        }
        queue.submit(Some(encoder.finish()));
    }

    fn window_event(&mut self, event: winit::event::WindowEvent) {
        match event {
            winit::event::WindowEvent::CursorMoved { position, .. } => {
                let position = glam::vec2(
                    position.x as f32 / self.view_size.width as f32 - 0.5,
                    position.y as f32 / self.view_size.height as f32 - 0.5,
                );
                self.view_rotation = glam::Quat::from_rotation_ypr(-position[0] * std::f32::consts::PI, -position[1] * std::f32::consts::PI, 0.0);
            }
            winit::event::WindowEvent::MouseWheel { delta, .. } => {
                let difference = match delta {
                    winit::event::MouseScrollDelta::LineDelta(x, y) => glam::vec2(x, y),
                    winit::event::MouseScrollDelta::PixelDelta(delta) => glam::vec2(delta.x as f32, delta.y as f32) * 0.1,
                };
                self.view_distance = (self.view_distance + difference[1]).clamp(2.0, 100.0);
            }
            _ => {}
        }
    }
}

fn main() {
    application_framework::ApplicationManager::run::<Application>("Contrast Renderer - Showcase");
}
