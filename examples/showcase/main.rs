#[path = "../application_framework.rs"]
mod application_framework;

const MSAA_SAMPLE_COUNT: u32 = 4;

struct Application {
    depth_stencil_texture_view: Option<wgpu::TextureView>,
    msaa_color_texture_view: Option<wgpu::TextureView>,
    path_renderer: contrast_render_engine::renderer::Renderer,
    fill_shape: contrast_render_engine::renderer::Shape,
}

impl application_framework::Application for Application {
    fn new(device: &wgpu::Device, _queue: &mut wgpu::Queue, swap_chain_descriptor: &wgpu::SwapChainDescriptor) -> Self {
        let fill_color_state = wgpu::ColorTargetState {
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
        let path_renderer = contrast_render_engine::renderer::Renderer::new(&device, fill_color_state, MSAA_SAMPLE_COUNT, 4, 4).unwrap();

        let data = include_bytes!("fonts/OpenSans-Regular.ttf");
        let face = ttf_parser::Face::from_slice(data, 0).unwrap();
        let mut paths = contrast_render_engine::text::paths_of_text(
            &face,
            contrast_render_engine::text::HorizontalAlignment::Center,
            contrast_render_engine::text::VerticalAlignment::Center,
            "Hello World",
        );
        for path in &mut paths {
            path.transform(&glam::Mat3::from_scale_angle_translation(glam::vec2(0.1, 0.1), 0.0, glam::vec2(0.0, 0.0)));
            path.reverse();
        }
        paths.insert(
            0,
            contrast_render_engine::path::Path::from_rounded_rect(glam::vec2(0.0, 0.0), glam::vec2(580.0, 130.0), 50.0),
        );
        let fill_shape = contrast_render_engine::renderer::Shape::new(&device, paths.as_slice());

        Self {
            depth_stencil_texture_view: None,
            msaa_color_texture_view: None,
            path_renderer,
            fill_shape,
        }
    }

    fn resize(&mut self, device: &wgpu::Device, queue: &mut wgpu::Queue, swap_chain_descriptor: &wgpu::SwapChainDescriptor) {
        let transform_uniform_data = [
            [2.0 / swap_chain_descriptor.width as f32, 0.0, 0.0, 0.0],
            [0.0, 2.0 / swap_chain_descriptor.height as f32, 0.0, 0.0],
            [0.0, 0.0, 1.0, 0.0],
            [0.0, 0.0, 0.0, 1.0],
        ];
        self.path_renderer.set_transform(&queue, &transform_uniform_data);
        let size = wgpu::Extent3d {
            width: swap_chain_descriptor.width,
            height: swap_chain_descriptor.height,
            depth: 1,
        };
        let depth_stencil_texture_descriptor = wgpu::TextureDescriptor {
            size,
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
                size,
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

    fn render(&mut self, device: &wgpu::Device, queue: &mut wgpu::Queue, frame: &wgpu::SwapChainTexture) {
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
            self.fill_shape.render_stencil(&self.path_renderer, &mut render_pass);
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
            self.fill_shape
                .render_solid_fill(&self.path_renderer, &mut render_pass, 0);
        }

        queue.submit(Some(encoder.finish()));
    }
}

fn main() {
    application_framework::ApplicationManager::run::<Application>("Contrast Render Engine - Showcase");
}
