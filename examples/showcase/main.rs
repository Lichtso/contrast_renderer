use winit::{
    event::{self, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
};
#[macro_use]
extern crate log;

#[cfg(not(target_arch = "wasm32"))]
use log::{Level, LevelFilter, Metadata, Record};

#[cfg(not(target_arch = "wasm32"))]
struct StdOutLogger;

#[cfg(not(target_arch = "wasm32"))]
impl log::Log for StdOutLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Info
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            println!("{} - {}", record.level(), record.args());
        }
    }

    fn flush(&self) {}
}

#[cfg(not(target_arch = "wasm32"))]
static LOGGER: StdOutLogger = StdOutLogger;

#[cfg(not(target_arch = "wasm32"))]
pub struct Spawner<'a> {
    executor: async_executor::LocalExecutor<'a>,
}

#[cfg(target_arch = "wasm32")]
use winit::platform::web::WindowExtWebSys;

#[cfg(not(target_arch = "wasm32"))]
impl<'a> Spawner<'a> {
    fn new() -> Self {
        Self {
            executor: async_executor::LocalExecutor::new(),
        }
    }

    #[allow(dead_code)]
    pub fn spawn_local(&self, future: impl std::future::Future<Output = ()> + 'a) {
        self.executor.spawn(future).detach();
    }

    fn run_until_stalled(&self) {
        while self.executor.try_tick() {}
    }
}

#[cfg(target_arch = "wasm32")]
pub struct Spawner {}

#[cfg(target_arch = "wasm32")]
impl Spawner {
    fn new() -> Self {
        Self {}
    }

    #[allow(dead_code)]
    pub fn spawn_local(&self, future: impl std::future::Future<Output = ()> + 'static) {
        wasm_bindgen_futures::spawn_local(future);
    }

    fn run_until_stalled(&self) {}
}

#[allow(dead_code)]
struct GuiSetup {
    window: winit::window::Window,
    instance: wgpu::Instance,
    size: winit::dpi::PhysicalSize<u32>,
    surface: wgpu::Surface,
    adapter: wgpu::Adapter,
    device: wgpu::Device,
    queue: wgpu::Queue,
    depth_stencil_texture_view: Option<wgpu::TextureView>,
    path_renderer: contrast_render_engine::renderer::Renderer,
    fillable_shape: contrast_render_engine::renderer::FillableShape,
}

impl GuiSetup {
    async fn new(event_loop: &EventLoop<()>) -> Self {
        let mut builder = winit::window::WindowBuilder::new();
        builder = builder.with_title("Contrast Render Engine - Showcase");
        let window = builder.build(event_loop).unwrap();

        #[cfg(not(target_arch = "wasm32"))]
        log::set_logger(&LOGGER)
            .map(|()| log::set_max_level(LevelFilter::Info))
            .expect("Could not initialize logger");

        #[cfg(target_arch = "wasm32")]
        {
            console_log::init().expect("Could not initialize logger");
            std::panic::set_hook(Box::new(console_error_panic_hook::hook));
            web_sys::window()
                .and_then(|win| win.document())
                .and_then(|doc| doc.body())
                .and_then(|body| body.append_child(&web_sys::Element::from(window.canvas())).ok())
                .expect("Couldn't append canvas to document body");
        }

        let instance = wgpu::Instance::new(wgpu::BackendBit::PRIMARY);
        let (size, surface) = unsafe {
            let size = window.inner_size();
            let surface = instance.create_surface(&window);
            (size, surface)
        };
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::default(),
                compatible_surface: Some(&surface),
            })
            .await
            .expect("No suitable GPU adapters found on the system!");

        let adapter_info = adapter.get_info();
        info!("Using {} ({:?})", adapter_info.name, adapter_info.backend);

        let required_features = wgpu::Features::default();
        let needed_limits = wgpu::Limits { ..wgpu::Limits::default() };
        let adapter_features = adapter.features();
        assert!(
            adapter_features.contains(required_features),
            "Adapter does not support required features: {:?}",
            required_features - adapter_features
        );
        let (device, queue) = adapter
            .request_device(
                &wgpu::DeviceDescriptor {
                    label: None,
                    features: adapter_features | required_features,
                    limits: needed_limits,
                },
                None,
            )
            .await
            .expect("Unable to find a suitable GPU adapter!");

        let path_renderer =
            contrast_render_engine::renderer::Renderer::new(&device, adapter.get_swap_chain_preferred_format(&surface));

        let data = include_bytes!("fonts/OpenSans-Regular.ttf");
        let face = ttf_parser::Face::from_slice(data, 0).unwrap();
        let mut paths = contrast_render_engine::text::paths_of_text(
            &face,
            contrast_render_engine::text::HorizontalAlignment::Center,
            contrast_render_engine::text::VerticalAlignment::Center,
            "Hello World",
        );
        for path in &mut paths {
            path.transform(glam::Mat3::from_scale_angle_translation(glam::vec2(0.1, 0.1), 0.0, glam::vec2(0.0, 0.0)));
            path.reverse();
        }
        paths.insert(0, contrast_render_engine::path::Path::from_rounded_rect(
            glam::vec2(0.0, 0.0),
            glam::vec2(600.0, 120.0),
            50.0,
        ));
        let fillable_shape = contrast_render_engine::renderer::FillableShape::new(&device, &paths);

        Self {
            window,
            instance,
            size,
            surface,
            adapter,
            device,
            queue,
            depth_stencil_texture_view: None,
            path_renderer,
            fillable_shape,
        }
    }

    fn resize(&mut self) -> wgpu::SwapChain {
        let transform_uniform_data = [
            [2.0 / self.size.width as f32, 0.0, 0.0, 0.0],
            [0.0, 2.0 / self.size.height as f32, 0.0, 0.0],
            [0.0, 0.0, 1.0, 0.0],
            [0.0, 0.0, 0.0, 1.0],
        ];
        self.path_renderer.set_transform(&self.queue, &transform_uniform_data);
        let color_format = self.adapter.get_swap_chain_preferred_format(&self.surface);
        let depth_stencil_texture_descriptor = wgpu::TextureDescriptor {
            size: wgpu::Extent3d {
                width: self.size.width,
                height: self.size.height,
                depth: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Depth24PlusStencil8,
            usage: wgpu::TextureUsage::RENDER_ATTACHMENT,
            label: None,
        };
        let depth_stencil_texture = self.device.create_texture(&depth_stencil_texture_descriptor);
        self.depth_stencil_texture_view = Some(depth_stencil_texture.create_view(&wgpu::TextureViewDescriptor {
            dimension: Some(wgpu::TextureViewDimension::D2),
            ..wgpu::TextureViewDescriptor::default()
        }));
        let swap_chain_descriptor = wgpu::SwapChainDescriptor {
            usage: wgpu::TextureUsage::RENDER_ATTACHMENT,
            format: color_format,
            width: self.size.width,
            height: self.size.height,
            present_mode: wgpu::PresentMode::Mailbox,
        };
        self.device.create_swap_chain(&self.surface, &swap_chain_descriptor)
    }

    fn render(&mut self, frame: &wgpu::SwapChainTexture) {
        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });

        {
            let mut render_pass =
                contrast_render_engine::renderer::Renderer::stencil_render_pass(&mut encoder, &self.depth_stencil_texture_view.as_ref().unwrap());
            self.fillable_shape.render_stencil(&self.path_renderer, &mut render_pass);
        }

        {
            let mut render_pass = contrast_render_engine::renderer::Renderer::cover_render_pass(
                &mut encoder,
                &self.depth_stencil_texture_view.as_ref().unwrap(),
                &frame.view,
            );
            self.fillable_shape.render_cover(&self.path_renderer, &mut render_pass);
        }

        self.queue.submit(Some(encoder.finish()));
    }

    fn run(mut self, event_loop: EventLoop<()>) {
        let mut swap_chain = self.resize();

        let spawner = Spawner::new();
        event_loop.run(move |event, _, control_flow| {
            let _ = (&self.instance, &self.adapter);
            *control_flow = ControlFlow::Poll;
            match event {
                event::Event::MainEventsCleared => {
                    spawner.run_until_stalled();

                    #[cfg(target_arch = "wasm32")]
                    self.window.request_redraw();
                }
                event::Event::WindowEvent {
                    event: WindowEvent::Resized(size),
                    ..
                } => {
                    self.size.width = size.width.max(1);
                    self.size.height = size.height.max(1);
                    swap_chain = self.resize();
                    self.window.request_redraw();
                }
                event::Event::WindowEvent { event, .. } => match event {
                    WindowEvent::KeyboardInput {
                        input:
                            event::KeyboardInput {
                                virtual_keycode: Some(event::VirtualKeyCode::Escape),
                                state: event::ElementState::Pressed,
                                ..
                            },
                        ..
                    }
                    | WindowEvent::CloseRequested => {
                        *control_flow = ControlFlow::Exit;
                    }
                    _ => {}
                },
                event::Event::RedrawRequested(_) => {
                    let frame = match swap_chain.get_current_frame() {
                        Ok(frame) => frame,
                        Err(_) => {
                            panic!("Failed to acquire next swap chain texture!")
                        }
                    };
                    self.render(&frame.output);
                }
                _ => {}
            }
        });
    }
}

fn main() {
    let event_loop = EventLoop::new();

    #[cfg(not(target_arch = "wasm32"))]
    {
        let setup = pollster::block_on(GuiSetup::new(&event_loop));
        setup.run(event_loop);
    }

    #[cfg(target_arch = "wasm32")]
    {
        wasm_bindgen_futures::spawn_local(async move {
            let setup = GuiSetup::new(&event_loop).await;
            setup.run(event_loop);
        });
    }
}
