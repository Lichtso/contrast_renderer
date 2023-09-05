#[cfg(not(target_arch = "wasm32"))]
struct StdOutLogger;

#[cfg(not(target_arch = "wasm32"))]
impl log::Log for StdOutLogger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= log::Level::Info
    }

    fn log(&self, record: &log::Record) {
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
use winit::platform::web::WindowExtWebSys;

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

pub trait Application {
    fn new(device: &wgpu::Device, queue: &mut wgpu::Queue, surface_configuration: &wgpu::SurfaceConfiguration) -> Self;
    fn resize(&mut self, device: &wgpu::Device, queue: &mut wgpu::Queue, surface_configuration: &wgpu::SurfaceConfiguration);
    fn render(&mut self, device: &wgpu::Device, queue: &mut wgpu::Queue, frame: &wgpu::SurfaceTexture, animation_time: f64);
    fn window_event(&mut self, event: winit::event::WindowEvent);
}

pub struct ApplicationManager {
    window: winit::window::Window,
    instance: wgpu::Instance,
    size: winit::dpi::PhysicalSize<u32>,
    surface: wgpu::Surface,
    adapter: wgpu::Adapter,
    device: wgpu::Device,
    queue: wgpu::Queue,
}

impl ApplicationManager {
    pub fn run<A: 'static + Application>(title: &'static str) {
        let event_loop = winit::event_loop::EventLoop::new();

        #[cfg(not(target_arch = "wasm32"))]
        {
            let setup = pollster::block_on(ApplicationManager::setup(&event_loop, title));
            setup.start_loop::<A>(event_loop);
        }

        #[cfg(target_arch = "wasm32")]
        {
            wasm_bindgen_futures::spawn_local(async move {
                let setup = ApplicationManager::setup(&event_loop, title).await;
                setup.start_loop::<A>(event_loop);
            });
        }
    }

    async fn setup(event_loop: &winit::event_loop::EventLoop<()>, title: &'static str) -> Self {
        let mut builder = winit::window::WindowBuilder::new();
        builder = builder.with_title(title);
        let window = builder.build(event_loop).unwrap();

        #[cfg(not(target_arch = "wasm32"))]
        log::set_logger(&LOGGER)
            .map(|()| log::set_max_level(log::LevelFilter::Info))
            .expect("Could not initialize logger");

        #[cfg(target_arch = "wasm32")]
        {
            console_log::init().expect("Could not initialize logger");
            std::panic::set_hook(Box::new(console_error_panic_hook::hook));
            web_sys::window()
                .and_then(|window| window.document())
                .and_then(|document| document.body())
                .and_then(|body| body.append_child(&web_sys::Element::from(window.canvas())).ok())
                .expect("Couldn't append canvas to document body");
        }

        let instance = wgpu::Instance::default();
        let (size, surface) = unsafe {
            let size = window.inner_size();
            let surface = instance.create_surface(&window).expect("WebGPU is not supported or not enabled");
            (size, surface)
        };
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::default(),
                force_fallback_adapter: false,
                compatible_surface: Some(&surface),
            })
            .await
            .expect("No suitable GPU adapters found on the system!");

        let adapter_info = adapter.get_info();
        log::info!("Using {} ({:?})", adapter_info.name, adapter_info.backend);

        let mut optional_features = wgpu::Features::default();
        optional_features |= wgpu::Features::TEXTURE_ADAPTER_SPECIFIC_FORMAT_FEATURES;
        let required_features = wgpu::Features::default();
        let required_limits = wgpu::Limits { ..wgpu::Limits::default() };
        let adapter_features = adapter.features();
        assert!(
            adapter_features.contains(required_features),
            "Adapter does not support required features: {:?}",
            required_features - adapter_features
        );
        optional_features -= optional_features - adapter_features;
        let (device, queue) = adapter
            .request_device(
                &wgpu::DeviceDescriptor {
                    label: None,
                    features: required_features | optional_features,
                    limits: required_limits,
                },
                None,
            )
            .await
            .expect("Unable to find a suitable GPU adapter!");

        Self {
            window,
            instance,
            size,
            surface,
            adapter,
            device,
            queue,
        }
    }

    fn generate_surface_configuration(&self) -> wgpu::SurfaceConfiguration {
        wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: wgpu::TextureFormat::Bgra8Unorm, // self.surface.get_supported_formats(&self.adapter)[0],
            view_formats: vec![wgpu::TextureFormat::Bgra8Unorm],
            width: self.size.width,
            height: self.size.height,
            present_mode: wgpu::PresentMode::Fifo, // self.surface.get_supported_present_modes(&self.adapter)[0],
            alpha_mode: wgpu::CompositeAlphaMode::Opaque, // self.surface.get_supported_alpha_modes(&adapter)[0],
        }
    }

    fn resize<A: 'static + Application>(&mut self, application: &mut A, size: winit::dpi::PhysicalSize<u32>) {
        self.size.width = size.width.max(1);
        self.size.height = size.height.max(1);
        self.window.request_redraw();
        let surface_configuration = self.generate_surface_configuration();
        self.surface.configure(&self.device, &surface_configuration);
        application.resize(&self.device, &mut self.queue, &surface_configuration);
    }

    fn start_loop<A: 'static + Application>(mut self, event_loop: winit::event_loop::EventLoop<()>) {
        let surface_configuration = self.generate_surface_configuration();
        let mut application = A::new(&self.device, &mut self.queue, &surface_configuration);
        self.resize(&mut application, self.size);
        #[cfg(not(target_arch = "wasm32"))]
        let start_time = std::time::Instant::now();
        #[cfg(target_arch = "wasm32")]
        let (clock, start_time) = {
            let clock = web_sys::window().and_then(|window| window.performance()).unwrap();
            let start_time = clock.now();
            (clock, start_time)
        };
        let mut prev_frame = start_time;
        let mut rolling_average = 0u32;
        let mut average_window = [0u32; 64];
        let mut average_window_slot = 0;

        let spawner = Spawner::new();
        event_loop.run(move |event, _, control_flow| {
            let _ = (&self.instance, &self.adapter);
            *control_flow = winit::event_loop::ControlFlow::Wait;
            match event {
                winit::event::Event::RedrawEventsCleared => {
                    spawner.run_until_stalled();
                }
                winit::event::Event::WindowEvent {
                    event: winit::event::WindowEvent::Resized(new_inner_size),
                    ..
                } => {
                    self.resize(&mut application, new_inner_size);
                }
                winit::event::Event::WindowEvent {
                    event: winit::event::WindowEvent::ScaleFactorChanged { new_inner_size, .. },
                    ..
                } => {
                    self.resize(&mut application, *new_inner_size);
                }
                winit::event::Event::WindowEvent { event, .. } => match event {
                    winit::event::WindowEvent::KeyboardInput {
                        input:
                            winit::event::KeyboardInput {
                                virtual_keycode: Some(winit::event::VirtualKeyCode::Escape),
                                state: winit::event::ElementState::Pressed,
                                ..
                            },
                        ..
                    }
                    | winit::event::WindowEvent::CloseRequested => {
                        *control_flow = winit::event_loop::ControlFlow::Exit;
                    }
                    _ => {
                        application.window_event(event);
                        self.window.request_redraw();
                    }
                },
                winit::event::Event::RedrawRequested(_) => {
                    #[cfg(not(target_arch = "wasm32"))]
                    let (animation_time, frame_time) = {
                        let now = std::time::Instant::now();
                        let animation_time = (now - start_time).as_secs_f64();
                        let frame_time = (now - prev_frame).subsec_micros();
                        prev_frame = now;
                        (animation_time, frame_time)
                    };
                    #[cfg(target_arch = "wasm32")]
                    let (animation_time, frame_time) = {
                        let now = clock.now();
                        let animation_time = (now - start_time) * 0.001;
                        let frame_time = ((now - prev_frame) * 1000.0) as u32;
                        prev_frame = now;
                        (animation_time, frame_time)
                    };
                    rolling_average -= average_window[average_window_slot];
                    average_window[average_window_slot] = frame_time;
                    rolling_average += average_window[average_window_slot];
                    average_window_slot = (average_window_slot + 1) % average_window.len();
                    log::info!(
                        "rolling_average={} frame_time={}",
                        rolling_average / average_window.len() as u32,
                        frame_time
                    );

                    let frame = self.surface.get_current_texture().unwrap();
                    application.render(&self.device, &mut self.queue, &frame, animation_time);
                    frame.present();
                }
                _ => {}
            }
        });
    }
}

#[allow(dead_code)]
fn main() {}
