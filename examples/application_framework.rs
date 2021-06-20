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
    fn new(device: &wgpu::Device, queue: &mut wgpu::Queue, swap_chain_descriptor: &wgpu::SwapChainDescriptor) -> Self;
    fn resize(&mut self, device: &wgpu::Device, queue: &mut wgpu::Queue, swap_chain_descriptor: &wgpu::SwapChainDescriptor);
    fn render(&mut self, device: &wgpu::Device, queue: &mut wgpu::Queue, frame: &wgpu::SwapChainTexture, animation_time: f64);
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
        log::info!("Using {} ({:?})", adapter_info.name, adapter_info.backend);

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

    fn resize(&mut self) -> (wgpu::SwapChainDescriptor, wgpu::SwapChain) {
        self.window.request_redraw();
        let swap_chain_descriptor = wgpu::SwapChainDescriptor {
            usage: wgpu::TextureUsage::RENDER_ATTACHMENT,
            format: self.adapter.get_swap_chain_preferred_format(&self.surface).unwrap(),
            width: self.size.width,
            height: self.size.height,
            present_mode: wgpu::PresentMode::Mailbox,
        };
        let swap_chain = self.device.create_swap_chain(&self.surface, &swap_chain_descriptor);
        (swap_chain_descriptor, swap_chain)
    }

    fn start_loop<A: 'static + Application>(mut self, event_loop: winit::event_loop::EventLoop<()>) {
        let (swap_chain_descriptor, mut swap_chain) = self.resize();
        let mut application = A::new(&self.device, &mut self.queue, &swap_chain_descriptor);
        application.resize(&self.device, &mut self.queue, &swap_chain_descriptor);
        let start_time = std::time::Instant::now();

        let spawner = Spawner::new();
        event_loop.run(move |event, _, control_flow| {
            let _ = (&self.instance, &self.adapter);
            *control_flow = winit::event_loop::ControlFlow::Poll;
            match event {
                winit::event::Event::MainEventsCleared => {
                    spawner.run_until_stalled();
                    self.window.request_redraw();
                }
                winit::event::Event::WindowEvent {
                    event: winit::event::WindowEvent::Resized(size),
                    ..
                } => {
                    self.size.width = size.width.max(1);
                    self.size.height = size.height.max(1);
                    let (swap_chain_descriptor, new_swap_chain) = self.resize();
                    swap_chain = new_swap_chain;
                    application.resize(&self.device, &mut self.queue, &swap_chain_descriptor);
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
                    _ => application.window_event(event),
                },
                winit::event::Event::RedrawRequested(_) => {
                    let frame = match swap_chain.get_current_frame() {
                        Ok(frame) => frame,
                        Err(_) => panic!("Failed to acquire next swap chain texture!"),
                    };
                    application.render(&self.device, &mut self.queue, &frame.output, start_time.elapsed().as_secs_f64());
                }
                _ => {}
            }
        });
    }
}

#[allow(dead_code)]
fn main() {}
