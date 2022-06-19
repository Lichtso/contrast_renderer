#[path = "../application_framework.rs"]
mod application_framework;

use contrast_renderer::{
    hash_map,
    ui::{wrapped_values::Value, Node, NodeOrObservableIdentifier},
};
use geometric_algebra::{ppga2d, ppga3d};
use std::rc::Rc;

const MSAA_SAMPLE_COUNT: u32 = 4;

const OPEN_SANS_TTF: &[u8] = include_bytes!("../../examples/fonts/OpenSans-Regular.ttf");
const KEYMAP: &str = include_str!("../../examples/keymaps/de_macos.txt");

struct Application {
    depth_stencil_texture_view: Option<wgpu::TextureView>,
    msaa_color_texture_view: Option<wgpu::TextureView>,
    ui_renderer: contrast_renderer::ui::renderer::Renderer,
    ui_node_hierarchy: contrast_renderer::ui::node_hierarchy::NodeHierarchy,
    ui_event_translator: contrast_renderer::ui::message::WinitEventTranslator,
    viewport_size: wgpu::Extent3d,
}

impl application_framework::Application for Application {
    fn new(device: &wgpu::Device, _queue: &mut wgpu::Queue, surface_configuration: &wgpu::SurfaceConfiguration) -> Self {
        let renderer = Rc::new(
            contrast_renderer::renderer::Renderer::new(
                &device,
                contrast_renderer::renderer::Configuration {
                    blending: wgpu::ColorTargetState {
                        format: surface_configuration.format,
                        blend: Some(wgpu::BlendState {
                            color: wgpu::BlendComponent {
                                src_factor: wgpu::BlendFactor::One,
                                dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
                                operation: wgpu::BlendOperation::Add,
                            },
                            alpha: wgpu::BlendComponent {
                                src_factor: wgpu::BlendFactor::One,
                                dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
                                operation: wgpu::BlendOperation::Add,
                            },
                        }),
                        write_mask: wgpu::ColorWrites::ALL,
                    },
                    cull_mode: None,
                    depth_compare: wgpu::CompareFunction::Always,
                    depth_write_enabled: false,
                    color_attachment_in_stencil_pass: true,
                    msaa_sample_count: MSAA_SAMPLE_COUNT,
                    clip_nesting_counter_bits: 7,
                    winding_counter_bits: 1,
                    alpha_layer_count: 0,
                },
            )
            .unwrap(),
        );
        let ui_renderer = contrast_renderer::ui::renderer::Renderer::new(renderer, device);
        let mut ui_node_hierarchy = contrast_renderer::ui::node_hierarchy::NodeHierarchy::default();
        ui_node_hierarchy.theme_properties = hash_map! {
            "font_face" => Value::TextFont(std::rc::Rc::new(contrast_renderer::text::Font::new("OpenSans-Regular".to_string(), OPEN_SANS_TTF))),
            "font_size" => Value::Float1(40.0.into()),
            "text_orientation" => Value::TextOrientation(contrast_renderer::text::Orientation::LeftToRight),
            "text_major_alignment" => Value::TextAlignment(contrast_renderer::text::Alignment::Center),
            "text_minor_alignment" => Value::TextAlignment(contrast_renderer::text::Alignment::Center),
            "text_color" => Value::Float4([1.0, 0.8, 0.5, 1.0].into()),
            "text_selection_color" => Value::Float4([1.0, 1.0, 1.0, 0.5].into()),

            "range_empty_color" => Value::Float4([0.05, 0.05, 0.05, 1.0].into()),
            "range_filled_color" => Value::Float4([0.1, 0.3, 0.8, 1.0].into()),
            "range_corner_radius" => Value::Float1(10.0.into()),

            "button_fill_color" => Value::Float4([0.03, 0.03, 0.03, 1.0].into()),
            "button_corner_radius" => Value::Float1(7.0.into()),
            "button_padding" => Value::Float2([10.0, 10.0].into()),

            "checkbox_unchecked_color" => Value::Float4([0.3, 0.3, 0.3, 1.0].into()),
            "checkbox_checked_color" => Value::Float4([0.1, 0.3, 0.5, 1.0].into()),
            "checkbox_checkmark_color" => Value::Float4([1.0, 1.0, 1.0, 1.0].into()),
            "checkbox_corner_radius" => Value::Float1(7.0.into()),
            "ckeckbox_half_extent" => Value::Float2([10.0, 10.0].into()),

            "speech_balloon_fill_color" => Value::Float4([0.02, 0.02, 0.02, 1.0].into()),
            "speech_balloon_stroke_color" => Value::Float4([0.8, 0.8, 0.8, 1.0].into()),
            "speech_balloon_stroke_width" => Value::Float1(2.0.into()),
            "speech_balloon_corner_radius" => Value::Float1(10.0.into()),
            "speech_balloon_round_bottom_left" => Value::Boolean(true),
            "speech_balloon_round_top_left" => Value::Boolean(true),
            "speech_balloon_round_top_right" => Value::Boolean(true),
            "speech_balloon_round_bottom_right" => Value::Boolean(true),
            "speech_balloon_arrow_extent" => Value::Float1(20.0.into()),
            "speech_balloon_arrow_origin" => Value::Float1(0.0.into()),
            "speech_balloon_arrow_side" => Value::Side(contrast_renderer::ui::Side::Top),

            "scroll_bar_fill_color" => Value::Float4([0.7, 0.7, 0.7, 1.0].into()),
            "scroll_bar_active_fill_color" => Value::Float4([0.9, 0.9, 0.9, 1.0].into()),
            "scroll_bar_stroke_color" => Value::Float4([0.8, 0.8, 0.8, 1.0].into()),
            "scroll_bar_stroke_width" => Value::Float1(1.0.into()),
            "scroll_bar_corner_radius" => Value::Float1(5.0.into()),
            "scroll_bar_half_min_length" => Value::Float1(10.0.into()),
            "scroll_corner_radius" => Value::Float1(10.0.into()),

            "list_margin" => Value::Float1(10.0.into()),
            "list_padding" => Value::Float2([20.0, 20.0].into()),

            "tab_fill_color" => Value::Float4([0.03, 0.03, 0.03, 1.0].into()),
            "tab_corner_radius" => Value::Float1(7.0.into()),
            "tab_handle_inactive_fill_color" => Value::Float4([0.02, 0.02, 0.02, 1.0].into()),
            "tab_handle_active_fill_color" => Value::Float4([0.6, 0.6, 0.6, 1.0].into()),
            "tab_handle_stroke_color" => Value::Float4([0.8, 0.8, 0.8, 1.0].into()),
            "tab_handle_stroke_width" => Value::Float1(2.0.into()),
            "tab_handle_half_extent" => Value::Float2([5.0, 5.0].into()),
            "tab_handle_margin" => Value::Float1(10.0.into()),
            "tab_handle_padding" => Value::Float1(20.0.into()),
            "tabs_splitter_width" => Value::Float1(20.0.into()),
            "tabs_margin" => Value::Float1(5.0.into()),
            "tabs_padding" => Value::Float2([10.0, 10.0].into()),
        };
        let mut ui_event_translator = contrast_renderer::ui::message::WinitEventTranslator::default();
        ui_event_translator.load_keymap(KEYMAP).unwrap();

        let overlay_container_node_id = ui_node_hierarchy.link_node(Node::new(contrast_renderer::ui::overlay::overlay_container, hash_map! {}), None);

        let tab_container_node_id = ui_node_hierarchy.link_node(
            Node::new(
                contrast_renderer::ui::tabs::tab_container,
                hash_map! {
                    "orientation" => Value::Orientation(contrast_renderer::ui::Orientation::Horizontal),
                    "half_extent" => Value::Float2([500.0, 300.0].into()),
                },
            ),
            Some((NodeOrObservableIdentifier::Named("content"), overlay_container_node_id)),
        );

        let weights = [0.1, 0.3, 0.6];
        let mut tab_node_id = 0;
        for (child_index, weight) in weights.iter().enumerate() {
            tab_node_id = ui_node_hierarchy.link_node(
                Node::new(contrast_renderer::ui::tabs::tab, hash_map! {}),
                Some((NodeOrObservableIdentifier::NamedAndIndexed("tab", child_index), tab_container_node_id)),
            );
            ui_node_hierarchy.link_node(
                Node::new(
                    contrast_renderer::ui::tabs::tab_handle,
                    hash_map! {
                        "weight" => Value::Float1(weight.into()),
                    },
                ),
                Some((NodeOrObservableIdentifier::NamedAndIndexed("handle", child_index), tab_container_node_id)),
            );
        }

        let scroll_node_id = ui_node_hierarchy.link_node(
            Node::new(
                contrast_renderer::ui::scroll::scroll,
                hash_map! {
                    "half_extent" => Value::Float2([150.0, 50.0].into()),
                    "proposed_half_extent" => Value::Float2([150.0, 50.0].into()),
                    "content_motor" => Value::Float4([1.0, 0.0, 0.0, 0.0].into()),
                    "content_scale" => Value::Float1(1.0.into()),
                    "horizontal_bar" => Value::ScrollBarType(contrast_renderer::ui::ScrollBarType::Overflow),
                    "vertical_bar" => Value::ScrollBarType(contrast_renderer::ui::ScrollBarType::Overflow),
                },
            ),
            Some((NodeOrObservableIdentifier::Named("content"), tab_node_id)),
        );

        let list_node_id = ui_node_hierarchy.link_node(
            Node::new(
                contrast_renderer::ui::list::list,
                hash_map! {
                    "half_extent" => Value::Float2([150.0, 100.0].into()),
                    "proposed_half_extent" => Value::Float2([0.0, 0.0].into()),
                    "reverse" => Value::Boolean(false),
                    "orientation" => Value::Orientation(contrast_renderer::ui::Orientation::Vertical),
                    "list_minor_axis_alignment" => Value::Float1(0.0.into()),
                },
            ),
            Some((NodeOrObservableIdentifier::Named("content"), scroll_node_id)),
        );

        ui_node_hierarchy.link_node(
            Node::new(
                contrast_renderer::ui::checkbox::checkbox,
                hash_map! {
                    "is_checked" => Value::Boolean(false),
                    "enable_interaction" => Value::Boolean(true),
                },
            ),
            Some((NodeOrObservableIdentifier::Indexed(0), list_node_id)),
        );

        ui_node_hierarchy.link_node(
            Node::new(
                contrast_renderer::ui::range::range,
                hash_map! {
                    "proposed_half_extent" => Value::Float2([100.0, 40.0].into()),
                    "orientation" => Value::Orientation(contrast_renderer::ui::Orientation::Horizontal),
                    "numeric_value" => Value::Float1(0.5.into()),
                    "numeric_value_range" => Value::Float2([0.0, 1.0].into()),
                    "enable_interaction" => Value::Boolean(true),
                    "textual_projection" => Value::TextualProjection(contrast_renderer::ui::TextualProjection {
                        forward: |value: f32| format!("{:.2}%", value * 100.0),
                        backward: |string: String| {
                            string.char_indices().last().and_then(|(index, character)| {
                                if character != '%' {
                                    None
                                } else {
                                    string[0..index].parse::<f32>().ok().map(|value| value / 100.0)
                                }
                            })
                        },
                    }),
                },
            ),
            Some((NodeOrObservableIdentifier::Indexed(1), list_node_id)),
        );

        ui_node_hierarchy.link_node(
            Node::new(
                contrast_renderer::ui::label::text_label,
                hash_map! {
                    "text_interaction" => Value::TextInteraction(contrast_renderer::ui::TextInteraction::Editing),
                    "text_content" => Value::TextString("Hello World".to_string()),
                },
            ),
            Some((NodeOrObservableIdentifier::Indexed(2), list_node_id)),
        );

        Self {
            depth_stencil_texture_view: None,
            msaa_color_texture_view: None,
            ui_renderer,
            ui_node_hierarchy,
            ui_event_translator,
            viewport_size: wgpu::Extent3d::default(),
        }
    }

    fn resize(&mut self, device: &wgpu::Device, _queue: &mut wgpu::Queue, surface_configuration: &wgpu::SurfaceConfiguration) {
        self.ui_event_translator.viewport_size = ppga2d::Point {
            g0: [0.0, surface_configuration.width as f32, surface_configuration.height as f32].into(),
        };
        self.viewport_size = wgpu::Extent3d {
            width: surface_configuration.width,
            height: surface_configuration.height,
            depth_or_array_layers: 1,
        };
        let depth_stencil_texture_descriptor = wgpu::TextureDescriptor {
            size: self.viewport_size,
            mip_level_count: 1,
            sample_count: MSAA_SAMPLE_COUNT,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Depth32FloatStencil8,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            label: None,
        };
        let depth_stencil_texture = device.create_texture(&depth_stencil_texture_descriptor);
        self.depth_stencil_texture_view = Some(depth_stencil_texture.create_view(&wgpu::TextureViewDescriptor {
            dimension: Some(wgpu::TextureViewDimension::D2),
            ..wgpu::TextureViewDescriptor::default()
        }));
        if MSAA_SAMPLE_COUNT > 1 {
            let msaa_color_texture_descriptor = wgpu::TextureDescriptor {
                size: self.viewport_size,
                mip_level_count: 1,
                sample_count: MSAA_SAMPLE_COUNT,
                dimension: wgpu::TextureDimension::D2,
                format: surface_configuration.format,
                usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
                label: None,
            };
            let msaa_color_texture = device.create_texture(&msaa_color_texture_descriptor);
            self.msaa_color_texture_view = Some(msaa_color_texture.create_view(&wgpu::TextureViewDescriptor {
                dimension: Some(wgpu::TextureViewDimension::D2),
                ..wgpu::TextureViewDescriptor::default()
            }));
        }
    }

    fn render(&mut self, device: &wgpu::Device, queue: &mut wgpu::Queue, frame: &wgpu::SurfaceTexture, animation_time: f64) {
        self.ui_renderer.projection_matrix = contrast_renderer::utils::matrix_multiplication(
            &contrast_renderer::utils::perspective_projection(
                std::f32::consts::PI * 0.5,
                self.viewport_size.width as f32 / self.viewport_size.height as f32,
                1.0,
                100000.0,
            ),
            &contrast_renderer::utils::motor3d_to_mat4(
                &(ppga3d::Motor {
                    g0: [1.0, 0.0, 0.0, 0.0].into(),
                    g1: [0.0, 0.0, 0.0, -0.25 * (self.viewport_size.height as f32)].into(),
                }),
            ),
        );
        self.ui_node_hierarchy
            .prepare_rendering(&mut self.ui_renderer, device, queue, animation_time);
        let frame_view = frame.texture.create_view(&wgpu::TextureViewDescriptor::default());
        let color_attachments = &[Some(wgpu::RenderPassColorAttachment {
            view: if MSAA_SAMPLE_COUNT == 1 {
                &frame_view
            } else {
                &self.msaa_color_texture_view.as_ref().unwrap()
            },
            resolve_target: if MSAA_SAMPLE_COUNT == 1 { None } else { Some(&frame_view) },
            ops: wgpu::Operations {
                load: wgpu::LoadOp::Load,
                store: true,
            },
        })];
        let depth_stencil_attachment = wgpu::RenderPassDepthStencilAttachment {
            view: &self.depth_stencil_texture_view.as_ref().unwrap(),
            depth_ops: Some(wgpu::Operations {
                load: wgpu::LoadOp::Load,
                store: false,
            }),
            stencil_ops: Some(wgpu::Operations {
                load: wgpu::LoadOp::Load,
                store: true,
            }),
        };
        let color_attachment = color_attachments[0].as_ref().unwrap();
        let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
        encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: None,
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: color_attachment.view,
                resolve_target: color_attachment.resolve_target,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                    store: true,
                },
            })],
            depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachment {
                view: depth_stencil_attachment.view,
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
        self.ui_renderer
            .encode_commands(&mut encoder, color_attachments, depth_stencil_attachment);
        queue.submit(Some(encoder.finish()));
    }

    fn window_event(&mut self, event: winit::event::WindowEvent) {
        for message in self.ui_event_translator.translate(event) {
            self.ui_node_hierarchy
                .notify_observers(contrast_renderer::ui::NodeOrObservableIdentifier::Named("root"), message);
        }
        self.ui_node_hierarchy.process_messengers();
    }
}

fn main() {
    application_framework::ApplicationManager::run::<Application>("Contrast Renderer - UI Showcase");
}
