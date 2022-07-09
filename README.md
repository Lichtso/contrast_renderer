[![actions](https://github.com/Lichtso/contrast_renderer/actions/workflows/actions.yml/badge.svg)](https://github.com/Lichtso/contrast_renderer/actions/workflows/actions.yml)
[![Docs](https://docs.rs/contrast_renderer/badge.svg)](https://docs.rs/contrast_renderer/)
[![crates.io](https://img.shields.io/crates/v/contrast_renderer.svg)](https://crates.io/crates/contrast_renderer)

# Contrast Renderer
Contrast is a [web-gpu](https://gpuweb.github.io/gpuweb/) based 2D renderer written in [Rust](https://www.rust-lang.org/).
It renders planar [vector graphics](https://en.wikipedia.org/wiki/Vector_graphics) and can easily be integrated with other forward-rendering code.
Filling uses [implicit curves](https://en.wikipedia.org/wiki/Implicit_curve) and is resolution-independent, while stroking uses [parametric curves](https://en.wikipedia.org/wiki/Parametric_equation) and is approximated via polygon [tesselation](https://en.wikipedia.org/wiki/Tessellation_(computer_graphics)).
This way you can have non-diegetic, diegetic and spacial [GUI](https://en.wikipedia.org/wiki/Graphical_user_interface) elements on any flat plane:
- As classic 2D menu overlay on top of the 3D scene.
- As [HUD](https://en.wikipedia.org/wiki/Head-up_display) fixed relative to the camera movement but occluded by the 3D scene.
- As decals on walls or [holograms](https://en.wikipedia.org/wiki/Holography_in_fiction) hovering in the 3D scene.

To get started, checkout the [showcase example](examples/showcase/main.rs).


## Feature Roadmap
    ✓ Supported and implemented
    ◯ Rudimentary support
    ✗ Planned support, not implemented

- Rendering
    - Anti Aliasing ◯
        - MSAA ✓
    - Custom (User Provided) Shaders ✓
    - Blending ✓
    - Group Opacity ✗
    - Depth Test ✗
    - Instancing ✓
- Filling
    - Paths
        - Polygons ✓
        - Bezier Curves
            - Integral (Normal)
                - Quadratic ✓
                - Cubic ✓
            - Rational (Weighted)
                - Quadratic ✓
                - Cubic ✓
    - Winding Fill Rules ✓
    - Nestable Clipping ✓
- Stroking
    - Paths
        - Polygons ✓
        - Bezier Curves
            - Approximation
                - Uniformly Spaced Parameters ✓
                - Uniform Tangent Angle ✓
                - Uniform Arc Length ✗
            - Integral (Normal)
                - Quadratic ✓
                - Cubic ✓
            - Rational (Weighted)
                - Quadratic ✓
                - Cubic ✓
    - Stroke Width ✓
    - Stroke Offset ◯
    - Closed / Open ✓
    - Line Joins
        - (Clipped) Miter ✓
        - Bevel ✓
        - Round ✓
    - Line Caps (Square, Round, Out, In, Right, Left, Butt) ✓
    - Dashing
        - Phase Translation ✓
        - Repeating Gap Intervals ✓
        - Dynamically Adjustable (for Animations) ✓
- Path Constructors
    - Polygon ✓
    - Bezier Curves
        - Integral (Normal)
            - Quadratic ✓
            - Cubic ✓
        - Rational (Weighted)
            - Quadratic ✓
            - Cubic ✓
    - Arc ✓
    - Rect ✓
    - Rounded Rect ✓
    - Ellipse ✓
    - Circle ✓
    - [Optional] Font (TTF)
        - Glyph ✓
        - Text ◯
- Graphical User Interface ✗


## Dependencies

### Dependencies of the Library
- Graphics API: [wgpu](https://wgpu.rs/)
- Geometric Algebra: [geometric_algebra](https://github.com/Lichtso/geometric_algebra)
- [Optional] Font Loader: [ttf-parser](https://github.com/RazrFalcon/ttf-parser)

### Dependencies of the Examples
- Window API: [winit](https://github.com/rust-windowing/winit)
- Logging: [log](https://github.com/rust-lang/log)
