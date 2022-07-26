struct DynamicStrokeDescriptor {
    gap_start: array<f32, 4u>,
    gap_end: array<f32, 4u>,
    caps: u32,
    count_dashed_join: u32,
    phase: f32,
};
@group(0) @binding(0)
var<storage> u_stroke: array<DynamicStrokeDescriptor>;



struct Instance {
    @location(0) transform_row_0: vec4<f32>,
    @location(1) transform_row_1: vec4<f32>,
    @location(2) transform_row_2: vec4<f32>,
    @location(3) transform_row_3: vec4<f32>,
};

fn instance_transform(instance: Instance) -> mat4x4<f32> {
    return mat4x4<f32>(
        instance.transform_row_0,
        instance.transform_row_1,
        instance.transform_row_2,
        instance.transform_row_3,
    );
}

struct Fragment0 {
    @builtin(position) gl_Position: vec4<f32>,
};

struct Fragment2f {
    @builtin(position) gl_Position: vec4<f32>,
    @location(0) @interpolate(perspective, sample) weights: vec2<f32>,
};

struct Fragment2f1u {
    @builtin(position) gl_Position: vec4<f32>,
    @location(0) @interpolate(perspective, sample) texcoord: vec2<f32>,
    @location(1) @interpolate(flat) bevel_path_index: u32,
    @location(2) @interpolate(flat) end_texcoord_y: f32,
};

struct Fragment3f {
    @builtin(position) gl_Position: vec4<f32>,
    @location(0) @interpolate(perspective, sample) weights: vec3<f32>,
};

struct Fragment3f1u {
    @builtin(position) gl_Position: vec4<f32>,
    @location(0) @interpolate(perspective, sample) texcoord: vec3<f32>,
    @location(1) @interpolate(flat) bevel_path_index: u32,
};

struct Fragment4f {
    @builtin(position) gl_Position: vec4<f32>,
    @location(0) @interpolate(perspective, sample) weights: vec4<f32>,
};

struct FragmentColor {
    @builtin(position) gl_Position: vec4<f32>,
    @location(0) @interpolate(flat) color: vec4<f32>,
};

@vertex
fn vertex0(
    instance: Instance,
    @location(4) position: vec2<f32>,
) -> Fragment0 {
    var stage_out: Fragment0;
    stage_out.gl_Position = instance_transform(instance) * vec4<f32>(position, 0.0, 1.0);
    return stage_out;
}

@vertex
fn vertex2f(
    instance: Instance,
    @location(4) position: vec2<f32>,
    @location(5) weights: vec2<f32>,
) -> Fragment2f {
    var stage_out: Fragment2f;
    stage_out.gl_Position = instance_transform(instance) * vec4<f32>(position, 0.0, 1.0);
    stage_out.weights = weights;
    return stage_out;
}

@vertex
fn vertex2f1u(
    instance: Instance,
    @location(4) position: vec2<f32>,
    @location(5) texcoord: vec2<f32>,
    @location(6) bevel_path_index: u32,
) -> Fragment2f1u {
    var stage_out: Fragment2f1u;
    stage_out.gl_Position = instance_transform(instance) * vec4<f32>(position, 0.0, 1.0);
    stage_out.texcoord = texcoord;
    stage_out.bevel_path_index = bevel_path_index;
    stage_out.end_texcoord_y = texcoord.y;
    return stage_out;
}

@vertex
fn vertex3f(
    instance: Instance,
    @location(4) position: vec2<f32>,
    @location(5) weights: vec3<f32>,
) -> Fragment3f {
    var stage_out: Fragment3f;
    stage_out.gl_Position = instance_transform(instance) * vec4<f32>(position, 0.0, 1.0);
    stage_out.weights = weights;
    return stage_out;
}

@vertex
fn vertex3f1u(
    instance: Instance,
    @location(4) position: vec2<f32>,
    @location(5) texcoord: vec3<f32>,
    @location(6) bevel_path_index: u32,
) -> Fragment3f1u {
    var stage_out: Fragment3f1u;
    stage_out.gl_Position = instance_transform(instance) * vec4<f32>(position, 0.0, 1.0);
    stage_out.texcoord = texcoord;
    stage_out.bevel_path_index = bevel_path_index;
    return stage_out;
}

@vertex
fn vertex4f(
    instance: Instance,
    @location(4) position: vec2<f32>,
    @location(5) weights: vec4<f32>,
) -> Fragment4f {
    var stage_out: Fragment4f;
    stage_out.gl_Position = instance_transform(instance) * vec4<f32>(position, 0.0, 1.0);
    stage_out.weights = weights;
    return stage_out;
}

@vertex
fn vertex_color(
    instance: Instance,
    @location(4) position: vec2<f32>,
    @location(5) color: vec4<f32>,
) -> FragmentColor {
    var stage_out: FragmentColor;
    stage_out.gl_Position = instance_transform(instance) * vec4<f32>(position, 0.0, 1.0);
    stage_out.color = color;
    return stage_out;
}



struct Coverage {
    @builtin(sample_mask) mask: u32,
};

fn coverage(gl_SampleID: u32, keep: bool) -> Coverage {
    var stage_out: Coverage;
    stage_out.mask = select(0u, 1u << (gl_SampleID & 31u), keep);
    return stage_out;
}

fn cap(texcoord: vec2<f32>, cap_type: u32) -> bool {
    switch(i32(cap_type & 15u)) {
        case 0: { // Square
            return texcoord.y > 0.5;
        }
        case 1: { // Round
            return dot(texcoord, texcoord) < 0.25;
        }
        case 2: { // Out
            return 0.5 - texcoord.y > abs(texcoord.x);
        }
        case 3: { // In
            return texcoord.y < abs(texcoord.x);
        }
        case 4: { // Right
            return 0.5 - texcoord.y > texcoord.x;
        }
        case 5: { // Left
            return texcoord.y - 0.5 < texcoord.x;
        }
        default: { // Butt
            return texcoord.y < 0.0;
        }
    }
}

fn joint(radius: f32, bevel: bool, count_dashed_join: u32) -> bool {
    switch(i32(count_dashed_join)) {
        default: { // Miter
            return true;
        }
        case 1: { // Bevel
            return bevel;
        }
        case 2: { // Round
            return radius <= 0.5;
        }
    }
}

fn stroke_dashed(path_index: u32, texcoord: vec2<f32>) -> bool {
    let last_interval_index: u32 = u_stroke[path_index].count_dashed_join >> 3u;
    let pattern_length: f32 = u_stroke[path_index].gap_end[last_interval_index];
    var interval_index: u32 = 0u;
    var gap_start: f32;
    var gap_end: f32;
    var position_in_pattern = (texcoord.y - u_stroke[path_index].phase) % pattern_length;
    if(position_in_pattern < 0.0) {
        position_in_pattern = position_in_pattern + pattern_length;
    }
    loop {
        gap_end = u_stroke[path_index].gap_end[interval_index] - position_in_pattern;
        if(gap_end >= 0.0 || interval_index >= last_interval_index) {
            break;
        }
        interval_index = interval_index + 1u;
    }
    gap_start = position_in_pattern - u_stroke[path_index].gap_start[interval_index];
    if(gap_start > 0.0) {
        let caps = u_stroke[path_index].caps >> (interval_index * 8u);
        let start_cap = cap(vec2<f32>(texcoord.x, gap_start), caps >> 4u);
        let end_cap = cap(vec2<f32>(texcoord.x, gap_end), caps);
        return start_cap || end_cap;
    } else {
        return true;
    }
}

@fragment
fn stencil_solid() {}

@fragment
fn stencil_integral_quadratic_curve(
    stage_in: Fragment2f,
    @builtin(sample_index) gl_SampleID: u32,
) -> Coverage {
    return coverage(gl_SampleID, stage_in.weights.x * stage_in.weights.x - stage_in.weights.y <= 0.0);
}

@fragment
fn stencil_integral_cubic_curve(
    stage_in: Fragment3f,
    @builtin(sample_index) gl_SampleID: u32,
) -> Coverage {
    return coverage(gl_SampleID, stage_in.weights.x * stage_in.weights.x * stage_in.weights.x - stage_in.weights.y * stage_in.weights.z <= 0.0);
}

@fragment
fn stencil_rational_quadratic_curve(
    stage_in: Fragment3f,
    @builtin(sample_index) gl_SampleID: u32,
) -> Coverage {
    return coverage(gl_SampleID, stage_in.weights.x * stage_in.weights.x - stage_in.weights.y * stage_in.weights.z <= 0.0);
}

@fragment
fn stencil_rational_cubic_curve(
    stage_in: Fragment4f,
    @builtin(sample_index) gl_SampleID: u32,
) -> Coverage {
    return coverage(gl_SampleID, stage_in.weights.x * stage_in.weights.x * stage_in.weights.x - stage_in.weights.y * stage_in.weights.z * stage_in.weights.w <= 0.0);
}

@fragment
fn stencil_stroke_line(
    stage_in: Fragment2f1u,
    @builtin(sample_index) gl_SampleID: u32,
) -> Coverage {
    let path_index = stage_in.bevel_path_index & 65535u;
    var fill: bool;
    if((u_stroke[path_index].count_dashed_join & 4u) != 0u) {
        fill = stroke_dashed(path_index, stage_in.texcoord);
    } else if((stage_in.bevel_path_index & 65536u) != 0u) {
        fill = cap(vec2<f32>(stage_in.texcoord.x, stage_in.texcoord.y - stage_in.end_texcoord_y), u_stroke[path_index].caps >> 4u);
    } else if(stage_in.texcoord.y < 0.0) {
        fill = cap(vec2<f32>(stage_in.texcoord.x, -stage_in.texcoord.y), u_stroke[path_index].caps);
    } else {
        fill = true;
    }
    return coverage(gl_SampleID, fill);
}

@fragment
fn stencil_stroke_joint(
    stage_in: Fragment3f1u,
    @builtin(sample_index) gl_SampleID: u32,
) -> Coverage {
    let radius = length(stage_in.texcoord.xy);
    let path_index = stage_in.bevel_path_index & 65535u;
    var fill: bool = joint(radius, (stage_in.bevel_path_index & 65536u) != 0u, u_stroke[path_index].count_dashed_join & 3u);
    let TAU: f32 = acos(-1.0) * 2.0;
    if(fill && (u_stroke[path_index].count_dashed_join & 4u) != 0u) {
        fill = stroke_dashed(path_index, vec2<f32>(radius, stage_in.texcoord.z + atan2(stage_in.texcoord.y, stage_in.texcoord.x) / TAU));
    }
    return coverage(gl_SampleID, fill);
}



@fragment
fn color_cover(
    stage_in: FragmentColor,
) -> @location(0) vec4<f32> {
    return vec4<f32>(stage_in.color.rgb * stage_in.color.a, stage_in.color.a);
}

@fragment
fn scale_alpha_context_cover(
    stage_in: FragmentColor,
) -> @location(0) vec4<f32> {
    return vec4<f32>(0.0, 0.0, 0.0, 1.0 - stage_in.color.a);
}

@group(0) @binding(0)
var frame: texture_2d<f32>;
@group(0) @binding(0)
var multisampled_frame: texture_multisampled_2d<f32>;

@fragment
fn save_alpha_context_cover(
    stage_in: Fragment0,
) -> @location(0) f32 {
    let alpha: f32 = textureLoad(frame, vec2<i32>(stage_in.gl_Position.xy), 0).a;
    return alpha;
}

@fragment
fn multisampled_save_alpha_context_cover(
    stage_in: Fragment0,
    @builtin(sample_index) gl_SampleID: u32,
) -> @location(0) f32 {
    let alpha: f32 = textureLoad(multisampled_frame, vec2<i32>(stage_in.gl_Position.xy), i32(gl_SampleID)).a;
    return alpha;
}

@fragment
fn restore_alpha_context_cover(
    stage_in: FragmentColor,
) -> @location(0) vec4<f32> {
    let alpha: f32 = textureLoad(frame, vec2<i32>(stage_in.gl_Position.xy), 0).x;
    return vec4<f32>(0.0, 0.0, 0.0, (1.0 - alpha) * (1.0 - stage_in.color.a));
}

@fragment
fn multisampled_restore_alpha_context_cover(
    stage_in: FragmentColor,
    @builtin(sample_index) gl_SampleID: u32,
) -> @location(0) vec4<f32> {
    let alpha: f32 = textureLoad(multisampled_frame, vec2<i32>(stage_in.gl_Position.xy), i32(gl_SampleID)).x;
    return vec4<f32>(0.0, 0.0, 0.0, (1.0 - alpha) * (1.0 - stage_in.color.a));
}
