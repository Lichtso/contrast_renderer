[[block]]
struct Transform {
    transform: mat4x4<f32>;
};
[[group(0), binding(0)]]
var u_transform: Transform;

let MAX_DASH_INTERVALS: u32 = 4u32;
let MAX_OPTION_GROUPS: u32 = 241u32;

struct DynamicStrokeDescriptor {
    gap_start: array<f32, MAX_DASH_INTERVALS>;
    gap_end: array<f32, MAX_DASH_INTERVALS>;
    caps: u32;
    meta: u32;
    phase: f32;
};
[[block]]
struct DynamicStrokeDescriptors {
    groups: array<DynamicStrokeDescriptor, MAX_OPTION_GROUPS>;
};
[[group(1), binding(0)]]
var u_stroke: DynamicStrokeDescriptors;

[[block]]
struct SolidFill {
    color: vec4<f32>;
};
[[group(1), binding(0)]]
var u_fill_solid: SolidFill;



struct Fragment0 {
    [[builtin(position)]] gl_Position: vec4<f32>;
};

struct Fragment2f {
    [[builtin(position)]] gl_Position: vec4<f32>;
    [[location(0), interpolate(perspective, sample)]] weights: vec2<f32>;
};

struct Fragment2f1u {
    [[builtin(position)]] gl_Position: vec4<f32>;
    [[location(0), interpolate(perspective, sample)]] texcoord: vec2<f32>;
    [[location(1), interpolate(flat)]] meta: u32;
    [[location(2), interpolate(flat)]] end_texcoord_y: f32;
};

struct Fragment3f {
    [[builtin(position)]] gl_Position: vec4<f32>;
    [[location(0), interpolate(perspective, sample)]] weights: vec3<f32>;
};

struct Fragment3f1u {
    [[builtin(position)]] gl_Position: vec4<f32>;
    [[location(0), interpolate(perspective, sample)]] texcoord: vec3<f32>;
    [[location(1), interpolate(flat)]] meta: u32;
};

struct Fragment4f {
    [[builtin(position)]] gl_Position: vec4<f32>;
    [[location(0), interpolate(perspective, sample)]] weights: vec4<f32>;
};

[[stage(vertex)]]
fn vertex0(
    [[location(0)]] position: vec2<f32>,
) -> Fragment0 {
    var out: Fragment0;
    out.gl_Position = u_transform.transform * vec4<f32>(position, 0.0, 1.0);
    return out;
}

[[stage(vertex)]]
fn vertex2f(
    [[location(0)]] position: vec2<f32>,
    [[location(1)]] weights: vec2<f32>,
) -> Fragment2f {
    var out: Fragment2f;
    out.gl_Position = u_transform.transform * vec4<f32>(position, 0.0, 1.0);
    out.weights = weights;
    return out;
}

[[stage(vertex)]]
fn vertex2f1u(
    [[location(0)]] position: vec2<f32>,
    [[location(1)]] texcoord: vec2<f32>,
    [[location(2)]] meta: u32,
) -> Fragment2f1u {
    var out: Fragment2f1u;
    out.gl_Position = u_transform.transform * vec4<f32>(position, 0.0, 1.0);
    out.texcoord = texcoord;
    out.meta = meta;
    out.end_texcoord_y = texcoord.y;
    return out;
}

[[stage(vertex)]]
fn vertex3f(
    [[location(0)]] position: vec2<f32>,
    [[location(1)]] weights: vec3<f32>,
) -> Fragment3f {
    var out: Fragment3f;
    out.gl_Position = u_transform.transform * vec4<f32>(position, 0.0, 1.0);
    out.weights = weights;
    return out;
}

[[stage(vertex)]]
fn vertex3f1u(
    [[location(0)]] position: vec2<f32>,
    [[location(1)]] texcoord: vec3<f32>,
    [[location(2)]] meta: u32,
) -> Fragment3f1u {
    var out: Fragment3f1u;
    out.gl_Position = u_transform.transform * vec4<f32>(position, 0.0, 1.0);
    out.texcoord = texcoord;
    out.meta = meta;
    return out;
}

[[stage(vertex)]]
fn vertex4f(
    [[location(0)]] position: vec2<f32>,
    [[location(1)]] weights: vec4<f32>,
) -> Fragment4f {
    var out: Fragment4f;
    out.gl_Position = u_transform.transform * vec4<f32>(position, 0.0, 1.0);
    out.weights = weights;
    return out;
}



struct Coverage {
    [[builtin(sample_mask)]] mask_out: u32;
};

fn coverage(gl_SampleID: u32, keep: bool) -> Coverage {
    var out: Coverage;
    out.mask_out = select(1u32 << (gl_SampleID & 31u32), 0u32, keep);
    return out;
}

fn cap(texcoord: vec2<f32>, cap_type: u32) -> bool {
    switch(i32(cap_type & 15u32)) {
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

fn joint(radius: f32, meta: u32, joint_type: u32) -> bool {
    switch(i32(joint_type & 3u32)) {
        default: { // Miter
            return true;
        }
        case 1: { // Bevel
            return (meta & 65536u32) != 0u32;
        }
        case 2: { // Round
            return radius <= 0.5;
        }
    }
}

fn stroke_dashed(path_index: u32, texcoord: vec2<f32>) -> bool {
    let last_interval_index: u32 = u_stroke.groups[path_index].meta >> 3u32;
    let pattern_length: f32 = u_stroke.groups[path_index].gap_end[last_interval_index];
    var interval_index: u32 = 0u32;
    var gap_start: f32;
    var gap_end: f32;
    let position_in_pattern = (texcoord.y - u_stroke.groups[path_index].phase) % pattern_length;
    loop {
        gap_end = u_stroke.groups[path_index].gap_end[interval_index] - position_in_pattern;
        if(gap_end < 0.0 && interval_index < last_interval_index) {
            interval_index = interval_index + 1u32;
        } else {
            break;
        }
    }
    gap_start = position_in_pattern - u_stroke.groups[path_index].gap_start[interval_index];
    if(gap_start > 0.0) {
        let caps = u_stroke.groups[path_index].caps >> (interval_index * 8u32);
        let start_cap = cap(vec2<f32>(texcoord.x, gap_start), caps >> 4u32);
        let end_cap = cap(vec2<f32>(texcoord.x, gap_end), caps);
        return start_cap || end_cap;
    } else {
        return true;
    }
}

[[stage(fragment)]]
fn stencil_solid() {}

[[stage(fragment)]]
fn stencil_integral_quadratic_curve(
    in: Fragment2f,
    [[builtin(sample_index)]] gl_SampleID: u32,
) -> Coverage {
    return coverage(gl_SampleID, in.weights.x * in.weights.x - in.weights.y <= 0.0);
}

[[stage(fragment)]]
fn stencil_integral_cubic_curve(
    in: Fragment3f,
    [[builtin(sample_index)]] gl_SampleID: u32,
) -> Coverage {
    return coverage(gl_SampleID, in.weights.x * in.weights.x * in.weights.x - in.weights.y * in.weights.z <= 0.0);
}

[[stage(fragment)]]
fn stencil_rational_quadratic_curve(
    in: Fragment3f,
    [[builtin(sample_index)]] gl_SampleID: u32,
) -> Coverage {
    return coverage(gl_SampleID, in.weights.x * in.weights.x - in.weights.y * in.weights.z <= 0.0);
}

[[stage(fragment)]]
fn stencil_rational_cubic_curve(
    in: Fragment4f,
    [[builtin(sample_index)]] gl_SampleID: u32,
) -> Coverage {
    return coverage(gl_SampleID, in.weights.x * in.weights.x * in.weights.x - in.weights.y * in.weights.z * in.weights.w <= 0.0);
}

[[stage(fragment)]]
fn stencil_stroke_line(
    in: Fragment2f1u,
    [[builtin(sample_index)]] gl_SampleID: u32,
) -> Coverage {
    let path_index = in.meta & 65535u32;
    var fill: bool;
    if((u_stroke.groups[path_index].meta & 4u32) != 0u32) {
        fill = stroke_dashed(path_index, in.texcoord);
    } elseif((in.meta & 65536u32) != 0u32) {
        fill = cap(vec2<f32>(in.texcoord.x, in.texcoord.y - in.end_texcoord_y), u_stroke.groups[path_index].caps >> 4u32);
    } elseif(in.texcoord.y < 0.0) {
        fill = cap(vec2<f32>(in.texcoord.x, -in.texcoord.y), u_stroke.groups[path_index].caps);
    } else {
        fill = true;
    }
    return coverage(gl_SampleID, fill);
}

[[stage(fragment)]]
fn stencil_stroke_joint(
    in: Fragment3f1u,
    [[builtin(sample_index)]] gl_SampleID: u32,
) -> Coverage {
    let radius = length(in.texcoord.xy);
    let path_index = in.meta & 65535u32;
    var fill: bool = joint(radius, in.meta, u_stroke.groups[path_index].meta);
    let TAU: f32 = acos(-1.0) * 2.0;
    if(fill && (u_stroke.groups[path_index].meta & 4u32) != 0u32) {
        fill = stroke_dashed(path_index, vec2<f32>(radius, in.texcoord.z + atan2(in.texcoord.y, in.texcoord.x) / TAU));
    }
    return coverage(gl_SampleID, fill);
}



[[stage(fragment)]]
fn fill_solid() -> [[location(0)]] vec4<f32> {
    return u_fill_solid.color;
}
