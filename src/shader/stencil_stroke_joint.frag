#version 460
#extension GL_GOOGLE_include_directive : require
#include "include/stencil_sample_rate_shading.glsl"
#include "include/stroke_dashed.glsl"

layout(location=0) sample in vec3 texcoord;
layout(location=1) flat in uint meta;

bool joint(uint joint_type) {
    switch(joint_type & 0x3) {
        default: // Miter
            return true;
        case 1: // Bevel
            return (meta & 0x10000) != 0;
        case 2: // Round
            return length(texcoord.xy) <= 0.5;
    }
}

void main() {
    uint path_index = meta & 0xFFFF;
    bool fill = joint(groups[path_index].meta);
    if(fill && (groups[path_index].meta & 0x4) != 0)
        fill = stroke_dashed(path_index, vec2(length(texcoord.xy), texcoord.z + atan(texcoord.y, texcoord.x) / radians(360)));
    set_coverage(fill);
}
