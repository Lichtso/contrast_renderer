#version 460
#extension GL_GOOGLE_include_directive : require
#include "include/stencil_sample_rate_shading.glsl"
#include "include/stroke_dashed.glsl"

layout(location=0) sample in vec2 texcoord;
layout(location=1) flat in uint meta;
layout(location=2) flat in float end_texcoord_y;

void main() {
    uint path_index = meta & 0xFFFF;
    bool fill;
    if((groups[path_index].meta & 0x4) != 0)
        fill = stroke_dashed(path_index, texcoord);
    else if((meta & 0x10000) != 0)
        fill = cap(vec2(texcoord.x, texcoord.y - end_texcoord_y), groups[path_index].caps >> 4);
    else if(texcoord.y < 0)
        fill = cap(vec2(texcoord.x, -texcoord.y), groups[path_index].caps);
    else
        fill = true;
    set_coverage(fill);
}
