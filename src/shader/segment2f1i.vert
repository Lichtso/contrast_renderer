#version 460
#extension GL_GOOGLE_include_directive : require
#include "include/vertex_transform.glsl"

layout(location=1) in vec2 a_texcoords;
layout(location=2) in uint a_meta;
layout(location=0) out vec2 texcoords;
layout(location=1) out flat uint meta;
layout(location=2) flat out float end_texcoord_y;

void main() {
    transform_position();
    texcoords = a_texcoords;
    end_texcoord_y = a_texcoords.y;
    meta = a_meta;
}
