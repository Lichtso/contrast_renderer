#version 460
#extension GL_GOOGLE_include_directive : require
#include "include/vertex_transform.glsl"

layout(location=1) in vec3 a_texcoords;
layout(location=2) in uint a_meta;
layout(location=0) out vec3 texcoords;
layout(location=1) out flat uint meta;

void main() {
    transform_position();
    texcoords = a_texcoords;
    meta = a_meta | (uint((gl_VertexIndex % 5) == 0) << 16);
}
