#version 460
#extension GL_GOOGLE_include_directive : require
#include "include/vertex_transform.glsl"

layout(location=1) in vec4 a_weights;
layout(location=0) out vec4 weights;

void main() {
    transform_position();
    weights = a_weights;
}
