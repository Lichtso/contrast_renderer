#version 460
#extension GL_GOOGLE_include_directive : require
#include "include/stencil_sample_rate_shading.glsl"

layout(location=0) sample in vec3 weights;

void main() {
    set_coverage(weights.x * weights.x * weights.x - weights.y * weights.z <= 0.0);
}
