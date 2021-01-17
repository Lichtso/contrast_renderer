#version 460

layout(location=0) in vec2 a_position;
layout(location=1) in vec2 a_weights;
layout(location=0) out vec2 weights;

layout(set=0, binding=0) uniform TransformUniforms {
    mat4 transform;
};

void main() {
    gl_Position = transform * vec4(a_position, 0.0, 1.0);
    weights = a_weights;
}
