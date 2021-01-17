#version 460

layout(location=0) out vec4 color;

layout(set=1, binding=0) uniform SolidCoverUniforms {
    vec4 fill_color;
};

void main() {
    color = fill_color;
}
