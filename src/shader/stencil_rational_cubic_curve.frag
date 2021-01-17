#version 460

layout(location=0) in vec4 weights;

void main() {
    if(weights.x * weights.x * weights.x - weights.y * weights.z * weights.w > 0.0)
        discard;
}
