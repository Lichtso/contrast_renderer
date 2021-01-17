#version 460

layout(location=0) in vec3 weights;

void main() {
    if(weights.x * weights.x * weights.x - weights.y * weights.z > 0.0)
        discard;
}
