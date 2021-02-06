#version 460

layout(location=0) in vec2 weights;

void main() {
    if(weights.x * weights.x - weights.y > 0.0)
        discard;
}
