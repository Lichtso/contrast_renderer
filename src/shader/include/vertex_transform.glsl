layout(location=0) in vec2 a_position;

layout(set=0, binding=0) uniform TransformUniforms {
    mat4 transform;
};

#define transform_position() \
    gl_Position = transform * vec4(a_position, 0.0, 1.0)
