#extension GL_OES_sample_variables : require
#extension GL_OES_shader_multisample_interpolation : require 
#extension GL_NV_sample_mask_override_coverage : require 

layout(override_coverage) out int gl_SampleMask[(gl_MaxSamples + 31) / 32];

void set_coverage(bool keep) {
    gl_SampleMask[gl_SampleID >> 5] = int(keep) << (gl_SampleID & 31);
}
