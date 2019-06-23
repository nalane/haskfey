#version 330
#extension GL_ARB_separate_shader_objects : enable

layout (location = 0) in vec3 fragColor;
layout (location = 1) in vec2 fragUV;

layout (location = 0) out vec4 color;

uniform sampler2D texSampler;

void main(void) {
    color = texture(texSampler, fragUV);
    //color = vec4(fragUV, 0.0, 1.0);
    //color = vec4(1.0);
}