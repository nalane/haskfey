#version 330
#extension GL_ARB_separate_shader_objects : enable

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 color;

uniform mat4 mvpMatrix;

layout (location = 0) out vec3 fragColor;

void main(void) {
    fragColor = color;
    gl_Position = mvpMatrix * vec4(position, 1.0);
}