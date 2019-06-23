#version 330
#extension GL_ARB_separate_shader_objects : enable

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 color;
layout (location = 2) in vec2 uvCoord;

uniform mat4 mvpMatrix;

layout (location = 0) out vec3 fragColor;
layout (location = 1) out vec2 fragUV;

void main(void) {
    fragColor = color;
    fragUV = uvCoord;
    gl_Position = mvpMatrix * vec4(position, 1.0);
}