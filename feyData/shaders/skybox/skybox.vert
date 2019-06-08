#version 450
#extension GL_ARB_separate_shader_objects : enable

layout (location = 0) in vec3 position;

layout (binding = 0) uniform SkyboxUniforms {
    mat4 vpMatrix;
} uniforms;

layout (location = 0) out vec3 TexCoords;

void main() {
    gl_Position = uniforms.vpMatrix * vec4(position, 1.0);  
    TexCoords = position;
}  