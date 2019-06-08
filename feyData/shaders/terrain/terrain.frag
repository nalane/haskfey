#version 450
#extension GL_ARB_separate_shader_objects : enable

#define MAX_LIGHTS 50

// Color specs of a material
struct material {
  vec4 ambient;
  vec4 diffuse;
  vec4 specular;
  float specularIntensity;
};

struct light {
  vec4 position;
  vec3 color;
};

layout (binding = 0, std140) uniform TerrainUniforms {
  // Vertex shader uniforms
  mat4 modelMatrix;
  mat4 viewMatrix;
  mat4 mvpMatrix;

  // Fragmaent shader uniforms
  material mat;
  int numLights;
  light lights[MAX_LIGHTS];
} uniforms;

layout (binding = 1) uniform sampler2D texSampler;

layout (location = 0) in vec3 teNormal;
layout (location = 1) in vec2 fragUV;

layout (location = 0) out vec4 color;

void main(void) {	
	color = texture(texSampler, fragUV);
}