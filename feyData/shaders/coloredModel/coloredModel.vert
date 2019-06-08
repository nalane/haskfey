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

layout (location = 0) in vec4 position;
layout (location = 1) in vec2 vertexUV;
layout (location = 2) in vec4 normal;

layout (binding = 0) uniform DefaultUniforms {
  // Vertex shader uniforms
  mat4 modelMatrix;
  mat4 viewMatrix;
  mat4 mvpMatrix;

  // Fragmaent shader uniforms
  material mat;
  int numLights;
  light lights[MAX_LIGHTS];
} uniforms;

layout (location = 0) out vec2 fragUV;
layout (location = 1) out vec4 fragNormal;
layout (location = 2) out vec4 fragView;

void main(void) {	
	fragUV = vertexUV;
  fragNormal = normalize(uniforms.modelMatrix * normal);	
	fragView = uniforms.modelMatrix * position;
	gl_Position = uniforms.mvpMatrix * position;
}
