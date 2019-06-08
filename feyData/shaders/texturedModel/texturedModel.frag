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

layout (location = 0) in vec2 fragUV;
layout (location = 1) in vec4 fragNormal;
layout (location = 2) in vec4 fragView; // Vertex position

layout (binding = 0, std140) uniform DefaultUniforms {
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

layout (location = 0) out vec4 color;

void main(void) {
  // Color of fragment without lighting
  vec3 tex_color = clamp(texture(texSampler, fragUV).rgb + uniforms.mat.diffuse.rgb, vec3(0.0), vec3(1.0));

  // Total light color
  vec3 light_color = vec3(0.0);  
  for (int i = 0; i < uniforms.numLights; i++) {
	  // Calculate vectors
    vec3 lightVector = normalize((uniforms.lights[i].position - fragView).xyz);
    vec3 halfVector = normalize(normalize(uniforms.viewMatrix * vec4(lightVector, 1.0)).xyz -
      normalize((uniforms.viewMatrix * fragView).xyz));

	  // Calculate diffuse and specular
    vec3 diffuseResult = max(dot(fragNormal.xyz, lightVector), 0.0) *
      tex_color * uniforms.lights[i].color.rgb;
    vec3 specularResult = pow(max(dot(fragNormal.xyz, halfVector), 0.0), uniforms.mat.specularIntensity) *
      uniforms.mat.specular.rgb * uniforms.lights[i].color.rgb;

    light_color += (diffuseResult);
  }

  // Final color
  color = vec4(clamp(uniforms.mat.ambient.rgb + light_color, vec3(0.0), vec3(1.0)), 1.0);
}
