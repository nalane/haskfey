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

layout(quads, equal_spacing, ccw) in;
layout (location = 0) out vec3 teNormal;
layout (location = 1) out vec2 fragUV;

void main(void) {
    // Set the UV coordinates
    fragUV = gl_TessCoord.xy;

    // Get the control points
    vec4 controlPoints[4][4];
	controlPoints[0][0] = gl_in[0].gl_Position;
    controlPoints[1][0] = gl_in[1].gl_Position;
    controlPoints[2][0] = gl_in[2].gl_Position;
    controlPoints[3][0] = gl_in[3].gl_Position;
    controlPoints[0][1] = gl_in[4].gl_Position;
    controlPoints[1][1] = gl_in[5].gl_Position;
    controlPoints[2][1] = gl_in[6].gl_Position;
    controlPoints[3][1] = gl_in[7].gl_Position;
    controlPoints[0][2] = gl_in[8].gl_Position;
    controlPoints[1][2] = gl_in[9].gl_Position;
    controlPoints[2][2] = gl_in[10].gl_Position;
    controlPoints[3][2] = gl_in[11].gl_Position;
    controlPoints[0][3] = gl_in[12].gl_Position;
    controlPoints[1][3] = gl_in[13].gl_Position;
    controlPoints[2][3] = gl_in[14].gl_Position;
    controlPoints[3][3] = gl_in[15].gl_Position;
    
    // Get the paramters for the terrain function
    float s = gl_TessCoord.x;
    float t = gl_TessCoord.y;

    float sCoefficients[4];
    sCoefficients[0] = (1.0 - s) * (1.0 - s) * (1.0 - s);
    sCoefficients[1] = 3.0 * s * (1.0 - s) * (1.0 - s);
    sCoefficients[2] = 3.0 * s * s * (1.0 - s);
    sCoefficients[3] = s * s * s;

    float tCoefficients[4];
    tCoefficients[0] = (1.0 - t) * (1.0 - t) * (1.0 - t);
    tCoefficients[1] = 3.0 * t * (1.0 - t) * (1.0 - t);
    tCoefficients[2] = 3.0 * t * t * (1.0 - t);
    tCoefficients[3] = t * t * t;

    // Calculate position
    vec4 position = vec4(0.0);
    for (int i = 0; i < 4; i++) {
        vec4 partialPosition = vec4(0.0);
        for (int j = 0; j < 4; j++) {
            partialPosition += (tCoefficients[j] * controlPoints[i][j]);
        }
        position += (sCoefficients[i] * partialPosition);
    }

    gl_Position = uniforms.mvpMatrix * vec4((position).xyz, 1.0);

    // To get the normal vector, take the derivative with respect to s and t
    float dsCoefficients[4];
    dsCoefficients[0] = -3.0 * (1.0 - s) * (1.0 - s);
    dsCoefficients[1] = 3.0 * (1.0 - s) * (1.0 - 3.0 * s);
    dsCoefficients[2] = 3.0 * s * (2.0 - 3.0 * s);
    dsCoefficients[3] = 3.0 * s * s;

    float dtCoefficients[4];
    dtCoefficients[0] = -3.0 * (1.0 - t) * (1.0 - t);
    dtCoefficients[1] = 3.0 * (1.0 - t) * (1.0 - 3.0 * t);
    dtCoefficients[2] = 3.0 * t * (2.0 - 3.0 * t);
    dtCoefficients[3] = 3.0 * t * t;

    vec4 dsPosition = vec4(0.0);
    for (int i = 0; i < 4; i++) {
        vec4 partialPosition = vec4(0.0);
        for (int j = 0; j < 4; j++) {
            partialPosition += (tCoefficients[j] * controlPoints[i][j]);
        }
        dsPosition += (dsCoefficients[i] * partialPosition);
    }

    vec4 dtPosition = vec4(0.0);
    for (int i = 0; i < 4; i++) {
        vec4 partialPosition = vec4(0.0);
        for (int j = 0; j < 4; j++) {
            partialPosition += (dtCoefficients[j] * controlPoints[i][j]);
        }
        dtPosition += (sCoefficients[i] * partialPosition);
    }

    dsPosition = uniforms.mvpMatrix * dsPosition;
    dtPosition = uniforms.mvpMatrix * dtPosition;

    teNormal = normalize(cross(dsPosition.xyz, dtPosition.xyz));
}