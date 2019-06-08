#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(vertices = 16) out;

void main(void) {	
	gl_out[gl_InvocationID].gl_Position = gl_in[gl_InvocationID].gl_Position;

    for (int i = 0; i < 4; i++) {
        gl_TessLevelOuter[i] = 16.0;
    }

    for (int i = 0; i < 2; i++) {
        gl_TessLevelInner[i] = 16.0;
    }
}