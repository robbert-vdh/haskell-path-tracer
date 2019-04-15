#version 330

in vec2 f_uv;

out vec4 color;

uniform int u_iterations;
uniform sampler2D u_texture;

void main() {
  color = vec4(texture(u_texture, f_uv).rgb / u_iterations, 1.0);
}
