#version 330

in vec2 f_uv;

out vec3 colour;

uniform Globals {
  int u_iterations;
};
uniform sampler2D u_texture;

void main() {
  colour = texture(u_texture, f_uv).rgb / u_iterations;
}
