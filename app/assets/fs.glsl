#version 330

in vec2 f_uv;

out vec4 color;

uniform int u_iterations;
uniform sampler2D u_texture;
uniform sampler2D u_text;

void main() {
  color = vec4(texture(u_texture, f_uv).rgb / u_iterations, 1.0);

  // The Y axis is inverted in SDL surfaces
  color += texture(u_text, vec2(f_uv.x, -f_uv.y));
}
