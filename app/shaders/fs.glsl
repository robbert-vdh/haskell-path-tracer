#version 330

in vec2 f_uv;

out vec4 colour;

// TODO: Revert this
// uniform Globals {
//   int u_iterations;
// };
// uniform sampler2D u_texture;

void main() {
  // colour = texture(u_texture, f_uv).rgb / u_iterations;

  colour = vec4(1.0, f_uv, 1.0);
}
