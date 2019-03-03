#version 330

in vec2 f_uv;

out vec3 colour;

// TODO: Revert this
// uniform Globals {
//   int u_iterations;
// };
// uniform sampler2D u_texture;

void main() {
  // colour = texture(u_texture, f_uv).rgb / u_iterations;

  colour = vec3(1.0, 0.1, 0.5);
}
