#version 120
attribute vec2 in_position;
attribute vec2 in_texCoord;
attribute vec4 in_color;
varying vec2 texCoord;
varying vec4 color;
uniform mat4 pmv;
uniform vec4 diffuse;
void main() {
  texCoord = in_texCoord;
  color = in_color * diffuse;
  gl_Position = pmv * vec4(in_position, 0.0, 1.0);
}