#version 120
attribute vec2 in_position;
attribute vec2 in_texCoord;
attribute vec4 in_color;
varying vec2 texCoord;
varying vec4 color;
uniform mat4 pmv;
uniform vec4 diffuse;
uniform float wave;
void main() {
  texCoord = in_texCoord;
  color = in_color * diffuse;
  vec2 vpos = vec2(sin(wave+in_position.y*0.1+in_position.x*0.05)*3+in_position.x, in_position.y);
  gl_Position = pmv * vec4(vpos, 0.0, 1.0);
}