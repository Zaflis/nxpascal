#version 120
varying vec2 texCoord;
varying vec4 color;
uniform sampler2D texture;
void main() {
  gl_FragColor = texture2D(texture, texCoord) * vec4(
    color.r*0.3, color.g*0.3, color.b, color.a);
}