unit nxShaders;

interface

uses classes;

procedure MakeVShader2D(text: TStrings);
procedure MakeFShader2D(text: TStrings);
procedure MakeVShader3D(text: TStrings);
procedure MakeFShader3D(text: TStrings; bump: boolean);

implementation

procedure MakeVShader2D(text: TStrings);
begin
  text.Text:='#version 120'+#13+
'attribute vec2 in_position;' +
'attribute vec2 in_texCoord;' +
'attribute vec4 in_color;' +
'varying vec2 texCoord;' +
'varying vec4 color;' +
'uniform mat4 pmv;' +
'uniform vec3 diffuse;' +
'void main() {' +
'  texCoord = in_texCoord;' +
'  color = in_color * vec4(diffuse, 1.0);' +
'  gl_Position = pmv * vec4(in_position, 0.0, 1.0);' +
'}';
end;

procedure MakeFShader2D(text: TStrings);
begin
  text.Text:='#version 120'+#13+
'varying vec2 texCoord;' +
'varying vec4 color;' +
'uniform sampler2D texture;' +
'void main() {' +
'  gl_FragColor = texture2D(texture, texCoord) * color;' +
'}';
end;

procedure MakeVShader3D(text: TStrings);
begin
  text.Text:='#version 120'+#13+
'varying vec3 lightVec;' +
'varying vec3 eyeVec;' +
'varying vec2 texCoord;' +

'void main() {' +
'  gl_Position = ftransform();' +
'  texCoord = gl_MultiTexCoord0.xy;' +

'  vec3 c1 = cross(gl_Normal, vec3(0.0, 0.0, 1.0));' +
'  vec3 c2 = cross(gl_Normal, vec3(0.0, 1.0, 0.0));' +

'  vec3 vTangent=c1;' +
'  if (length(c2)>length(vTangent)) {' +
'    vTangent=c2;' +
'  }' +
'  vTangent = normalize(vTangent);' +

'  vec3 n = normalize(gl_NormalMatrix * gl_Normal);' +
'  vec3 t = normalize(gl_NormalMatrix * vTangent);' +
'  vec3 b = cross(n, t);' +

'  vec3 vVertex = vec3(gl_ModelViewMatrix * gl_Vertex);' +
'  vec3 tmpVec = gl_LightSource[0].position.xyz - vVertex;' +

'  lightVec.x = dot(tmpVec, t);' +
'  lightVec.y = dot(tmpVec, b);' +
'  lightVec.z = dot(tmpVec, n);' +

'  tmpVec = -vVertex;' +
'  eyeVec.x = dot(tmpVec, t);' +
'  eyeVec.y = dot(tmpVec, b);' +
'  eyeVec.z = dot(tmpVec, n);' +
'}';
end;

procedure MakeFShader3D(text: TStrings; bump: boolean);
begin
  text.Text:='#version 120'+#13+
'varying vec3 lightVec;' +
'varying vec3 eyeVec;' +
'varying vec2 texCoord;' +
'uniform sampler2D colorMap;';
  if bump then text.Text:=text.Text +
    'uniform sampler2D normalMap;';
text.Text:=text.Text +
'void main() {' +
'  float distSqr = dot(lightVec, lightVec);' +
'  float att = clamp(1.0 - 0.0005 * sqrt(distSqr), 0.0, 1.0);' +
'  vec3 lVec = lightVec * inversesqrt(distSqr);' +

'  vec3 vVec = normalize(eyeVec);' +
'  vec4 base = texture2D(colorMap, texCoord);';
  if bump then text.Text:=text.Text +
    '  vec3 bump = normalize(texture2D(normalMap, texCoord).xyz * 2.0 - 1.0);'
  else text.Text:=text.Text +
    '  vec3 bump = normalize(vec3(0.5, 0.5, 1.0) * 2.0 - 1.0);';

  text.Text:=text.Text +
'  vec4 vAmbient = gl_LightSource[0].ambient * gl_FrontMaterial.ambient;' +

'  float diffuse = max(dot(lVec, bump), 0.0);' +
'  vec4 vDiffuse = gl_LightSource[0].diffuse * gl_FrontMaterial.diffuse * 	diffuse;' +

'  float specular = pow(clamp(dot(reflect(-lVec, bump), vVec), 0.0, 1.0), gl_FrontMaterial.shininess);' +
'  vec4 vSpecular = gl_LightSource[0].specular * gl_FrontMaterial.specular * specular;' +

'  gl_FragColor = (vAmbient*base + vDiffuse*base + vSpecular) * att;' +
'}';
end;

end.
