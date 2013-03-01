unit TextureGen;

{$mode objfpc}{$H+}

interface

uses dglOpenGL, nxGL, math, nxMath, nxTypes;

procedure AddForceFieldTex;
procedure AddGlowTex;

implementation

procedure AddForceFieldTex;
var data: PointerArrayType; w, h, x, y, n, i: integer;
begin
  w:=128; h:=128;
  data:=allocmem(w*h*3);
  fillchar(data[0], w*h*3, 30);
  for i:=1 to 30 do begin
    y:=random(h div 2)*2;
    for x:=0 to w-1 do begin
      n:=(y*w+x)*3;
      fillchar(data[n], 3, 255-random(180));
    end;
    x:=random(w div 2)*2;
    for y:=0 to h-1 do begin
      n:=(y*w+x)*3;
      fillchar(data[n], 3, 255-random(180));
    end;
  end;
  n:=tex.TextureQuality;
  //tex.TextureQuality:=GL_NEAREST; // Blocky quality
  tex.AddTexture('forcefield', data, w, h, 3, GL_RGB, GL_RGB);
  tex.TextureQuality:=n;
end;

procedure AddGlowTex;
var data: PointerArrayType; w, h, x, y, n: integer;
    d: single;
begin
  w:=64; h:=64; n:=0;
  data:=allocmem(w*h*3);
  for y:=0 to h-1 do
    for x:=0 to w-1 do begin
      d:=hypot(x-31.5, y-31.5);
      d:=1-smoothen(d/32);
      fillchar(data[n], 3, byte(trunc(d*255)));
      inc(n, 3);
    end;
  tex.AddTexture('glow', data, w, h, 3, GL_RGB, GL_RGB);
end;

end.

