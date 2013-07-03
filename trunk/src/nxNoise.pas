unit nxNoise;

{ For nxPascal translated by Teemu Valo, from
  http://staffwww.itn.liu.se/~stegu/simplexnoise/SimplexNoise.java

  Originally made by Stefan Gustavson
   ~ You may use it as you see fit, but attribution is appreciated. }

{$I nxInc.inc}

interface

uses nxTypes;

type

  { TSimplexNoise }

  TSimplexNoise = class
  private
    // To remove the need for index wrapping, double the permutation table length
    perm: array[0..511] of byte;
    permMod12: array[0..511] of byte;
    F2, G2, F3, G3, F4, G4: single;
    procedure InitWithSeed(const seed: int64);
  public
    constructor Create; overload;
    constructor Create(const seed: int64); overload;
    function Noise(const x, y: single): single; overload;
    function Noise(const x, y, z: single): single; overload;
    function Noise(const x, y, z, w: single): single; overload;
  end;

  T8Bytes = array[0..7] of byte;

var
  nxNoiseSeedMult: int64 = 85123154182917;

implementation

var
  grad3: array[0..11] of TVector3f = (
(x:1;y:1;z:0), (x:-1;y:1;z:0), (x:1;y:-1;z:0), (x:-1;y:-1;z:0),
(x:1;y:0;z:1), (x:-1;y:0;z:1), (x:1;y:0;z:-1), (x:-1;y:0;z:-1),
(x:0;y:1;z:1), (x:0;y:-1;z:1), (x:0;y:1;z:-1), (x:0;y:-1;z:-1));

  grad4: array[0..31] of TVector4f = (
(x:0;y:1;z:1;w:1), (x:0;y:1;z:1;w:-1), (x:0;y:1;z:-1;w:1), (x:0;y:1;z:-1;w:-1),
(x:0;y:-1;z:1;w:1),(x:0;y:-1;z:1;w:-1),(x:0;y:-1;z:-1;w:1),(x:0;y:-1;z:-1;w:-1),
(x:1;y:0;z:1;w:1), (x:0;y:-1;z:1;w:-1),(x:0;y:-1;z:-1;w:1),(x:0;y:-1;z:-1;w:-1),
(x:-1;y:0;z:1;w:1),(x:-1;y:0;z:1;w:-1),(x:-1;y:0;z:-1;w:1),(x:-1;y:0;z:-1;w:-1),
(x:1;y:1;z:0;w:1), (x:1;y:1;z:0;w:-1), (x:1;y:-1;z:0;w:1), (x:1;y:-1;z:0;w:-1),
(x:-1;y:1;z:0;w:1),(x:-1;y:1;z:0;w:-1),(x:-1;y:-1;z:0;w:1),(x:-1;y:-1;z:0;w:-1),
(x:1;y:1;z:1;w:0), (x:1;y:1;z:-1;w:0), (x:1;y:-1;z:1;w:0), (x:1;y:-1;z:-1;w:0),
(x:-1;y:1;z:1;w:0),(x:-1;y:1;z:-1;w:0),(x:-1;y:-1;z:1;w:0),(x:-1;y:-1;z:-1;w:0));

  p: array[0..255] of byte = (151,160,137,91,90,15,
131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180);

function Dot(const v: PVector3f; const x, y: single): single; overload;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result:=v^.x*x + v^.y*y;
end;

function Dot(const v: PVector3f; const x, y, z: single): single; overload;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result:=v^.x*x + v^.y*y + v^.z*z;
end;

function Dot(const v: PVector4f; const x, y, z, w: single): single; overload;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result:=v^.x*x + v^.y*y + v^.z*z + v^.w*w;
end;

function fastfloor(x: single): integer;
var xi: integer;
begin
  xi:=trunc(x);
  if x<xi then result:=xi-1
  else result:=xi;
end;

{ TSimplexNoise }

procedure TSimplexNoise.InitWithSeed(const seed: int64);
var i: integer;
begin
  F2 := 0.5*(sqrt(3.0)-1.0);
  G2 := (3.0-sqrt(3.0))/6.0;
  F3 := 1.0/3.0;
  G3 := 1.0/6.0;
  F4 := (sqrt(5.0)-1.0)/4.0;
  G4 := (5.0-sqrt(5.0))/20.0;
  // Add custom made seed xor-operation
  for i:=0 to 511 do begin
    perm[i]:=p[i and 255] xor T8Bytes(seed)[i mod 8];
    permMod12[i]:=byte(perm[i] mod 12);
  end;
end;

constructor TSimplexNoise.Create;
begin
  InitWithSeed(0);
end;

constructor TSimplexNoise.Create(const seed: int64);
begin
  // Seed overflowing is expected and ok
  InitWithSeed(seed * nxNoiseSeedMult);
end;

function TSimplexNoise.Noise(const x, y: single): single;
var i1, j1, // Offsets for second (middle) corner of simplex in (i,j) coords
    i, j, ii, jj, gi0, gi1, gi2: integer;
    n0, n1, n2, // Noise contributions from the three corners
    s, t, X0, Y0, xx, yy, x1, y1, x2, y2, t0, t1, t2: single;
begin
  // Skew the input space to determine which simplex cell we're in
  s := (x+y)*F2; // Hairy factor for 2D
  i := fastfloor(x+s);
  j := fastfloor(y+s);
  t := (i+j)*G2;
  X0 := i-t; // Unskew the cell origin back to (x,y) space
  Y0 := j-t;
  xx := x-X0; // The x,y distances from the cell origin
  yy := y-Y0;
  // For the 2D case, the simplex shape is an equilateral triangle.
  // Determine which simplex we are in.
  if (xx>yy) then begin
    i1:=1; j1:=0;
  end else begin // lower triangle, XY order: (0,0)->(1,0)->(1,1)
    i1:=0; j1:=1;   // upper triangle, YX order: (0,0)->(0,1)->(1,1)
  end;
  // A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
  // a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
  // c = (3-sqrt(3))/6
  x1 := xx - i1 + G2; // Offsets for middle corner in (x,y) unskewed coords
  y1 := yy - j1 + G2;
  x2 := xx - 1.0 + 2.0 * G2; // Offsets for last corner in (x,y) unskewed coords
  y2 := yy - 1.0 + 2.0 * G2;
  // Work out the hashed gradient indices of the three simplex corners
  ii := i and 255;
  jj := j and 255;
  gi0 := permMod12[ii+perm[jj]];
  gi1 := permMod12[ii+i1+perm[jj+j1]];
  gi2 := permMod12[ii+1+perm[jj+1]];
  // Calculate the contribution from the three corners
  t0 := 0.5 - xx*xx-yy*yy;
  if (t0<0) then n0 := 0.0
  else begin
    t0 := t0 * t0;
    n0 := t0 * t0 * dot(@grad3[gi0], xx, yy);  // (x,y) of grad3 used for 2D gradient
  end;
  t1 := 0.5 - x1*x1-y1*y1;
  if (t1<0) then n1 := 0.0
  else begin
    t1 := t1 * t1;
    n1 := t1 * t1 * dot(@grad3[gi1], x1, y1);
  end;
  t2 := 0.5 - x2*x2-y2*y2;
  if (t2<0) then n2 := 0.0
  else begin
    t2 := t2 * t2;
    n2 := t2 * t2 * dot(@grad3[gi2], x2, y2);
  end;
  // Add contributions from each corner to get the final noise value.
  // The result is scaled to return values in the interval [-1,1].
  result:=70.0 * (n0 + n1 + n2);
end;

function TSimplexNoise.Noise(const x, y, z: single): single;
var i1, j1, k1, // Offsets for second corner of simplex in (i,j,k) coords
    i2, j2, k2, // Offsets for third corner of simplex in (i,j,k) coords
    i, j, k, ii, jj, kk, gi0, gi1, gi2, gi3: integer;
    n0, n1, n2, n3, // Noise contributions from the four corners
    s, t, X0, Y0, Z0, xx, yy, zz, x1, y1, z1, x2, y2, z2, x3, y3, z3,
    t0, t1, t2, t3: single;
begin
  // Skew the input space to determine which simplex cell we're in
  s := (x+y+z)*F3; // Very nice and simple skew factor for 3D
  i := fastfloor(x+s);
  j := fastfloor(y+s);
  k := fastfloor(z+s);
  t := (i+j+k)*G3;
  X0 := i-t; // Unskew the cell origin back to (x,y,z) space
  Y0 := j-t;
  Z0 := k-t;
  xx := x-X0; // The x,y,z distances from the cell origin
  yy := y-Y0;
  zz := z-Z0;
  // For the 3D case, the simplex shape is a slightly irregular tetrahedron.
  // Determine which simplex we are in.
  if (xx>=yy) then begin
    if (yy>=zz) then begin
      i1:=1; j1:=0; k1:=0; i2:=1; j2:=1; k2:=0;
     // X Y Z order
    end else if (xx>=zz) then begin
      i1:=1; j1:=0; k1:=0; i2:=1; j2:=0; k2:=1; // X Z Y order
    end else begin
      i1:=0; j1:=0; k1:=1; i2:=1; j2:=0; k2:=1;
    end; // Z X Y order
  end else begin // x0<y0
    if (yy<zz) then begin
      i1:=0; j1:=0; k1:=1; i2:=0; j2:=1; k2:=1;  // Z Y X order
    end else if(xx<zz) then begin
      i1:=0; j1:=1; k1:=0; i2:=0; j2:=1; k2:=1;  // Y Z X order
    end else begin
      i1:=0; j1:=1; k1:=0; i2:=1; j2:=1; k2:=0;  // Y X Z order
    end;
  end;
  // A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
  // a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and
  // a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where
  // c = 1/6.
  x1 := xx - i1 + G3; // Offsets for second corner in (x,y,z) coords
  y1 := yy - j1 + G3;
  z1 := zz - k1 + G3;
  x2 := xx - i2 + 2.0*G3; // Offsets for third corner in (x,y,z) coords
  y2 := yy - j2 + 2.0*G3;
  z2 := zz - k2 + 2.0*G3;
  x3 := xx - 1.0 + 3.0*G3; // Offsets for last corner in (x,y,z) coords
  y3 := yy - 1.0 + 3.0*G3;
  z3 := zz - 1.0 + 3.0*G3;
  // Work out the hashed gradient indices of the four simplex corners
  ii := i and 255;
  jj := j and 255;
  kk := k and 255;
  gi0 := permMod12[ii+perm[jj+perm[kk]]];
  gi1 := permMod12[ii+i1+perm[jj+j1+perm[kk+k1]]];
  gi2 := permMod12[ii+i2+perm[jj+j2+perm[kk+k2]]];
  gi3 := permMod12[ii+1+perm[jj+1+perm[kk+1]]];
  // Calculate the contribution from the four corners
  t0 := 0.6 - xx*xx - yy*yy - zz*zz;
  if (t0<0) then n0 := 0.0
  else begin
    t0 := t0 * t0;
    n0 := t0 * t0 * dot(@grad3[gi0], xx, yy, zz);
  end;

  t1 := 0.6 - x1*x1 - y1*y1 - z1*z1;
  if (t1<0) then n1 := 0.0
  else begin
    t1 := t1 * t1;
    n1 := t1 * t1 * dot(@grad3[gi1], x1, y1, z1);
  end;

  t2 := 0.6 - x2*x2 - y2*y2 - z2*z2;
  if (t2<0) then n2 := 0.0
  else begin
    t2 := t2 * t2;
    n2 := t2 * t2 * dot(@grad3[gi2], x2, y2, z2);
  end;

  t3 := 0.6 - x3*x3 - y3*y3 - z3*z3;
  if (t3<0) then n3 := 0.0
  else begin
    t3 := t3 * t3;
    n3 := t3 * t3 * dot(@grad3[gi3], x3, y3, z3);
  end;
  // Add contributions from each corner to get the final noise value.
  // The result is scaled to stay just inside [-1,1]
  result:= 32.0*(n0 + n1 + n2 + n3);
end;

function TSimplexNoise.Noise(const x, y, z, w: single): single;
var i, j, k, l, ii, jj, kk, ll, rankx, ranky, rankz, rankw, gi0, gi1, gi2, gi3, gi4,
    i1, j1, k1, l1, i2, j2, k2, l2, i3, j3, k3, l3: integer;
    n0, n1, n2, n3, n4, X0, Y0, Z0, W0, xx, yy, zz, ww, s, t, t0, t1, t2, t3, t4,
    x1, y1, z1, w1, x2, y2, z2, w2, x3, y3, z3, w3, x4, y4, z4, w4: single;
begin
  // Skew the (x,y,z,w) space to determine which cell of 24 simplices we're in
  s := (x + y + z + w) * F4; // Factor for 4D skewing
  i := fastfloor(x + s);
  j := fastfloor(y + s);
  k := fastfloor(z + s);
  l := fastfloor(w + s);
  t := (i + j + k + l) * G4; // Factor for 4D unskewing
  X0 := i - t; // Unskew the cell origin back to (x,y,z,w) space
  Y0 := j - t;
  Z0 := k - t;
  W0 := l - t;
  xx := x - X0;  // The x,y,z,w distances from the cell origin
  yy := y - Y0;
  zz := z - Z0;
  ww := w - W0;
  // For the 4D case, the simplex is a 4D shape I won't even try to describe.
  // To find out which of the 24 possible simplices we're in, we need to
  // determine the magnitude ordering of x0, y0, z0 and w0.
  // Six pair-wise comparisons are performed between each possible pair
  // of the four coordinates, and the results are used to rank the numbers.
  rankx := 0;
  ranky := 0;
  rankz := 0;
  rankw := 0;
  if(xx > yy) then inc(rankx) else inc(ranky);
  if(xx > zz) then inc(rankx) else inc(rankz);
  if(xx > ww) then inc(rankx) else inc(rankw);
  if(yy > zz) then inc(ranky) else inc(rankz);
  if(yy > ww) then inc(ranky) else inc(rankw);
  if(zz > ww) then inc(rankz) else inc(rankw);
  // simplex[c] is a 4-vector with the numbers 0, 1, 2 and 3 in some order.
  // Many values of c will never occur, since e.g. x>y>z>w makes x<z, y<w and x<w
  // impossible. Only the 24 indices which have non-zero entries make any sense.
  // We use a thresholding to set the coordinates in turn from the largest magnitude.
  // Rank 3 denotes the largest coordinate.
  if rankx >= 3 then i1:=1 else i1:=0;
  if ranky >= 3 then j1:=1 else j1:=0;
  if rankz >= 3 then k1:=1 else k1:=0;
  if rankw >= 3 then l1:=1 else l1:=0;
  // Rank 2 denotes the second largest coordinate.
  if rankx >= 2 then i2:=1 else i2:=0;
  if ranky >= 2 then j2:=1 else j2:=0;
  if rankz >= 2 then k2:=1 else k2:=0;
  if rankw >= 2 then l2:=1 else l2:=0;
  // Rank 1 denotes the second smallest coordinate.
  if rankx >= 1 then i3:=1 else i3:=0;
  if ranky >= 1 then j3:=1 else j3:=0;
  if rankz >= 1 then k3:=1 else k3:=0;
  if rankw >= 1 then l3:=1 else l3:=0;
  // The fifth corner has all coordinate offsets = 1, so no need to compute that.
  x1 := xx - i1 + G4; // Offsets for second corner in (x,y,z,w) coords
  y1 := yy - j1 + G4;
  z1 := zz - k1 + G4;
  w1 := ww - l1 + G4;
  x2 := xx - i2 + 2.0*G4; // Offsets for third corner in (x,y,z,w) coords
  y2 := yy - j2 + 2.0*G4;
  z2 := zz - k2 + 2.0*G4;
  w2 := ww - l2 + 2.0*G4;
  x3 := xx - i3 + 3.0*G4; // Offsets for fourth corner in (x,y,z,w) coords
  y3 := yy - j3 + 3.0*G4;
  z3 := zz - k3 + 3.0*G4;
  w3 := ww - l3 + 3.0*G4;
  x4 := xx - 1.0 + 4.0*G4; // Offsets for last corner in (x,y,z,w) coords
  y4 := yy - 1.0 + 4.0*G4;
  z4 := zz - 1.0 + 4.0*G4;
  w4 := ww - 1.0 + 4.0*G4;
  // Work out the hashed gradient indices of the five simplex corners
  ii := i and 255;
  jj := j and 255;
  kk := k and 255;
  ll := l and 255;
  gi0 := perm[ii+perm[jj+perm[kk+perm[ll]]]] mod 32;
  gi1 := perm[ii+i1+perm[jj+j1+perm[kk+k1+perm[ll+l1]]]] mod 32;
  gi2 := perm[ii+i2+perm[jj+j2+perm[kk+k2+perm[ll+l2]]]] mod 32;
  gi3 := perm[ii+i3+perm[jj+j3+perm[kk+k3+perm[ll+l3]]]] mod 32;
  gi4 := perm[ii+1+perm[jj+1+perm[kk+1+perm[ll+1]]]] mod 32;
  // Calculate the contribution from the five corners
  t0 := 0.6 - xx*xx - yy*yy - zz*zz - ww*ww;
  if (t0<0) then n0 := 0.0
  else begin
    t0 := t0 * t0;
    n0 := t0 * t0 * dot(@grad4[gi0], xx, yy, zz, ww);
  end;

  t1 := 0.6 - x1*x1 - y1*y1 - z1*z1 - w1*w1;
  if (t1<0) then n1 := 0.0
  else begin
    t1 := t1 * t1;
    n1 := t1 * t1 * dot(@grad4[gi1], x1, y1, z1, w1);
  end;

  t2 := 0.6 - x2*x2 - y2*y2 - z2*z2 - w2*w2;
  if (t2<0) then n2 := 0.0
  else begin
    t2 := t2 * t2;
    n2 := t2 * t2 * dot(@grad4[gi2], x2, y2, z2, w2);
  end;

  t3 := 0.6 - x3*x3 - y3*y3 - z3*z3 - w3*w3;
  if (t3<0) then n3 := 0.0
  else begin
    t3 := t3 * t3;
    n3 := t3 * t3 * dot(@grad4[gi3], x3, y3, z3, w3);
  end;

  t4 := 0.6 - x4*x4 - y4*y4 - z4*z4 - w4*w4;
  if (t4<0) then n4 := 0.0
  else begin
    t4 := t4 * t4;
    n4 := t4 * t4 * dot(@grad4[gi4], x4, y4, z4, w4);
  end;
  // Sum up and scale the result to cover the range [-1,1]
  result:= 27.0 * (n0 + n1 + n2 + n3 + n4);
end;

end.
