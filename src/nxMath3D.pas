unit nxMath3D;

{$I nxInc.inc}

interface

{ TODO:
- Recheck MatrixOnPlane
}

uses nxTypes;

  { General 3D Functions }          

  function Angle(const v1,v2: TVector; axis: integer): single; overload;
  function Bezier(const a,b: TVector; const delta: single): TVector; overload;
  function Bezier(const p: array of TVector; const count: word;
    const delta: single): TVector; overload;
  function Catmull(const a,b,c,d: TVector; const delta: single): TVector; overload;
  function ClosestPointOnLine(const l1,l2,p: TVector): TVector;
  function CrossProduct(const a,b: TVector): TVector;{$IFDEF CanInline}inline;{$ENDIF}
  function Dot(const a,b: TVector): single; overload;{$IFDEF CanInline}inline;{$ENDIF}
  function GetAvgNormal(const n1,n2,n3,n4: PVector): TVector;
  function HalfBezier(const a,b,c: TVector; const delta: single): TVector;
  function Hypot3f(const x,y,z: single): single;
  function Hypot3d(const x,y,z: double): double;
  function Interpolate(const v1,v2: TVector; const s: single): TVector; overload;
  function Invert(const v: TVector): TVector; overload;
  function Multiply(const a,b: TVector): TVector; overload;
  procedure Norm(var x,y,z: single; const h: PSingle = nil); overload;
  procedure Norm(var x,y,z: double; const h: PDouble = nil); overload;
  function Norm(const v: TVector; const h: PSingle = nil): TVector; overload;stdcall;{$IFDEF CanInline}inline;{$ENDIF}
  function PointInSphere(const p,sphere: TVector; const radius: single): boolean;
  function RayPlaneIntersect(const rayOrigin, rayDirection,
    planeOrigin, planeNormal: TVector; intersection: PVector): single;
  function RaySphereIntersect(const rayStart: TVector; rayDirection: TVector;
    const sphereCenter: TVector; const sphereRadius: Single;
    const i1, i2: PVector): Integer;
  function RayTriangleIntersect(const rayStart, rayDirection: TVector;
    const p1, p2, p3: TVector; intersect: PVector = nil;
    intersectNormal: PVector = nil): Boolean;{$IFDEF CanInline}inline;{$ENDIF}
  function Reflect(const rayStart, rayDir, wallPoint: TVector): TVector; overload;
  function Reflect(const rayDir, wallNormal: TVector): TVector; overload;
  function Rotate(const v: TVector; const angle: single; axis: TVector): TVector; overload;
  function Scale(const v: TVector; s: single): TVector; overload;
  function Tangent(const a, b, c: TVector): TVector; overload;stdcall;{$IFDEF CanInline}inline;{$ENDIF}
  function VectorAdd(const a, b: TVector): TVector; overload;
  function VectorDist(const a, b: TVector): single; stdcall;{$IFDEF CanInline}inline;{$ENDIF}
  function VectorDiv(const v: TVector; s: single): TVector;
  function VectorMatch(const a, b: TVector; delta: single=0.01): boolean; overload;
  function VectorLen(const v: TVector): single; stdcall;
  function VectorSub(const a, b: TVector): TVector; overload;

  { Matrix Functions }

  function CreateMatrix(const axis: TVector; const angle: Single): TMatrix; overload;stdcall;{$IFDEF CanInline}inline;{$ENDIF}
  function CreateMatrix(const x,y,z: TVector): TMatrix; overload;
  function CreateMatrix2(const x,y,z: TVector): TMatrix;
  function CreateTranslateMatrix(const p: TVector): TMatrix;
  function Determinant(const M: TMatrix): Single;
  function GetAngle(const M: TMatrix; const axis: integer): single;
  function GetRotation(const mat: TMatrix): TMatrix;
  function GetVector(const M: TMatrix; const axis: integer): TVector; stdcall;{$IFDEF CanInline}inline;{$ENDIF}
  function Interpolate(const a, b: TMatrix; s: single): TMatrix; overload; stdcall;{$IFDEF CanInline}inline;{$ENDIF}
  function Invert(const M: TMatrix): TMatrix; overload;
  function LookAt(const eye, target, up: TVector): TMatrix;
  function MatrixOnPlane(const cPos,cDir: TVector; const angle: single = 0): TMatrix;
  function Multiply(const A,B: TMatrix): TMatrix; stdcall;{$IFDEF CanInline}inline;{$ENDIF}overload;
  function MultiplyRotation(const A,B: TMatrix): TMatrix; stdcall;{$IFDEF CanInline}inline;{$ENDIF}
  function Multiply(const V: TVector; const M: TMatrix): TVector; overload; stdcall;{$IFDEF CanInline}inline;{$ENDIF}
  function Rotate(const M: TMatrix; const axis: TVector; const Angle: Single;
    withPos: boolean = true): TMatrix; overload; stdcall;{$IFDEF CanInline}inline;{$ENDIF}
  function Rotate(const M: TMatrix; const axis: integer; const Angle: Single;
    withPos: boolean = true): TMatrix; overload;
  function Scale(const M: TMatrix; const s: Single): TMatrix; overload;
  function Scale(const M: TMatrix; const v: TVector): TMatrix; overload;
  procedure SetVector(var M: TMatrix; const v: TVector; const axis: integer);
  function Slerp(const a, b: TMatrix; s: single): TMatrix; overload; stdcall;{$IFDEF CanInline}inline;{$ENDIF}
  procedure Translate(var M: TMatrix; const v: TVector);

  { Quaternions }

  function Dot(const a, b: TQuaternion): single; overload;
  function Lerp(const a, b: TQuaternion; s: single): TQuaternion; overload;
  function Multiply(const q: TQuaternion; s: single): TQuaternion; overload;
  function Multiply(const a, b: TQuaternion): TQuaternion; overload;
  function NewQuaternion: TQuaternion;
  function Norm(const q: TQuaternion): TQuaternion; overload;
  function Quaternion(x, y, z, w: single): TQuaternion; overload;
  function Quaternion(const M: TMatrix): TQuaternion; overload; stdcall;{$IFDEF CanInline}inline;{$ENDIF}
  function Quaternion(const x, y, z: TVector): TQuaternion; overload;
  function Quaternion(const x, y, z: single): TQuaternion; overload;
  function Quaternion(const axis: TVector; a: single): TQuaternion; overload;
  function QuaternionAdd(const a, b: TQuaternion): TQuaternion;
  function QuaternionSub(const a, b: TQuaternion): TQuaternion;
  function QuaternionToMat(const q: TQuaternion; const position: TVector): TMatrix; stdcall;{$IFDEF CanInline}inline;{$ENDIF}
  function Slerp(a, b: TQuaternion; s: single): TQuaternion; overload; stdcall;

  { Operator overloading }

{ Note: Delphi VER170 onwards may support operator overloading
        through records "class operators". }
{$IFDEF fpc}
  operator :=(const v: TVector2f): TVector;
  operator +(const a, b: TVector): TVector;
  operator -(const a, b: TVector): TVector;
  operator *(const a, b: TVector): TVector;{$IFDEF CanInline}inline;{$ENDIF}
  operator *(const a: TVector; n: single): TVector;
  operator /(const a: TVector; n: single): TVector;
  operator *(const a, b: TMatrix): TMatrix;{$IFDEF CanInline}inline;{$ENDIF}
  operator *(const v: TVector; const m: TMatrix): TVector;{$IFDEF CanInline}inline;{$ENDIF}
  operator +(const m: TMatrix; const v: TVector): TMatrix;
  operator -(const m: TMatrix; const v: TVector): TMatrix;
  operator :=(const q: TQuaternion): TMatrix;{$IFDEF CanInline}inline;{$ENDIF}
  operator :=(const m: TMatrix): TQuaternion;{$IFDEF CanInline}inline;{$ENDIF}
  operator +(const a, b: TQuaternion): TQuaternion;
  operator -(const a, b: TQuaternion): TQuaternion;
  operator *(const a, b: TQuaternion): TQuaternion;
  operator *(const a: TQuaternion; s: single): TQuaternion;
{$ENDIF}

var
  NewMatrix: TMatrix;

implementation

uses math, nxMath;

var
  EPSILON: Single = 1e-40;
  EPSILON2: Single = 1e-30;
  nullVector: TVector;

{ Internal functions }

function getNewMatrix: TMatrix;
var i,j: integer;
begin
  for i:=0 to 3 do
    for j:=0 to 3 do
      if i=j then result[i,j]:=1
      else result[i,j]:=0;
end;

function MatrixDet(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
begin
  Result:=  a1 * (b2 * c3 - b3 * c2)
          - b1 * (a2 * c3 - a3 * c2)
          + c1 * (a2 * b3 - a3 * b2);
end;

function PointProject(const p, origin, direction: TVector): Single;
begin
  Result:= direction.x*(p.x-origin.x)
          +direction.y*(p.y-origin.y)
          +direction.z*(p.z-origin.z);
end;

function VectorCombine(const V1, V2: TVector; const F2: Single): TVector;
begin
  result.x:=V1.x + (F2 * V2.x);
  result.y:=V1.y + (F2 * V2.y);
  result.z:=V1.z + (F2 * V2.z);
end;

function VectorDistance2(const v1, v2: TVector): Single;
begin
  result:=Sqr(v2.x-v1.x)+Sqr(v2.y-v1.y)+Sqr(v2.z-v1.z);
end;

{ 3D Functions }

function Angle(const v1, v2: TVector; axis: integer): single; overload;
begin
  case axis of
    0: result:=nxMath.angle(v1.x,v1.z, v2.x,v2.z); // X (horizontal angle)
    1: result:=nxMath.angle(v1.z,v1.y, v2.z,v2.y); // Y (vertical angle)
    else result:=nxMath.angle(v1.x,v1.y, v2.x,v2.y); // Z (roll)
  end;
end;

// delta = 0..1
function Bezier(const a,b: TVector; const delta: single): TVector;
var d1: single;
begin
  d1:=1-delta;
  Bezier.x:=a.x*d1+b.x*delta;
  Bezier.y:=a.y*d1+b.y*delta;
  Bezier.z:=a.z*d1+b.z*delta;
end;

// Always call with count >= 2
function Bezier(const p: array of TVector; const count: word; const delta: single): TVector;
var v: array of TVector; i: integer;
begin
  if count>2 then begin
    setlength(v,count-1);
    for i:=0 to count-2 do
      v[i]:=Bezier(p[i],p[i+1],delta);
    Bezier:=Bezier(v,count-1,delta);
  end else if count=2 then
    Bezier:=Bezier(p[0],p[1],delta);
end;

function Catmull(const a,b,c,d: TVector; const delta: single): TVector;
begin
  result.x:=nxMath.Catmull(a.x, b.x, c.x, d.x, delta);
  result.y:=nxMath.Catmull(a.y, b.y, c.y, d.y, delta);
  result.z:=nxMath.Catmull(a.z, b.z, c.z, d.z, delta);
end;

function ClosestPointOnLine(const l1, l2, p: TVector): TVector;
var c,v: TVector; d,t: single;
begin
  // Determine the length of the vector from a to b
  c:=VectorSub(p,l1);
  v:=VectorSub(l2,l1);
  v:=Norm(v,@d);
  t:=Dot(v, c);
  // Check to see if ‘t’ is beyond the extents of the line segment
  if t<0 then begin
    result:=l1; exit;
  end else if t>d then begin
    result:=l2; exit;
  end;
  // Return the point between ‘a’ and ‘b’
  result.x:=v.x*t+l1.x;
  result.y:=v.y*t+l1.y;
  result.z:=v.z*t+l1.z;
end;

// Returns tangent that is against plane that is formed by A and B
function CrossProduct(const a,b: TVector): TVector;{$IFDEF CanInline}inline;{$ENDIF}
begin
  Result.x:=a.y*b.z-a.z*b.y;
  Result.y:=a.z*b.x-a.x*b.z;
  Result.z:=a.x*b.y-a.y*b.x;
end;

// Example: returns square of A if A = B, or
//  -2*square A if B is inverse and twice as long
// Using normalized vectors returns 1 if A = B
function Dot(const a, b: TVector): single;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result:=a.x*b.x + a.y*b.y + a.z*b.z;
end;

function GetAvgNormal(const n1, n2, n3, n4: PVector): TVector;
var n: integer;
begin
  result.x:=0; result.y:=0; result.z:=0; n:=0;
  if n1<>nil then begin
    result.x:=result.x+n1^.x;
    result.y:=result.y+n1^.y;
    result.z:=result.z+n1^.z; inc(n);
    if n2<>nil then begin
      result.x:=result.x+n2^.x;
      result.y:=result.y+n2^.y;
      result.z:=result.z+n2^.z; inc(n);
      if n3<>nil then begin // Up to triangle
        result.x:=result.x+n3^.x;
        result.y:=result.y+n3^.y;
        result.z:=result.z+n3^.z; inc(n);
        if n4<>nil then begin // Up to quad
          result.x:=result.x+n4^.x;
          result.y:=result.y+n4^.y;
          result.z:=result.z+n4^.z; inc(n);
        end;
      end;
    end;
  end;
  if n>0 then norm(result)
  else result.y:=1;
end;

function HalfBezier(const a,b,c: TVector; const delta: single): TVector;
var i,j,b1,b2: TVector;
begin
  i.x:=(a.x+b.x)/2; i.y:=(a.y+b.y)/2; i.z:=(a.z+b.z)/2;
  j.x:=(b.x+c.x)/2; j.y:=(b.y+c.y)/2; j.z:=(b.z+c.z)/2;
  b1:=Bezier(i,b,delta); b2:=Bezier(b,j,delta);
  HalfBezier:=Bezier(b1,b2,delta);
end;

function Hypot3f(const x,y,z: single): single;
begin
  result:=sqrt(x*x+y*y+z*z);
end;

function Hypot3d(const x, y, z: double): double;
begin
  result:=sqrt(x*x+y*y+z*z);
end;

function Interpolate(const v1,v2: TVector; const s: single): TVector;
begin
  result.x:=v1.x+s*(v2.x-v1.x);
  result.y:=v1.y+s*(v2.y-v1.y);
  result.z:=v1.z+s*(v2.z-v1.z);
end;

function Invert(const v: TVector): TVector;
begin
  result.x:=-v.x; result.y:=-v.y; result.z:=-v.z;
end;

function Multiply(const a, b: TVector): TVector;
begin
  result.x:=a.x*b.x; result.y:=a.y*b.y; result.z:=a.z*b.z;
end;

procedure Norm(var x,y,z: single; const h: PSingle);
var _h: single;
begin
  _h:=Hypot3f(x,y,z);
  if h<>nil then h^:=_h;
  if _h>0 then begin
    _h:=1/_h; x:=x*_h; y:=y*_h; z:=z*_h;
  end else begin
    x:=1; y:=0; z:=0;
  end;
end;

procedure Norm(var x,y,z: double; const h: PDouble);
var _h: double;
begin
  _h:=Hypot3d(x,y,z);
  if h<>nil then h^:=_h;
  if _h>0 then begin
    _h:=1/_h; x:=x*_h; y:=y*_h; z:=z*_h;
  end else begin
    x:=1; y:=0; z:=0;
  end;
end;

function Norm(const v: TVector; const h: PSingle): TVector;stdcall;{$IFDEF CanInline}inline;{$ENDIF}
var _h: single;
begin
  _h:=Hypot3f(v.x,v.y,v.z);
  if h<>nil then h^:=_h;
  if _h>0 then begin
    _h:=1/_h; result.x:=v.x*_h; result.y:=v.y*_h; result.z:=v.z*_h;
  end else begin
    result.x:=1; result.y:=0; result.z:=0;
  end;
end;

function PointInSphere(const p,sphere: TVector; const radius: single): boolean;
var x,y,z: single;
begin
  x:=sphere.x-p.x; y:=sphere.y-p.y; z:=sphere.z-p.z;
  PointInSphere:=(x*x+y*y+z*z)<(radius*radius);
end;

// Result >= 0 if intersection happens
function RayPlaneIntersect(const rayOrigin, rayDirection,
  planeOrigin, planeNormal: TVector; intersection: PVector): single;
var d,numer,denom: single;
begin
  d:=planeNormal.x*planeOrigin.x+planeNormal.y*planeOrigin.y+planeNormal.z*planeOrigin.z;
  numer:=-planeNormal.x*rayOrigin.x-planeNormal.y*rayOrigin.y-planeNormal.z*rayOrigin.z+d;
  denom:=planeNormal.x*rayDirection.x+planeNormal.y*rayDirection.y+planeNormal.z*rayDirection.z;
  if denom=0 then begin
    result:=-1; exit;
  end;
  result:=numer/denom;
  if intersection<>nil then begin
    intersection^.x:=rayOrigin.x+result*rayDirection.x;
    intersection^.y:=rayOrigin.y+result*rayDirection.y;
    intersection^.z:=rayOrigin.z+result*rayDirection.z;
  end;
end;

{ Calculates the intersections between a sphere and a ray.<p>
  Returns 0 if no intersection is found (i1 and i2 untouched),
  1 if one intersection was found (i1 defined, i2 untouched),
  2 is two intersections were found (i1 and i2 defined).

  Doesn't look ray backwards. (old note, untested) }
function RaySphereIntersect(const rayStart: TVector; rayDirection: TVector;
  const sphereCenter: TVector; const sphereRadius: Single; const i1,
  i2: PVector): Integer;
var lf, s: single; h: TVector;
begin
  result:=0;
  rayDirection:=norm(rayDirection);
  h := VectorSub(sphereCenter, rayStart);
  lf := dot(rayDirection, h);
  s := sqr(sphereRadius)-dot(h, h)+sqr(lf);
  if s < 0.0 then exit;
  s := sqrt(s);
  if lf < s then begin
    if lf+s >= 0 then begin
      s := -s;
      result := 1;
    end;
  end else result := 2;
  if i1<>nil then i1^:=VectorCombine(rayStart, rayDirection, lf-s);
  if i2<>nil then i2^:=VectorCombine(rayStart, rayDirection, lf+s);
end;

// Tests both-sided intersection for line and triangle
function RayTriangleIntersect(const rayStart, rayDirection: TVector; const p1, p2,
  p3: TVector; intersect: PVector; intersectNormal: PVector): Boolean;{$IFDEF CanInline}inline;{$ENDIF}
var pvec, v1, v2, qvec, tvec: TVector;
    t, u, v, det, invDet: Single;
begin
  v1:=VectorSub(p2, p1);
  v2:=VectorSub(p3, p1);
  pvec:=CrossProduct(rayDirection, v2);
  det:=Dot(v1, pvec);
  if (det<EPSILON2) and (det>-EPSILON2) then begin // vector is parallel to triangle's plane
    Result:=False; Exit;
  end;
  invDet:=1/det;
  tvec:=VectorSub(rayStart, p1);
  u:=Dot(tvec, pvec)*invDet;
  if (u<0) or (u>1) then
    Result:=False
  else begin
    qvec:=CrossProduct(tvec, v1);
    v:=Dot(rayDirection, qvec)*invDet;
    Result:=(v>=0) and (u+v<=1);
    if Result then begin
      t:=Dot(v2, qvec)*invDet;
      if t>0 then begin
        if intersect<>nil then
          intersect^:=VectorCombine(rayStart, rayDirection, t);
        if intersectNormal<>nil then
          intersectNormal^:=norm(CrossProduct(v1, v2));
      end else Result:=False;
    end;
  end;
end;

// Returns direction vector after reflection
function Reflect(const rayStart, rayDir, wallPoint: TVector): TVector;
var n: TVector;
begin
  n.x:=rayStart.x-wallPoint.x;
  n.y:=rayStart.y-wallPoint.y;
  n.z:=rayStart.z-wallPoint.z;
  result:=Reflect(rayDir, norm(n));
end;

// Returns direction vector after reflection
function Reflect(const rayDir, wallNormal: TVector): TVector;
var temp: single;
begin
  temp:=2*(rayDir.x*wallNormal.x+rayDir.y*wallNormal.y+rayDir.z*wallNormal.z);
  result.x:=rayDir.x-wallNormal.x*temp;
  result.y:=rayDir.y-wallNormal.y*temp;
  result.z:=rayDir.z-wallNormal.z*temp;
end;

function Rotate(const v: TVector; const angle: single; axis: TVector): TVector;
var costheta, sintheta,px,py,pz: single;
begin
  if angle=0 then begin
    result:=v; exit;
  end;
  Norm(axis);
  px:=v.x; py:=v.y; pz:=v.z;
  costheta := cos(angle);
  sintheta := sin(angle);
  result.x:=px*( axis.x*axis.x * ( 1.0 - costheta ) + costheta)
          + py*( axis.x*axis.y * ( 1.0 - costheta ) - axis.z * sintheta)
          + pz*( axis.x*axis.z * ( 1.0 - costheta ) + axis.y * sintheta);
  result.y:=px*( axis.y*axis.x * ( 1.0 - costheta ) + axis.z * sintheta)
          + py*( axis.y*axis.y * ( 1.0 - costheta ) + costheta)
          + pz*( axis.y*axis.z * ( 1.0 - costheta ) - axis.x * sintheta);
  result.z:=px*( axis.z*axis.x * ( 1.0 - costheta ) - axis.y * sintheta)
          + py*( axis.z*axis.y * ( 1.0 - costheta ) + axis.x * sintheta)
          + pz*( axis.z*axis.z * ( 1.0 - costheta ) + costheta);
end;

function Tangent(const a,b,c: TVector): TVector; stdcall;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result.x:=(b.y-a.y)*(c.z-a.z)-(b.z-a.z)*(c.y-a.y);
  result.y:=-((b.x-a.x)*(c.z-a.z)-(b.z-a.z)*(c.x-a.x));
  result.z:=((b.x-a.x)*(c.y-a.y)-(b.y-a.y)*(c.x-a.x)); // --Z (OpenGL compatible)
  result:=Norm(result);
end;

// Returns V1 + V2
function VectorAdd(const a, b: TVector): TVector;
begin
  result.x:=a.x+b.x; result.y:=a.y+b.y; result.z:=a.z+b.z;
end;

function VectorDist(const a, b: TVector): single; stdcall;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result:=hypot3f(a.x-b.x, a.y-b.y, a.z-b.z);
end;

function VectorDiv(const v: TVector; s: single): TVector;
begin
  result.x:=v.x/s; result.y:=v.y/s; result.z:=v.z/s;
end;

// Returns V * Scale
function Scale(const v: TVector; s: single): TVector;
begin
  result.x:=v.x*s; result.y:=v.y*s; result.z:=v.z*s;
end;

function VectorMatch(const a, b: TVector; delta: single): boolean;
begin
  result:=(abs(a.x-b.x)<delta) and (abs(a.y-b.y)<delta) and (abs(a.z-b.z)<delta);
end;

function VectorLen(const v: TVector): single; stdcall;
begin
  result:=hypot3d(v.x, v.y, v.z);
end;

// Returns V1 - V2
function VectorSub(const a, b: TVector): TVector;
begin
  result.x:=a.x-b.x; result.y:=a.y-b.y; result.z:=a.z-b.z;
end;

{ Matrix functions }

// Before use, make sure that axis vector length is 1
function CreateMatrix(const axis: TVector; const angle: Single): TMatrix;stdcall;{$IFDEF CanInline}inline;{$ENDIF}
var c, s, t, tx, ty, tz, sx, sy, sz: single;
begin
  // Same as: result:=QuaternionToMat(Quaternion(axis, angle));
  c := cos(angle); s := sin(angle); t := 1.0 - c;
  tx := t * axis.X; ty := t * axis.Y; tz := t * axis.Z;
  sx := s * axis.X; sy := s * axis.Y; sz := s * axis.Z;
  Result[0, 0]:=tx * axis.X + c;
  Result[0, 1]:=tx * axis.Y + sz;
  Result[0, 2]:=tx * axis.Z - sy;
  Result[0, 3]:=0;
  Result[1, 0]:=ty * axis.X - sz;
  Result[1, 1]:=ty * axis.Y + c;
  Result[1, 2]:=ty * axis.Z + sx;
  Result[1, 3]:=0;
  Result[2, 0]:=tz * axis.X + sy;
  Result[2, 1]:=tz * axis.Y - sx;
  Result[2, 2]:=tz * axis.Z + c;
  Result[2, 3]:=0;
  Result[3, 0]:=0; Result[3, 1]:=0; Result[3, 2]:=0; Result[3, 3]:=1;
end;

// Create a rotation matrix
function CreateMatrix(const x, y, z: TVector): TMatrix;
begin
  result[0,0]:=x.x; result[0,1]:=x.y; result[0,2]:=x.z; result[0,3]:=0;
  result[1,0]:=y.x; result[1,1]:=y.y; result[1,2]:=y.z; result[1,3]:=0;
  result[2,0]:=z.x; result[2,1]:=z.y; result[2,2]:=z.z; result[2,3]:=0;
  result[3,0]:=0;   result[3,1]:=0;   result[3,2]:=0;   result[3,3]:=1;
end;

// Rotation matrix written with rows and columns switched
function CreateMatrix2(const x, y, z: TVector): TMatrix;
begin
  result[0,0]:=x.x; result[1,0]:=x.y; result[2,0]:=x.z; result[3,0]:=0;
  result[0,1]:=y.x; result[1,1]:=y.y; result[2,1]:=y.z; result[3,1]:=0;
  result[0,2]:=z.x; result[1,2]:=z.y; result[2,2]:=z.z; result[3,2]:=0;
  result[0,3]:=0;   result[1,3]:=0;   result[2,3]:=0;   result[3,3]:=1;
end;

function CreateTranslateMatrix(const p: TVector): TMatrix;
begin
  result:=NewMatrix;
  result[3,0]:=p.x; result[3,1]:=p.y; result[3,2]:=p.z;
end;

function Determinant(const M: TMatrix): Single;
begin
  Result:= M[0, 0]*MatrixDet(M[1, 1], M[2, 1], M[3, 1], M[1, 2], M[2, 2], M[3, 2], M[1, 3], M[2, 3], M[3, 3])
          -M[0, 1]*MatrixDet(M[1, 0], M[2, 0], M[3, 0], M[1, 2], M[2, 2], M[3, 2], M[1, 3], M[2, 3], M[3, 3])
          +M[0, 2]*MatrixDet(M[1, 0], M[2, 0], M[3, 0], M[1, 1], M[2, 1], M[3, 1], M[1, 3], M[2, 3], M[3, 3])
          -M[0, 3]*MatrixDet(M[1, 0], M[2, 0], M[3, 0], M[1, 1], M[2, 1], M[3, 1], M[1, 2], M[2, 2], M[3, 2]);
end;

function GetAngle(const M: TMatrix; const axis: integer): single;
begin
  case axis of
    0: result:=nxMath.angle(0,0,M[0,0],M[0,2]); // X (horizontal rotation)
    1: result:=nxMath.angle(0,0,M[2,2],M[2,1]); // Y (vertical angle)
    else result:=nxMath.angle(0,0,M[0,0],M[0,1]); // Z (roll)
  end;
end;

function GetRotation(const mat: TMatrix): TMatrix;
var i,j: integer;
begin
  for j:=0 to 2 do begin
    for i:=0 to 2 do result[i,j]:=mat[i,j];
    result[j, 3]:=0; result[3, j]:=0;
  end;
  result[3, 3]:=1;
end;

function GetVector(const M: TMatrix; const axis: integer): TVector;stdcall;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result.x:=M[axis,0]; result.y:=M[axis,1]; result.z:=M[axis,2];
end;

function Interpolate(const a, b: TMatrix; s: single): TMatrix;stdcall;{$IFDEF CanInline}inline;{$ENDIF}
var i, j: integer;
begin
  for j:=0 to 3 do
    for i:=0 to 3 do
      result[i, j]:=interpolate(a[i, j], b[i, j], s);
end;

function Invert(const M: TMatrix): TMatrix;
var det, a1, a2, a3, a4, b1, b2, b3, b4,
    c1, c2, c3, c4, d1, d2, d3, d4: Single;
begin
  det:=Determinant(M);
  if Abs(Det)<EPSILON then result:=NewMatrix
  else begin
    a1:= M[0, 0]; b1:= M[0, 1]; c1:= M[0, 2]; d1:= M[0, 3];
    a2:= M[1, 0]; b2:= M[1, 1]; c2:= M[1, 2]; d2:= M[1, 3];
    a3:= M[2, 0]; b3:= M[2, 1]; c3:= M[2, 2]; d3:= M[2, 3];
    a4:= M[3, 0]; b4:= M[3, 1]; c4:= M[3, 2]; d4:= M[3, 3];
    result[0, 0]:= MatrixDet(b2, b3, b4, c2, c3, c4, d2, d3, d4);
    result[1, 0]:=-MatrixDet(a2, a3, a4, c2, c3, c4, d2, d3, d4);
    result[2, 0]:= MatrixDet(a2, a3, a4, b2, b3, b4, d2, d3, d4);
    result[3, 0]:=-MatrixDet(a2, a3, a4, b2, b3, b4, c2, c3, c4);
    result[0, 1]:=-MatrixDet(b1, b3, b4, c1, c3, c4, d1, d3, d4);
    result[1, 1]:= MatrixDet(a1, a3, a4, c1, c3, c4, d1, d3, d4);
    result[2, 1]:=-MatrixDet(a1, a3, a4, b1, b3, b4, d1, d3, d4);
    result[3, 1]:= MatrixDet(a1, a3, a4, b1, b3, b4, c1, c3, c4);
    result[0, 2]:= MatrixDet(b1, b2, b4, c1, c2, c4, d1, d2, d4);
    result[1, 2]:=-MatrixDet(a1, a2, a4, c1, c2, c4, d1, d2, d4);
    result[2, 2]:= MatrixDet(a1, a2, a4, b1, b2, b4, d1, d2, d4);
    result[3, 2]:=-MatrixDet(a1, a2, a4, b1, b2, b4, c1, c2, c4);
    result[0, 3]:=-MatrixDet(b1, b2, b3, c1, c2, c3, d1, d2, d3);
    result[1, 3]:= MatrixDet(a1, a2, a3, c1, c2, c3, d1, d2, d3);
    result[2, 3]:=-MatrixDet(a1, a2, a3, b1, b2, b3, d1, d2, d3);
    result[3, 3]:= MatrixDet(a1, a2, a3, b1, b2, b3, c1, c2, c3);
    result:=Scale(result, 1/det);
  end;
end;

function LookAt(const eye, target, up: TVector): TMatrix;
var x,y,z: TVector;
begin
  z:=norm(VectorSub(eye, target));
  x:=norm(crossproduct(up, z));
  y:=crossproduct(z, x);
  result:=CreateMatrix2(x, y, z);
  translate(result, vector(-dot(eye, x), -dot(eye, y), -dot(eye, z)));
end;

function MatrixOnPlane(const cPos, cDir: TVector; const angle: single): TMatrix;
var x,y,z: TVector;
begin

// This function could be made better by creating vectors first by (0,1,0) and
// then rotating them to cDir. Current setup looks good mainly, but rotation
// with different angles looks like "slowing down" a little on certain directions.

  y:=norm(cDir);
  if y.y<>0 then
    z:=norm(crossproduct( vector(cos(angle),0,sin(angle)), y ))
  else if y.x<>0 then
    z:=norm(crossproduct( vector(0,cos(angle),sin(angle)), y ))
  else
    z:=norm(crossproduct( vector(cos(angle),sin(angle),0), y ));
  x:=norm(crossproduct(y,z));
  result:=CreateMatrix(x,y,z); // This works
  //result:=CreateMatrix(norm(cDir), angle); // Why does this not work?
  Translate(result, cPos);
end;

function Multiply(const A,B: TMatrix): TMatrix; stdcall; {$IFDEF CanInline}inline;{$ENDIF}
var I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result[I, J] := A[I, 0] * B[0, J] + A[I, 1] * B[1, J] +
        A[I, 2] * B[2, J] + A[I, 3] * B[3, J];
end;

function MultiplyRotation(const A, B: TMatrix): TMatrix;stdcall;{$IFDEF CanInline}inline;{$ENDIF}
var I, J: Integer;
begin
  for I := 0 to 2 do begin
    for J := 0 to 2 do
      Result[I, J] := A[I, 0] * B[0, J] + A[I, 1] * B[1, J] +
        A[I, 2] * B[2, J];
    result[3, I]:=A[3, I];
    result[I, 3]:=A[I, 3];
  end;
  result[3, 3]:=A[3, 3];
end;

function Multiply(const V: TVector; const M: TMatrix): TVector;stdcall;{$IFDEF CanInline}inline;{$ENDIF}
begin
  Result.x:=V.x * M[0, 0] + V.y * M[1, 0] + V.z * M[2, 0] + M[3, 0];
  Result.y:=V.x * M[0, 1] + V.y * M[1, 1] + V.z * M[2, 1] + M[3, 1];
  Result.z:=V.x * M[0, 2] + V.y * M[1, 2] + V.z * M[2, 2] + M[3, 2];
end;

// Make sure that axis vector length is 1
function Rotate(const M: TMatrix; const axis: TVector; const Angle: Single;
  withPos: boolean): TMatrix;stdcall;{$IFDEF CanInline}inline;{$ENDIF}
begin
  if not withPos then
    result:=MultiplyRotation(M, CreateMatrix(axis, Angle))
  else
    result:=Multiply(M, CreateMatrix(axis, Angle));
end;

function Rotate(const M: TMatrix; const axis: integer; const Angle: Single;
  withPos: boolean): TMatrix;
begin
  case axis of
    1: result:=rotate(M, vector(1,0,0), angle, withPos);
    2: result:=rotate(M, vector(0,0,1), angle, withPos);
    else result:=rotate(M, vector(0,1,0), angle, withPos);
  end;
end;

function Scale(const M: TMatrix; const s: Single): TMatrix;
var i: Integer;
begin
  for i:=0 to 2 do begin
    result[i, 0]:=M[i, 0] * s;
    result[i, 1]:=M[i, 1] * s;
    result[i, 2]:=M[i, 2] * s;
    result[i, 3]:=M[i, 3];
    result[3, i]:=M[3, i];
  end;
  result[3, 3]:=M[3, 3];
end;

function Scale(const M: TMatrix; const v: TVector): TMatrix; overload;
begin
  result:=NewMatrix;
  result[0,0]:=v.x; result[1,1]:=v.y; result[2,2]:=v.z;
  result:=Multiply(M, result);
end;

procedure SetVector(var M: TMatrix; const v: TVector; const axis: integer);
begin
  M[axis,0]:=v.x; M[axis,1]:=v.y; M[axis,2]:=v.z;
end;

function Slerp(const a, b: TMatrix; s: single): TMatrix; stdcall;{$IFDEF CanInline}inline;{$ENDIF}
var q1, q2: TQuaternion; v1, v2: TVector;
begin
  v1:=GetVector(a, 3); v2:=GetVector(b, 3);
  q1:=Quaternion(a); q2:=Quaternion(b);
  result:=QuaternionToMat(Slerp(q1, q2, s), Interpolate(v1, v2, s));
end;

procedure Translate(var M: TMatrix; const v: TVector);
begin
  M[3,0]:=M[3,0]+v.x; M[3,1]:=M[3,1]+v.y; M[3,2]:=M[3,2]+v.z;
end;

{ Quaternions }

function NewQuaternion: TQuaternion;
begin
  result.x:=0; result.y:=0; result.z:=0; result.w:=1;
end;

function Dot(const a, b: TQuaternion): single;
begin
  result:=(a.X * b.X) + (a.Y * b.Y) + (a.Z * b.Z) + (a.W * b.W);
end;

function Lerp(const a, b: TQuaternion; s: single): TQuaternion;
begin
	result:=Norm(QuaternionAdd(Multiply(a, 1.0-s), Multiply(b, s)));
end;

function Multiply(const q: TQuaternion; s: single): TQuaternion;
begin
  result.x:=q.x*s;
  result.y:=q.y*s;
  result.z:=q.z*s;
  result.w:=q.w*s;
end;

function Multiply(const a, b: TQuaternion): TQuaternion;
begin
	result.X := (b.W * a.X) + (b.X * a.W) + (b.Y * a.Z) - (b.Z * a.Y);
	result.Y := (b.W * a.Y) - (b.X * a.Z) + (b.Y * a.W) + (b.Z * a.X);
	result.Z := (b.W * a.Z) + (b.X * a.Y) - (b.Y * a.X) + (b.Z * a.W);
  result.W := (b.W * a.W) - (b.X * a.X) - (b.Y * a.Y) - (b.Z * a.Z);
end;

function Norm(const q: TQuaternion): TQuaternion;
var n: single;
begin
  n := q.X*q.X + q.Y*q.Y + q.Z*q.Z + q.W*q.W;
  if n=1.0 then result:=q
  else result:=Multiply(q, 1.0/sqrt(n)); // Reciprocal squareroot
end;

function Quaternion(x, y, z, w: single): TQuaternion;
begin
  result.x:=x; result.y:=y; result.z:=z; result.w:=w;
end;

// Make new quaternion from rotation matrix
function Quaternion(const M: TMatrix): TQuaternion;stdcall;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result:=Quaternion(PVector(@M[0, 0])^, PVector(@M[1, 0])^, PVector(@M[2, 0])^);
end;

// Make new quaternion from rotation vectors
function Quaternion(const x, y, z: TVector): TQuaternion;
var diag, scale: single;
begin
  diag := x.x + y.y + z.z + 1;
	if (diag>0.0) then begin
		scale := sqrt(diag) * 2.0;
    result.X := (y.z - z.y) / scale;
		result.Y := (z.x - x.z) / scale;
		result.Z := (x.y - y.x) / scale;
		result.W := 0.25 * scale;
	end else if (x.x>y.y) and (x.x>z.z) then	begin
		scale := sqrt(1.0 + x.x - y.y - z.z) * 2.0;
		result.X := 0.25 * scale;
    result.Y := (y.x + x.y) / scale;
		result.Z := (x.z + z.x) / scale;
		result.W := (y.z - z.y) / scale;
	end	else if (y.y>z.z) then begin
		scale := sqrt(1.0 + y.y - x.x - z.z) * 2.0;
    result.X := (x.y + y.x) / scale;
		result.Y := 0.25 * scale;
    result.Z := (z.y + y.z) / scale;
		result.W := (z.x - x.z) / scale;
	end	else begin
		scale := sqrt(1.0 + 1.0 - x.x - y.y) * 2.0;
    result.X := (z.x + x.z) / scale;
		result.Y := (z.y + y.z) / scale;
		result.Z := 0.25 * scale;
    result.W := (x.y - y.x) / scale;
	end;
	result:=norm(result);
end;

// Make new quaternion from euler angles
function Quaternion(const x, y, z: single): TQuaternion;
var a, sr, cr, sp, cp, sy, cy, cpcy, spcy, cpsy, spsy: single;
begin
	a := x * 0.5;
	sr := sin(a);
	cr := cos(a);
	a := y * 0.5;
	sp := sin(a);
	cp := cos(a);
	a := z * 0.5;
	sy := sin(a);
	cy := cos(a);
	cpcy := cp * cy;
	spcy := sp * cy;
	cpsy := cp * sy;
	spsy := sp * sy;
	result.X := sr * cpcy - cr * spsy;
	result.Y := cr * spcy + sr * cpsy;
	result.Z := cr * cpsy - sr * spcy;
	result.W := cr * cpcy + sr * spsy;
	result:=norm(result);
end;

function Quaternion(const axis: TVector; a: single): TQuaternion;
var fHalfAngle, fSin: single;
begin
  fHalfAngle := 0.5*a;
  fSin := sin(fHalfAngle);
  result.W := cos(fHalfAngle);
  result.X := fSin*axis.X;
  result.Y := fSin*axis.Y;
  result.Z := fSin*axis.Z;
end;

function QuaternionAdd(const a, b: TQuaternion): TQuaternion;
begin
  result.x:=a.x+b.x;
  result.y:=a.y+b.y;
  result.z:=a.z+b.z;
  result.w:=a.w+b.w;
end;

function QuaternionSub(const a, b: TQuaternion): TQuaternion;
begin
  result.x:=a.x-b.x;
  result.y:=a.y-b.y;
  result.z:=a.z-b.z;
  result.w:=a.w-b.w;
end;

function QuaternionToMat(const q: TQuaternion; const position: TVector): TMatrix;stdcall;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result[0, 0] := 1.0 - 2.0*q.Y*q.Y - 2.0*q.Z*q.Z;
  result[0, 1] :=       2.0*q.X*q.Y + 2.0*q.Z*q.W;
  result[0, 2] :=       2.0*q.X*q.Z - 2.0*q.Y*q.W;
  result[0, 3] := 0.0;
  result[1, 0] :=       2.0*q.X*q.Y - 2.0*q.Z*q.W;
  result[1, 1] := 1.0 - 2.0*q.X*q.X - 2.0*q.Z*q.Z;
  result[1, 2] :=       2.0*q.Z*q.Y + 2.0*q.X*q.W;
  result[1, 3] := 0.0;
  result[2, 0] :=       2.0*q.X*q.Z + 2.0*q.Y*q.W;
  result[2, 1] :=       2.0*q.Z*q.Y - 2.0*q.X*q.W;
  result[2, 2] := 1.0 - 2.0*q.X*q.X - 2.0*q.Y*q.Y;
  result[2, 3] := 0.0;
  result[3, 0] := position.x;
  result[3, 1] := position.y;
  result[3, 2] := position.z;
  result[3, 3] := 1.0;
end;

function Slerp(a, b: TQuaternion; s: single): TQuaternion; stdcall;
var angle, theta: single; invsintheta, scale, invscale: single;
    //q: TQuaternion; theta2: single;
begin
  angle := dot(a, b);
  if angle < 0.0 then begin
    a:=Multiply(a, -1.0);	angle:=-angle;
  end;
  if angle <= 0.9995 then begin
    // Style 1
    theta := arccos(angle);
    invsintheta := 1/sin(theta);
    scale := sin(theta * (1.0-s)) * invsintheta;
    invscale := sin(theta * s) * invsintheta;
    result := QuaternionAdd(Multiply(a, scale), Multiply(b, invscale));

    // Style 2
    {theta := arccos(angle);
    theta2 := theta*s;
    q:=norm(QuaternionSub(b, Multiply(a, angle)));
    result := QuaternionAdd(Multiply(a, cos(theta2)), Multiply(q, sin(theta2)));}
  end else // linear interpolation
    result:=lerp(a, b, s);
end;

{ Operator overloading }

{$IFDEF fpc}
operator:=(const v: TVector2f): TVector;
begin
  result.x:=v.x; result.y:=v.y; result.z:=0;
end;

operator+(const a, b: TVector): TVector;
begin
  result.x:=a.x+b.x; result.y:=a.y+b.y; result.z:=a.z+b.z;
end;

operator-(const a, b: TVector): TVector;
begin
  result.x:=a.x-b.x; result.y:=a.y-b.y; result.z:=a.z-b.z;
end;

operator*(const a, b: TVector): TVector;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result:=CrossProduct(a, b);
end;

operator*(const a: TVector; n: single): TVector;
begin
  result.x:=a.x*n; result.y:=a.y*n; result.z:=a.z*n;
end;

operator/(const a: TVector; n: single): TVector;
begin
  result.x:=a.x/n; result.y:=a.y/n; result.z:=a.z/n;
end;

operator*(const a, b: TMatrix): TMatrix;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result:=multiply(a, b);
end;

operator*(const v: TVector; const m: TMatrix): TVector;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result:=Multiply(v, m);
end;

operator+(const m: TMatrix; const v: TVector): TMatrix;
begin
  result:=m;
  result[3,0]:=M[3,0]+v.x; result[3,1]:=M[3,1]+v.y; result[3,2]:=M[3,2]+v.z;
end;

operator-(const m: TMatrix; const v: TVector): TMatrix;
begin
  result:=m;
  result[3,0]:=M[3,0]-v.x; result[3,1]:=M[3,1]-v.y; result[3,2]:=M[3,2]-v.z;
end;

operator:=(const q: TQuaternion): TMatrix;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result:=QuaternionToMat(q, nullVector);
end;

operator:=(const m: TMatrix): TQuaternion;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result:=Quaternion(m);
end;

operator+(const a, b: TQuaternion): TQuaternion;
begin
  result:=QuaternionAdd(a, b);
end;

operator-(const a, b: TQuaternion): TQuaternion;
begin
  result:=QuaternionSub(a, b);
end;

operator*(const a, b: TQuaternion): TQuaternion;
begin
  result:=Multiply(a, b);
end;

operator*(const a: TQuaternion; s: single): TQuaternion;
begin
  result:=Multiply(a, s);
end;
{$ENDIF}

initialization

  NewMatrix:=getNewMatrix;
  nullVector:=vector(0, 0, 0);

end.
