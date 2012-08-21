unit nxMath3D;


{TODO:
CreateMatrix() is untested since last changes
}


interface

uses nxTypes;

  { General 3D Functions }          

  function Angle(const v1,v2: TVector; axis: integer): single; overload;
  function Bezier(const a,b: TVector; const delta: single): TVector; overload;
  function Bezier(const p: array of TVector; const count: word;
    const delta: single): TVector; overload;
  function Catmull(const a,b,c,d: TVector; const delta: single): TVector; overload;
  function ClosestPointOnLine(const l1,l2,p: TVector): TVector;
  function CrossProduct(const a,b: TVector): TVector;
  function Dot(const a,b: TVector): single;
  function GetAvgNormal(const n1,n2,n3,n4: PVector): TVector;
  function HalfBezier(const a,b,c: TVector; const delta: single): TVector;
  function Hypot3f(const x,y,z: single): single;
  function Hypot3d(const x,y,z: double): double;
  function Interpolate(const v1,v2: TVector; const s: single): TVector; overload;
  function Invert(const v: TVector): TVector; overload;
  procedure Norm(var v: TVector; const h: PSingle = nil); overload;
  procedure Norm(var x,y,z: single; const h: PSingle = nil); overload;
  procedure dNorm(var x,y,z: double; const h: PDouble = nil); overload;
  function Norm2(const v: TVector; const h: PSingle = nil): TVector;
  function PointInSphere(const p,sphere: TVector; const radius: single): boolean;
  function RayPlaneIntersect(const rayOrigin, rayDirection,
    planeOrigin, planeNormal: TVector; intersection: PVector): single;
  function RaySphereIntersect(const rayStart, rayDirection: TVector;
    const sphereCenter: TVector; const sphereRadius: Single;
    const i1, i2: PVector): Integer;
  function RayTriangleIntersect(const rayStart, rayDirection: TVector;
    const p1, p2, p3: TVector; intersect: PVector = nil;
    intersectNormal: PVector = nil): Boolean;
  function Reflect(const rayStart, rayDir, wallPoint: TVector): TVector; overload;
  function Reflect(const rayDir, wallNormal: TVector): TVector; overload;
  function Rotate(const v: TVector; const angle: single; axis: TVector): TVector; overload;
  function Scale(const v: TVector; s: single): TVector; overload;
  function Tangent(const a, b, c: TVector): TVector;
  procedure VectorAdd(var a: TVector; const b: TVector); overload;
  function VectorAdd2(const a, b: TVector): TVector;
  function VectorDist(const a, b: TVector): single;
  procedure VectorDiv(var a: TVector; s: single);
  function VectorMatch(const a, b: TVector; delta: single=0.01): boolean; overload;
  function VectorMatch(const a, b: TVector2f; delta: single=0.01): boolean; overload;
  procedure VectorSub(var a: TVector; const b: TVector); overload;
  function VectorSub2(const a, b: TVector): TVector;

  { Matrix Functions }

  function CreateMatrix(const axis: TVector; const angle: Single): TMatrix; overload;
  function CreateMatrix(const x,y,z: TVector): TMatrix; overload;
  function CreateMatrix2(const x,y,z: TVector): TMatrix;
  function CreateTranslateMatrix(const p: TVector): TMatrix;
  function GetAngle(const M: TMatrix; const axis: integer): single;
  function GetRotation(const mat: TMatrix): TMatrix;
  function GetVector(const M: TMatrix; const axis: integer): TVector;
  procedure Invert(var M: TMatrix); overload;
  function Determinant(const M: TMatrix): Single;
  function MatrixOnPlane(const cPos,cDir: TVector; const angle: single = 0): TMatrix;
  function Multiply(const A,B: TMatrix): TMatrix;
  function MultiplyRotation(const A,B: TMatrix): TMatrix;
  function LookAt(const eye, target, up: TVector): TMatrix;
  procedure Rotate(var M: TMatrix; const axis: TVector; const Angle: Single;
    withPos: boolean = true); overload;
  procedure Rotate(var M: TMatrix; const axis: integer; const Angle: Single;
    withPos: boolean = true); overload;
  procedure Scale(var M: TMatrix; const s: Single); overload;
  procedure Scale(var M: TMatrix; const v: TVector); overload;
  procedure SetVector(var M: TMatrix; const v: TVector; const axis: integer);
  procedure Translate(var M: TMatrix; const v: TVector);
  function VectorMultiply(const V: TVector; const M: TMatrix): TVector;

var
  NewMatrix: TMatrix;

implementation

uses nxMath;

var
  EPSILON: Single = 1e-40;
  EPSILON2: Single = 1e-30;

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

{ Public 3D Functions }

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
  c:=VectorSub2(p,l1);
  v:=VectorSub2(l2,l1);
  v:=Norm2(v,@d);
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
function CrossProduct(const a,b: TVector): TVector;
begin
  Result.x:=a.y*b.z-a.z*b.y;
  Result.y:=a.z*b.x-a.x*b.z;
  Result.z:=a.x*b.y-a.y*b.x;
end;

// Example: returns square of A if A = B, or
//  -2*square A if B is inverse and twice as long
// Using normalized vectors returns 1 if A = B
function Dot(const a, b: TVector): single;
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
  end;
  if n2<>nil then begin
    result.x:=result.x+n2^.x;
    result.y:=result.y+n2^.y;
    result.z:=result.z+n2^.z; inc(n);
  end;
  if n3<>nil then begin // Up to triangle
    result.x:=result.x+n3^.x;
    result.y:=result.y+n3^.y;
    result.z:=result.z+n3^.z; inc(n);
  end;
  if n4<>nil then begin // Up to quad
    result.x:=result.x+n4^.x;
    result.y:=result.y+n4^.y;
    result.z:=result.z+n4^.z; inc(n);
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

// Returns inverse of V
function Invert(const v: TVector): TVector;
begin
  result.x:=-v.x; result.y:=-v.y; result.z:=-v.z;
end;

procedure Norm(var v: TVector; const h: PSingle = nil);
var _h: single;
begin
  _h:=Hypot3f(v.x, v.y, v.z);
  if h<>nil then h^:=_h;
  if _h>0 then begin
    _h:=1/_h; v.x:=v.x*_h; v.y:=v.y*_h; v.z:=v.z*_h;
  end else begin
    v.x:=1; v.y:=0; v.z:=0;
  end;
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

procedure dNorm(var x,y,z: double; const h: PDouble);
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

function Norm2(const v: TVector; const h: PSingle): TVector;
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
  Doesn't look ray backwards.}
function RaySphereIntersect(const rayStart, rayDirection: TVector;
  const sphereCenter: TVector; const sphereRadius: Single; const i1,
  i2: PVector): Integer;
var proj, d2: Single;
    id2: Integer;
    projPoint: TVector;
begin
  proj:=PointProject(sphereCenter, rayStart, rayDirection);
  projPoint:=VectorCombine(rayStart, rayDirection, proj);
  d2:=sphereRadius*sphereRadius-VectorDistance2(sphereCenter, projPoint);
  id2:=PInteger(@d2)^;
  if id2>=0 then begin
    if id2=0 then begin
      if PInteger(@proj)^>0 then begin
        i1^:=VectorCombine(rayStart, rayDirection, proj);
        Result:=1; Exit;
      end;
    end else if id2>0 then begin
      d2:=Sqrt(d2);
      if proj>=d2 then begin
        i1^:=VectorCombine(rayStart, rayDirection, proj-d2);
        i2^:=VectorCombine(rayStart, rayDirection, proj+d2);
        Result:=2; Exit;
      end else if proj+d2>=0 then begin
        i1^:=VectorCombine(rayStart, rayDirection, proj+d2);
        Result:=1; Exit;
      end;
    end;
  end;
  Result:=0;
end;

function RayTriangleIntersect(const rayStart, rayDirection: TVector; const p1, p2,
  p3: TVector; intersect: PVector; intersectNormal: PVector): Boolean;
var pvec, v1, v2, qvec, tvec: TVector;
    t, u, v, det, invDet: Single;
begin
  v1:=VectorSub2(p2, p1);
  v2:=VectorSub2(p3, p1);
  pvec:=CrossProduct(rayDirection, v2);
  det:=Dot(v1, pvec);
  if (det<EPSILON2) and (det>-EPSILON2) then begin // vector is parallel to triangle's plane
    Result:=False; Exit;
  end;
  invDet:=1/det;
  tvec:=VectorSub2(rayStart, p1);
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
          intersectNormal^:=norm2(CrossProduct(v1, v2));
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
  result:=Reflect(rayDir, norm2(n));
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

function Tangent(const a,b,c: TVector): TVector;
begin
  Tangent.x:=(b.y-a.y)*(c.z-a.z)-(b.z-a.z)*(c.y-a.y);
  Tangent.y:=-((b.x-a.x)*(c.z-a.z)-(b.z-a.z)*(c.x-a.x));
  Tangent.z:=((b.x-a.x)*(c.y-a.y)-(b.y-a.y)*(c.x-a.x)); // --Z (OpenGL compatible)
  Norm(result);
end;

procedure VectorAdd(var a: TVector; const b: TVector);
begin
  a.x:=a.x+b.x; a.y:=a.y+b.y; a.z:=a.z+b.z;
end;

// Returns V1 + V2
function VectorAdd2(const a, b: TVector): TVector;
begin
  result.x:=a.x+b.x; result.y:=a.y+b.y; result.z:=a.z+b.z;
end;

function VectorDist(const a, b: TVector): single;
begin
  result:=hypot3f(a.x-b.x, a.y-b.y, a.z-b.z);
end;

procedure VectorDiv(var a: TVector; s: single);
begin
  a.x:=a.x/s; a.y:=a.y/s; a.z:=a.z/s;
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

function VectorMatch(const a, b: TVector2f; delta: single): boolean;
begin
  result:=(abs(a.x-b.x)<delta) and (abs(a.y-b.y)<delta);
end;

// Returns V1 - V2
procedure VectorSub(var a: TVector; const b: TVector);
begin
  a.x:=a.x-b.x; a.y:=a.y-b.y; a.z:=a.z-b.z;
end;

function VectorSub2(const a, b: TVector): TVector;
begin
  result.x:=a.x-b.x; result.y:=a.y-b.y; result.z:=a.z-b.z;
end;

{ Public Matrix functions }

// Before use, make sure that axis vector length is 1
function CreateMatrix(const axis: TVector; const angle: Single): TMatrix;
var cosine, sine, one_minus_cosine: Single;
begin

{ ! Is it bugged? More testing may be needed !
  Some testing indicate that rotation matrix may go haywire
  if the axis is not surely normalized before. Otherwise seems ok.
}

  sine:=sin(angle); cosine:=cos(angle);
  one_minus_cosine:=1-cosine;
  Result[0, 0]:=(one_minus_cosine * axis.x * axis.x) + cosine;
  Result[0, 1]:=(one_minus_cosine * axis.x * axis.y) - (axis.z * sine);
  Result[0, 2]:=(one_minus_cosine * axis.z * axis.x) + (axis.y * sine);
  Result[0, 3]:=0;
  Result[1, 0]:=(one_minus_cosine * axis.x * axis.y) + (axis.z * sine);
  Result[1, 1]:=(one_minus_cosine * axis.y * axis.y) + cosine;
  Result[1, 2]:=(one_minus_cosine * axis.y * axis.z) - (axis.x * sine);
  Result[1, 3]:=0;
  Result[2, 0]:=(one_minus_cosine * axis.z * axis.x) - (axis.y * sine);
  Result[2, 1]:=(one_minus_cosine * axis.y * axis.z) + (axis.x * sine);
  Result[2, 2]:=(one_minus_cosine * axis.z * axis.z) + cosine;
  Result[2, 3]:=0;
  Result[3, 0]:=0;
  Result[3, 1]:=0;
  Result[3, 2]:=0;
  Result[3, 3]:=1;
end;

function Determinant(const M: TMatrix): Single;
begin
  Result:= M[0, 0]*MatrixDet(M[1, 1], M[2, 1], M[3, 1], M[1, 2], M[2, 2], M[3, 2], M[1, 3], M[2, 3], M[3, 3])
          -M[0, 1]*MatrixDet(M[1, 0], M[2, 0], M[3, 0], M[1, 2], M[2, 2], M[3, 2], M[1, 3], M[2, 3], M[3, 3])
          +M[0, 2]*MatrixDet(M[1, 0], M[2, 0], M[3, 0], M[1, 1], M[2, 1], M[3, 1], M[1, 3], M[2, 3], M[3, 3])
          -M[0, 3]*MatrixDet(M[1, 0], M[2, 0], M[3, 0], M[1, 1], M[2, 1], M[3, 1], M[1, 2], M[2, 2], M[3, 2]);
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
    result[j, 3]:=0;
    result[3, j]:=0;
  end;
  result[3, 3]:=1;
end;

function GetVector(const M: TMatrix; const axis: integer): TVector;
begin
  result.x:=M[axis,0]; result.y:=M[axis,1]; result.z:=M[axis,2];
end;

procedure Invert(var M: TMatrix);
  procedure AdjointMatrix;
  var a1, a2, a3, a4, b1, b2, b3, b4,
      c1, c2, c3, c4, d1, d2, d3, d4: Single;
  begin
    a1:= M[0, 0]; b1:= M[0, 1]; c1:= M[0, 2]; d1:= M[0, 3];
    a2:= M[1, 0]; b2:= M[1, 1]; c2:= M[1, 2]; d2:= M[1, 3];
    a3:= M[2, 0]; b3:= M[2, 1]; c3:= M[2, 2]; d3:= M[2, 3];
    a4:= M[3, 0]; b4:= M[3, 1]; c4:= M[3, 2]; d4:= M[3, 3];
    M[0, 0]:= MatrixDet(b2, b3, b4, c2, c3, c4, d2, d3, d4);
    M[1, 0]:=-MatrixDet(a2, a3, a4, c2, c3, c4, d2, d3, d4);
    M[2, 0]:= MatrixDet(a2, a3, a4, b2, b3, b4, d2, d3, d4);
    M[3, 0]:=-MatrixDet(a2, a3, a4, b2, b3, b4, c2, c3, c4);
    M[0, 1]:=-MatrixDet(b1, b3, b4, c1, c3, c4, d1, d3, d4);
    M[1, 1]:= MatrixDet(a1, a3, a4, c1, c3, c4, d1, d3, d4);
    M[2, 1]:=-MatrixDet(a1, a3, a4, b1, b3, b4, d1, d3, d4);
    M[3, 1]:= MatrixDet(a1, a3, a4, b1, b3, b4, c1, c3, c4);
    M[0, 2]:= MatrixDet(b1, b2, b4, c1, c2, c4, d1, d2, d4);
    M[1, 2]:=-MatrixDet(a1, a2, a4, c1, c2, c4, d1, d2, d4);
    M[2, 2]:= MatrixDet(a1, a2, a4, b1, b2, b4, d1, d2, d4);
    M[3, 2]:=-MatrixDet(a1, a2, a4, b1, b2, b4, c1, c2, c4);
    M[0, 3]:=-MatrixDet(b1, b2, b3, c1, c2, c3, d1, d2, d3);
    M[1, 3]:= MatrixDet(a1, a2, a3, c1, c2, c3, d1, d2, d3);
    M[2, 3]:=-MatrixDet(a1, a2, a3, b1, b2, b3, d1, d2, d3);
    M[3, 3]:= MatrixDet(a1, a2, a3, b1, b2, b3, c1, c2, c3);
  end;

var det: Single;
begin
   det:=Determinant(M);
   if Abs(Det)<EPSILON then M:=NewMatrix
   else begin
     AdjointMatrix; Scale(M, 1/det);
   end;
end;

function MatrixOnPlane(const cPos, cDir: TVector; const angle: single): TMatrix;
var x,y,z: TVector;
begin

// This function could be made better by creating vectors first by (0,1,0) and
// then rotating them to cDir. Current setup looks good mainly, but rotation
// with different angles looks like "slowing down" a little on certain directions.

  y:=norm2(cDir);
  if y.y<>0 then
    z:=norm2(crossproduct( vector(cos(angle),0,sin(angle)), y ))
  else if y.x<>0 then
    z:=norm2(crossproduct( vector(0,cos(angle),sin(angle)), y ))
  else
    z:=norm2(crossproduct( vector(cos(angle),sin(angle),0), y ));
  x:=norm2(crossproduct(y,z));
  result:=CreateMatrix(x,y,z); // This works
 // result:=CreateMatrix(norm(cDir),angle); // Why does this not work?
  Translate(result,cPos);
end;

function Multiply(const A,B: TMatrix): TMatrix;
var I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result[I, J] := A[I, 0] * B[0, J] + A[I, 1] * B[1, J] +
        A[I, 2] * B[2, J] + A[I, 3] * B[3, J];
end;

function MultiplyRotation(const A, B: TMatrix): TMatrix;
var I, J: Integer;
begin
  for I := 0 to 2 do begin
    for J := 0 to 2 do
      Result[I, J] := A[I, 0] * B[0, J] + A[I, 1] * B[1, J] +
        A[I, 2] * B[2, J];
    result[3, I]:=0;
    result[I, 3]:=0;
  end;
  result[3, 3]:=1;
end;

function LookAt(const eye, target, up: TVector): TMatrix;
var x,y,z: TVector;
begin
  z:=norm2(VectorSub2(eye, target));
  x:=norm2(crossproduct(up, z));
  y:=crossproduct(z, x);
  result:=CreateMatrix2(x, y, z);
  translate(result, vector(-dot(eye, x), -dot(eye, y), -dot(eye, z)));
end;

// To use make sure that axis vector length is 1
procedure Rotate(var M: TMatrix; const axis: TVector; const Angle: Single;
  withPos: boolean);
var v: TVector;
begin
  if not withPos then begin
    v:=getVector(M, 3);
    M[3,0]:=0; M[3,1]:=0; M[3,2]:=0;
  end;
  M:=Multiply(M, CreateMatrix(axis, Angle));
  if not withPos then SetVector(M, v, 3);
end;

procedure Rotate(var M: TMatrix; const axis: integer; const Angle: Single;
  withPos: boolean);
var v: TVector;
begin
  if not withPos then begin
    v:=getVector(M,3);
    M[3,0]:=0; M[3,1]:=0; M[3,2]:=0;
  end;
  case axis of
    0: rotate(M,vector(0,1,0),angle,true);
    1: rotate(M,vector(1,0,0),angle,true);
    2: rotate(M,vector(0,0,1),angle,true);
  end;
  if not withPos then SetVector(M,v,3);
end;

procedure Scale(var M: TMatrix; const s: Single);
var i: Integer;
begin
  for i:=0 to 2 do begin
    M[i, 0]:=M[i, 0] * s;
    M[i, 1]:=M[i, 1] * s;
    M[i, 2]:=M[i, 2] * s;
  end;
end;

procedure Scale(var M: TMatrix; const v: TVector); overload;
var M2: TMatrix;
begin
  M2:=NewMatrix;
  M2[0,0]:=v.x; M2[1,1]:=v.y; M2[2,2]:=v.z;
  M:=Multiply(M,M2);
end;

procedure SetVector(var M: TMatrix; const v: TVector; const axis: integer);
begin
  M[axis,0]:=v.x; M[axis,1]:=v.y; M[axis,2]:=v.z;
end;

// Translate matrix by vector
procedure Translate(var M: TMatrix; const v: TVector);
begin
  M[3,0]:=M[3,0]+v.x; M[3,1]:=M[3,1]+v.y; M[3,2]:=M[3,2]+v.z;
end;

// Multiply vector by matrix
function VectorMultiply(const V: TVector; const M: TMatrix): TVector;
begin
  Result.x:=V.x * M[0, 0] + V.y * M[1, 0] + V.z * M[2, 0] + M[3, 0];
  Result.y:=V.x * M[0, 1] + V.y * M[1, 1] + V.z * M[2, 1] + M[3, 1];
  Result.z:=V.x * M[0, 2] + V.y * M[1, 2] + V.z * M[2, 2] + M[3, 2];
end;

initialization

  NewMatrix:=getNewMatrix;

end.
