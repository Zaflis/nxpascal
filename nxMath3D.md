#nxMath3D documentation

# nxMath3D #

---

Uses [nxMath](nxMath.md), [nxTypes](nxTypes.md)

## Functions and procedures ##

```
function Bezier(const a,b: TVector; const delta: single): TVector;
function Bezier(const p: array of TVector; const count: word;	const delta: single): TVector;
function Catmull(const a,b,c,d: TVector; const delta: single): TVector;
function HalfBezier(const a,b,c: TVector; const delta: single): TVector;
```
Bezier curve with delta is making a curving path towards points, but not actually going through them (unlike catmull).
Half-bezier is going towards the points more than normal bezier.
You can see various curves in [this thread](http://www.pascalgamedevelopment.com/showthread.php?4621-Bezier-curves).

```
function Angle(const v1,v2: TVector; axis: integer): single;
function ClosestPointOnLine(const l1,l2,p: TVector): TVector;
function CrossProduct(const a,b: TVector): TVector;
function Dot(const a,b: TVector): single;
function GetAvgNormal(const n1,n2,n3,n4: PVector): TVector;
function Hypot3f(const x,y,z: single): single;
function Hypot3d(const x,y,z: double): double;
function Interpolate(const v1,v2: TVector; const s: single): TVector;
function Invert(const v: TVector): TVector;
function Multiply(const a,b: TVector): TVector;
procedure Norm(var x,y,z: single; const h: PSingle = nil);
procedure Norm(var x,y,z: double; const h: PDouble = nil);
function Norm(const v: TVector; const h: PSingle = nil): TVector;
function PointInSphere(const p,sphere: TVector; const radius: single): boolean;
function RayPlaneIntersect(const rayOrigin, rayDirection,	planeOrigin, planeNormal: TVector; intersection: PVector): single;
function RaySphereIntersect(const rayStart: TVector; rayDirection: TVector;	const sphereCenter: TVector; const sphereRadius: Single; const i1, i2: PVector): Integer;
function RayTriangleIntersect(const rayStart, rayDirection: TVector; const p1, p2, p3: TVector; intersect: PVector = nil; intersectNormal: PVector = nil): Boolean;
function Reflect(const rayStart, rayDir, wallPoint: TVector): TVector;
function Reflect(const rayDir, wallNormal: TVector): TVector;
function Rotate(const v: TVector; const radians: single; axis: TVector): TVector;
function Scale(const v: TVector; s: single): TVector;
function Tangent(const a, b, c: TVector): TVector;
function VectorAdd(const a, b: TVector): TVector;
function VectorDist(const a, b: TVector): single;
function VectorDiv(const v: TVector; s: single): TVector;
function VectorMatch(const a, b: TVector; delta: single=0.01): boolean;
function VectorLen(const v: TVector): single;
function VectorSub(const a, b: TVector): TVector;
```
3D-math.

```
function CreateMatrix(const axis: TVector; const radians: Single): TMatrix;
function CreateMatrix(const x,y,z: TVector): TMatrix; overload;
function CreateMatrix2(const x,y,z: TVector): TMatrix;
function CreateTranslateMatrix(const p: TVector): TMatrix;
function Determinant(const M: TMatrix): Single;
function GetAngle(const M: TMatrix; const axis: integer): single;
function GetRotation(const mat: TMatrix): TMatrix;
function GetVector(const M: TMatrix; const axis: integer): TVector;
function Interpolate(const a, b: TMatrix; s: single): TMatrix;
function Invert(const M: TMatrix): TMatrix;
function LookAt(const eye, target, up: TVector): TMatrix;
function Matrix(const mat: TMatrix4d): TMatrix;
function MatrixOnPlane(const cPos,cDir: TVector; const radians: single = 0): TMatrix;
function Multiply(const A,B: TMatrix): TMatrix;
function MultiplyRotation(const A,B: TMatrix): TMatrix;
function Multiply(const V: TVector; const M: TMatrix): TVector;
function Rotate(const M: TMatrix; const axis: TVector; const radians: Single;	withPos: boolean = true): TMatrix;
function Rotate(const M: TMatrix; const axis: integer; const radians: Single;	withPos: boolean = true): TMatrix;
function Scale(const M: TMatrix; const s: Single): TMatrix;
function Scale(const M: TMatrix; const v: TVector): TMatrix;
procedure SetVector(var M: TMatrix; const v: TVector; const axis: integer);
function Slerp(const a, b: TMatrix; s: single): TMatrix;
procedure Translate(var M: TMatrix; const v: TVector);
```
Matrix operations.

```
function Dot(const a, b: TQuaternion): single;
function Lerp(const a, b: TQuaternion; s: single): TQuaternion;
function Multiply(const q: TQuaternion; s: single): TQuaternion;
function Multiply(const a, b: TQuaternion): TQuaternion;
function NewQuaternion: TQuaternion;
function Norm(const q: TQuaternion): TQuaternion;
function Quaternion(x, y, z, w: single): TQuaternion;
function Quaternion(const M: TMatrix): TQuaternion;
function Quaternion(const x, y, z: TVector): TQuaternion;
function Quaternion(const x, y, z: single): TQuaternion;
function Quaternion(const axis: TVector; radians: single): TQuaternion;
function QuaternionAdd(const a, b: TQuaternion): TQuaternion;
function QuaternionSub(const a, b: TQuaternion): TQuaternion;
function QuaternionToMat(const q: TQuaternion; const position: TVector): TMatrix;
function Slerp(a, b: TQuaternion; s: single): TQuaternion;
```
Quaternion operations.

## Other ##

```
var NewMatrix: TMatrix;
```
Identity matrix. Can be used as a copy template to new rotation and/or translation matrix.