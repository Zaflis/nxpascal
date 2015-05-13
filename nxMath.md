#nxMath documentation

# nxMath #

---

Uses [nxTypes](nxTypes.md)

## Functions and procedures ##

```
  function Angle(const px1,py1,px2,py2: single): single;
```
Returns vector angle from point 1 to point 2, in radians.

```
  function Angle(srcRadian, destRadian: single): single;
```
Returns difference of 2 radian vector angles between -pi to pi.
Example Angle(0, pi\*2) would return 0, because second angle makes a full circle.
Used for example determining which way to turn, when 2 objects are in open space, with free rotations, and when wanting to rotate other object towards the other. With axis-functions, this information is useful in 3D-math too.

```
  function Catmull(const p0,p1,p2,p3,t: single): single;
  function Catmull(const a,b,c,d: TVector2f; const delta: single): TVector2f;
```
Catmull-rom is a curve where all parts of the curve goes through all the points in it.
It is especially useful when making smooth paths, or approximations of rounded shapes. Points 0, 1, 2, 3 are used in linear sequence. Value t is delta where 0.0 is at point 1, and 1.0 at point 2. If points 0 or 3 are unknown, use values from nearest point to it, example copy of 1 or 2. This will cause "curve" to go straight at that part.

```
  function Distance(x1,y1,x2,y2, px,py: single): single;
```
Returns distance from line (x1,y1, x2,y2) to point (px, py).

```
  function Distance2(x1,y1,x2,y2, px,py: single): single;
```
Same as Distance(), but returns + or - depending on which side the point is in relation to vector direction towards (x2, y2).

```
  function dmod(a, b: double): double;
  function fmod(a, b: single): single;
```
Returns floating point remainder of a mod b. (a - int(a / b) **b)**

```
  function Dot(const a,b: TVector2f): single;
```
Returns dot-product of a and b.

```
  function Interpolate(const v1,v2,s: single): single;
  function Interpolatei(const v1,v2: integer; s: single): integer;
  function Interpolate(const v1,v2: TVector2f; const s: single): TVector2f;
```
Returns linear interpolation of v1 and v2, with delta s (range 0..1). For example Interpolate (4.0, 6.0, 0.5) would return 5.0, which is half way between 4.0 and 6.0.

```
  function Invert(const v: TVector2f): TVector2f;
```
Returns negation of vector.

```
  function LinesCross(const x0, y0, x1, y1, x2, y2, x3, y3: Integer): Boolean;
```
Returns true if lines (x0,y0, x1,y1) and (x2,y2, x3,y3) are crossing. Otherwise false.

```
  function Mod2(a,b: integer): integer;
```
Returns modulus which acts consistently with negative values too. Normal modulus returns (-9) mod 10 as -9, but this function will return -1.

```
  function Norm(const v: TVector2f; const h: PSingle = nil): TVector2f;
  procedure Norm(var x,y: single; const h: PSingle = nil);
  procedure Norm(var x,y: double; const h: PDouble);
```
Normalizes (scales) a vector to length 1.0. If parameter pointer h is given, returns length that vector had before.

```
  function PointInCircle(const p,circle: TVector2f; const radius: single): boolean;
  function PointInCircle(const pX,pY,circleX,circleY,radius: single): boolean;
```
Returns true if point is inside a circle. This is faster than comparing vector length differences with hypot(), because no sqrt() is called.

```
  function PointInRect(const x, y: integer; rect: TRecti): boolean;
  function PointInRect(const x, y: single; rect: TRectf): boolean;
```
Returns true if point is inside a rectangle.

```
  function Pow2fit(n: integer): integer;
```
Returns a power of 2 int value that is >= n. For example: Pow2near(140) would return 256.

```
  function Pow2near(n: integer): integer;
```
Returns power of 2 int value rounded as near to n as possible. For example: Pow2near(140) would return 128.

```
  function Reflect(const rayStart, rayDir, wallPoint: TVector2f): TVector2f;
  function Reflect(const rayDir, wallNormal: TVector2f): TVector2f; overload;
```
Returns a reflection vector with given vector parameters.

```
  function Rotate(const pt,center: TVector2f; const degrees: single): TVector2f;
  procedure Rotate(var x,y: single; const degrees,centerX,centerY: single);
```
Rotates point around a center point. Angle given in degrees.

```
  function Smoothen(n: single): single;
```
Returns value from range 0..1. n<0, returns 0, and n>1 returns 1. If graph 0..1 was drawn, it would look like a section of sine-wave; starting to grow slowly, accelerates towards middle and decelerates towards 1.0.

```
function Smoothen(const source, dest: single; n: single): single;
```
Same smoothen result as above, but smoothens custom range between source and dest.

```
  function Tangent(const p1, p2: TVector2f): TVector2f; 
```
Returns tangent vector of a vector made by 2 points p1 and p2.

```
  function Vector2f(const x, y: single): TVector2f;
```
Returns vector(x, y)

```
  function VectorAdd(const v1, v2: TVector2f): TVector2f; 
```
Returns v1 + v2.

```
  function VectorMatch(const a, b: TVector2f; delta: single=0.01): boolean;
```
Returns true if vector a match b, with delta precision.

```
  function VectorSub(const v1, v2: TVector2f): TVector2f;
```
Returns v1 - v2.