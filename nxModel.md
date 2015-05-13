#nxModel documentation

# nxModel #

---

Uses [nxMath](nxMath.md), [nxMath3D](nxMath3D.md), [nxStrings](nxStrings.md), [nxTypes](nxTypes.md)

## Classes ##
  * T3DModel
  * TModelMS3D
  * TModelW3D (T3DModel)
  * TPolyModel (T3DModel)
  * TTriModel (T3DModel)

## T3DModel class ##

```
procedure Center(_x, _y, _z: boolean);
procedure Clear;
function GetRadius: single;
procedure Rotate(_angle: single; axis: TVector);
procedure RotateUV(_angle, centerX, centerY: single);
procedure Scale(x, y, z: single);
procedure ScaleTo(size: single);
procedure ScaleUV(x, y: single);
procedure Translate(x, y, z: single);
procedure TranslateUV(x, y: single);
```

## TModelMS3D class ##

```
constructor Create(filename: string);
procedure AssignToTri(tri: TTriModel; FreeSelf: boolean = false);
```

## TModelW3D class ##

```
procedure LoadFromFile(filename: string; obj: integer = -1);
destructor Destroy;
procedure AssignTo(const poly: TPolyModel);
procedure AssignTo(const tri: TTriModel);
procedure DoTextureCorrection;
procedure SaveToFile(filename: string);
```

## TPolyModel class ##

```
function AddFace(const points: array of word; _vCount: byte; ccw: boolean = false): word;
procedure AssignTo(tri: TTriModel);
procedure AssignTo(w3d: TModelW3D);
procedure Clear;
procedure DivideTriangles;
function GetTangent(const fIndex: word): TVector;
procedure LoadFromFile(filename: string);
procedure LoadFromMS3D(filename: string);
procedure LoadFromOBJ(filename: string);
procedure LoadFromW3D(filename: string; obj: integer = -1);
procedure MakeNormals;
procedure SaveToOBJ(filename: string);
procedure SaveToW3D(filename: string);
```

## TTriModel class ##

```
constructor Create;
constructor Create(filename: string);
constructor CreateCube(segments: integer = 1);
constructor CreatePlane(cols, rows: integer);
constructor CreateSphere(cols, rows: integer);
constructor CreateTorus(cols, rows: integer; inRadius: single);
function AddFace(const gIndex: word): integer;
procedure AssignTo(poly: TPolyModel);
procedure Clear;
procedure DoTextureCorrection;
procedure FlipFaces;
function GetTangent(const fIndex: word): TVector;
procedure LoadFromFile(filename: string);
procedure LoadFromMS3D(filename: string);
procedure LoadFromOBJ(filename: string);
procedure LoadFromW3D(filename: string; obj: integer = -1);
procedure MakeNormals;
function rayIntersect(const rayStart, rayDir: TVector; findClosest: boolean; const intersect: PVector=nil; const normal: PVector=nil): integer;
function rayIntersect(const rayStart, rayDir: TVector; findClosest: boolean; const position: TVector; const intersect: PVector=nil;	const normal: PVector=nil): integer;
function rayIntersect(rayStart, rayDir: TVector; findClosest: boolean; const position: TVector; rotation: TMatrix; const intersect: PVector=nil; const normal: PVector=nil): integer;
procedure RotateUV(_angle, centerX, centerY: single; group: integer);
procedure ScaleUV(x, y: single; group: integer);
procedure SaveToFile(filename: string);
procedure TranslateUV(x, y: single; group: integer);
```