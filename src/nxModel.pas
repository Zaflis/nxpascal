unit nxModel;

{$IFDEF fpc}{$mode delphi}{$H+}{$ENDIF}

{ TODO:
- Bones
- TVertexFrame texture animation
}

interface

uses SysUtils, nxTypes;

type
  TIndexArray = array of word;

  TMaterial = record
    name, texture: string;
    texIndex: smallint;
    addMode, transparent: boolean;
    color: TRGBA;
    shininess, specular: single;
  end;
  TFaceGroup = record
    first, count: word;
    matIndex: shortint;
    visible: boolean;
  end;
  TVertexFrame = record
    va, na: array of TVector; // Vertices, Normals
    ta: array of TVector2f; // Textures
    time: word; // frame length as milliseconds
  end;
  PVertexFrame = ^TVertexFrame;

  TBone = record
    rot: TMatrix3f;
    position: TVector;
  end;

  { T3DModel }

  T3DModel = class
    bCount: integer;
    va, na: array of TVector; // Vertices, Normals
    ta: array of TVector2f; // Textures
    mat: array of TMaterial;
    grp: array of TFaceGroup;
    bone: array of TBone;
    UseMaterials, UseColors: boolean;
  private
    FvCount, FGroups, FmCount, vCount2, fCount2: longint;
    procedure SetGroups(n: integer);
    procedure SetmCount(n: integer);
    procedure SetvCount(n: longint);
  public
    property groups: integer read FGroups write SetGroups;
    property mCount: integer read FmCount write SetmCount;
    property vCount: longint read FvCount write SetvCount;
    procedure Center(_x, _y, _z: boolean);
    procedure Clear;
    procedure CombineGroups(separateMaterials: boolean = true);
    function GetRadius(cubic: boolean = false): single;
    function GetRadiusX(): single;
    function GetRadiusY(): single;
    function GetRadiusZ(): single;
    procedure Rotate(_radians: single; axis: TVector);
    procedure RotateUV(_radians, centerX, centerY: single); overload;
    procedure Scale(x, y, z: single);
    procedure ScaleTo(size: single); overload;
    procedure ScaleTo(sizeX, sizeY, sizeZ: single); overload;
    procedure ScaleTo(ref: T3DModel); overload;
    procedure ScaleUV(x, y: single); overload;
    procedure Translate(x, y, z: single);
    procedure TranslateUV(x, y: single); overload;
  end;

  TModelW3D = class;
  TPolyModel = class;
  TVertexModel = class;

  TTriFaceIndices = array[0..2] of word;

  { TTriModel }

  TTriModel = class(T3DModel)
    frames: integer;
    fa: array of TTriFaceIndices; // Faces
    frame: array of TVertexFrame;
  private
    FfCount: longint;
    procedure GetGrpVertices(out vi: TIndexArray; out vc: longint; group: integer);
    procedure SetfCount(n: longint);
  public
    property fCount: longint read FfCount write SetfCount;
    constructor Create(filename: string); overload;
    constructor CreateCube(segments: integer = 1);
    constructor CreatePlane(cols, rows: integer);
    constructor CreateSphere(cols, rows: integer);
    constructor CreateTorus(cols, rows: integer; inRadius: single);
    function AddFace(const gIndex: word): longint;
    procedure AssignTo(poly: TPolyModel);
    procedure Clear;
    procedure DoTextureCorrection;
    function FindNearest(const source: TVector;
      const nearest: PVector=nil; const normal: PVector=nil): longint;
    procedure FlipFaces;
    function GetTangent(const fIndex: word): TVector;
    procedure LoadFromFile(filename: string);
    procedure LoadFromMS3D(filename: string);
    procedure LoadFromOBJ(filename: string);
    procedure LoadFromW3D(filename: string; obj: longint = -1);
    procedure MakeNormals;
    function rayIntersect(const rayStart, rayDir: TVector; findClosest: boolean;
      const intersect: PVector=nil; const normal: PVector=nil): longint; overload; stdcall;
    function rayIntersect(const rayStart, rayDir: TVector; findClosest: boolean;
      const position: TVector; const intersect: PVector=nil;
      const normal: PVector=nil): longint; overload; stdcall;
    function rayIntersect(rayStart, rayDir: TVector; findClosest: boolean;
      const position: TVector; rotation: TMatrix;
      const intersect: PVector=nil; const normal: PVector=nil): longint; overload; stdcall;
    procedure RotateUV(_angle, centerX, centerY: single; group: integer); overload;
    procedure ScaleUV(x, y: single; group: integer); overload;
    procedure SaveToFile(filename: string);
    procedure TranslateUV(x, y: single; group: integer); overload;
  end;

  TPolyFaceIndices = packed record
    count: word;
    index: array of word;
  end;

  { TPolyModel }

  TPolyModel = class(T3DModel)
    fa: array of TPolyFaceIndices; // Faces
  private
    FfCount: longint;
    procedure SetfCount(n: longint);
  public
    property fCount: longint read FfCount write SetfCount;
    function AddFace(const points: array of word; _vCount: byte;
      ccw: boolean = false): word;
    procedure AssignTo(tri: TTriModel); overload;
    procedure AssignTo(w3d: TModelW3D); overload;
    procedure AssignTo(vm: TVertexModel); overload;
    procedure Clear;
    procedure DivideTriangles;
    function GetTangent(const fIndex: word): TVector;
    procedure LoadFromFile(filename: string);
    procedure LoadFromMS3D(filename: string);
    procedure LoadFromOBJ(filename: string);
    procedure LoadFromW3D(filename: string; obj: longint = -1);
    procedure MakeNormals;
    function rayIntersect(const rayStart, rayDir: TVector; findClosest: boolean;
      const intersect: PVector=nil; const normal: PVector=nil): longint; overload; stdcall;
    function rayIntersect(rayStart, rayDir: TVector; findClosest: boolean;
      const position: TVector; rotation: TMatrix;
      const intersect: PVector=nil; const normal: PVector=nil): longint; overload; stdcall;
    function RayPolyIntersect(const rayStart, rayDirection: TVector;
      const vi: PWordArray; Count: longint; BothSided: boolean;
      intersect: PVector; intersectNormal: PVector): Boolean; stdcall;
    procedure SaveToFile(filename: string);
    procedure SaveToOBJ(filename: string);
    procedure SaveToW3D(filename: string);
  end;

  { TVertexModel }

  TVertexModel = class
    fa: array of TTriFaceIndices;
    va: array of T3DVertex;
    ca: array of TRGBA;
    grp: array of TFaceGroup;
    bone: array of TBone;
    frame: array of TVertexFrame;
  private
  public
    procedure Center(_x, _y, _z: boolean);
    procedure Clear;
    function GetRadius: single;
    procedure Rotate(_radians: single; axis: TVector);
    procedure RotateUV(_radians, centerX, centerY: single); overload;
    procedure Scale(x, y, z: single);
    procedure ScaleTo(size: single);
    procedure ScaleUV(x, y: single); overload;
    procedure Translate(x, y, z: single);
    procedure TranslateUV(x, y: single); overload;
  end;

  TW3DFace = record
    MatIndex, Count: byte;
    SmoothGroup: word;
    v: array of word;
    n: array of TVector;
    uv: array of TVector2f;
  end;

  { TModelW3D }

  TModelW3D = class(T3DModel)
    fa: array of TW3DFace; // Faces
  private
    FfCount: longint;
    procedure SetfCount(n: longint);
  public
    property fCount: longint read FfCount write SetfCount;
    procedure LoadFromFile(filename: string; obj: longint = -1);
    destructor Destroy; override;
    procedure AssignTo(const poly: TPolyModel); overload;
    procedure AssignTo(const tri: TTriModel); overload;
    procedure DoTextureCorrection;
    procedure SaveToFile(filename: string);
  end;

//      MilkShape 3D 1.4.0
// http://local.wasp.uwa.edu.au/~pbourke/dataformats/ms3d/ms3dspec.h
  TMS3DHeader = packed record
    id: array[0..9] of char;    // always "MS3D000000"
    version: longint;           // 3
  end;
  TMS3DVertex = packed record
    flags: byte; // SELECTED(1) | SELECTED2(4) | HIDDEN(2)
    x,y,z: single;
    boneId: shortint; // -1 = no bone
    refCount: byte;
  end;
  TMS3DTri = packed record
    flags: word; // SELECTED(1) | SELECTED2(4) | HIDDEN(2)
    v: array[0..2] of word;
    n: array[0..2, 0..2] of single;
    s,t: array[0..2] of single;
    smoothGroup, groupIndex: byte;
  end;
  TMS3DGroup = record
    flags: byte;  // SELECTED(1) | HIDDEN(2)
    name: array[0..31] of char;
    numtriangles: word;
    triangleIndices: array of word; // the groups group the triangles
    materialIndex: shortint;   // -1 = no material
  end;
  TMS3DMaterial = packed record
    name: array[0..31] of char;
    ambient, diffuse, specular, emissive: array[0..3] of single;
    shininess, transparency: single;  // shine 0-128, trans 0-1
    mode: shortint;      // 0, 1, 2 is unused now
    texture, alphamap: array[0..127] of char;
  end;
  TMS3DKeyframe_rot = packed record
    time: single;          // time in seconds
    rotation: array[0..2] of single;   // x, y, z angles
  end;
  TMS3DKeyframe_pos = packed record
    time: single;          // time in seconds
    position: array[0..2] of single;   // local position
  end;
  TMS3DJoint = record
    flags: byte;     // SELECTED(1) | DIRTY(8)
    name, parentName: array[0..31] of char;
    rotation, position: array[0..2] of single; // local reference matrix
    numKeyFramesRot, numKeyFramesTrans: word;
    keyFramesRot: array of TMS3DKeyframe_rot;   // local animation matrices
    keyFramesTrans: array of TMS3DKeyframe_pos; // local animation matrices
  end;
  TModelMS3D = class
    vCount, fCount, groups, mats, joints: word;
    AnimationFPS, CurrentTime: single;
    TotalFrames: longint;
    va: array of TMS3DVertex; // Vertices
    fa: array of TMS3DTri;
    grp: array of TMS3DGroup;
    mat: array of TMS3DMaterial;
    joint: array of TMS3DJoint;
  public
    constructor Create(filename: string);
    procedure AssignToTri(tri: TTriModel; FreeSelf: boolean = false);
  end;
// 0. Build the transformation matrices from the rotation and position
// 1. Multiply the vertices by the inverse of local reference matrix (lmatrix0)
// 2. then translate the result by (lmatrix0 * keyFramesTrans)
// 3. then multiply the result by (lmatrix0 * keyFramesRot)
// For normals skip step 2.

implementation

uses Classes, math, nxStrings, nxMath, nxMath3D
  {$IFDEF fpc}, FileUtil{$ENDIF};

{ T3DModel }

procedure T3DModel.Clear;
begin
  vCount:=0; groups:=0; mCount:=0; bCount:=0;
  setlength(bone, 0);
end;

procedure T3DModel.CombineGroups(separateMaterials: boolean);
var i, j: integer;
begin
  i:=0;
  while i<groups-2 do begin
    j:=0;
    while j<groups do begin
      // Face indices of 2 compared groups must be next to eachother
      if (i<>j) and (grp[j].first=grp[i].first+grp[i].count) and
         ((not separateMaterials) or (grp[i].matIndex=grp[j].matIndex)) then begin
        inc(grp[i].count, grp[j].count);
        // Move combined group in front of grp[i]
        if j<>i+1 then switchvar(@grp[i+1], @grp[j], sizeof(TFaceGroup));
        // Delete group, and move remaining groups backwards
        for j:=i+1 to groups-2 do grp[j]:=grp[j+1];
        groups:=groups-1;
        j:=0;
      end else inc(j);
    end;
    inc(i);
  end;
end;

function T3DModel.GetRadius(cubic: boolean): single;
  procedure _SetMax(var original: single; value: single);
  begin
    value:=abs(value);
    if value>original then original:=value;
  end;
var i: longint; d: single;
begin
  result:=0;
  for i:=0 to vCount-1 do begin
    if cubic then begin
      _SetMax(result, va[i].x);
      _SetMax(result, va[i].y);
      _SetMax(result, va[i].z);
    end else begin
      d:=hypot3f(va[i].x, va[i].y, va[i].z);
      if d>result then result:=d;
    end;
  end;
end;

function T3DModel.GetRadiusX: single;
var i: longint; d: single;
begin
  result:=0;
  for i:=0 to vCount-1 do begin
    d:=abs(va[i].x);
    if d>result then result:=d;
  end;
end;

function T3DModel.GetRadiusY: single;
var i: longint; d: single;
begin
  result:=0;
  for i:=0 to vCount-1 do begin
    d:=abs(va[i].y);
    if d>result then result:=d;
  end;
end;

function T3DModel.GetRadiusZ: single;
var i: longint; d: single;
begin
  result:=0;
  for i:=0 to vCount-1 do begin
    d:=abs(va[i].z);
    if d>result then result:=d;
  end;
end;

procedure T3DModel.Rotate(_radians: single; axis: TVector);
var i: longint;
begin
  for i:=0 to vCount-1 do begin
    va[i]:=nxMath3D.Rotate(va[i], _radians, axis);
    na[i]:=nxMath3D.Rotate(na[i], _radians, axis);
  end;
end;

procedure T3DModel.RotateUV(_radians, centerX, centerY: single);
var i: longint;
begin
  for i:=0 to vCount-1 do
    nxMath.Rotate(ta[i].x, ta[i].y, _radians, centerX, centerY);
end;

procedure T3DModel.Scale(x, y, z: single);
var i: longint;
begin
  for i:=0 to vCount-1 do begin
    va[i].x:=va[i].x*x; va[i].y:=va[i].y*y; va[i].z:=va[i].z*z;
  end;
end;

procedure T3DModel.ScaleTo(size: single);
var i: longint; minx,miny,minz, maxx,maxy,maxz, d,dx,dy,dz: single;
begin
  if vCount<=0 then exit;
  minx:=va[0].x; maxx:=va[0].x;
  miny:=va[0].y; maxy:=va[0].y;
  minz:=va[0].z; maxz:=va[0].z;
  for i:=1 to vCount-1 do begin
    if va[i].x<minx then minx:=va[i].x
    else if va[i].x>maxx then maxx:=va[i].x;
    if va[i].y<miny then miny:=va[i].y
    else if va[i].y>maxy then maxy:=va[i].y;
    if va[i].z<minz then minz:=va[i].z
    else if va[i].z>maxz then maxz:=va[i].z;
  end;
  dx:=size/(maxx-minx);
  dy:=size/(maxy-miny);
  dz:=size/(maxz-minz);
  if dx<dy then d:=dx
  else d:=dy;
  if d<dz then d:=dz;
  for i:=0 to vCount-1 do begin
    va[i].x:=va[i].x*d;
    va[i].y:=va[i].y*d;
    va[i].z:=va[i].z*d;
  end;
end;

procedure T3DModel.ScaleTo(sizeX, sizeY, sizeZ: single);
var i: longint; minx,miny,minz, maxx,maxy,maxz, dx,dy,dz: single;
begin
  if vCount<=0 then exit;
  minx:=va[0].x; maxx:=va[0].x;
  miny:=va[0].y; maxy:=va[0].y;
  minz:=va[0].z; maxz:=va[0].z;
  for i:=1 to vCount-1 do begin
    if va[i].x<minx then minx:=va[i].x
    else if va[i].x>maxx then maxx:=va[i].x;
    if va[i].y<miny then miny:=va[i].y
    else if va[i].y>maxy then maxy:=va[i].y;
    if va[i].z<minz then minz:=va[i].z
    else if va[i].z>maxz then maxz:=va[i].z;
  end;
  dx:=sizeX/(maxx-minx);
  dy:=sizeY/(maxy-miny);
  dz:=sizeZ/(maxz-minz);
  for i:=0 to vCount-1 do begin
    va[i].x:=va[i].x*dx;
    va[i].y:=va[i].y*dy;
    va[i].z:=va[i].z*dz;
  end;
end;

procedure T3DModel.ScaleTo(ref: T3DModel);
begin
  ScaleTo(ref.GetRadiusX*2, ref.GetRadiusY*2, ref.GetRadiusZ*2);
end;

procedure T3DModel.ScaleUV(x, y: single);
var i: longint;
begin
  for i:=0 to vCount-1 do begin
    ta[i].x:=ta[i].x*x; ta[i].y:=ta[i].y*y;
  end;
end;

procedure T3DModel.Translate(x, y, z: single);
var i: longint;
begin
  for i:=0 to vCount-1 do begin
    va[i].x:=va[i].x+x; va[i].y:=va[i].y+y; va[i].z:=va[i].z+z;
  end;
end;

procedure T3DModel.TranslateUV(x, y: single);
var i: longint;
begin
  for i:=0 to vCount-1 do begin
    ta[i].x:=ta[i].x+x; ta[i].y:=ta[i].y+y;
  end;
end;

procedure T3DModel.SetGroups(n: integer);
begin
  FGroups:=n; setlength(grp, FGroups);
end;

procedure T3DModel.SetmCount(n: integer);
begin
  FmCount:=n;
  setlength(mat, FmCount);
end;

procedure T3DModel.SetvCount(n: longint);
begin
  FvCount:=n;
  if n>100 then n:=(n div 100+1)*100;
  if n<>vCount2 then begin
    vCount2:=n;
    setlength(va, vCount2);
    setlength(na, vCount2);
    setlength(ta, vCount2);
  end;
end;

procedure T3DModel.Center(_x, _y, _z: boolean);
var i: longint; minx,miny,minz, maxx,maxy,maxz, d: single;
begin
  if vCount<=0 then exit;
  if _x then begin
    minx:=va[0].x; maxx:=va[0].x;
    for i:=1 to vCount-1 do begin
      if va[i].x<minx then minx:=va[i].x
      else if va[i].x>maxx then maxx:=va[i].x;
    end;
    d:=(maxx+minx)/2;
    for i:=0 to vCount-1 do va[i].x:=va[i].x-d;
  end;
  if _y then begin
    miny:=va[0].y; maxy:=va[0].y;
    for i:=1 to vCount-1 do begin
      if va[i].y<miny then miny:=va[i].y
      else if va[i].y>maxy then maxy:=va[i].y;
    end;
    d:=(maxy+miny)/2;
    for i:=0 to vCount-1 do va[i].y:=va[i].y-d;
  end;
  if _z then begin
    minz:=va[0].z; maxz:=va[0].z;
    for i:=1 to vCount-1 do begin
      if va[i].z<minz then minz:=va[i].z
      else if va[i].z>maxz then maxz:=va[i].z;
    end;
    d:=(maxz+minz)/2;
    for i:=0 to vCount-1 do va[i].z:=va[i].z-d;
  end;
end;

{ TPolyModel }

function TPolyModel.AddFace(const points: array of word; _vCount: byte;
  ccw: boolean): word;
var i: longint;
begin
  if fCount>65534 then begin
    result:=0; exit;
  end;
  result:=fCount; SetfCount(fCount+1);
  with fa[result] do begin
    count:=_vCount; setlength(index, count);
    if ccw then begin
      for i:=0 to _vCount-1 do
        index[i]:=points[i];
    end else
      for i:=_vCount-1 downto 0 do
        index[_vCount-1-i]:=points[i];
  end;
end;

procedure TPolyModel.AssignTo(tri: TTriModel);
var i, j: longint;
begin
  tri.Clear;
  tri.UseMaterials:=UseMaterials; tri.UseColors:=UseColors;
  DivideTriangles;
  tri.fCount:=fCount; tri.vCount:=vCount;
  tri.groups:=groups; tri.mCount:=mCount; tri.bCount:=bCount;
  setlength(tri.mat, tri.mCount);
  setlength(tri.grp, groups);
  setlength(tri.bone, bCount);
  for i:=0 to vCount-1 do begin
    tri.va[i]:=va[i]; tri.na[i]:=na[i]; tri.ta[i]:=ta[i];
  end;
  for i:=0 to fCount-1 do
    for j:=0 to 2 do
      if j<fa[i].count then tri.fa[i,j]:=fa[i].index[j]
      else tri.fa[i,j]:=fa[i].index[0];
  for i:=0 to mCount-1 do tri.mat[i]:=mat[i];
  for i:=0 to groups-1 do tri.grp[i]:=grp[i];
  for i:=0 to bCount-1 do tri.bone[i]:=bone[i];
end;

procedure TPolyModel.AssignTo(w3d: TModelW3D);
var i, j: longint;
begin
  w3d.Clear;
  w3d.UseMaterials:=UseMaterials; w3d.UseColors:=UseColors;
  w3d.fCount:=fCount; w3d.vCount:=vCount;
  w3d.groups:=groups; w3d.mCount:=mCount; w3d.bCount:=bCount;
  setlength(w3d.mat, w3d.mCount);
  setlength(w3d.grp, groups);
  setlength(w3d.bone, bCount);
  for i:=0 to vCount-1 do w3d.va[i]:=va[i];
  for i:=0 to fCount-1 do begin
    setlength(w3d.fa[i].v, fa[i].count);
    setlength(w3d.fa[i].n, fa[i].count);
    setlength(w3d.fa[i].uv, fa[i].count);
    w3d.fa[i].Count:=fa[i].count;
    for j:=0 to fa[i].count-1 do begin
      w3d.fa[i].v[j]:=fa[i].index[j];
      w3d.fa[i].n[j]:=na[fa[i].index[j]];
      w3d.fa[i].uv[j]:=ta[fa[i].index[j]];
    end;
  end;
  for i:=0 to mCount-1 do w3d.mat[i]:=mat[i];
  for i:=0 to groups-1 do begin
    w3d.grp[i]:=grp[i];
    for j:=grp[i].first to longint(grp[i].first)+grp[i].count-1 do begin
      w3d.fa[j].SmoothGroup:=i;
      w3d.fa[j].MatIndex:=grp[i].matIndex;
    end;
  end;
  for i:=0 to bCount-1 do w3d.bone[i]:=bone[i];
end;

procedure TPolyModel.AssignTo(vm: TVertexModel);
var i, j: longint;
begin
  vm.Clear;
  DivideTriangles;
  setlength(vm.fa, fCount);
  setlength(vm.va, fCount);
  setlength(vm.grp, groups);
  setlength(vm.bone, bCount);
  for i:=0 to vCount-1 do begin
    vm.va[i].v:=va[i];
    vm.va[i].n:=na[i];
    vm.va[i].uv:=ta[i];
  end;
  for i:=0 to fCount-1 do
    for j:=0 to 2 do
      if j<fa[i].count then vm.fa[i,j]:=fa[i].index[j]
      else vm.fa[i,j]:=fa[i].index[0];
  for i:=0 to groups-1 do vm.grp[i]:=grp[i];
  for i:=0 to bCount-1 do vm.bone[i]:=bone[i];
end;

procedure TPolyModel.Clear;
begin
  inherited Clear;
  fCount:=0; setlength(fa, 0);
end;

procedure TPolyModel.DivideTriangles;
var gIndex: array of word;

  procedure SubDivideTriangles(const fIndex: longint);
  var i, n: longint; pa: array[0..2] of Word;
  begin
    if fa[fIndex].Count>3 then begin
      pa[0]:=fa[fIndex].index[0];
      for i:=1 to fa[fIndex].Count-3 do begin
        pa[1]:=fa[fIndex].index[i+1]; pa[2]:=fa[fIndex].index[i+2];
        n:=AddFace(pa, 3, true);
        setlength(gIndex, fCount);
        gIndex[n]:=gIndex[fIndex];
      end;
      fa[fIndex].Count:=3; setlength(fa[fIndex].index, 3);
    end;
  end;

var i, j, n: longint;
begin
  setlength(gIndex, fCount);
  if groups=0 then begin
    groups:=1;
    grp[0].first:=0; grp[0].count:=fCount;
    grp[0].matIndex:=0; grp[0].visible:=true;
  end;
  for j:=0 to groups-1 do
    for i:=grp[j].first to longint(grp[j].first+grp[j].count)-1 do
      gIndex[i]:=j;
  for i:=fCount-1 downto 0 do
    SubDivideTriangles(i);

  // Sort faces by groups
  for i:=0 to fCount-2 do
    for j:=i+1 to fCount-1 do
      if gIndex[i]>gIndex[j] then begin
        switchvar(@gIndex[i], @gIndex[j], sizeof(word));
        switchvar(@fa[i], @fa[j], sizeof(TPolyFaceIndices));
      end;

  // Calculate group contents
  if fCount>0 then begin
    n:=gIndex[0]; grp[0].first:=0; grp[0].count:=0;
    for i:=0 to fCount-1 do begin
      if gIndex[i]<>n then begin
        n:=gIndex[i]; grp[n].first:=i;
        grp[n].count:=1;
      end else
        inc(grp[n].count);
    end;
  end;
end;

function TPolyModel.GetTangent(const fIndex: word): TVector;
begin
  with fa[fIndex] do
    if count>2 then
      result:=Norm(Tangent(va[index[0]], va[index[(count-1) div 2]],
        va[index[count-1]]))
    else if count=2 then
      result:=Norm(CrossProduct(va[index[0]], va[index[1]]))
    else
      result:=Vector(1,0,0);
end;

procedure TPolyModel.LoadFromFile(filename: string);
var ext: string;
begin
  FixPath(filename);
  ext:=lowercase(extractfileext(filename));
  if ext='.w3d' then LoadFromW3D(filename)
  else if ext='.obj' then LoadFromOBJ(filename)
  else if ext='.ms3d' then LoadFromMS3D(filename);
end;

procedure TPolyModel.LoadFromMS3D(filename: string);
var tri: TTriModel;
begin
  tri:=TTriModel.Create;
  tri.LoadFromMS3D(filename);
  tri.AssignTo(self);
  tri.Free;
end;

procedure TPolyModel.LoadFromOBJ(filename: string);
  function IndexMatch(f1, f2, index: word): boolean;
  var i1,i2: longint;
  begin
    i1:=fa[f1].index[index]; i2:=fa[f2].index[index];
    result:=VectorMatch(va[i1], va[i2])
      and VectorMatch(na[i1], na[i2]) and VectorMatch(ta[i1], ta[i2]);
  end;

var F: TextFile; s: string; sa: array[0..127] of string;
    sa2: array[0..2] of string;
    i,j,k,n, ttCount,vvCount,nnCount: longint; //curS: integer;
    noTex: boolean;
    tt: array of TVector2f;
    vv, nn: array of TVector;
begin
  FixPath(filename);
  if not fileexists(filename) then begin
    nxSetError(format('File "%s" not found.',[filename])); exit;
  end;
  Clear; UseMaterials:=false; UseColors:=false;
  assignfile(F, filename); reset(F);
  ttCount:=0; vvCount:=0; nnCount:=0; //curS:=1;
  groups:=1; grp[0].count:=0;
  grp[0].matIndex:=-1;
  repeat
    readln(F, s); s:=trim(s);
    if s<>'' then begin
      k:=readstrings(s, ' ', sa);
      if (sa[0]='v') and (k>3) then begin
        vvCount:=vvCount+1; setlength(vv, vvCount);
        with vv[vvCount-1] do begin
          x:=strtofloat(sa[1])*0.01;
          y:=strtofloat(sa[2])*0.01;
          z:=strtofloat(sa[3])*0.01;
        end;
      end else if (sa[0]='vt') and (k>2) then begin
        inc(ttCount); setlength(tt, ttCount);
        tt[ttCount-1].x:=strtofloat(sa[1]);
        tt[ttCount-1].y:=strtofloat(sa[2]);
      end else if (sa[0]='vn') and (k>3) then begin
        nnCount:=nnCount+1; setlength(nn, nnCount);
        with nn[nnCount-1] do begin
          x:=strtofloat(sa[1]);
          y:=strtofloat(sa[2]);
          z:=strtofloat(sa[3]);
        end;
      end else if (sa[0]='f') and (k>3) and (vvCount>0) then begin
        fCount:=fCount+1;
        inc(grp[groups-1].count);
        with fa[fCount-1] do begin
          count:=k-1; setlength(index, count);
          noTex:=pos('//', s)>0;
          for i:=0 to count-1 do begin
            k:=readstrings(sa[1+i], '/', sa2);
            vCount:=vCount+1;
            index[i]:=vCount-1;
            n:=strtoint(sa2[0])-1;
            if n<vvCount then va[index[i]]:=vv[n]
            else va[index[i]]:=vv[0];
            if (k>0) and (not noTex) and (ttCount>0) then begin
              n:=strtoint(sa2[1])-1;
              if n<ttCount then ta[index[i]]:=tt[n]
              else ta[index[i]]:=tt[0];
            end;
            if k=3 then begin
              n:=strtoint(sa2[2])-1;
              if n<nnCount then na[index[i]]:=nn[n]
              else na[index[i]]:=nn[0];
            end else if (k=2) and NoTex then begin
              n:=strtoint(sa2[1])-1;
              if n<nnCount then na[index[i]]:=nn[n]
              else na[index[i]]:=nn[0];
            end;
            for j:=0 to fCount-2 do
              if (fa[j].count=fa[fCount-1].count) then
                if IndexMatch(j, fCount-1, i) then begin
                  fa[fCount-1].index[i]:=fa[j].index[i];
                  vCount:=vCount-1; break;
                end;
          end;
        end;
      end else if (sa[0]='g') and (k>1) then begin
        if grp[groups-1].count>0 then begin
          groups:=groups+1;
          grp[groups-1].first:=fCount;
          grp[groups-1].count:=0;
          grp[groups-1].matIndex:=-1;
          grp[groups-1].visible:=true;
        end;
      {end else if (sa[0]='s') and (k>1) then begin
        // Smoothing groups. Not used if normals are read from file,
        // but more important if autogenerating them.
        // Messes up grouping in other cases.
        n:=strtointdef(sa[1], 0)-1;
        if (n<>curS) and (grp[groups-1].count>0) then begin
          groups:=groups+1;
          grp[groups-1].first:=fCount;
          grp[groups-1].count:=0;
          grp[groups-1].matIndex:=-1;
        end;
        curS:=n;}

      //end else if sa[0]='mtllib' then begin
        // Load material file...

      end else if (sa[0]='usemtl') and (k>1) then begin
        // Set material for group
        if grp[groups-1].count>0 then begin
          groups:=groups+1;
          grp[groups-1].first:=fCount;
          grp[groups-1].count:=0;
          grp[groups-1].matIndex:=-1;
          grp[groups-1].visible:=true;
        end;
      end;
    end;
  until eof(F);
  closefile(F);
  setlength(tt, 0); setlength(vv, 0);
  //MakeNormals; // If skip all 'vn', then autogenerate normals
end;

procedure TPolyModel.LoadFromW3D(filename: string; obj: longint);
var w3d: TModelW3D;
begin
  w3d:=TModelW3D.Create;
  w3d.LoadFromFile(filename, obj);
  w3d.AssignTo(self);
  w3d.Free;
end;

procedure TPolyModel.MakeNormals;
var preTangent: array of TVector;
  procedure MakeGrpNormals(const gIndex: word);
  var f, i: longint;
  begin
    for f:=longint(grp[gIndex].first + grp[gIndex].count)-1 downto
           grp[gIndex].first do
      for i:=0 to fa[f].count-1 do
        na[fa[f].index[i]]:=VectorAdd(na[fa[f].index[i]], preTangent[f]);
  end;
var i: longint;
begin
  setlength(preTangent, fCount);
  for i:=0 to fCount-1 do preTangent[i]:=GetTangent(i);
  fillchar(na[0], vCount*sizeof(na[0]), 0);
  for i:=0 to groups-1 do MakeGrpNormals(i);
  for i:=0 to vCount-1 do na[i]:=Norm(na[i]);
end;

function TPolyModel.rayIntersect(const rayStart, rayDir: TVector; findClosest: boolean; const intersect: PVector; const normal: PVector): longint; stdcall;
var g, i: longint; d: single; nearest: single;
    vI,vN: TVector;
begin
  result:=-1; nearest:=-2;
  for g:=0 to groups-1 do
    if grp[g].visible then with grp[g] do
      for i:=first to longint(first)+Count-1 do
        if RayPolyIntersect(rayStart, RayDir,
           @fa[i].index[0], fa[i].count,
           false, intersect, normal) then begin
          if (not findClosest) or (intersect=nil) then begin
            result:=i; exit;
          end else begin
            d:=VectorDist(rayStart, intersect^);
            if (d<nearest) or (nearest<-1) then begin
              nearest:=d; vI:=intersect^;
              if normal<>nil then vN:=normal^;
              result:=i;
            end;
          end;
        end;
  if nearest>-1 then begin
    intersect^:=vI;
    if normal<>nil then normal^:=vN;
  end;
end;

function TPolyModel.rayIntersect(rayStart, rayDir: TVector; findClosest: boolean; const position: TVector; rotation: TMatrix; const intersect: PVector; const normal: PVector): longint; stdcall;
begin
  // Correct ray
  rayDir:=Multiply(rayDir, Invert(rotation));
  // Correct position
  SetVector(rotation, position, 3);
  rotation:=invert(rotation);
  rayStart:=Multiply(rayStart, rotation);
  result:=rayIntersect(rayStart, rayDir, findClosest, intersect,
    normal);
end;

// Credits to Dan (PascalGameDev forums)
function TPolyModel.RayPolyIntersect(const rayStart, rayDirection: TVector; const vi: PWordArray; Count: longint; BothSided: boolean; intersect: PVector; intersectNormal: PVector): Boolean; stdcall;
var d: Single; i: longint;
    Hit, nn, vx, vy: TVector;
    pi, pj: PVector;
    Hit2, pi2, pj2: TVector2f;
begin
  Result:=False;
  if Count<3 then Exit; //no need to proceed if we have less than 3 points.
  vx:=Norm(VectorSub(va[vi[Count div 3]], va[vi[0]]));
  nn:=Norm(CrossProduct(vx, VectorSub(va[vi[Count*2 div 3]], va[vi[0]])));
  // Exit if ray is coming from behind the triangle
  if (not BothSided) and (Dot(nn, rayDirection)>0) then exit;
  d:=RayPlaneIntersect(rayStart, rayDirection, va[vi[0]], nn,
     intersect);
  if d>=0 then Hit:=VectorAdd(rayStart, nxMath3D.Scale(rayDirection, d))
  else Exit; // the ray is parallel to the polygons plane. quit
  vy:=CrossProduct(nn, vx); // Don't need to norm(), length is 1
  // vx and vy are the 2d space projection vectors.
  Hit2:=Vector2f(Dot(vx, Hit), -Dot(vy, Hit));
  // and that's how you project a point to 2d space
  // and finally we're checking if the intersection point is inside the polygon.
  pj:=@va[vi[Count-1]];
  pj2:=Vector2f(Dot(vx, pj^), -Dot(vy, pj^));
  for i:=0 to Count-1 do begin
    pi:=@va[vi[i]];
    pi2:=Vector2f(Dot(vx, pi^), -Dot(vy, pi^));
    if ( ((pi2.y<=Hit2.y) and (Hit2.y<pj2.y)) or
         ((pj2.y<=Hit2.y) and (Hit2.y<pi2.y)) ) and (
       Hit2.x < (pj2.x-pi2.x)*(Hit2.y-pi2.y)/(pj2.y-pi2.y)+pi2.x
       ) then
    Result:=not Result;
    pj:=pi;
    pj2:=Vector2f(Dot(vx, pj^), -Dot(vy, pj^));
  end;
  if intersectNormal<>nil then intersectNormal^:=nn;
end;

procedure TPolyModel.SaveToFile(filename: string);
var ext: string;
begin
  FixPath(filename);
  ext:=lowercase(extractfileext(filename));
  if ext='.w3d' then SaveToW3D(filename)
  else if ext='.obj' then SaveToOBJ(filename);
end;

procedure TPolyModel.SaveToOBJ(filename: string);
var F: TextFile; g,i,j,n,tri,poly: longint; s: string;
    //ffirst: array of word; vIndex: longint;
begin
  assignfile(F, filename); rewrite(F);
  writeln(F, '# OBJ exported by nxPascal');

  {setlength(ffirst, fCount); vIndex:=1;
  for g:=0 to groups-1 do begin
    writeln(F);
    n:=0;
    for i:=grp[g].first to longint(grp[g].first)+grp[g].count-1 do begin
      ffirst[i]:=vIndex;
      for j:=0 to fa[i].Count-1 do
        writeln(F, format('v %.4f %.4f %.4f', [va[fa[i].index[j]].x*100,
          va[fa[i].index[j]].y*100, va[fa[i].index[j]].z*100]));
      inc(vIndex, fa[i].Count);
      inc(n, fa[i].Count);
    end;
    writeln(F, '# ', n, ' vertices');
    writeln(F);
    n:=0;
    for i:=grp[g].first to longint(grp[g].first)+grp[g].count-1 do begin
      for j:=0 to fa[i].Count-1 do
        writeln(F, format('vn %.4f %.4f %.4f',
          [na[fa[i].index[j]].x, na[fa[i].index[j]].y, na[fa[i].index[j]].z]));
      inc(n, fa[i].Count);
    end;
    writeln(F, '# ', n, ' vertex normals');
    writeln(F);
    n:=0;
    for i:=grp[g].first to longint(grp[g].first)+grp[g].count-1 do begin
      for j:=0 to fa[i].Count-1 do
        writeln(F, format('vt %.4f %.4f %.4f',
          [ta[fa[i].index[j]].x, ta[fa[i].index[j]].y, 0.0]));
      inc(n, fa[i].Count);
    end;
    writeln(F, '# ', n, ' texture coords');
    writeln(F);
    writeln(F, 'g grp', g);
    writeln(F, 's ', g+1);
    tri:=0; poly:=0;
    for i:=grp[g].first to longint(grp[g].first)+grp[g].count-1 do begin
      s:='';
      for j:=0 to fa[i].count-1 do begin
        n:=ffirst[i]+j;
        if j>0 then s:=s+' ';
        s:=s+format('%d/%d/%d', [n, n, n]);
      end;
      writeln(F, 'f ', s);
      if fa[i].count=3 then inc(tri)
      else inc(poly);
    end;
    writeln(F, '# ', poly, ' polygons - ', tri, ' triangles');
  end;}

  writeln(F);
  for i:=0 to vCount-1 do
    writeln(F, format('v %.4f %.4f %.4f', [va[i].x*100,
      va[i].y*100, va[i].z*100]));
  writeln(F, '# ', vCount, ' vertices');
  writeln(F);
  for i:=0 to vCount-1 do
    writeln(F, format('vn %.4f %.4f %.4f',
      [na[i].x, na[i].y, na[i].z]));
  writeln(F, '# ', vCount, ' vertex normals');
  writeln(F);
  for i:=0 to vCount-1 do
    writeln(F, format('vt %.4f %.4f %.4f',
      [ta[i].x, ta[i].y, 0.0]));
  writeln(F, '# ', vCount, ' texture coords');
  for g:=0 to groups-1 do begin
    writeln(F);
    writeln(F, 'g grp', g);
    writeln(F, 's ', g+1);
    tri:=0; poly:=0;
    for i:=grp[g].first to longint(grp[g].first)+grp[g].count-1 do begin
      s:='';
      for j:=0 to fa[i].count-1 do begin
        n:=fa[i].index[j]+1;
        if j>0 then s:=s+' ';
        s:=s+format('%d/%d/%d', [n, n, n]);
      end;
      writeln(F, 'f ', s);
      if fa[i].count=3 then inc(tri)
      else inc(poly);
    end;
    writeln(F, '# ', poly, ' polygons - ', tri, ' triangles');
  end;

  closefile(F);
  //setlength(ffirst, 0);
end;

procedure TPolyModel.SaveToW3D(filename: string);
var w3d: TModelW3D;
begin
  w3d:=TModelW3D.Create;
  AssignTo(w3d);
  w3d.SaveToFile(filename);
  w3d.Free;
end;

procedure TPolyModel.SetfCount(n: longint);
begin
  FfCount:=n;
  if n>100 then n:=(n div 100+1)*100;
  if n<>fCount2 then begin
    fCount2:=n; setlength(fa, fCount2);
  end;
end;

{ TModelMS3D }

constructor TModelMS3D.Create(filename: string);
var F: TFileStream; i: longint;
begin
  FixPath(filename);
  F:=TFileStream.Create(filename,fmOpenRead);
  vCount:=0; fCount:=0; F.Position:=14;
  F.Read(vCount, 2); setlength(va, vCount);
  F.Read(va[0],vCount*sizeof(TMS3DVertex));
  F.Read(fCount, 2); setlength(fa, fCount);
  F.Read(fa[0],fCount*sizeof(TMS3DTri));
  F.Read(groups, 2); setlength(grp, groups);
  for i:=0 to groups-1 do with grp[i] do begin
    F.Read(flags, 1);  F.Read(name, 32);  F.Read(numtriangles, 2);
    setlength(triangleIndices, numtriangles);
    F.Read(triangleIndices[0], numtriangles*2);
    F.Read(materialIndex, 1);
  end;
  F.Read(mats, 2); setlength(mat, mats);
  F.Read(mat[0], mats*sizeof(TMS3DMaterial));
  F.Read(AnimationFPS, 4); F.Read(CurrentTime, 4); F.Read(TotalFrames, 4);
  F.Read(joints,2); setlength(joint,joints);
  for i:=0 to joints-1 do with joint[i] do begin
    F.Read(flags, 1);  F.Read(name, 32);  F.Read(parentName, 32);
    F.Read(rotation[0], 4*3); F.Read(position[0], 4*3);
    F.Read(numKeyFramesRot, 2); F.Read(numKeyFramesTrans, 2);
    setlength(keyFramesRot,numKeyFramesRot);
    setlength(keyFramesTrans,numKeyFramesTrans);
    F.Read(keyFramesRot[0], numKeyFramesRot*sizeof(TMS3DKeyframe_rot));
    F.Read(keyFramesTrans[0], numKeyFramesTrans*sizeof(TMS3DKeyframe_pos));
  end;
  F.Free;
end;

procedure TModelMS3D.AssignToTri(tri: TTriModel; FreeSelf: boolean);
var i,j,g,k,n,faceN: longint;
begin
  tri.Clear;
  tri.fCount:=fCount; tri.vCount:=fCount*3;
  tri.groups:=groups; tri.mCount:=mats;
  setlength(tri.grp, groups);
  setlength(tri.mat, mats);
  n:=0; faceN:=0;
  for g:=0 to groups-1 do begin
    tri.grp[g].first:=faceN;
    tri.grp[g].count:=grp[g].numtriangles;
    tri.grp[g].matIndex:=grp[g].materialIndex;
    tri.grp[g].visible:=true;
    for k:=0 to grp[g].numtriangles-1 do begin
      for j:=0 to 2 do begin
        i:=grp[g].triangleIndices[k];
        tri.fa[i,j]:=n;
        tri.va[n].x:=va[fa[i].v[j]].x;
        tri.va[n].y:=va[fa[i].v[j]].y;
        tri.va[n].z:=va[fa[i].v[j]].z;
        tri.na[n].x:=fa[i].n[j,0];
        tri.na[n].y:=fa[i].n[j,1];
        tri.na[n].z:=fa[i].n[j,2];
        tri.ta[n].x:=fa[i].s[j];
        tri.ta[n].y:=fa[i].t[j];
        inc(n);
      end;
      inc(faceN);
    end;
  end;
  for i:=0 to mats-1 do begin
    tri.mat[i].texture:=mat[i].texture;
  end;
  if FreeSelf then Free;
end;

{ TModelW3D }

procedure TModelW3D.SetfCount(n: longint);
begin
  FfCount:=n;
  if n>100 then n:=(n div 100+1)*100;
  if n<>fCount2 then begin
    fCount2:=n; setlength(fa, fCount2);
  end;
end;

procedure TModelW3D.LoadFromFile(filename: string; obj: longint);
var F: TextFile; s: string; sa: array[0..127] of string;
    i,j,k,objCount,o,_vCount,_fCount,_nCount: longint;
    gIndex: array of word; i2,j2: longint;
    tmpN: array of TVector;
    vPos, fPos: longint;
    oPos: TVector; isOld: boolean;
begin
  FixPath(filename);
  if not fileexists(filename) then begin
    nxSetError(format('File "%s" not found.',[filename])); exit;
  end;
  Clear; UseMaterials:=true; UseColors:=true;
  assignfile(F, filename); reset(F);
  readln(F, s); // Version (or object count in old version)
  isOld:=pos('version', lowercase(s))<1;
  if isOld then begin
    closefile(F); exit; // Old format not supported!
  end else
    readln(F, objCount); // objects
  readln(F, i); // materials
  mCount:=i;
  vPos:=0; fPos:=0;
  for o:=0 to objCount-1 do begin
    readln(F); // Skip object name
    readln(F, _vCount, _fCount, _nCount);
    if obj<0 then begin
      vCount:=vCount+_vCount; fCount:=fCount+_fCount;
      setlength(tmpN, _nCount);
    end else if o=obj then begin
      vCount:=_vCount; fCount:=_fCount;
      setlength(tmpN, _nCount);
    end;
    readln(F, s); ReadStrings(s, ' ', sa);
    oPos.x:=strtofloat(sa[0])*0.01;
    oPos.y:=strtofloat(sa[1])*0.01;
    oPos.z:=strtofloat(sa[2])*0.01;
    // (Rotation angles ignored)
    for i:=vPos to vPos+_vCount-1 do
      if (o=obj) or (obj<0) then begin
        readln(F, va[i].x, va[i].y, va[i].z);
        va[i].x:=va[i].x*0.01;
        va[i].y:=va[i].y*0.01;
        va[i].z:=va[i].z*0.01;
        if obj<0 then va[i]:=VectorAdd(va[i], oPos);
      end else readln(F);

    for i:=0 to _nCount-1 do
      if (o=obj) or (obj<0) then readln(F, tmpN[i].x, tmpN[i].y, tmpN[i].z)
      else readln(F);

    for i:=fPos to fPos+_fCount-1 do
      if (o=obj) or (obj<0) then begin
        readln(F, s); ReadStrings(s, ' ', sa);
        fa[i].MatIndex:=strtoint(sa[0]);
        fa[i].Count:=strtoint(sa[1]);
        fa[i].SmoothGroup:=strtoint(sa[2]);
        setlength(fa[i].v, fa[i].Count);
        setlength(fa[i].n, fa[i].Count);
        setlength(fa[i].uv, fa[i].Count);
        for j:=0 to fa[i].Count-1 do
          fa[i].v[j]:=vPos+strtoint(sa[j+3]);

        readln(F, s); ReadStrings(s, ' ', sa);
        for j:=0 to fa[i].Count-1 do
          fa[i].n[j]:=tmpN[strtoint(sa[j])];

        readln(F, s); ReadStrings(s, ' ', sa);
        for j:=0 to fa[i].Count-1 do begin
          fa[i].uv[j].x:=strtofloat(sa[j*2]);
          fa[i].uv[j].y:=strtofloat(sa[j*2+1]);
        end;
      end else begin
        readln(F); readln(F); readln(F);
      end;
    if obj<0 then begin
      vPos:=vPos+_vCount; fPos:=fPos+_fCount;
    end;
  end;
  setlength(tmpN, 0);

  for i:=0 to mCount-1 do begin
    mat[i].addMode:=false; mat[i].transparent:=false;
    mat[i].texIndex:=-1;
    readln(F, mat[i].name); // Material name
    readln(F, mat[i].texture);
    readln(F, s); k:=ReadStrings(s, ' ', sa);
    mat[i].color.a:=round(strtofloat2(sa[0])*255);
    mat[i].shininess:=strtofloat2(sa[1]);
    if k>2 then begin
      mat[i].transparent:=true;
      readln(F); // Skip alpha file
    end;
    readln(F); // Skip ambient

    readln(F, s); ReadStrings(s, ' ', sa);
    mat[i].color:=RGBA(round(strtofloat2(sa[0])*255),
      round(strtofloat2(sa[1])*255), round(strtofloat2(sa[2])*255),
      mat[i].color.a);

    readln(F, s); ReadStrings(s, ' ', sa);
    mat[i].specular:=max(max(strtofloat2(sa[0]), strtofloat2(sa[1])),
      strtofloat2(sa[2]));
  end;
  closefile(F);

  if fCount<1 then exit;

  // Make groups
  setlength(gIndex, fCount); groups:=1;
  grp[0].first:=0; gIndex[0]:=0;
  grp[0].visible:=true;
  for i:=1 to fCount-1 do
    with fa[i] do begin
      k:=-1;
      for j:=groups-1 downto 0 do
        if (MatIndex=fa[grp[j].first].MatIndex) and
           (SmoothGroup=fa[grp[j].first].SmoothGroup) then begin
          k:=j; break;
        end;
      if k=-1 then begin
        groups:=groups+1;
        grp[groups-1].first:=i;
        grp[groups-1].matIndex:=MatIndex;
        grp[groups-1].visible:=true;
        gIndex[i]:=groups-1;
      end else
        gIndex[i]:=k;
    end;

  // Sort faces by groups
  for i:=0 to fCount-2 do
    for j:=i+1 to fCount-1 do
      if gIndex[i]>gIndex[j] then begin
        switchvar(@gIndex[i], @gIndex[j], sizeof(gIndex[0]));
        switchvar(@fa[i], @fa[j], sizeof(fa[0]));
      end;

  // Calculate group contents
  if fCount>0 then begin                
    k:=gIndex[0]; grp[0].first:=0; grp[0].count:=0;
    for i:=0 to fCount-1 do begin
      if gIndex[i]<>k then begin
        k:=gIndex[i]; grp[k].first:=i;
        grp[k].count:=1;
      end else
        inc(grp[k].count);
    end;
  end;

  // Duplicate vertices for different groups
  for i:=0 to fCount-2 do
    for j:=i+1 to fCount-1 do
      if gIndex[i]<>gIndex[j] then
        for i2:=0 to fa[i].Count-1 do begin
          for j2:=0 to fa[j].Count-1 do
            if fa[i].v[i2]=fa[j].v[j2] then begin
              vCount:=vCount+1;
              va[vCount-1]:=va[fa[i].v[i2]];
              fa[i].v[i2]:=vCount-1;
            end;
    end;

  // *** TEST CODE ***
  //for i:=0 to fCount-1 do
  //  for j:=0 to fa[i].Count-1 do
  //    va[fa[i].v[j]]:=VectorAdd(va[fa[i].v[j]], nxMath3D.scale(fa[i].n[j], 0.05));

end;

destructor TModelW3D.Destroy;
begin
  setlength(va,0); setlength(fa,0);
  inherited;
end;

procedure TModelW3D.AssignTo(const poly: TPolyModel);
var i, j: longint;
begin
  DoTextureCorrection;

  poly.Clear;
  poly.UseColors:=UseColors; poly.UseMaterials:=UseMaterials;
  poly.vCount:=vCount; poly.fCount:=fCount;
  poly.mCount:=mCount; poly.bCount:=bCount;
  poly.groups:=groups;
  setlength(poly.mat, mCount);
  setlength(poly.grp, groups);
  setlength(poly.bone, bCount);
  for i:=0 to vCount-1 do poly.va[i]:=va[i];
  for i:=0 to fCount-1 do begin
    poly.fa[i].count:=fa[i].Count;
    setlength(poly.fa[i].index, fa[i].Count);
    for j:=0 to fa[i].Count-1 do begin
      poly.fa[i].index[j]:=fa[i].v[j];
      poly.na[fa[i].v[j]]:=fa[i].n[j];
      poly.ta[fa[i].v[j]]:=fa[i].uv[j];
    end;
  end;
  for i:=0 to mCount-1 do poly.mat[i]:=mat[i];
  for i:=0 to groups-1 do poly.grp[i]:=grp[i];
  for i:=0 to bCount-1 do poly.bone[i]:=bone[i];
end;

procedure TModelW3D.AssignTo(const tri: TTriModel);
var poly: TPolyModel;
begin
  poly:=TPolyModel.Create;
  AssignTo(poly);
  poly.AssignTo(tri);
  poly.Free;
end;

procedure TModelW3D.DoTextureCorrection;
var g, i: longint;

  function BalanceOK(const a, b: single): boolean;
  var j: longint;
  begin
    if ((a<=0) and (b>0)) or ((b<=0) and (a>0)) or
       ((a<1) and (b>=1)) or ((b<1) and (a>=1)) then begin
      result:=false;
      vCount:=vCount+fa[i].Count;
      for j:=0 to fa[i].Count-1 do begin
        va[vCount-fa[i].Count+j]:=va[fa[i].v[j]];
        fa[i].v[j]:=vCount-fa[i].Count+j;
      end;
    end else result:=true;
  end;

var t0, t1, t2: TVector2f; //UnFinishedFunction: boolean;
begin
  //EXIT; // This function does fix something, but brake others... or not

  // Go through each face and duplicate whole face vertices
  // if tex-coords loop over boundaries
  for g:=groups-1 downto 0 do
    with grp[g] do begin
      for i:=longint(first)+count-1 downto first do begin
        t0:=fa[i].uv[0];
        t1:=fa[i].uv[1];
        t2:=fa[i].uv[2];
        if BalanceOK(t0.x, t1.x) and BalanceOK(t0.x, t2.x) and
           BalanceOK(t1.x, t2.x) and BalanceOK(t0.y, t1.y) and
           BalanceOK(t0.y, t2.y) then BalanceOK(t1.y, t2.y);
      end;
    end;
end;

procedure TModelW3D.SaveToFile(filename: string);
var F: TextFile; i,j: longint; s: string;
begin
  try
    assignfile(F, filename); rewrite(F);
    writeln(F, 'Version: 1');
    writeln(F, 1); // Objects
    writeln(F, mCount); // Materials
    writeln(F, 'Object0'); // Object name
    writeln(F, format('%d %d %d',[vCount, fCount, vCount]));
    writeln(F, format('%.2f %.2f %.2f %.2f %.2f %.2f',
      [0.0, 0.0, 0.0, 0.0, 0.0, 0.0])); // position, rotation
    for i:=0 to vCount-1 do begin
      writeln(F,format('%f %f %f',[va[i].x*100, va[i].y*100, va[i].z*100]));
    end;
    for i:=0 to fCount-1 do
      for j:=0 to fa[i].Count-1 do
        na[fa[i].v[j]]:=fa[i].n[j];
    for i:=0 to vCount-1 do
      writeln(F,format('%f %f %f',[na[i].x, na[i].y, na[i].z]));
    for i:=0 to fCount-1 do
      with fa[i] do begin
        s:=format('%d %d %d',[matIndex, count, SmoothGroup]);
        for j:=0 to count-1 do s:=s+' '+inttostr(v[j]);
        writeln(F,s);
        s:=inttostr(v[0]);
        for j:=1 to count-1 do s:=s+' '+inttostr(v[j]);
        writeln(F, s);
        s:=format('%f %f', [uv[0].x, uv[0].y]);
        for j:=1 to count-1 do s:=s+format(' %f %f', [uv[j].x, uv[j].y]);
        writeln(F,s);
      end;
    for i:=0 to mCount-1 do
      with mat[i] do begin
        writeln(F, name);
        writeln(F, texture);
        s:=format('%.2f %.2f',[color.a/255, shininess]);
        writeln(F, s);
        s:=format('%.2f %.2f %2f',[1.0, 1.0, 1.0]);
        writeln(F, s); // Ambient
        with color do
          s:=format('%.2f %.2f %2f',[r/255, g/255, b/255]);
        writeln(F, s); // Diffuse
        s:=format('%.2f %.2f %2f',[specular, specular, specular]);
        writeln(F, s); // Specular
      end;
  finally
    closefile(F);
  end;  
end;

{ TTriModel }

procedure TTriModel.Clear;
begin
  inherited Clear;
  frames:=0; fCount:=0;
  setlength(fa, 0); setlength(frame, 0);
end;

procedure TTriModel.DoTextureCorrection;
{var g, i: longint;

  function BalanceOK(const a, b: single): boolean;
  var j: longint;
  begin
    if ((a<0) and (b>=0)) or ((b<0) and (a>=0)) or
       ((a<1) and (b>=1)) or ((b<1) and (a>=1)) then begin
      result:=false;
      vCount:=vCount+3;
      for j:=0 to 2 do begin
        va[vCount-3+j]:=va[fa[i, j]];
        na[vCount-3+j]:=na[fa[i, j]];
        ta[vCount-3+j]:=ta[fa[i, j]];
        fa[i, j]:=vCount-3+j;
      end;
    end else result:=true;
  end;

var t0, t1, t2: TVector2f; UnneededFunction: boolean;}
begin
  {EXIT; // Done in W3D loading, so not needed here

  // Go through each face. If texture U or V difference is above 0.5
  // then duplicate vertex
  for g:=groups-1 downto 0 do
    with grp[g] do begin
      for i:=longint(first)+count-1 downto first do begin
        t0:=ta[fa[i, 0]];
        t1:=ta[fa[i, 1]];
        t2:=ta[fa[i, 2]];
        if BalanceOK(t0.x, t1.x) and BalanceOK(t0.x, t2.x) and
           BalanceOK(t1.x, t2.x) and BalanceOK(t0.y, t1.y) and
           BalanceOK(t0.y, t2.y) then BalanceOK(t1.y, t2.y);
      end;
    end; }
end;

function TTriModel.FindNearest(const source: TVector; const nearest: PVector; const normal: PVector): longint;
var i: longint; p: TVector; d, nearD: single;
begin
  if fCount<1 then begin
    result:=-1; exit;
  end;
  result:=0;
  p:=NearTriPoint(@va[fa[0, 0]], @va[fa[0, 1]], @va[fa[0, 2]], @source);
  nearD:=VectorDist(source, p);
  if nearest<>nil then nearest^:=p;
  for i:=1 to fCount-1 do begin
    p:=NearTriPoint(@va[fa[i, 0]], @va[fa[i, 1]], @va[fa[i, 2]], @source);
    d:=VectorDist(source, p);
    if d<nearD then begin
      nearD:=d; result:=i;
      if nearest<>nil then nearest^:=p;
    end;
  end;
  if normal<>nil then normal^:=GetTangent(result);
end;

procedure TTriModel.FlipFaces;
var f, temp, i: longint;
begin
  for f:=0 to fCount-1 do begin
    temp:=fa[f, 1]; fa[f, 1]:=fa[f, 2]; fa[f, 2]:=temp;
  end;
  for i:=0 to vCount-1 do na[i]:=invert(na[i]);
end;

function TTriModel.GetTangent(const fIndex: word): TVector;
begin
  result:=Tangent(va[fa[fIndex,0]], va[fa[fIndex,1]], va[fa[fIndex,2]]);
end;

procedure TTriModel.LoadFromFile(filename: string);
var ext: string;
begin
  FixPath(filename);
  if (filename<>'') and (not FileExistsUTF8(filename)) then begin
    nxSetError(format('[TriModel] File "%s" not found.',[filename]));
    exit;
  end;
  ext:=lowercase(extractfileext(filename));
  if ext='.w3d' then LoadFromW3D(filename)
  else if ext='.obj' then LoadFromOBJ(filename)
  else if ext='.ms3d' then LoadFromMS3D(filename);
end;

procedure TTriModel.LoadFromMS3D(filename: string);
var ms3d: TModelMS3D;
begin
  UseMaterials:=true; UseColors:=false;
  ms3d:=TModelMS3D.Create(filename);
  ms3d.AssignToTri(self);
  ms3d.Free;
  DoTextureCorrection;
end;

procedure TTriModel.LoadFromOBJ(filename: string);
var poly: TPolyModel;
begin
  poly:=TPolyModel.Create;
  poly.LoadFromOBJ(filename);
  poly.AssignTo(self);
  poly.Free;
  DoTextureCorrection;
end;

procedure TTriModel.LoadFromW3D(filename: string; obj: longint);
var w3d: TModelW3D;
begin
  w3d:=TModelW3D.Create;
  w3d.LoadFromFile(filename, obj);
  w3d.AssignTo(self);
  w3d.Free;
  DoTextureCorrection;
end;

procedure TTriModel.MakeNormals;
var preTangent: array of TVector;

  procedure MakeGrpNormals(const gIndex: word);
  var f, i: longint;
  begin
    for f:=longint(grp[gIndex].first)+grp[gIndex].count-1 downto
           grp[gIndex].first do
      for i:=0 to 2 do
        na[fa[f, i]]:=VectorAdd(na[fa[f, i]], preTangent[f]);
  end;

var i: longint;
begin
  setlength(preTangent, fCount);
  fillchar(na[0], vCount*sizeof(na[0]), 0);
  for i:=0 to fCount-1 do preTangent[i]:=GetTangent(i);
  for i:=0 to groups-1 do MakeGrpNormals(i);
  for i:=0 to vCount-1 do na[i]:=Norm(na[i]);
end;

// Returns >= 0 if intersects, it is index of the face
function TTriModel.rayIntersect(const rayStart, rayDir: TVector; findClosest: boolean; const intersect: PVector; const normal: PVector): longint; stdcall;
var g, i: longint; d: single; nearest: single;
    vI,vN: TVector;
begin
  result:=-1; nearest:=-2;
  for g:=0 to groups-1 do
    if grp[g].visible then with grp[g] do
      for i:=first to longint(first)+Count-1 do

        //if RayTriangleIntersect(rayStart, RayDir,
        //   va[fa[i][0]], va[fa[i][1]], va[fa[i][2]],
        //   false, intersect, normal) then begin
        if RayTriangleIntersect(rayStart, RayDir,
           va[fa[i][0]], va[fa[i][1]], va[fa[i][2]],
           false, intersect, normal) then begin

          if (not findClosest) or (intersect=nil) then begin
            result:=i; exit;
          end else begin
            d:=VectorDist(rayStart, intersect^);
            if (d<nearest) or (nearest<-1) then begin
              nearest:=d; vI:=intersect^;
              if normal<>nil then vN:=normal^;
              result:=i;
            end;
          end;
        end;
  if nearest>-1 then begin
    intersect^:=vI;
    if normal<>nil then normal^:=vN;
  end;
end;

// Returns >= 0 if intersects, it is index of the face
// includes parameter position for model
function TTriModel.rayIntersect(const rayStart, rayDir: TVector; findClosest: boolean; const position: TVector; const intersect: PVector; const normal: PVector): longint; stdcall;
begin
  result:=rayIntersect(VectorSub(rayStart, position), rayDir, findClosest, intersect, normal);
end;

// Returns >= 0 if intersects, it is index of the face
// includes parameters position and rotation for model
function TTriModel.rayIntersect(rayStart, rayDir: TVector; findClosest: boolean; const position: TVector; rotation: TMatrix; const intersect: PVector; const normal: PVector): longint; stdcall;
begin
  // Correct ray
  rayDir:=Multiply(rayDir, Invert(rotation));
  // Correct position
  SetVector(rotation, position, 3);
  rotation:=invert(rotation);
  rayStart:=Multiply(rayStart, rotation);
  result:=rayIntersect(rayStart, rayDir, findClosest, intersect, normal);
end;

procedure TTriModel.RotateUV(_angle, centerX, centerY: single; group: integer);
var i, vc: longint; vi: TIndexArray;
begin
  GetGrpVertices(vi, vc, group);
  for i:=0 to vc-1 do
    nxMath.Rotate(ta[vi[i]].x, ta[vi[i]].y, _angle, centerX, centerY);
  setlength(vi, 0);
end;

procedure TTriModel.ScaleUV(x, y: single; group: integer);
var i, vc: longint; vi: TIndexArray;
begin
  GetGrpVertices(vi, vc, group);
  for i:=0 to vc-1 do begin
    ta[vi[i]].x:=ta[vi[i]].x*x;
    ta[vi[i]].y:=ta[vi[i]].y*y;
  end;
  setlength(vi, 0);
end;

procedure TTriModel.SaveToFile(filename: string);
var poly: TPolyModel;
begin
  poly:=TPolyModel.Create;
  AssignTo(poly);
  poly.SaveToFile(filename);
  poly.Free;
end;

procedure TTriModel.TranslateUV(x, y: single; group: integer);
var i, vc: longint; vi: TIndexArray;
begin
  GetGrpVertices(vi, vc, group);
  for i:=0 to vc-1 do begin
    ta[vi[i]].x:=ta[vi[i]].x+x;
    ta[vi[i]].y:=ta[vi[i]].y+y;
  end;
  setlength(vi, 0);
end;

procedure TTriModel.GetGrpVertices(out vi: TIndexArray; out vc: longint; group: integer);
var i, j, k, vc2: longint; exist: boolean;
begin
  vc:=0; vc2:=8; setlength(vi, vc2);
  with grp[group] do
    for i:=first to longint(first)+count-1 do
      for j:=0 to 2 do begin
        exist:=false;
        for k:=vc-1 downto 0 do
          if vi[k]=fa[i, j] then begin
            exist:=true; break;
          end;
        if not exist then begin
          inc(vc);
          if vc>vc2 then begin // Size array with increments of 8
            inc(vc2, 8); setlength(vi, vc2);
          end;
          vi[vc-1]:=fa[i, j];
        end;
      end;
end;

procedure TTriModel.SetfCount(n: longint);
begin
  FfCount:=n;
  if n>100 then n:=(n div 100+1)*100;
  if n<>fCount2 then begin
    fCount2:=n; setlength(fa, fCount2);
  end;
end;

constructor TTriModel.Create(filename: string);
begin
  Create;
  LoadFromFile(filename);
end;

constructor TTriModel.CreateCube(segments: integer);
var s: single; v_per_side: longint;
  procedure _FaceIndices(fn, n: longint); // 0 | /| 3
  begin                                   // 1 |/ | 2
    fa[fn  , 0]:=n+0; fa[fn  , 1]:=n+1; fa[fn  , 2]:=n+3;
    fa[fn+1, 0]:=n+3; fa[fn+1, 1]:=n+1; fa[fn+1, 2]:=n+2;
  end;
  procedure _Normals(n: longint; x, y, z: single);
  var i: longint;
  begin
    for i:=n to n+v_per_side-1 do begin
      na[i].x:=x; na[i].y:=y; na[i].z:=z;
    end;
  end;
  procedure _TexCoords(n, i, j: longint);
  begin
    ta[n+0].x:=(i+0)*s; ta[n+0].y:=(j+0)*s;
    ta[n+1].x:=(i+0)*s; ta[n+1].y:=(j+1)*s;
    ta[n+2].x:=(i+1)*s; ta[n+2].y:=(j+1)*s;
    ta[n+3].x:=(i+1)*s; ta[n+3].y:=(j+0)*s;
  end;
var i, j, f_per_side, n, fn, n2, fn2: longint;
begin
  Create;
  if segments<1 then segments:=1;
  f_per_side:=segments*segments*2; // 2 triangles per segment
  v_per_side:=segments*segments*4; // 4 vertex-index per segment
  fCount:=6*f_per_side; vCount:=6*v_per_side;
  s:=1/segments; groups:=6;
  for i:=0 to groups-1 do
    with grp[i] do begin
      count:=f_per_side; first:=i*f_per_side; matIndex:=0;
      visible:=true;
    end;

  // Front
  n:=0; fn:=0; _Normals(n, 0, 0, 1);
  for j:=0 to segments-1 do
    for i:=0 to segments-1 do begin
      n2:=n+(i+j*segments)*4; fn2:=fn+(i+j*segments)*2;
      _FaceIndices(fn2, n2); _TexCoords(n2, i, j);
      va[n2+0].x:=-0.5+(i+0)*s;
      va[n2+0].y:= 0.5-(j+0)*s;
      va[n2+0].z:= 0.5;
      va[n2+1].x:=-0.5+(i+0)*s;
      va[n2+1].y:= 0.5-(j+1)*s;
      va[n2+1].z:= 0.5;
      va[n2+2].x:=-0.5+(i+1)*s;
      va[n2+2].y:= 0.5-(j+1)*s;
      va[n2+2].z:= 0.5;
      va[n2+3].x:=-0.5+(i+1)*s;
      va[n2+3].y:= 0.5-(j+0)*s;
      va[n2+3].z:= 0.5;
    end;

  // Back
  n:=n+v_per_side; fn:=fn+f_per_side; _Normals(n, 0, 0, -1);
  for j:=0 to segments-1 do
    for i:=0 to segments-1 do begin
      n2:=n+(i+j*segments)*4; fn2:=fn+(i+j*segments)*2;
      _FaceIndices(fn2, n2); _TexCoords(n2, i, j);
      va[n2+0].x:= 0.5-(i+0)*s;
      va[n2+0].y:= 0.5-(j+0)*s;
      va[n2+0].z:=-0.5;
      va[n2+1].x:= 0.5-(i+0)*s;
      va[n2+1].y:= 0.5-(j+1)*s;
      va[n2+1].z:=-0.5;
      va[n2+2].x:= 0.5-(i+1)*s;
      va[n2+2].y:= 0.5-(j+1)*s;
      va[n2+2].z:=-0.5;
      va[n2+3].x:= 0.5-(i+1)*s;
      va[n2+3].y:= 0.5-(j+0)*s;
      va[n2+3].z:=-0.5;
    end;

  // Left
  n:=n+v_per_side; fn:=fn+f_per_side; _Normals(n, -1, 0, 0);
  for j:=0 to segments-1 do
    for i:=0 to segments-1 do begin
      n2:=n+(i+j*segments)*4; fn2:=fn+(i+j*segments)*2;
      _FaceIndices(fn2, n2); _TexCoords(n2, i, j);
      va[n2+0].x:=-0.5;
      va[n2+0].y:= 0.5-(j+0)*s;
      va[n2+0].z:=-0.5+(i+0)*s;
      va[n2+1].x:=-0.5;
      va[n2+1].y:= 0.5-(j+1)*s;
      va[n2+1].z:=-0.5+(i+0)*s;
      va[n2+2].x:=-0.5;
      va[n2+2].y:= 0.5-(j+1)*s;
      va[n2+2].z:=-0.5+(i+1)*s;
      va[n2+3].x:=-0.5;
      va[n2+3].y:= 0.5-(j+0)*s;
      va[n2+3].z:=-0.5+(i+1)*s;
    end;

  // Right
  n:=n+v_per_side; fn:=fn+f_per_side; _Normals(n, 1, 0, 0);
  for j:=0 to segments-1 do
    for i:=0 to segments-1 do begin
      n2:=n+(i+j*segments)*4; fn2:=fn+(i+j*segments)*2;
      _FaceIndices(fn2, n2); _TexCoords(n2, i, j);
      va[n2+0].x:=0.5;
      va[n2+0].y:=0.5-(j+0)*s;
      va[n2+0].z:=0.5-(i+0)*s;
      va[n2+1].x:=0.5;
      va[n2+1].y:=0.5-(j+1)*s;
      va[n2+1].z:=0.5-(i+0)*s;
      va[n2+2].x:=0.5;
      va[n2+2].y:=0.5-(j+1)*s;
      va[n2+2].z:=0.5-(i+1)*s;
      va[n2+3].x:=0.5;
      va[n2+3].y:=0.5-(j+0)*s;
      va[n2+3].z:=0.5-(i+1)*s;
    end;

  // Top
  n:=n+v_per_side; fn:=fn+f_per_side; _Normals(n, 0, 1, 0);
  for j:=0 to segments-1 do
    for i:=0 to segments-1 do begin
      n2:=n+(i+j*segments)*4; fn2:=fn+(i+j*segments)*2;
      _FaceIndices(fn2, n2); _TexCoords(n2, i, j);
      va[n2+0].x:=-0.5+(i+0)*s;
      va[n2+0].y:= 0.5;
      va[n2+0].z:=-0.5+(j+0)*s;
      va[n2+1].x:=-0.5+(i+0)*s;
      va[n2+1].y:= 0.5;
      va[n2+1].z:=-0.5+(j+1)*s;
      va[n2+2].x:=-0.5+(i+1)*s;
      va[n2+2].y:= 0.5;
      va[n2+2].z:=-0.5+(j+1)*s;
      va[n2+3].x:=-0.5+(i+1)*s;
      va[n2+3].y:= 0.5;
      va[n2+3].z:=-0.5+(j+0)*s;
    end;

  // Bottom
  n:=n+v_per_side; fn:=fn+f_per_side; _Normals(n, 0, -1, 0);
  for j:=0 to segments-1 do
    for i:=0 to segments-1 do begin
      n2:=n+(i+j*segments)*4; fn2:=fn+(i+j*segments)*2;
      _FaceIndices(fn2, n2); _TexCoords(n2, i, j);
      va[n2+0].x:=-0.5+(i+0)*s;
      va[n2+0].y:=-0.5;
      va[n2+0].z:= 0.5-(j+0)*s;
      va[n2+1].x:=-0.5+(i+0)*s;
      va[n2+1].y:=-0.5;
      va[n2+1].z:= 0.5-(j+1)*s;
      va[n2+2].x:=-0.5+(i+1)*s;
      va[n2+2].y:=-0.5;
      va[n2+2].z:= 0.5-(j+1)*s;
      va[n2+3].x:=-0.5+(i+1)*s;
      va[n2+3].y:=-0.5;
      va[n2+3].z:= 0.5-(j+0)*s;
    end;
end;

constructor TTriModel.CreatePlane(cols, rows: integer);
var i, j, n: longint; sx, sy: single; flip: boolean;
begin
  Create;
  if cols<1 then cols:=1;
  if rows<1 then rows:=1;
  fCount:=2*rows*cols; vCount:=(rows+1)*(cols+1);
  sx:=1/cols; sy:=1/rows; groups:=1;
  with grp[0] do begin
    count:=fCount; first:=0; matIndex:=0; visible:=true;
  end;
  for j:=0 to rows do
    for i:=0 to cols do begin
      n:=i+j*(cols+1);
      na[n].x:=0; na[n].y:=1; na[n].z:=0;
      va[n].x:=-0.5+sx*i;
      va[n].y:=0;
      va[n].z:=-0.5+sy*j;
      ta[n].x:=i/cols; ta[n].y:=j/rows;
    end;
  for j:=0 to rows-1 do begin
    flip:=(j mod 2)=0;
    for i:=0 to cols-1 do begin
      flip:=not flip;
      n:=(i+j*cols)*2;
      if flip then begin
        fa[n,   0]:=(i+0)+(j+0)*(cols+1);
        fa[n,   1]:=(i+0)+(j+1)*(cols+1);
        fa[n,   2]:=(i+1)+(j+0)*(cols+1);
        fa[n+1, 0]:=(i+1)+(j+0)*(cols+1);
        fa[n+1, 1]:=(i+0)+(j+1)*(cols+1);
        fa[n+1, 2]:=(i+1)+(j+1)*(cols+1);
      end else begin
        fa[n,   0]:=(i+0)+(j+0)*(cols+1);
        fa[n,   1]:=(i+1)+(j+1)*(cols+1);
        fa[n,   2]:=(i+1)+(j+0)*(cols+1);
        fa[n+1, 0]:=(i+0)+(j+0)*(cols+1);
        fa[n+1, 1]:=(i+0)+(j+1)*(cols+1);
        fa[n+1, 2]:=(i+1)+(j+1)*(cols+1);
      end;
    end;
  end;
end;

constructor TTriModel.CreateSphere(cols, rows: integer);
var i, j, n: longint; flip: boolean; ax, ay: single;
begin
  Create;
  if cols<3 then cols:=3;
  if rows<2 then rows:=2;
  fCount:=2*rows*cols; vCount:=(rows+1)*(cols+1);
  groups:=1;
  with grp[0] do begin
    count:=fCount; first:=0; matIndex:=0; visible:=true;
  end;
  ax:=pi*2/cols; ay:=pi/rows;
  for j:=0 to rows do
    for i:=0 to cols do begin
      n:=i+j*(cols+1);
      na[n].x:=sin(j*ay) * cos(i*ax);
      na[n].y:=cos(j*ay);
      na[n].z:=-sin(j*ay) * sin(i*ax);
      va[n]:=nxMath3D.scale(na[n], 0.5);
      ta[n].x:=i/cols; ta[n].y:=j/rows;
    end;
  for j:=0 to rows-1 do begin
    flip:=(j mod 2)=0;
    for i:=0 to cols-1 do begin
      flip:=not flip;
      n:=(i+j*cols)*2;
      if flip then begin
        fa[n,   0]:=(i+0)+(j+0)*(cols+1);
        fa[n,   1]:=(i+0)+(j+1)*(cols+1);
        fa[n,   2]:=(i+1)+(j+0)*(cols+1);
        fa[n+1, 0]:=(i+1)+(j+0)*(cols+1);
        fa[n+1, 1]:=(i+0)+(j+1)*(cols+1);
        fa[n+1, 2]:=(i+1)+(j+1)*(cols+1);
      end else begin
        fa[n,   0]:=(i+0)+(j+0)*(cols+1);
        fa[n,   1]:=(i+1)+(j+1)*(cols+1);
        fa[n,   2]:=(i+1)+(j+0)*(cols+1);
        fa[n+1, 0]:=(i+0)+(j+0)*(cols+1);
        fa[n+1, 1]:=(i+0)+(j+1)*(cols+1);
        fa[n+1, 2]:=(i+1)+(j+1)*(cols+1);
      end;
    end;
  end;
end;

constructor TTriModel.CreateTorus(cols, rows: integer; inRadius: single);
var i, j, n: longint; flip: boolean; radius, a1, a2: single;
begin
  Create;
  if cols<3 then cols:=3;
  if rows<3 then rows:=3;
  radius:=0.5-abs(inRadius);
  fCount:=2*rows*cols; vCount:=(rows+1)*(cols+1);
  groups:=1;
  with grp[0] do begin
    count:=fCount; first:=0; matIndex:=0; visible:=true;
  end;
  a1:=pi*2/rows; a2:=pi*2/cols;
  for j:=0 to rows do
    for i:=0 to cols do begin
      n:=i+j*(cols+1);
      va[n].x:=(radius+inRadius*cos(j*a2))*cos(i*a1);
      va[n].y:=inRadius*sin(j*a2);
      va[n].z:=(radius+inRadius*cos(j*a2))*sin(i*a1);
      na[n].x:=cos(j*a2)*cos(i*a1);
      na[n].y:=sin(j*a2);
      na[n].z:=cos(j*a2)*sin(i*a1);
      ta[n].x:=0.5-i/cols; ta[n].y:=0.5-j/rows;
    end;
  for j:=0 to rows-1 do begin
    flip:=(j mod 2)=0;
    for i:=0 to cols-1 do begin
      flip:=not flip;
      n:=(i+j*cols)*2;
      if flip then begin
        fa[n,   0]:=(i+0)+(j+0)*(cols+1);
        fa[n,   1]:=(i+0)+(j+1)*(cols+1);
        fa[n,   2]:=(i+1)+(j+0)*(cols+1);
        fa[n+1, 0]:=(i+1)+(j+0)*(cols+1);
        fa[n+1, 1]:=(i+0)+(j+1)*(cols+1);
        fa[n+1, 2]:=(i+1)+(j+1)*(cols+1);
      end else begin
        fa[n,   0]:=(i+0)+(j+0)*(cols+1);
        fa[n,   1]:=(i+1)+(j+1)*(cols+1);
        fa[n,   2]:=(i+1)+(j+0)*(cols+1);
        fa[n+1, 0]:=(i+0)+(j+0)*(cols+1);
        fa[n+1, 1]:=(i+0)+(j+1)*(cols+1);
        fa[n+1, 2]:=(i+1)+(j+1)*(cols+1);
      end;
    end;
  end;
end;

function TTriModel.AddFace(const gIndex: word): longint;
var i, g: longint;
begin
  if gIndex>=groups then begin
    result:=-1; exit;
  end;
  vCount:=vCount+3;
  fCount:=fCount+1;
  with grp[gIndex] do begin
    result:=first+count;
    for i:=fCount-1 downto first+count do fa[i]:=fa[i-1];
    inc(count, 1);
    fa[longint(first)+count-1, 0]:=vCount-3;
    fa[longint(first)+count-1, 1]:=vCount-2;
    fa[longint(first)+count-1, 2]:=vCount-1;
  end;
  for g:=0 to groups-1 do
    if grp[g].first>=result then inc(grp[g].first, 1);
end;

procedure TTriModel.AssignTo(poly: TPolyModel);
var i, j: longint;
begin
  poly.Clear;
  poly.UseMaterials:=UseMaterials; poly.UseColors:=UseColors;
  poly.fCount:=fCount; poly.vCount:=vCount;
  poly.groups:=groups; poly.mCount:=mCount; poly.bCount:=bCount;
  setlength(poly.mat, poly.mCount);
  setlength(poly.grp, groups);
  setlength(poly.bone, bCount);
  for i:=0 to vCount-1 do begin
    poly.va[i]:=va[i]; poly.na[i]:=na[i]; poly.ta[i]:=ta[i];
  end;
  for i:=0 to fCount-1 do begin
    poly.fa[i].count:=3; setlength(poly.fa[i].index, 3);
    for j:=0 to 2 do poly.fa[i].index[j]:=fa[i, j];
  end;
  for i:=0 to mCount-1 do poly.mat[i]:=mat[i];
  for i:=0 to groups-1 do poly.grp[i]:=grp[i];
  for i:=0 to bCount-1 do poly.bone[i]:=bone[i];
end;

{ TVertexModel }

procedure TVertexModel.Center(_x, _y, _z: boolean);
var i: longint; minx,miny,minz, maxx,maxy,maxz, d: single;
begin
  if length(va)<=0 then exit;
  if _x then begin
    minx:=va[0].v.x; maxx:=va[0].v.x;
    for i:=1 to high(va) do begin
      if va[i].v.x<minx then minx:=va[i].v.x
      else if va[i].v.x>maxx then maxx:=va[i].v.x;
    end;
    d:=(maxx+minx)/2;
    for i:=0 to high(va) do va[i].v.x:=va[i].v.x-d;
  end;
  if _y then begin
    miny:=va[0].v.y; maxy:=va[0].v.y;
    for i:=1 to high(va) do begin
      if va[i].v.y<miny then miny:=va[i].v.y
      else if va[i].v.y>maxy then maxy:=va[i].v.y;
    end;
    d:=(maxy+miny)/2;
    for i:=0 to high(va) do va[i].v.y:=va[i].v.y-d;
  end;
  if _z then begin
    minz:=va[0].v.z; maxz:=va[0].v.z;
    for i:=1 to high(va) do begin
      if va[i].v.z<minz then minz:=va[i].v.z
      else if va[i].v.z>maxz then maxz:=va[i].v.z;
    end;
    d:=(maxz+minz)/2;
    for i:=0 to high(va) do va[i].v.z:=va[i].v.z-d;
  end;
end;

procedure TVertexModel.Clear;
begin
  setlength(va, 0); setlength(fa, 0); setlength(ca, 0);
  setlength(bone, 0); setlength(grp, 0); setlength(frame, 0);
end;

function TVertexModel.GetRadius: single;
var i: longint; d: single;
begin
  result:=0;
  for i:=0 to high(va) do begin
    d:=hypot3f(va[i].v.x, va[i].v.y, va[i].v.z);
    if d>result then result:=d;
  end;
end;

procedure TVertexModel.Rotate(_radians: single; axis: TVector);
var i: longint;
begin
  for i:=0 to high(va) do begin
    va[i].v:=nxMath3D.Rotate(va[i].v, _radians, axis);
    va[i].n:=nxMath3D.Rotate(va[i].n, _radians, axis);
  end;
end;

procedure TVertexModel.RotateUV(_radians, centerX, centerY: single);
var i: longint;
begin
  for i:=0 to high(va) do
    nxMath.Rotate(va[i].uv.x, va[i].uv.y, _radians, centerX, centerY);
end;

procedure TVertexModel.Scale(x, y, z: single);
var i: longint;
begin
  for i:=0 to high(va) do begin
    va[i].v.x:=va[i].v.x*x;
    va[i].v.y:=va[i].v.y*y;
    va[i].v.z:=va[i].v.z*z;
  end;
end;

procedure TVertexModel.ScaleTo(size: single);
var i: longint; minx,miny,minz, maxx,maxy,maxz, d,dx,dy,dz: single;
begin
  if length(va)<=0 then exit;
  minx:=va[0].v.x; maxx:=va[0].v.x;
  miny:=va[0].v.y; maxy:=va[0].v.y;
  minz:=va[0].v.z; maxz:=va[0].v.z;
  for i:=1 to high(va) do begin
    if va[i].v.x<minx then minx:=va[i].v.x
    else if va[i].v.x>maxx then maxx:=va[i].v.x;
    if va[i].v.y<miny then miny:=va[i].v.y
    else if va[i].v.y>maxy then maxy:=va[i].v.y;
    if va[i].v.z<minz then minz:=va[i].v.z
    else if va[i].v.z>maxz then maxz:=va[i].v.z;
  end;
  dx:=size/(maxx-minx);
  dy:=size/(maxy-miny);
  dz:=size/(maxz-minz);
  if dx<dy then d:=dx
  else d:=dy;
  if d<dz then d:=dz;
  for i:=0 to high(va) do begin
    va[i].v.x:=va[i].v.x*d;
    va[i].v.y:=va[i].v.y*d;
    va[i].v.z:=va[i].v.z*d;
  end;
end;

procedure TVertexModel.ScaleUV(x, y: single);
var i: longint;
begin
  for i:=0 to high(va) do begin
    va[i].uv.x:=va[i].uv.x*x; va[i].uv.y:=va[i].uv.y*y;
  end;
end;

procedure TVertexModel.Translate(x, y, z: single);
var i: longint;
begin
  for i:=0 to high(va) do begin
    va[i].v.x:=va[i].v.x+x;
    va[i].v.y:=va[i].v.y+y;
    va[i].v.z:=va[i].v.z+z;
  end;
end;

procedure TVertexModel.TranslateUV(x, y: single);
var i: longint;
begin
  for i:=0 to high(va) do begin
    va[i].uv.x:=va[i].uv.x+x; va[i].uv.y:=va[i].uv.y+y;
  end;
end;

initialization

{$IFDEF delphi}
  decimalseparator:='.';
{$ENDIF}

end.
