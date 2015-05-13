#nxGL documentation

# nxGL #

---

uses [dglOpenGL](http://wiki.delphigl.com/index.php/dglOpenGL.pas/en), [nxGraph](nxGraph.md), [nxMath3D](nxMath3D.md), [nxModel](nxModel.md), [nxShaders](nxShaders.md), [nxStrings](nxStrings.md), [nxTypes](nxTypes.md), (LazOpenGLContext with Lazarus)

## Classes ##

  * TCamera
  * TCameraPath
  * TDisplayList
  * TFrameBuffer
  * TGLFont (TNXFont)
  * TGLModel (TTriModel)
  * TGLRenderer
  * TGLShader
  * TGLTextureSet (TTextureSet) [nxGraph](nxGraph.md)
  * TNXGL (TNXCustomEngine) [nxGraph](nxGraph.md)
  * TRenderSettings
  * TVBO (TCustomVertexArray) [nxGraph](nxGraph.md)
  * TVertexArray (TCustomVertexArray) [nxGraph](nxGraph.md)

## Functions and procedures ##

```
procedure nxInitGL;
```
Initializes OpenGL, called after initializing rendering context.
Is automatically called with nx.CreateGLWindow.

## Other ##
```
nxFARZ: single = 1000;
nxNEARZ: single = 0.1;
nxFov: single = 45;
nxAspectRatio: single = 1.33;
```
3D-perspective settings should be altered before nxInitGL activates, but are involved with procedures nx.Perspective and nx.SetView.
nxFARZ will change maximum view-distance when using Z-buffer. nxFov determines field-of-view.

```
nx: TNXGL;
tex: TGLTextureSet;
```
**nx**, and **tex** are automatically instantiated class objects when unit is added to uses-list of your program.
**nx: TNXGL** is basically the core of everything; graphics, rendering settings, fonts, rendering context management, error reporting.
**tex: TGLTextureSet** is class handling all texture-operations; loading of PNG, JPG, BMP formats.

## TCamera class ##

```
path: array of TCameraPath;
constructor Create;
destructor Destroy;
function AddPath: TCameraPath;
procedure DeletePath(n: integer);
procedure GetFromModelView;
function GetMatrix: TMatrix;
function IndexOfPath(p: TCameraPath): integer;
procedure Interpolate(const mat2: TMatrix; const delta: single; doSet: boolean = true);
procedure Load(n: integer; doSet: boolean = true);
procedure LookAt(const eye, target, up: TVector; doSet: boolean = true);
procedure LookAt(const target, up: TVector; doSet: boolean = true);
procedure LookAt(const target: TVector; doSet: boolean = true);
procedure Multiply(const mat2: TMatrix; doSet: boolean = true);
function PathCount: integer;
procedure Pop(doSet: boolean = true);
procedure Push;
procedure Reset(doSet: boolean = true);
procedure ResetStack;
procedure Rotate(const degrees: single; const axis: TVector; doSet: boolean = true);
procedure Rotate(const degrees: single; const ax, ay, az: single; doSet: boolean = true);
function SaveCount: integer;
procedure Save(n: integer);
procedure SetCamera;
procedure Translate(const x, y, z: single; doSet: boolean = true);
procedure Translate(const v: TVector; doSet: boolean = true);
```

## TCameraPath class ##

```
mat: TMatrix;
node: array of TMatrix;
parent: TCamera;
pathmode: TPathingMode;
wrapping, AutoSetCamera: boolean;
property Position: single;
constructor Create(owner: TCamera);
function AddNode(const _mat: TMatrix): integer;
function Count: integer;
function GetMatrix(const dTime: single): TMatrix;
procedure SetCamera;
```

## TDisplayList class ##

```
constructor Create(make: boolean = false);
destructor Destroy;
procedure UpdateList(render: boolean = false);
procedure EndList;
procedure Draw;
procedure DrawAt(x,y,z: single);
procedure SetTexRef(TexN: integer; isUsed: boolean);
```

## TFrameBuffer class ##

```
constructor Create(const _texture: integer; _depth: boolean = false);
constructor Create(const width, height: integer; transparency: boolean;	_depth: boolean = false);
destructor Destroy; override;
procedure Bind;
procedure SetTexture(const _texture: integer; _unbind: boolean = true);
procedure UnBind;
```

## TGLFont class ##

```
constructor CreateFont(fontName: string; fontSize, _TexSize: integer);
procedure Draw(x,y: single; s: string; maxW: integer = 0);
procedure DrawC(x,y: single; s: string; maxW: integer = 0);
procedure DrawCScaled(x,y, scaleX,scaleY: single; s: string; maxW: integer = 0);
procedure DrawRotate(x,y, scaleX,scaleY, _angle: single; s: string; maxW: integer = 0);
procedure DrawScaled(x,y, scaleX,scaleY: single; s: string; maxW: integer = 0);
procedure DrawTextArea(r: TBoundsRect; s: TStrings; x_scroll,y_scroll: integer);
procedure DrawWrap(r: TBoundsRect; s: TStrings; y_scroll: integer = 0);
procedure SetColor;
procedure SetTexture;
```

## TGLModel class ##

```
destructor Destroy; override;
procedure Assign(_va: TCustomVertexArray);
procedure AssignTo(_va: TCustomVertexArray);
procedure EnableStates;
procedure CopyToFrame(index: integer);
procedure DisableStates;
procedure FreeTextures;
procedure LoadTextures(path: string);
procedure MakeDisplayList(var list: TDisplayList);
function NewMaterial(_texIndex: integer): integer;
procedure Render(Initialize: boolean = true);
procedure SetFrames(count: integer);
procedure SetFrame(t: single; fStart,fEnd: integer; loop: boolean);
procedure SetFrameEx(a,b,c,d: PVertexFrame; delta: single);
procedure SetPointers;
```

## TGLRenderer class ##

```
(incomplete)
```

## TGLShader class ##

```
constructor Create(UseVertex, UseFragment: boolean);
destructor Destroy;
procedure AttachFragmentShaderFrom(shader: TGLShader);
procedure AttachVertexShaderFrom(shader: TGLShader);
procedure DeleteProgram;
procedure Disable;
procedure Enable;
procedure GetErrorInfo(source: GLHandle; target: TStrings);
procedure GetFragmentShaderInfo(target: TStrings);
procedure GetVertexShaderInfo(target: TStrings);
function GetUniform(name: string): GLint;
function Link: boolean;
function LoadDefaultFShader2D: boolean;
function LoadDefaultVShader2D: boolean;
function LoadDefaultFShader3D(bump: boolean): boolean;
function LoadDefaultVShader3D: boolean;
function LoadFragmentSource(filename: string): boolean;
function LoadVertexSource(filename: string): boolean;
procedure MakeProgram(UseVertex, UseFragment: boolean);
function SetFragmentSource(s: string): boolean;
function SetVertexSource(s: string): boolean;
```

## TGLTextureSet class ##

```
destructor Destroy; override;
constructor Create;
function AddEmptyTexture(name: string; width, height: word; transparency: boolean = false): integer;
function AddTexture(name, filename: string; transparency: boolean = false): integer;
function Add3DTexture(name,filename: string; cols,rows: integer; transparency: boolean = false): integer;
procedure Clear;
procedure Disable;
procedure Enable;
procedure ChangeTexMode3D(_enable: boolean; force: boolean = false);
procedure RemoveTexture(n: integer; force: boolean = false);
procedure ReloadTexture(n: integer; _filename: string; transparency: boolean = false);
procedure Restore(tex: PTexture);
procedure Restore3D(tex: PTexture);
procedure Restore(tex: integer);
procedure Restore3D(tex: integer);
procedure SetByName(name: string; force: boolean = false);
procedure SetClamp(x_repeat, y_repeat: boolean);
procedure SetTex(n: integer; force: boolean = false);
procedure SetTextureUnit(n: integer);
```

## TNXGL class ##

```
rs: TRenderSettings;
property FPS: integer read GetFFPS;
function AllOK: boolean;
function CanRender: boolean;
constructor Create;
destructor Destroy;
procedure Clear(color,depth: boolean);
procedure ClearFonts;
procedure CreateBasicFont;
function CreateFont(fontName: string; fontSize, TexSize: integer): integer;
function CreateGlWindow(hWindow: TWinControl): boolean;
procedure DefaultLights(Enable: boolean = true);
procedure DeleteFont(index: integer);
procedure Disable2D;
procedure Draw(x,y: integer; pattern: word = 0);
procedure DrawRotate(x,y: single; pattern: word; a, cx,cy: single);
procedure DrawRotateS(x,y: single; pattern: word; a, cx,cy, sizeX,sizeY: single);
procedure DrawScaled(x,y, sizeX,sizeY: single; pattern: word = 0);
procedure DrawSections(x,y, sizeX,sizeY,pattern: integer; bdSize: single = 0.25);
procedure DrawTexRepeat(x,y, sizeX,sizeY: integer);
procedure Enable2D(flip: boolean = true);
procedure Enable2D(X, Y, _Width, _Height: integer; flip: boolean = true);
procedure Flip;
procedure FontFromImage(TexSize: integer);
function GetEXT: string;
procedure GetMouseRay(mx, my: single; const p, normal: PVector; rayMove: single = -100);
function GLInfo(ext: array of string): boolean;
function GLInfo(ext: string): boolean;
function Initialized: boolean;
procedure KillGLWindow;
procedure Line(x,y,x2,y2: integer);
function MouseRayAtPlane(const mx,my: single; const planePos,planeNormal: TVector): TVector;
function MouseRayAtXZPlane(const mx,my: single): TVector;
procedure OutLine(x,y,_width,_height: integer);
procedure Perspective(Stretch: boolean = false);
function ProgDir: string;
procedure RectT(x1,y1,x2,y2: single);
procedure RectZT(x1,z1,x2,z2,y: single);
procedure SetClearColor(r,g,b: single; a: single=0);
procedure SetColor(r,g,b: single);
procedure SetColor(r,g,b,a: single);
procedure SetColor(const rgb: TRGB);
procedure SetColor(const rgba: TRGBA);
procedure SetFont(index: integer);
procedure SetLight(light, pname: TGLenum; x,y,z: single; w: single=1);
procedure SetPixel(x,y: integer);
procedure SetSpecular(Enable: boolean; r,g,b,shininess: single);
procedure SetView(X, Y, _Width, _Height: integer);
procedure SetWireframe(enable: boolean = true);
```

## TRenderSettings class ##

```
property AddBlend: boolean;
property CullBack: boolean;
property CullFront: boolean;
property DepthTest: boolean;
property Lighting: boolean;
property SubBlend: boolean;
property WireFrame: boolean;
function Push: boolean;
function Pop: boolean;
```

## TVBO class ##

```
UseShader: boolean;
constructor Create;
constructor Create(Faces, Vertices: integer; _rendermode: cardinal;
	textures, normals, colors: boolean);
constructor Create(Faces: integer; _rendermode: cardinal;
	textures, normals, colors: boolean);
destructor Destroy;
function GetIndexCount(const faceCount: integer): integer;
function IsStatic: boolean;
procedure Render;
procedure Render(first, _count: integer);
procedure SetData;
procedure SetPointers;
procedure SetStatic(enable: boolean);
```

## TVertexArray class ##

```
procedure DisableStates;
procedure EnableStates;
function GetIndexCount(const faceCount: integer): integer;
procedure Render(Indexed: boolean = true);
procedure Render(first, _count: integer; Indexed: boolean = true);
procedure SetPointers;
```