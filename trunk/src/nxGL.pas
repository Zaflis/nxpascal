unit nxGL;

{$IFDEF fpc}
{$mode objfpc}{$H+}
{$ENDIF}

{
  In Lazarus you may have to do this for each project:
   "Project" menu -> "Project inspector"
   -> Press "+" button -> New requirement
     -> Add "LazOpenGLContext"
     -> Add "Lazmouseandkeyinput" (if using nxGame unit)
}

// Remove . if you don't want to use Lazarus OpenGL context.
// In custom mode, initialization check is ignored with fpc.
// Delphi always checks nx.nxRC <> 0
// In normal mode fpc checks nx.window <> nil
{.$define NX_CUSTOM_WINDOW}

{ TODO:
- Test and finalize TNXGL.MousePick()
- TGLRenderer
}

interface

uses Classes, dglOpenGL, Controls, Forms, ExtCtrls,
  {$IFDEF fpc}LCLtype, LCLIntf, FileUtil, LazUTF8,
    {$IFnDEF NX_CUSTOM_WINDOW}OpenGLContext,{$ENDIF}
  {$ELSE}Windows,{$ENDIF} nxShaders, nxModel, nxTypes, nxGraph;

const
  NX_RS_COUNT = 16; // Render settings stack size

type
  TRenderProc = procedure(index: longint) of object;

  TGLViewDetails = record
    modelM, projM: TGLMatrixd4;
    viewPort: TGLVectori4;
  end;
  PGLViewDetails = ^TGLViewDetails;

  { TGLTextureSet }

  TGLTextureSet = class(TTextureSet)
  private
    FTextureUnit: integer;
    procedure SetTextureUnit(n: integer);
  public
    property TextureUnit: integer read FTextureUnit write SetTextureUnit;
    destructor Destroy; override;
    constructor Create;
    function AddEmptyTexture(name: string; width, height: word; transparency: boolean = false): longint;
    function AddTexture(name, filename: string; transparency: boolean = false): longint; overload;
    function AddTexture(name: string; data: DynamicByteArray;
      width, height, values: integer; format, intFormat: cardinal): longint; overload;
    function Add3DTexture(name,filename: string; cols,rows: integer; transparency: boolean = false): longint;
    procedure Clear;
    procedure Disable;
    procedure Enable;
    procedure ChangeTexMode3D(_enable: boolean; force: boolean = false);
    procedure RemoveTexture(n: longint; force: boolean = false);
    procedure ReloadTexture(n: longint; _filename: string; transparency: boolean = false);
    procedure Restore(tex: PTexture); overload;
    procedure Restore3D(tex: PTexture); overload;
    procedure Restore(tex: longint); overload;
    procedure Restore3D(tex: longint); overload;
    procedure SetByName(name: string; force: boolean = false);
    procedure SetClamp(x_repeat, y_repeat: boolean);
    procedure SetTex(n: longint; force: boolean = false);
  end;

  TCamera = class;

  { TCameraPath }

  TCameraPath = class
  private
    FPosition: single;
    procedure SetPosition(p: single);
  public
    mat: TMatrix;
    node: array of TMatrix;
    parent: TCamera;
    pathmode: TPathingMode;
    wrapping, AutoSetCamera: boolean;
    property Position: single read FPosition write SetPosition;
    constructor Create(owner: TCamera);
    function AddNode(const _mat: TMatrix): longint;
    function Count: longint;
    function GetMatrix(const dTime: single): TMatrix;{$IFDEF CanInline}inline;{$ENDIF}
    procedure SetCamera;
  end;

  { TCamera }

  TCamera = class
  private
    index: word;
  protected
    mat: array of TMatrix;
    saved: array of TMatrix;
  public
    path: array of TCameraPath;
    constructor Create;
    destructor Destroy; override;
    function AddPath: TCameraPath;
    procedure DeletePath(n: longint);
    procedure GetFromModelView;
    function GetMatrix: TMatrix;{$IFDEF CanInline}inline;{$ENDIF}
    function GetVector(const axis: integer): TVector;{$IFDEF CanInline}inline;{$ENDIF}
    function IndexOfPath(p: TCameraPath): longint;
    procedure Interpolate(const mat2: TMatrix; const delta: single; doSet: boolean = true);
    procedure Load(n: longint; doSet: boolean = true);
    procedure LookAt(const eye, target, up: TVector; doSet: boolean = true); overload; stdcall; {$IFDEF CanInline}inline;{$ENDIF}
    procedure LookAt(const target, up: TVector; doSet: boolean = true); overload;
    procedure LookAt(const target: TVector; doSet: boolean = true); overload;
    procedure Multiply(const mat2: TMatrix; doSet: boolean = true); overload; stdcall;{$IFDEF CanInline}inline;{$ENDIF}
    procedure Multiply(const mat2: TMatrix3f; doSet: boolean = true); overload; stdcall;{$IFDEF CanInline}inline;{$ENDIF}
    function PathCount: longint;
    procedure Pop(doSet: boolean = true);
    procedure Push;
    procedure Reset(doSet: boolean = true);
    procedure ResetStack;
    procedure Rotate(const degrees: single; const axis: TVector; doSet: boolean = true); overload;
    procedure Rotate(const degrees: single; const ax, ay, az: single; doSet: boolean = true); overload;
    function SaveCount: longint;
    procedure Save(n: longint);
    procedure Scale(const s: single; doSet: boolean = true);
    procedure SetCamera;
    procedure Translate(const x, y, z: single; doSet: boolean = true); overload;
    procedure Translate(const v: TVector; doSet: boolean = true); overload;
  end;

  { TVertexArray }

  TVertexArray = class(TCustomVertexArray)
  public
    procedure DisableStates;
    procedure EnableStates;
    function GetIndexCount(const faceCount: longint): longint; override;
    procedure Render(Indexed: boolean = true); overload;
    procedure Render(first, _count: longint; Indexed: boolean = true); overload;
    procedure SetPointers;
  end;

  { TVBO }

  TVBO = class(TCustomVertexArray)
  private
    FStatic, dataIsSet: boolean;
    procedure SetDefaultPointers;
    procedure SetShaderPointers;
    function StaticVal: cardinal;
  public
    bIndex, bVertex, bNormal, bTexture, bColor: cardinal;
    UseShader: boolean;
    constructor Create; overload;
    constructor Create(Faces, Vertices: longint; _rendermode: cardinal;
      textures, normals, colors: boolean); overload;
    constructor Create(Faces: longint; _rendermode: cardinal;
      textures, normals, colors: boolean); overload;
    destructor Destroy; override;
    function GetIndexCount(const faceCount: longint): longint; override;
    function IsStatic: boolean;
    procedure Render; overload;
    procedure Render(first, _count: longint); overload;
    procedure SetData;
    procedure SetPointers;
    procedure SetStatic(enable: boolean);
  end;

  {TShaderVBO = class
    va: array of T3DVertex;
  end;}

  { TShaderSource }

  TShaderSource = class
  private
    FCompiled: boolean;
    FShaderType: GLenum;
  public
    handle: GLhandle;
    property compiled: boolean read FCompiled;
    property ShaderType: GLenum read FShaderType;
    constructor Create(_shaderType: GLenum);
    constructor CreateFragment;
    constructor CreateVertex;
    destructor Destroy; override;
    procedure GetLastErrorInfo(target: TStrings);
    function LoadDefaultFShader2D: boolean;
    function LoadDefaultVShader2D: boolean;
    function LoadDefaultFShader3D(bump: boolean): boolean;
    function LoadDefaultVShader3D: boolean;
    function LoadSource(filename: string): boolean;
    function SetSource(const s: string): boolean;
  end;

  TShaderIndexDetails = record
    index: GLint;
    name: string;
  end;

  { TShaderProgram }

  TShaderProgram = class
  private
    FLinked: boolean;
  public
    handle: GLhandle;
    attached: array of TShaderSource;
    attrib, uniform: array of TShaderIndexDetails;
    property linked: boolean read FLinked;
    constructor Create;
    destructor Destroy; override;
    function AddAttrib(aname: string): integer;
    function AddUniform(uname: string): integer;
    function AttachShader(shader: TShaderSource): boolean;
    procedure DetachAllShaders;
    procedure DetachShader(index: integer);
    procedure Disable;
    procedure Enable;
    // Get index directly from the shader
    function GetAttrib(aname: string): GLint;
    function GetUniform(uname: string): GLint;
    // Get cached array index in TShaderProgram
    function FindAttrib(aname: string): integer;
    function FindUniform(uname: string): integer;
    function isCompiled: boolean;
    function Link: boolean;
  end;

  { TGLShader }

  TGLShader = class
  private
  public
    current: TShaderProgram;
    programs: array of TShaderProgram;
    shaders: array of TShaderSource;
    destructor Destroy; override;
    function AddProgram(const autoAttach: boolean=true): TShaderProgram;
    function AddProgramAndLink: boolean;
    function AddShader(const s: string; isVertex: boolean): TShaderSource;
    function AddDefault2D: TShaderProgram;
    function AddDefault3D(bump: boolean = false): TShaderProgram;
    procedure AttachShader(index: integer);
    procedure DeleteProgram(index: integer);
    procedure Disable;
    procedure Enable;
    function GetUniform(uname: string): GLint;
    function isCompiled: boolean;
    function Link: boolean;
    function LoadShader(filename: string; isVertex: boolean): TShaderSource;
    procedure SelectProgram(index: integer; doEnable: boolean = true); overload;
    procedure SelectProgram(prog: TShaderProgram; doEnable: boolean = true); overload;
  end;

  { TGLRenderer }

  TGLRenderer = class
  private
    FisBuffer3D, FisEnabled: boolean;
    FpolyMode: GLuint;
  public
    ambient, diffuse: TfRGBA;
    color: TfRGBA;
    shininess: single;
    cam: TCamera;
    shader: TGLShader;
    curTexture: longint;
    va2D: array of T2DVertex;
    va3D: array of T3DVertex;
    program2D, program3D: TShaderProgram;
    lights: array of T3DLight;
    buf2D, buf3D: GLuint;
    att_2Dpos, att_2Dtex, att_2Dcol: GLint;
    att_3Dpos, att_3Dtex, att_3Dcol: GLint;
    uniModel, uniNormal, uniDiffuse2D: GLint;
    uniPmv2D, uniPmv3D, uniTex2D, uniTex3D: GLint;
    uniLightcount, uniLights, uniAmbient3D, uniSpecular, uniShininess: GLint;
    count, Length2D, Length3D: word;
    property isEnabled: boolean read FisEnabled;
    property isBuffer3D: boolean read FisBuffer3D;
    property polyMode: GLuint read FpolyMode;
    function AddProgram: integer; overload;
    function AddProgram(vert, frag: TShaderSource): integer; overload;
    function AddProgram(vert_file, frag_file: string): integer; overload;
    constructor Create; overload;
    constructor Create(buffer2D, buffer3D: word); overload;
    destructor Destroy; override;
    procedure Disable;
    function Draw(x, y: single; pattern: longint = 0): word;
    function DrawRotateS(x, y: single; pattern: longint; radians, cx, cy,
      sizeX, sizeY: single): word;
    procedure Enable2D(ForceUpdateUniforms: boolean = false);
    procedure Enable3D(ForceUpdateUniforms: boolean = false);
    procedure EnableProgram(index: integer; is3D: boolean;
      ForceUpdateUniforms: boolean = false); overload;
    procedure EnableProgram(prog: TShaderProgram; is3D: boolean;
      ForceUpdateUniforms: boolean = false); overload;
    function NewQuad: word;
    function NewTriangle: word;
    procedure Render;
    procedure Reset;
    procedure SetColor(const r, g, b: single; const a: single = 1.0); overload;
    procedure SetColor(const index: word; const r, g, b, a: single); overload;
    procedure SetDiffuse(const r, g, b: single; const a: single = 1.0);
    procedure SetNormal(const index: word; const nx, ny, nz: single);
    procedure SetTexture(n: longint); overload;
    procedure SetTexture(texname: string); overload;
    procedure SetUniforms(camera: TCamera = nil);
    procedure SetVertex(const index: word; const x, y, u, v: single); overload;
    procedure SetVertex(const index: word; const x, y, z, u, v: single); overload;
  end;

  { TDisplayList }

  TDisplayList = class
  private
    list: TGLuint;
    texRef: array of longint;
    texRefCount: word;
  public
    constructor Create(make: boolean = false);
    destructor Destroy; override;
    procedure UpdateList(render: boolean = false);
    procedure EndList;
    procedure Draw;
    procedure DrawAt(x,y,z: single);
    procedure SetTexRef(TexN: longint; isUsed: boolean);
  end;

  TNXGL = class;

  { TRenderSettings }

  TRenderSettings = class
  private
    parent: TNXGL;
    FAddBlend: array[0..NX_RS_COUNT-1] of boolean;
    FCullBack: array[0..NX_RS_COUNT-1] of boolean;
    FCullFront: array[0..NX_RS_COUNT-1] of boolean;
    FDepthTest: array[0..NX_RS_COUNT-1] of boolean;
    FLighting: array[0..NX_RS_COUNT-1] of boolean;
    FSubBlend: array[0..NX_RS_COUNT-1] of boolean;
    FWireFrame: array[0..NX_RS_COUNT-1] of boolean;
    stackLevel: shortint;
    function GetAddBlend: boolean;
    function GetCullBack: boolean;
    function GetCullFront: boolean;
    function GetDepthTest: boolean;
    function GetLighting: boolean;
    function GetSubBlend: boolean;
    function GetWireFrame: boolean;
    procedure SetAddBlend(enable: boolean);
    procedure SetBlend;
    procedure SetCull;
    procedure SetCullBack(enable: boolean);
    procedure SetCullFront(enable: boolean);
    procedure SetDepthTest(enable: boolean);
    procedure SetLighting(enable: boolean);
    procedure SetSubBlend(enable: boolean);
    procedure SetWireFrame(enable: boolean);
  public
    constructor Create(_nx: TNXGL);
    property AddBlend: boolean read GetAddBlend write SetAddBlend;
    property CullBack: boolean read GetCullBack write SetCullBack;
    property CullFront: boolean read GetCullFront write SetCullFront;
    property DepthTest: boolean read GetDepthTest write SetDepthTest;
    property Lighting: boolean read GetLighting write SetLighting;
    property SubBlend: boolean read GetSubBlend write SetSubBlend;
    property WireFrame: boolean read GetWireFrame write SetWireFrame;
    function Push: boolean;
    function Pop: boolean;
    procedure ResetStack;
  end;

  { TGLModel }

  TGLModel = class(TTriModel)
  public
    destructor Destroy; override;
    procedure Assign(_va: TCustomVertexArray);
    procedure AssignTo(_va: TCustomVertexArray);
    procedure EnableStates;
    procedure CopyToFrame(index: integer);
    procedure DisableStates;
    procedure FreeTextures;
    procedure LoadTextures(path: string);
    procedure MakeDisplayList(var list: TDisplayList);
    function NewMaterial(_texIndex: longint): integer;
    procedure Render(Initialize: boolean = true);
    procedure SetFrames(count: integer);
    procedure SetFrame(t: single; fStart,fEnd: longint; loop: boolean);
    procedure SetFrameEx(a,b,c,d: PVertexFrame; delta: single);
    procedure SetPointers;
  end;

  { TGLFont }

  TGLFont = class(TNXFont)
  public
    constructor CreateFont(fontName: string; fontSize, _TexSize: longint);
    constructor LoadFont(filename: string);
    procedure Draw(x,y: single; s: string; maxW: integer = 0); overload; override;
    procedure DrawCScaled(x,y, scaleX,scaleY: single; s: string; maxW: integer = 0);
    procedure DrawRotate(x,y, scaleX,scaleY, _angle: single; s: string; maxW: integer = 0);
    procedure DrawScaled(x,y, scaleX,scaleY: single; s: string; maxW: integer = 0);
    procedure DrawTextArea(r: TBoundsRect; s: TStrings; x_scroll,y_scroll: longint);
    procedure DrawWrap(r: TBoundsRect; s: TStrings; y_scroll: longint = 0);
    function RDraw(renderer: TGLRenderer; x, y, scaleX, scaleY: single; s: string; maxW: single=-1): integer;
    function RDrawC(renderer: TGLRenderer; x, y, scaleX, scaleY: single; s: string; maxW: single=-1): integer;
    procedure SetColor; overload;
    procedure SetTexture; override;
  end;

  { TNXGL }

  TNXGL = class(TNXCustomEngine)
  private
    FBorderStyle: TFormBorderStyle;
    {$IFnDEF NX_CUSTOM_WINDOW}procedure doResize(Sender: TObject);{$ENDIF}
    function GetFFPS: integer;
    function NewFontName(base: string): string;
    function GetFont(index: integer): TGLFont;
  public
    {$IFDEF fpc}{$IFnDEF NX_CUSTOM_WINDOW}window: TOpenGLControl;{$ENDIF}{$ENDIF}
    rs: TRenderSettings;
    property Font[index: integer]: TGLFont read GetFont;
    property FPS: integer read GetFFPS;
    function AllOK: boolean;
    function CanRender: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Clear(color,depth: boolean);
    procedure ClearFonts;
    procedure CreateBasicFont;
    function CreateFont(fontName: string; fontSize, TexSize: integer): integer;
    function LoadFont(filename: string): integer;
    {$IFnDEF NX_CUSTOM_WINDOW}
    function CreateGlWindow(hWindow: TWinControl): boolean;
    {$ENDIF}
    procedure DefaultLights(Enable: boolean = true);
    procedure DeleteFont(index: integer);
    procedure Disable2D;
    procedure Draw(x,y: longint; pattern: word = 0);
    procedure DrawRotate(x,y: single; pattern: word; degrees,cx,cy: single);
    procedure DrawRotateS(x,y: single; pattern: word; degrees,cx,cy,
      sizeX,sizeY: single);
    procedure DrawScaled(x,y,sizeX,sizeY: single; pattern: word = 0);
    procedure DrawSections(x,y,sizeX,sizeY,pattern: longint; bdSize: single = 0.25);
    procedure DrawTexRepeat(x,y,sizeX,sizeY: longint);
    procedure Enable2D(flip: boolean = true); overload;
    procedure Enable2D(X, Y, _Width, _Height: integer; flip: boolean = true); overload;
    procedure FillCircle(x, y, radiusX, radiusY: single; sides: longint);
    procedure FillRect(x, y, _width, _height: longint);
    {$IFnDEF NX_CUSTOM_WINDOW}
    procedure Flip;
    {$ENDIF}
    procedure FontFromImage(TexSize: integer);
    function GetEXT: string;
    procedure GetMouseRay(mx, my: single; const p, normal: PVector;
      rayMove: single = -100); overload;
    procedure GetMouseRay(mx, my: single; const ray: PMouseRay;
      rayMove: single = -100); overload;
    procedure GetViewDetails(pdetails: PGLViewDetails);
    function GLInfo(ext: array of string): boolean; overload;
    function GLInfo(ext: string): boolean; overload;
    function Initialized: boolean;
    function IsMaxFullscreen: boolean;
    {$IFnDEF NX_CUSTOM_WINDOW}
    procedure KillGLWindow(force: boolean = false);
    {$ENDIF}
    procedure Line(x, y, x2, y2: longint);
    function MouseRayAtPlane(const mx, my: single; const planePos,planeNormal: TVector): TVector;
    function MouseRayAtXZPlane(const mx, my: single): TVector;
    procedure OutLine(x, y, _width, _height: longint);
    procedure Perspective(Stretch: boolean = false);
    function ProgDir: string;
    procedure RectT(x1, y1, x2, y2: single);
    procedure RectZT(x1, z1, x2, z2, y: single);
    procedure SetClearColor(r, g, b: single; a: single=0);
    procedure SetColor(r, g, b: single); overload;
    procedure SetColor(r, g, b, a: single); overload;
    procedure SetColor(const rgb: TRGB); overload;
    procedure SetColor(const rgba: TRGBA); overload;
    procedure SetFont(index: integer);
    procedure SetLight(light, pname: TGLenum; x,y,z: single; w: single=1);
    procedure SetMaxFullscreen(enable: boolean);
    procedure SetPixel(x,y: longint);
    procedure SetSpecular(Enable: boolean; r, g, b, shininess: single);
    procedure SetView(X, Y, _Width, _Height: integer);
    procedure SetWireframe(enable: boolean = true);
    procedure ToggleMaxFullscreen;
  end;

  { TFrameBuffer }

  TFrameBuffer = class
  private
    fb, rb: cardinal;
    depth: boolean;
    function GetWidth: integer;
    function GetHeight: integer;
  public
    texture: longint;
    constructor Create(const _texture: longint; _depth: boolean = false); overload;
    constructor Create(const width, height: integer; transparency: boolean;
      _depth: boolean = false); overload;
    destructor Destroy; override;
    procedure Bind;
    procedure SetTexture(const _texture: longint; _unbind: boolean = true);
    procedure UnBind;
    property Height: integer read GetHeight;
    property Width: integer read GetWidth;
  end;

var
  nxFARZ: single = 1000;
  nxNEARZ: single = 0.1;
  nxFov: single = 45;
  nxAspectRatio: single = 1.33;
  nx: TNXGL;
  tex: TGLTextureSet;
  va_states_set: boolean;
  va_pointers_owner: TCustomVertexArray;

  procedure nxInitGL;
  procedure nxFreeModelTextures(model: T3DModel);
  procedure nxLoadModelTextures(model: T3DModel; path: string);

implementation

uses SysUtils, math, nxMath, nxMath3D, nxStrings;

procedure nxInitGL;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(nxFov, nxAspectRatio, nxNEARZ, nxFARZ);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glClearColor(0.0, 0.0, 0.0, 0.0);
  glColor3f(1, 1, 1);
  glEnable(GL_TEXTURE_2D);
  nx.rs.CullBack:=true;
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GREATER, 0.008);
  glEnable(GL_NORMALIZE);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);

  nx.FrameTick:=nx.GetTick; nx.secTick:=nx.FrameTick+1000;
end;

procedure _SetTex(index: TGLuint; tex3D: boolean);
begin
  if not tex3D then glBindTexture(GL_TEXTURE_2D,index)
  else glBindTexture(GL_TEXTURE_3D,index);
end;

procedure _AddBlend(Enable: boolean);
begin
  if Enable then glBlendFunc(GL_SRC_ALPHA,GL_ONE)
  else glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
end;

procedure nxFreeModelTextures(model: T3DModel);
var i: integer;
begin
  with model do
    for i:=0 to mCount-1 do
      if mat[i].texIndex>=0 then tex.RemoveTexture(mat[i].texIndex);
end;

procedure nxLoadModelTextures(model: T3DModel; path: string);
var i, n: longint;
begin
  FixPath(path);
  if (path<>'') and (copy(path, length(path), 1)<>PathChar) then
    path:=path+PathChar;
  with model do
    for i:=0 to mCount-1 do begin
      n:=tex.IndexOf(mat[i].texture);
      if n>=0 then mat[i].texIndex:=n
      else if mat[i].texture<>'' then
        mat[i].texIndex:=tex.AddTexture(mat[i].texture,
          path+mat[i].texture, mat[i].transparent)
      else mat[i].texIndex:=-1;
    end;
end;

{ TFrameBuffer }

function TFrameBuffer.GetWidth: integer;
begin
  result:=tex.texture[texture].sizeX;
end;

function TFrameBuffer.GetHeight: integer;
begin
  result:=tex.texture[texture].sizeY;
end;

constructor TFrameBuffer.Create(const _texture: longint; _depth: boolean);
begin
  depth:=_depth;
  glGenFramebuffersEXT(1,@fb);
  SetTexture(_texture, false);
  if depth then begin
    glGenRenderbuffersEXT(1, @rb);
    glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, rb);
    with tex.texture[texture] do
      glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT24,
        sizeX, sizeY);
    glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT,
      GL_RENDERBUFFER_EXT, rb);
  end;
  unbind;
end;

constructor TFrameBuffer.Create(const width, height: integer; transparency: boolean; _depth: boolean);
var n: longint;
begin
  n:=tex.AddEmptyTexture(tex.NewName('__fb'), width, height, transparency);
  Create(n, _depth);
end;

destructor TFrameBuffer.Destroy;
begin
  glDeleteFramebuffersEXT(1,@fb);
  if depth then
    glDeleteRenderbuffersEXT(1,@rb);
  inherited Destroy;
end;

procedure TFrameBuffer.Bind;
begin
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fb);
  if depth then
    glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, rb);
end;

procedure TFrameBuffer.UnBind;
begin
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  if depth then
    glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
end;

procedure TFrameBuffer.SetTexture(const _texture: longint; _unbind: boolean);
begin
  texture:=_texture;
  Bind;
  glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT,
    GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, tex.texture[texture].index, 0);
  // ! Depth seems to be giving glError $0502 : Invalid operation !
  {if depth then
    with tex.texture[texture] do
      glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT24,
        sizeX, sizeY); }
  if _unbind then Unbind;
end;

{ TRenderSettings }

constructor TRenderSettings.Create(_nx: TNXGL);
begin
  parent:=_nx;
end;

function TRenderSettings.Push: boolean;
begin
  if StackLevel<NX_RS_COUNT-1 then begin
    inc(StackLevel);
    FAddBlend[StackLevel]:=FAddBlend[StackLevel-1];
    FCullBack[StackLevel]:=FCullBack[StackLevel-1];
    FCullFront[StackLevel]:=FCullFront[StackLevel-1];
    FDepthTest[StackLevel]:=FDepthTest[StackLevel-1];
    FLighting[StackLevel]:=FLighting[StackLevel-1];
    FSubBlend[StackLevel]:=FSubBlend[StackLevel-1];
    FWireFrame[StackLevel]:=FWireFrame[StackLevel-1];
    result:=true;
  end else result:=false;
end;

function TRenderSettings.Pop: boolean;
var n: integer;
begin
  if StackLevel>0 then begin
    n:=StackLevel-1;
    AddBlend:=FAddBlend[n];
    CullBack:=FCullBack[n];
    CullFront:=FCullFront[n];
    DepthTest:=FDepthTest[n];
    Lighting:=FLighting[n];
    SubBlend:=FSubBlend[n];
    WireFrame:=FWireFrame[n];
    dec(StackLevel);
    result:=true;
  end else result:=false;
end;

procedure TRenderSettings.ResetStack;
begin
  FAddBlend[0]:=AddBlend;
  FCullBack[0]:=CullBack;
  FCullFront[0]:=CullFront;
  FDepthTest[0]:=DepthTest;
  FLighting[0]:=Lighting;
  FSubBlend[0]:=SubBlend;
  FWireFrame[0]:=WireFrame;
  stackLevel:=0;
end;

function TRenderSettings.GetDepthTest: boolean;
begin
  result:=FDepthTest[stackLevel];
end;

function TRenderSettings.GetAddBlend: boolean;
begin
  result:=FAddBlend[stackLevel];
end;

function TRenderSettings.GetCullBack: boolean;
begin
  result:=FCullBack[stackLevel];
end;

function TRenderSettings.GetCullFront: boolean;
begin
  result:=FCullFront[stackLevel];
end;

function TRenderSettings.GetLighting: boolean;
begin
  result:=FLighting[stackLevel];
end;

function TRenderSettings.GetSubBlend: boolean;
begin
  result:=FSubBlend[stackLevel];
end;

function TRenderSettings.GetWireFrame: boolean;
begin
  result:=FWireFrame[stackLevel];
end;

procedure TRenderSettings.SetAddBlend(enable: boolean);
begin
  if enable<>FAddBlend[stackLevel] then begin
    FAddBlend[stackLevel]:=enable;
    if FAddBlend[stackLevel] and FSubBlend[stackLevel] then begin
      FSubBlend[stackLevel]:=false;
      glBlendEquationEXT(GL_FUNC_ADD_EXT);
    end;
    SetBlend;
  end;
end;

procedure TRenderSettings.SetBlend;
begin
  if SubBlend then begin
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  end else begin
    if AddBlend then glBlendFunc(GL_SRC_ALPHA, GL_ONE)
    else glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end;
end;

procedure TRenderSettings.SetCull;
begin
  if FCullBack[stackLevel] or FCullFront[stackLevel] then begin
    glEnable(GL_CULL_FACE);
    if FCullBack[stackLevel] and FCullFront[stackLevel] then
      glCullFace(GL_FRONT_AND_BACK)
    else if FCullBack[stackLevel] then
      glCullFace(GL_BACK)
    else
      glCullFace(GL_FRONT);
  end else
    glDisable(GL_CULL_FACE);
end;

procedure TRenderSettings.SetCullBack(enable: boolean);
begin
  if enable<>FCullBack[stackLevel] then begin
    FCullBack[stackLevel]:=enable;
    SetCull;
  end;
end;

procedure TRenderSettings.SetCullFront(enable: boolean);
begin
  if enable<>FCullFront[stackLevel] then begin
    FCullFront[stackLevel]:=enable;
    SetCull;
  end;
end;

procedure TRenderSettings.SetDepthTest(enable: boolean);
begin
  if enable<>FDepthTest[stackLevel] then begin
    FDepthTest[stackLevel]:=enable;
    if enable then glEnable(GL_DEPTH_TEST)
    else glDisable(GL_DEPTH_TEST);
  end;
end;

procedure TRenderSettings.SetLighting(enable: boolean);
begin
  if enable<>FLighting[stackLevel] then begin
    FLighting[stackLevel]:=enable;
    if enable then glEnable(GL_LIGHTING)
    else glDisable(GL_LIGHTING);
  end;
end;

procedure TRenderSettings.SetSubBlend(enable: boolean);
begin
  if enable<>FSubBlend[stackLevel] then begin
    FSubBlend[stackLevel]:=enable;
    if FSubBlend[stackLevel] and FAddBlend[stackLevel] then
      FAddBlend[stackLevel]:=false;
    if enable then glBlendEquationEXT(GL_FUNC_REVERSE_SUBTRACT_EXT)
    else glBlendEquationEXT(GL_FUNC_ADD_EXT);
    SetBlend;
  end;
end;

procedure TRenderSettings.SetWireFrame(enable: boolean);
begin
  if enable<>FWireFrame[stackLevel] then begin
    FWireFrame[stackLevel]:=enable;
    if enable then glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
    else glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  end;
end;

{ TNXGL }

procedure TNXGL.Clear(color, depth: boolean);
var mask: TGLbitfield;
begin
  if color then mask:=GL_COLOR_BUFFER_BIT
  else mask:=0;
  if depth then mask:=mask or GL_DEPTH_BUFFER_BIT;
  glClear(mask);
end;

procedure TNXGL.ClearFonts;
var i: integer;
begin
  for i:=0 to FontCount-1 do FFont[i].Free;
  FontCount:=0; setlength(FFont, 0);
end;

procedure TNXGL.CreateBasicFont;
begin
  if FontCount=0 then CreateFont('Arial', 9, 256);
end;

function TNXGL.CreateFont(fontName: string; fontSize, TexSize: integer): integer;
begin
  result:=fontCount;
  inc(FontCount); setlength(FFont, fontcount);
  FFont[result]:=TGLFont.CreateFont(fontName, fontSize, TexSize);
  FFont[result].name:=NewFontName(fontName);
end;

function TNXGL.LoadFont(filename: string): integer;
begin
  result:=fontCount;
  inc(FontCount); setlength(FFont, fontcount);
  FFont[result]:=TGLFont.LoadFont(filename);
  FFont[result].name:=NewFontName(filename);
end;

procedure TNXGL.FontFromImage(TexSize: integer);
begin
  inc(FontCount); setlength(FFont, fontcount);
  FFont[fontcount-1]:=TGLFont.Create(TexSize);
end;

{$IFnDEF NX_CUSTOM_WINDOW}
function TNXGL.CreateGlWindow(hWindow: TWinControl): boolean;
var err: cardinal;
{$IFnDEF fpc}
  pfd: TPIXELFORMATDESCRIPTOR; pf: longint;
{$ELSE}
  parentForm: TForm;
{$ENDIF}
begin
  CreateGlWindow:=false;
  if hWindow=nil then begin
    nxSetError('Error! hWindow is null'); Exit;
  end;
  nxHWND:=hWindow;

  InitOpenGL;

  {$IFDEF fpc}
  try
    window:=TOpenGLControl.Create(hWindow);
    window.Parent:=hWindow;
    window.SendToBack;
    window.SetBounds(0,0,hWindow.Width,hWindow.Height);
    window.OnResize:=@doResize;
    window.Align:=alClient;
    window.MakeCurrent(false);
    width:=window.Width; height:=window.Height;
    ReadExtensions; ReadImplementationProperties;
    if hWindow is TForm then parentForm:=TForm(hWindow)
    else if hWindow.Parent is TForm then
      parentForm:=TForm(hWindow.Parent)
    else parentForm:=nil;

    if parentForm<>nil then with parentForm do begin
      window.Cursor:=Cursor;
      if assigned(onMouseMove) then window.OnMouseMove:=onMouseMove;
      if assigned(onMouseDown) then window.onMouseDown:=onMouseDown;
      if assigned(onMouseUp) then window.onMouseUp:=onMouseUp;
      if assigned(onMouseWheel) then window.onMouseWheel:=onMouseWheel;
      if assigned(onDblClick) then window.onDblClick:=onDblClick;
    end;
  except
    nxHWND:=nil; FreeAndNil(window); exit;
  end;
  nxInitGL;
  {$ELSE} // Delphi
  nxDC:=GetDC(nxHWND.Handle);
  if nxDC=0 then exit;

  zeromemory(@pfd,sizeof(pfd));
  pfd.nSize:=sizeof(pfd); pfd.nVersion:=1;
  pfd.dwFlags:=PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType:=PFD_TYPE_RGBA;
  pfd.cColorBits:=24; pfd.cDepthBits:=16;
  pfd.iLayerType:=PFD_MAIN_PLANE;

  pf:=ChoosePixelFormat(nxDC, @pfd);
  if pf=0 then exit;
  SetPixelFormat(nxDC, pf, @pfd);

  nxRC:=wglCreateContext(nxDC);
  if nxRC=0 then exit;
  wglMakeCurrent(nxDC,nxRC);
  ReadExtensions; ReadImplementationProperties;
  nxInitGL;

  if (nxHWND is TForm) and (not assigned(TForm(nxHWND).OnResize)) then
    TForm(nxHWND).OnResize:=doResize
  else if (nxHWND is TPanel) and (not assigned(TPanel(nxHWND).OnResize)) then
    TPanel(nxHWND).OnResize:=doResize;
  nx.SetView(0,0,nxHWND.ClientWidth,nxHWND.ClientHeight);
  {$ENDIF}

  err:=glGetError();
  if err>0 then begin
    nxSetError(format('CreateGLWindow.Error code: %d',[err]));
  end;

  result:=true;
end;
{$ENDIF}

procedure TNXGL.DefaultLights(Enable: boolean);
var c: TfRGBA;
begin
  if Enable then begin
    rs.Lighting:=true;
    glEnable(GL_LIGHT0); // Ambient directional light
    nx.SetLight(0, GL_AMBIENT, 0.6, 0.6, 0.6);
    nx.SetLight(0, GL_DIFFUSE, 0.7, 0.7, 0.7);
    c:=fRGBA(0, 0, 0, 1);
    glMaterialfv(GL_FRONT, GL_AMBIENT, PGLFloat(@c));
    glMaterialfv(GL_FRONT, GL_SPECULAR, PGLFloat(@c));
    c:=fRGBA(1, 1, 1, 1);
    glMaterialfv(GL_FRONT, GL_DIFFUSE, PGLFloat(@c));
    glEnable(GL_COLOR_MATERIAL);
    glColor3f(0.7, 0.7, 0.7);
  end else
    rs.Lighting:=false;
end;

procedure TNXGL.DeleteFont(index: integer);
begin
  FFont[index].Free;
  dec(FontCount); setlength(FFont, fontcount);
end;

destructor TNXGL.Destroy;
begin
  ClearFonts;
  rs.Free;
  inherited Destroy;
end;

procedure TNXGL.Disable2D;
begin
  if not Enabled2D then exit;
  Enabled2D:=false;
  if nx.Initialized then begin
    glPopMatrix;
    glMatrixMode(GL_PROJECTION);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    rs.Pop;
  end;
end;

{$IFnDEF NX_CUSTOM_WINDOW}
procedure TNXGL.doResize(Sender: TObject);
begin
  {$IFDEF fpc}
  width:=window.Width; height:=window.Height;
  {$ELSE}
  width:=nxHWND.ClientWidth; height:=nxHWND.ClientHeight;
  {$ENDIF}
  SetView(0,0,Width,Height);
  Perspective(PerspectiveStretch);
end;

function TNXGL.GetFFPS: integer;
begin
  result:=FFPS;
end;

{$ENDIF}

procedure TNXGL.Draw(x, y: longint; pattern: word);
var w,h,cols,rows,mult: longint; tx,ty,tw,th,sx,sy: single;
    pTex: PTexture;
begin
  if tex.LastTexIndex<0 then exit;
  pTex:=@nxGL.tex.texture[tex.LastTexIndex];
  if pTex^.PatternWidth>0 then begin
    w:=pTex^.patternwidth; h:=pTex^.patternheight;
    tw:=pTex^.PatternWidth/pTex^.SizeX;
    th:=pTex^.PatternHeight/pTex^.SizeY;
    sx:=pTex^.SkipWidth/pTex^.SizeX;
    sy:=pTex^.SkipHeight/pTex^.SizeY;
    cols:=(pTex^.Width+pTex^.SkipWidth) div
      (pTex^.PatternWidth+pTex^.SkipWidth);
    rows:=(pTex^.Height+pTex^.SkipHeight) div
      (pTex^.PatternHeight+pTex^.SkipHeight);
    mult:=cols*rows;
    if mult>0 then pattern:=pattern mod mult
    else pattern:=0;
    tx:=(pattern mod cols)*(tw+sx);
    ty:=(pattern div cols)*(th+sy);
  end else begin
    w:=pTex^.Width; h:=pTex^.Height;
    tx:=0; ty:=0; tw:=1; th:=1;
  end;
  glBegin(GL_QUADS);
    glTexCoord2f(tx,ty); glVertex2i(x,y);
    glTexCoord2f(tx,ty+th); glVertex2i(x,y+h);
    glTexCoord2f(tx+tw,ty+th); glVertex2i(x+w,y+h);
    glTexCoord2f(tx+tw,ty); glVertex2i(x+w,y);
  glEnd;
end;

procedure TNXGL.DrawRotate(x, y: single; pattern: word; degrees, cx, cy: single);
var w,h,cols,rows: integer; tx,ty,tw,th,sx,sy: single;
    pTex: PTexture;
begin
  if tex.LastTexIndex<0 then exit;
  pTex:=@nxGL.tex.texture[tex.LastTexIndex];
  if pTex^.PatternWidth>0 then begin
    w:=pTex^.patternwidth; h:=pTex^.patternheight;
    tw:=pTex^.PatternWidth/pTex^.SizeX;
    th:=pTex^.PatternHeight/pTex^.SizeY;
    sx:=pTex^.SkipWidth/pTex^.SizeX;
    sy:=pTex^.SkipHeight/pTex^.SizeY;
    cols:=(pTex^.Width+pTex^.SkipWidth) div
      (pTex^.PatternWidth+pTex^.SkipWidth);
    rows:=(pTex^.Height+pTex^.SkipHeight) div
      (pTex^.PatternHeight+pTex^.SkipHeight);
    pattern:=pattern mod (cols*rows);
    tx:=(pattern mod cols)*(tw+sx);
    ty:=(pattern div cols)*(th+sy);
  end else begin
    w:=pTex^.Width; h:=pTex^.Height;
    tx:=0; ty:=0; tw:=1; th:=1;
  end;
  glPushMatrix;
  glTranslatef(x,y,0);
  glRotatef(degrees,0,0,1);
  glTranslatef(-cx*w,-cy*h,0);
  glBegin(GL_QUADS);
    glTexCoord2f(tx,ty); glVertex2f(0,0);
    glTexCoord2f(tx,ty+th); glVertex2f(0,h);
    glTexCoord2f(tx+tw,ty+th); glVertex2f(w,h);
    glTexCoord2f(tx+tw,ty); glVertex2f(w,0);
  glEnd;
  glPopMatrix;
end;

procedure TNXGL.DrawRotateS(x, y: single; pattern: word; degrees, cx, cy, sizeX, sizeY: single);
var tx,ty,tw,th: single;
begin
  tex.GetPatternCoords(tx,ty,tw,th,pattern);
  glPushMatrix;
  glTranslatef(x,y,0);
  glRotatef(degrees,0,0,1);
  glTranslatef(-cx*sizeX,-cy*sizeY,0);
  glBegin(GL_QUADS);
    glTexCoord2f(tx,ty); glVertex2f(0,0);
    glTexCoord2f(tx,ty+th); glVertex2f(0,sizeY);
    glTexCoord2f(tx+tw,ty+th); glVertex2f(sizeX,sizeY);
    glTexCoord2f(tx+tw,ty); glVertex2f(sizeX,0);
  glEnd;
  glPopMatrix;
end;

procedure TNXGL.DrawScaled(x, y, sizeX, sizeY: single; pattern: word);
var tx,ty,tw,th: single;
begin
  {.$HINTS OFF}tex.GetPatternCoords(tx,ty,tw,th,pattern);{.$HINTS ON}
  //glPushMatrix;
  //glTranslatef(x,y,0);
  glBegin(GL_QUADS);
    glTexCoord2f(tx,    ty);    glVertex2f(x,       y);
    glTexCoord2f(tx,    ty+th); glVertex2f(x,       y+sizeY);
    glTexCoord2f(tx+tw, ty+th); glVertex2f(x+sizeX, y+sizeY);
    glTexCoord2f(tx+tw, ty);    glVertex2f(x+sizeX, y);
  glEnd;
  //glPopMatrix;
end;

procedure TNXGL.DrawTexRepeat(x, y, sizeX, sizeY: longint);
var tw,th: single; pTex: PTexture;
begin
  if tex.LastTexIndex<0 then exit;
  tex.SetClamp(true,true);
  pTex:=@nxGL.tex.texture[tex.LastTexIndex];
  tw:=sizeX/pTex^.SizeX;
  th:=sizeY/pTex^.SizeY;
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);  glVertex2f(x,      y);
    glTexCoord2f(0, th); glVertex2f(x,      y+sizeY);
    glTexCoord2f(tw,th); glVertex2f(x+sizeX,y+sizeY);
    glTexCoord2f(tw,0);  glVertex2f(x+sizeX,y);
  glEnd;
end;

procedure TNXGL.DrawSections(x, y, sizeX, sizeY, pattern: longint; bdSize: single);
var tx,ty,tw,th,bw,bh,btw,bth: single; pTex: PTexture;
begin
  {$HINTS OFF}tex.GetPatternCoords(tx,ty,tw,th,pattern);{$HINTS ON}
  bw:=sizeX*bdSize; bh:=sizeY*bdSize;
  btw:=tw*bdSize; bth:=th*bdSize;
  if tex.LastTexIndex>=0 then begin
    pTex:=@tex.texture[tex.LastTexIndex];
    if pTex^.PatternWidth*bdSize<bw then bw:=pTex^.PatternWidth*bdSize;
    if pTex^.PatternHeight*bdSize<bh then bh:=pTex^.PatternHeight*bdSize;
  end;
  glPushMatrix;
  glTranslatef(x,y,0);
  glMatrixMode(GL_TEXTURE);
  glPushMatrix;
  glTranslatef(tx,ty,0);
  glMatrixMode(GL_MODELVIEW);
  // Up sections left to right
  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0, 0); glVertex2f(0, 0); // left
    glTexCoord2f(0, bth); glVertex2f(0, bh);
    glTexCoord2f(btw, 0); glVertex2f(bw, 0); // left border
    glTexCoord2f(btw, bth); glVertex2f(bw, bh);
    glTexCoord2f(tw-btw, 0); glVertex2f(sizeX-bw, 0); // right border
    glTexCoord2f(tw-btw, bth); glVertex2f(sizeX-bw, bh);
    glTexCoord2f(tw, 0); glVertex2f(sizeX, 0); // right
    glTexCoord2f(tw, bth); glVertex2f(sizeX, bh);
  glEnd;
  // Middle sections
  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0, bth); glVertex2f(0, bh);  // left
    glTexCoord2f(0, th-bth); glVertex2f(0, sizeY-bh);
    glTexCoord2f(btw, bth); glVertex2f(bw, bh); // left border
    glTexCoord2f(btw, th-bth); glVertex2f(bw, sizeY-bh);
    glTexCoord2f(tw-btw, bth); glVertex2f(sizeX-bw, bh); // right border
    glTexCoord2f(tw-btw, th-bth); glVertex2f(sizeX-bw, sizeY-bh);
    glTexCoord2f(tw, bth); glVertex2f(sizeX, bh); // right
    glTexCoord2f(tw, th-bth); glVertex2f(sizeX, sizeY-bh);
  glEnd;
  // Bottom sections
  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0, th-bth); glVertex2f(0, sizeY-bh);  // left
    glTexCoord2f(0, th); glVertex2f(0, sizeY);
    glTexCoord2f(btw, th-bth); glVertex2f(bw, sizeY-bh); // left border
    glTexCoord2f(btw, th); glVertex2f(bw, sizeY);
    glTexCoord2f(tw-btw, th-bth); glVertex2f(sizeX-bw, sizeY-bh); // right border
    glTexCoord2f(tw-btw, th); glVertex2f(sizeX-bw, sizeY);
    glTexCoord2f(tw, th-bth); glVertex2f(sizeX, sizeY-bh); // right
    glTexCoord2f(tw, th); glVertex2f(sizeX, sizeY);
  glEnd;
  glMatrixMode(GL_TEXTURE);
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
end;

procedure TNXGL.Enable2D(X, Y, _Width, _Height: integer; flip: boolean);
var temp: boolean;
begin
  if Enabled2D then exit;
  if nx.Initialized then begin
    Enabled2D:=true;
    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    if flip then gluOrtho2D(X,X+_Width,Y+_Height,Y)
    else gluOrtho2D(X,X+_Width,Y,Y+_Height);
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glLoadIdentity;
    rs.Push;
    rs.DepthTest:=false;
    rs.Lighting:=false;
    if not flip then begin
      temp:=rs.CullBack;
      rs.CullBack:=rs.CullFront;
      rs.CullFront:=temp;
    end;
  end;
end;

procedure TNXGL.FillCircle(x, y, radiusX, radiusY: single; sides: longint);
var n: longint; aa: single;
begin
  if sides<3 then exit;
  aa:=-2*pi/sides;
  glBegin(GL_POLYGON);
    for n:=0 to sides-1 do
      glVertex2f(x+radiusX*cos(aa*n), y+radiusY*sin(aa*n));
  glEnd;
end;

procedure TNXGL.FillRect(x, y, _width, _height: longint);
begin
  glBegin(GL_QUADS);
    glVertex2f(x, y); glVertex2f(x, y+_height);
    glVertex2f(x+_width, y+_height); glVertex2f(x+_width, y);
  glEnd;
end;

procedure TNXGL.Enable2D(flip: boolean);
begin
  Enable2D(Left,Top,Width,Height,flip);
end;

{$IFnDEF NX_CUSTOM_WINDOW}
procedure TNXGL.Flip;
begin
  {$IFDEF fpc}if Initialized then window.SwapBuffers;
  {$ELSE}SwapBuffers(nxDC);
  {$ENDIF}
  FrameTick:=GetTick;
  if FrameTick>=secTick then begin
    if secTick<FrameTick-1000 then secTick:=FrameTick+1000
    else inc(secTick, 1000);
    FFPS:=frames; frames:=1;
  end else inc(frames);
end;
{$ENDIF}

function TNXGL.GetEXT: string;
begin
  GetEXT:=glGetString(GL_EXTENSIONS);
end;

procedure TNXGL.GetMouseRay(mx, my: single; const p, normal: PVector;
  rayMove: single);
var view: TGLViewDetails;
    x1,y1,z1,x2,y2,z2: double;
    n: TVector;
begin
  my:=Height-1-my;
  GetViewDetails(@view);
  {$HINTS OFF} // hints about x1..z2 not initialized
  gluUnProject(mx, my, view.modelM[2, 3], view.modelM, view.projM,
    view.viewport, @x1, @y1, @z1);
  gluUnProject(mx, my, view.modelM[2, 3]-1, view.modelM, view.projM,
    view.viewport, @x2, @y2, @z2);
  {$HINTS ON}
  n.x:=x1-x2; n.y:=y1-y2; n.z:=z1-z2; n:=Norm(n);
  if p<>nil then begin
    p^.x:=x1+n.x*rayMove;
    p^.y:=y1+n.y*rayMove;
    p^.z:=z1+n.z*rayMove;
  end;
  if normal<>nil then normal^:=n;
end;

procedure TNXGL.GetMouseRay(mx, my: single; const ray: PMouseRay;
  rayMove: single);
begin
  GetMouseRay(mx, my, @ray^.start, @ray^.dir, rayMove);
end;

procedure TNXGL.GetViewDetails(pdetails: PGLViewDetails);
begin
  if pdetails=nil then exit;
  glGetIntegerv(GL_VIEWPORT, @pdetails^.viewPort);
  glGetDoublev(GL_PROJECTION_MATRIX, @pdetails^.projM);
  glGetDoublev(GL_MODELVIEW_MATRIX, @pdetails^.modelM);
end;

function TNXGL.GLInfo(ext: array of string): boolean;
var s: string; i: longint;
begin
  result:=true; s:=GetEXT;
  for i:=low(ext) to high(ext) do
    result:=result and (pos(ext[i], s)>0);
end;

function TNXGL.GLInfo(ext: string): boolean;
begin
  result:=GLInfo([ext]);
end;

function TNXGL.Initialized: boolean;
begin
  {$IFDEF fpc}
    {$IFDEF NX_CUSTOM_WINDOW}
      // It is impossible to tell *in all cases* if custom
      // window is initialized with fpc
      result:=true; exit; 
    {$ELSE}result:=window<>nil;
  {$ENDIF}
  {$ELSE}result:=nxRC<>0;
  {$ENDIF}
end;

function TNXGL.IsMaxFullscreen: boolean;
var parentForm: TForm;
begin
  result:=false;
  if nxHWND=nil then exit
  else if nxHWND is TForm then parentForm:=TForm(nxHWND)
  else if nxHWND.Parent is TForm then parentForm:=TForm(nxHWND.Parent)
  else exit;
  result:=parentForm.WindowState=wsMaximized;
end;

{$IFnDEF NX_CUSTOM_WINDOW}
procedure TNXGL.KillGLWindow(force: boolean);
begin
  tex.Clear;
  application.ProcessMessages;
  {$IFnDEF fpc}
  if Initialized then DeactivateRenderingContext;
  if nxRC>0 then wglDeleteContext(nxRC);
  if (nxHWND.Handle>0) and (nxDC>0) then
    ReleaseDC(nxHWND.Handle, nxDC);
  nxDC:=0; nxRC:=0;
  {$ELSE}
  if force and (window<>nil) then FreeAndNil(window);
  {$ENDIF}
  nxHWND:=nil;
end;
{$ENDIF}

procedure TNXGL.Line(x, y, x2, y2: longint);
const d = 0.5;
begin
  glBegin(GL_LINES);
    glVertex2f(x+d, y+d); glVertex2f(x2+d, y2+d);
  glEnd;
end;

function TNXGL.MouseRayAtPlane(const mx,my: single; const planePos,
  planeNormal: TVector): TVector;
var rayOrigin, rayDirection: TVector;
begin
  GetMouseRay(mx, my, @rayOrigin, @rayDirection);
  rayPlaneIntersect(rayOrigin, rayDirection, planePos, planeNormal,
    @result);
end;

function TNXGL.MouseRayAtXZPlane(const mx, my: single): TVector;
begin
  result:=MouseRayAtPlane(mx, my, Vector(0,0,0), Vector(0,1,0));
end;

function TNXGL.NewFontName(base: string): string;
var i,n: integer; h: boolean;
begin
  result:=base+'1'; n:=1;
  repeat
    h:=true;
    for i:=0 to FontCount-1 do
      if result=FFont[i].name then begin
        h:=false; inc(n); result:=base+inttostr(n);
      end;
  until h;
end;

function TNXGL.GetFont(index: integer): TGLFont;
begin
  result:=TGLFont(FFont[index]);
end;

function TNXGL.AllOK: boolean;
begin
  result:=Initialized and (nxError='');
end;

function TNXGL.CanRender: boolean;
begin
  result:=glCheckFramebufferStatus(GL_FRAMEBUFFER)=
    GL_FRAMEBUFFER_COMPLETE;
end;

constructor TNXGL.Create;
begin
  rs:=TRenderSettings.Create(self);
  FrameTick:=GetTick; secTick:=FrameTick+1000;
end;

procedure TNXGL.OutLine(x, y, _width, _height: longint);
const d = 0.5;
begin
  glBegin(GL_LINE_LOOP);
    glVertex2f(x+d, y+d); glVertex2f(x+d, y+d+_height-1);
    glVertex2f(x+d+_width-1, y+d+_height-1);
    glVertex2f(x+d+_width-1, y+d);
  glEnd;
end;

procedure TNXGL.Perspective(Stretch: boolean = false);
begin
  PerspectiveStretch:=Stretch;
  if Initialized then begin
    if Height<1 then Height:=1;
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    if Stretch then
      gluPerspective(nxFov, nxAspectRatio, nxNEARZ, nxFARZ)
    else
      gluPerspective(nxFov, Width/Height, nxNEARZ, nxFARZ);
    glMatrixMode(GL_MODELVIEW);
  end;
end;

function TNXGL.ProgDir: string;
begin
  result:=ExtractFilePath(application.ExeName);
end;

procedure TNXGL.RectT(x1,y1,x2,y2: single);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(x1, y1);
    glTexCoord2f(0, 1); glVertex2f(x1, y2);
    glTexCoord2f(1, 1); glVertex2f(x2, y2);
    glTexCoord2f(1, 0); glVertex2f(x2, y1);
  glEnd;
end;

procedure TNXGL.RectZT(x1, z1, x2, z2, y: single);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex3f(x1, y, z1);
    glTexCoord2f(0, 1); glVertex3f(x1, y, z2);
    glTexCoord2f(1, 1); glVertex3f(x2, y, z2);
    glTexCoord2f(1, 0); glVertex3f(x2, y, z1);
  glEnd;
end;

procedure TNXGL.SetClearColor(r, g, b: single; a: single);
begin
  glClearColor(r, g, b, a);
end;

procedure TNXGL.SetSpecular(Enable: boolean; r,g,b,shininess: single);
var c: TfRGBA;
begin
  if Enable then begin
    glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, GL_SEPARATE_SPECULAR_COLOR);
    c.r:=r; c.g:=g; c.b:=b; c.a:=1;
    glMaterialfv(GL_FRONT, GL_SPECULAR, @c);
    glMaterialf(GL_FRONT, GL_SHININESS, shininess);
  end else glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, GL_SINGLE_COLOR);
end;

procedure TNXGL.SetView(X, Y, _Width, _Height: integer);
begin
  Left:=X; Top:=Y; Width:=_Width; Height:=_Height;
  nxAspectRatio:=_Width/_Height;
  if Initialized then begin
    glViewport(X, Y, _Width, _Height);
    {$IFDEF fpc}window.SetBounds(X, Y, _Width, _Height);{$ENDIF}
  end;
end;

procedure TNXGL.SetColor(r, g, b, a: single);
begin
  glColor4f(r, g, b, a);
end;

procedure TNXGL.SetColor(const rgb: TRGB);
begin
  glColor3ubv(@rgb);
end;

procedure TNXGL.SetColor(const rgba: TRGBA);
begin
  glColor4ubv(@rgba);
end;

procedure TNXGL.SetFont(index: integer);
begin
  TGLFont(FFont[index]).SetTexture;
end;

procedure TNXGL.SetColor(r, g, b: single);
begin
  glColor3f(r, g, b);
end;

procedure TNXGL.SetWireframe(enable: boolean = true);
begin
  if enable then glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
  else glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
end;

procedure TNXGL.ToggleMaxFullscreen;
begin
  with TForm(nxHWND) do
    SetMaxFullscreen(not
      ((BorderStyle=bsNone) and (WindowState=wsMaximized)) );
end;

// Use light index starting from 0
procedure TNXGL.SetLight(light, pname: TGLenum; x, y, z: single; w: single);
var v: TVector4f;
begin
  v.x:=x; v.y:=y; v.z:=z; v.w:=w;
  glLightfv(GL_LIGHT0+light, pname, @v);
end;

procedure TNXGL.SetMaxFullscreen(enable: boolean);
var hWindow: TWinControl; parentForm: TForm;
begin
  if nxHWND=nil then exit
  else if nxHWND is TForm then parentForm:=TForm(nxHWND)
  else if nxHWND.Parent is TForm then parentForm:=TForm(nxHWND.Parent)
  else exit;
  with parentForm do begin
    if ((BorderStyle=bsNone) and (WindowState=wsMaximized)) = enable then
      exit; // State is already set
    if enable then begin
      FBorderStyle:=BorderStyle;
      BorderStyle:=bsNone; WindowState:=wsMaximized;
    end else begin
      WindowState:=wsNormal; BorderStyle:=FBorderStyle;
    end;
  end;
  hWindow:=nxHWND;
  KillGLWindow(true);
  CreateGlWindow(hWindow);
end;

procedure TNXGL.SetPixel(x, y: longint);
begin
  glBegin(GL_QUADS);
    glVertex2i(x,y);     glVertex2i(x,y+1);
    glVertex2i(x+1,y+1); glVertex2i(x+1,y);
  glEnd;
end;

{ TGLModel }

procedure TGLModel.CopyToFrame(index: integer);
begin
  setlength(frame[index].va, vCount);
  setlength(frame[index].na, vCount);
  move(va[0], frame[index].va[0], sizeof(va[0])*vCount);
  move(na[0], frame[index].na[0], sizeof(na[0])*vCount);
end;

procedure TGLModel.DisableStates;
begin
  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
end;

procedure TGLModel.FreeTextures;
begin
  nxFreeModelTextures(self);
end;

destructor TGLModel.Destroy;
begin
  FreeTextures;
  inherited Destroy;
end;

procedure TGLModel.Assign(_va: TCustomVertexArray);
var i, j: longint;
begin
  if (_va=nil) or (_va.rendermode<>GL_TRIANGLES) then exit;
  vCount:=_va.vCount;
  fCount:=_va.fCount div 3;
  for i:=0 to fCount-1 do
    for j:=0 to 2 do
      fa[i, j]:=_va.fa[i*3+j];
  for i:=0 to vCount-1 do begin
    va[i]:=_va.va[i]; na[i]:=_va.na[i];
    ta[i].x:=_va.ta[i*2]; ta[i].y:=_va.ta[i*2+1];
  end;
  groups:=1;
  with grp[0] do begin
    first:=0; Count:=fCount; MatIndex:=0;
  end;
end;

procedure TGLModel.AssignTo(_va: TCustomVertexArray);
var i, j: longint;
begin
  if _va=nil then exit;
  _va.rendermode:=GL_TRIANGLES;
  _va._textures:=true; _va._normals:=true; _va._colors:=false;
  _va._3Dtextures:=false; _va._AlphaColors:=false;
  _va.vCount:=vCount; _va.fCount:=fCount*3;
  _va.Count:=_va.GetIndexCount;
  _va.MakeArrays;
  for i:=0 to fCount*3-1 do
    for j:=0 to 2 do
      _va.fa[i*3+j]:=fa[i, j];
  for i:=0 to vCount-1 do begin
    _va.va[i]:=va[i]; _va.na[i]:=na[i];
    _va.ta[i*2]:=ta[i].x; _va.ta[i*2+1]:=ta[i].y;
  end;
end;

procedure TGLModel.EnableStates;
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
end;

procedure TGLModel.LoadTextures(path: string);
begin
  nxLoadModelTextures(self, path);
end;

// Shouldn't be using this function.
// Clientstate changes are not allowed in displaylists,
// which themselves are almost deprecated.
procedure TGLModel.MakeDisplayList(var list: TDisplayList);
var i: integer;
begin
  SetPointers;
  if (list=nil) or (not assigned(list)) then
    list:=TDisplayList.Create(true)
  else list.UpdateList;
  for i:=0 to mCount-1 do
    list.SetTexRef(mat[i].texIndex, true);
  EnableStates;
  Render(false);
  DisableStates;
  list.EndList;
end;

procedure TGLModel.Render(Initialize: boolean);
var i: longint;
begin
  if Initialize then begin
    SetPointers; EnableStates;
  end;
  nx.rs.Push;
  for i:=0 to groups-1 do
    if grp[i].visible then with grp[i] do begin
      if UseMaterials and (matIndex>-1) and (mCount>0) then
        with mat[matIndex] do begin
          tex.SetTex(texIndex);
          nx.SetSpecular(specular>0.01, specular,specular,specular, shininess);
          nx.rs.AddBlend:=addMode;
          if UseColors then glColor4ubv(@color);
        end;
      glDrawElements(GL_TRIANGLES, Count*3, GL_UNSIGNED_SHORT,
        @fa[first]);
    end;
  nx.rs.Pop;
  if Initialize then DisableStates;
end;

procedure TGLModel.SetFrame(t: single; fStart, fEnd: longint; loop: boolean);
var delta: single; a,b,c,d,count: longint;
begin
  delta:=frac(t);
  if fStart>fEnd then begin
    a:=fEnd; fEnd:=fStart; fStart:=a;
  end;
  t:=t-fStart; fEnd:=fEnd-fStart;
  b:=trunc(t);
  if loop then begin
    count:=fEnd+1;
    while b<0 do inc(b,count);
    while b>fEnd do dec(b,count);
    a:=b-1; if a<0 then inc(a,count);
    c:=b+1; if c>fEnd then dec(c,count);
    d:=c+1; if d>fEnd then dec(d,count);
  end else begin // Not looping
    if b<0 then b:=0;
    if b>fEnd then b:=fEnd;
    a:=b-1; if a<0 then a:=0;
    c:=b+1; if c>fEnd then c:=fEnd;
    d:=b+2; if d>fEnd then d:=fEnd;
  end;
  if (a>=0) and (d<frames) then
    SetFrameEx(@frame[a],@frame[b],@frame[c],@frame[d],delta);
end;

procedure TGLModel.SetFrameEx(a, b, c, d: PVertexFrame; delta: single);
var i: longint;
begin
  for i:=0 to vCount-1 do begin
    va[i]:=Catmull(a^.va[i], b^.va[i], c^.va[i], d^.va[i], delta);
    na[i]:=Catmull(a^.na[i], b^.na[i], c^.na[i], d^.na[i], delta);
    //ta[i]:=Catmull(a^.ta[i], b^.ta[i], c^.ta[i], d^.ta[i], delta);
    //va[i]:=Interpolate(b^.va[i], c^.va[i], delta);
    //na[i]:=Interpolate(b^.na[i], c^.na[i], delta);
  end;
end;

procedure TGLModel.SetFrames(count: integer);
var i: integer;
begin
  for i:=count to frames-1 do begin
    setlength(frame[i].va, 0); setlength(frame[i].na, 0);
    //setlength(frame[i].ta, 0);
  end;
  frames:=count; setlength(frame, count);
end;

procedure TGLModel.SetPointers;
begin
  glVertexPointer(3, GL_FLOAT, 0, @(va[0]));
  glNormalPointer(GL_FLOAT, 0, @(na[0]));
  glTexCoordPointer(2, GL_FLOAT, 0, @(ta[0]));
end;

{ TTextureSet }

function TGLTextureSet.AddEmptyTexture(name: string; width, height: word;
  transparency: boolean): longint;
var size,i: cardinal;
begin
  result:=AddTexture2(name,'',transparency);
  if result>=0 then begin
    inc(texture[result].RefCount);
    if texture[result].index=0 then
      glGenTextures(1,@texture[result].index);
    ChangeTexMode3D(false);
    texture[result].Width:=width;
    texture[result].Height:=height;
    with texture[result] do begin
      sizeX:=width; sizeY:=height;
      patternWidth:=width; patternHeight:=height;
      if transparency then begin
        values:=4; Format:=GL_RGBA;
      end else begin
        values:=3; Format:=GL_RGB;
      end;
      intFormat:=Format;
      size:=sizeX*sizeY*values;
      setlength(Data, size);
      fillchar(data[0],size,0);
      if values=4 then begin
        for i:=0 to (size-1) div 4 do
          Data[i*4+3]:=255; // Full alpha
      end;
    end;
    Restore(result);
  end;
end;

function TGLTextureSet.AddTexture(name, filename: string; transparency: boolean): longint;
begin
  FixPath(filename);
  result:=AddTexture2(name, filename, transparency);
  if result>=0 then begin
    inc(texture[result].RefCount);
    if texture[result].index=0 then
      glGenTextures(1,@texture[result].index);
    ChangeTexMode3D(false);
    ReloadTexture(result, filename, transparency);
  end;
end;

function TGLTextureSet.AddTexture(name: string; data: DynamicByteArray;
  width, height, values: integer; format, intFormat: cardinal): longint;
begin
  result:=AddTexture2(name, '', values>3);
  if result>=0 then begin
    texture[result].sizeX:=width;
    texture[result].sizeY:=height;
    texture[result].Width:=width;
    texture[result].Height:=height;
    texture[result].format:=format;
    texture[result].intFormat:=intFormat;
    texture[result].Data:=data;
    texture[result].values:=values;
    inc(texture[result].RefCount);
    if texture[result].index=0 then
      glGenTextures(1,@texture[result].index);
    ChangeTexMode3D(false);
    Restore(result);
    SetPattern(result, width, height, 0, 0);
  end;
end;

function TGLTextureSet.Add3DTexture(name, filename: string; cols, rows: integer; transparency: boolean): longint;
begin
  FixPath(filename);
  result:=Add3DTexture2(name,filename,cols,rows,transparency);
  if result>=0 then begin
    inc(texture[result].RefCount);
    if texture[result].index=0 then
      glGenTextures(1,@texture[result].index);
    ChangeTexMode3D(true);
    ReloadTexture(result,filename,transparency);
  end;
end;

procedure TGLTextureSet.ChangeTexMode3D(_enable: boolean; force: boolean);
begin
  if (enable3Dtex<>_enable) or force then begin
    if _enable then begin
      glDisable(GL_TEXTURE_2D); glEnable(GL_TEXTURE_3D);
    end else begin
      glDisable(GL_TEXTURE_3D); glEnable(GL_TEXTURE_2D);
    end;
    enable3Dtex:=_enable;
  end;
end;

procedure TGLTextureSet.Clear;
var i: longint;
begin
  for i:=0 to Count-1 do
    if texture[i].index>0 then
      glDeleteTextures(1,@texture[i].index);
  inherited Clear;
end;

constructor TGLTextureSet.Create;
begin
  inherited;
  TextureQuality:=GL_LINEAR;
  LastTexIndex:=-1;
end;

destructor TGLTextureSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TGLTextureSet.Disable;
begin
  glDisable(GL_TEXTURE_2D);
end;

procedure TGLTextureSet.Enable;
begin
  glEnable(GL_TEXTURE_2D);
end;

procedure TGLTextureSet.ReloadTexture(n: longint; _filename: string; transparency: boolean);
begin
  if (n>=Count) or (_filename='') then exit;
  with texture[n] do begin
    ChangeTexMode3D(tex3D);
    if transparency then begin
      values:=4; Format:=GL_RGBA;
    end else begin
      values:=3; Format:=GL_RGB;
    end;
    intFormat:=Format;
    if texture[n].Data<>nil then begin
      setlength(texture[n].Data, 0);
    end;
    if not LoadTextureData(@texture[n],_filename) then begin
      if texture[n].Data<>nil then begin
        setlength(texture[n].Data, 0);
      end;
      exit;
    end;
  end;
  if not (toNoLoad in Options) then
    if not texture[n].tex3D then Restore(@texture[n])
    else Restore3D(@texture[n]);
end;

procedure TGLTextureSet.RemoveTexture(n: longint; force: boolean);
begin
  // Uncomment for testing. Wrong n value would be error elsewhere.
  //if (n>=count) or (n<0) then exit;

  dec(texture[n].RefCount);
  if force or (texture[n].RefCount=0) then begin
    if texture[n].index>0 then
      glDeleteTextures(1,@texture[n].index);
    inherited RemoveTexture(n);
  end;
end;

procedure TGLTextureSet.Restore(tex: PTexture);
var i: longint;
begin
  if tex^.data<>nil then begin
    while nx.RenderThreadReserved do begin
      application.ProcessMessages;
    end;
    nx.RenderThreadReserved:=true;
    glFinish;
    if tex^.values>0 then begin
      if tex^.format=0 then begin
        if tex^.values=4 then tex^.format:=GL_RGBA
        else if tex^.values=3 then tex^.format:=GL_RGB;
        tex^.intFormat:=tex^.Format;
      end;
      i:=FindTex(tex);
      if i<0 then begin
        _SetTex(tex^.index,tex^.tex3D);
        LastTexIndex:=-17;
      end else SetTex(i,true);
      glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, TextureQuality);
      if not (toMipMap in Options) then begin
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, TextureQuality);
        glTexImage2D(GL_TEXTURE_2D, 0, tex^.intFormat, tex^.sizeX, tex^.sizeY, 0,
          tex^.Format, GL_UNSIGNED_BYTE, @tex^.Data[0]);
      end else begin
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
        gluBuild2DMipmaps(GL_TEXTURE_2D, tex^.Values, tex^.sizeX, tex^.sizeY,
          tex^.Format, GL_UNSIGNED_BYTE, @tex^.Data[0]);
      end;
    end;
    if not (toKeepData in Options) then begin
      setlength(tex^.data, 0);
    end;
    nx.RenderThreadReserved:=false;
  end;
end;

procedure TGLTextureSet.Restore(tex: longint);
begin
  if tex>=0 then Restore(@texture[tex]);
end;

procedure TGLTextureSet.Restore3D(tex: PTexture);
var temp: TTexture; i,j,k,rs,dest,src: longint;
begin
  if tex^.data<>nil then begin
    while nx.RenderThreadReserved do begin
      application.ProcessMessages;
    end;
    nx.RenderThreadReserved:=true;
    glFinish;
    if tex^.values>0 then begin
      if tex^.Format=0 then begin
        if tex^.values=4 then tex^.format:=GL_RGBA
        else if tex^.values=3 then tex^.format:=GL_RGB;
        tex^.intFormat:=tex^.Format;
      end;
      i:=FindTex(tex);
      if i<0 then begin
        _SetTex(tex^.index,tex^.tex3D);
        LastTexIndex:=-17;
      end else SetTex(i,true);
      temp:=tex^; tex^.Data:=nil;
      tex^.sizeX:=temp.sizeX div cols3D;
      tex^.sizeY:=temp.sizeY div rows3D;
      tex^.sizeZ:=cols3D*rows3D;
      rs:=tex^.sizeX*tex^.values; // Row size
      try
        setlength(tex^.Data, tex^.sizeX*tex^.sizeY*tex^.sizeZ*tex^.values);
        dest:=0;
        for k:=0 to tex^.sizeZ-1 do
          for j:=0 to tex^.sizeY-1 do begin
            src:=(k mod cols3D+j*cols3D+(k div cols3D)*tex^.sizeY*cols3D)*rs;
            move(temp.Data[src], tex^.Data[dest], rs);
            dest:=dest+rs;
          end;
      finally
        setlength(temp.data, 0);
      end;
      _SetTex(tex^.index, tex^.tex3D);
      glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, TextureQuality);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_REPEAT);
      //if not (toMipMap in Options) then begin
        glTexParameteri(GL_TEXTURE_3D,GL_TEXTURE_MIN_FILTER, TextureQuality);
        glTexImage3D(GL_TEXTURE_3D, 0, tex^.intFormat, tex^.sizeX, tex^.sizeY, tex^.sizeZ,
          0, tex^.Format, GL_UNSIGNED_BYTE, @tex^.Data[0]);
      //end else begin

        // insert 3D MipMapping here

        //glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
        //gluBuild3DMipmaps(GL_TEXTURE_3D, tex^.values, tex^.sizeX, tex^.sizeY, tex^.sizeZ,
        //  tex^.Format, GL_UNSIGNED_BYTE, tex^.Data);
      //end;
      if not (toKeepData in Options) then begin
        setlength(tex^.data, 0);
      end;
    end;
    nx.RenderThreadReserved:=false;
  end;
end;

procedure TGLTextureSet.SetByName(name: string; force: boolean);
var i: longint;
begin
  i:=indexof(name);
  if i>=0 then settex(i, force);
end;

procedure TGLTextureSet.SetClamp(x_repeat, y_repeat: boolean);
var x, y, x2, y2: TGLint; target: TGLenum;
begin
  if x_repeat then x:=GL_REPEAT else x:=GL_CLAMP;
  if y_repeat then y:=GL_REPEAT else y:=GL_CLAMP;
  if not enable3Dtex then target:=GL_TEXTURE_2D
  else target:=GL_TEXTURE_3D;
  glGetTexParameteriv(target, GL_TEXTURE_WRAP_S, @x2);
  glGetTexParameteriv(target, GL_TEXTURE_WRAP_T, @y2);
  if x<>x2 then glTexParameteri(target, GL_TEXTURE_WRAP_S, x);
  if y<>y2 then glTexParameteri(target, GL_TEXTURE_WRAP_T, y);
end;

procedure TGLTextureSet.Restore3D(tex: longint);
begin
  if tex>=0 then Restore3D(@texture[tex]);
end;

procedure TGLTextureSet.SetTex(n: longint; force: boolean);
begin
  if (n<>LastTexIndex) or force then begin
    if (n>=0) and (n<count) then begin
      ChangeTexMode3D(texture[n].tex3D);
      _SetTex(texture[n].index,texture[n].tex3D);
    end else
      _SetTex(0,enable3Dtex);
    LastTexIndex:=n;
  end;
end;

procedure TGLTextureSet.SetTextureUnit(n: integer);
begin
  if FTextureUnit<>n then begin
    LastTexIndex:=-2; FTextureUnit:=n;
  end;
  if n>=0 then glActiveTexture(GL_TEXTURE0+n);
end;

function TGLModel.NewMaterial(_texIndex: longint): integer;
begin
  mCount:=mCount+1; result:=mCount-1;
  with mat[result] do begin
    texIndex:=_texIndex;
    if texIndex>=0 then inc(tex.texture[texIndex].RefCount);
    shininess:=20; specular:=0.8;
  end;
  if (mCount=1) and (UseMaterials=false) then UseMaterials:=true;
end;

{ TVertexArray }

procedure TVertexArray.DisableStates;
begin
  glDisableClientState(GL_VERTEX_ARRAY);
  if _normals then glDisableClientState(GL_NORMAL_ARRAY);
  if _textures or _3Dtextures then glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  if _colors or _AlphaColors then glDisableClientState(GL_COLOR_ARRAY);
  va_states_set:=false;
end;

procedure TVertexArray.EnableStates;
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  if _normals then glEnableClientState(GL_NORMAL_ARRAY);
  if _textures or _3Dtextures then glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  if _colors or _AlphaColors then glEnableClientState(GL_COLOR_ARRAY);
  va_states_set:=true;
end;

function TVertexArray.GetIndexCount(const faceCount: longint): longint;
begin
  if faceCount>0 then begin
    case rendermode of
      GL_TRIANGLES: result:=faceCount*3;      // TRIANGLES
      GL_QUADS: result:=faceCount*4;          // QUADS
      GL_TRIANGLE_STRIP: result:=faceCount+2; // TRIANGLE_STRIP
      GL_TRIANGLE_FAN: result:=faceCount+1;   // TRIANGLE_FAN
      GL_LINES: result:=faceCount*2;          // LINES
      GL_LINE_LOOP: result:=faceCount;        // LINE_LOOP
      GL_LINE_STRIP: result:=faceCount+1;     // LINE_STRIP
      else result:=faceCount;                 // POINTS
    end;
  end else
    result:=0;
end;

procedure TVertexArray.Render(Indexed: boolean);
begin
  Render(0, fCount, Indexed);
end;

procedure TVertexArray.Render(first, _count: longint; Indexed: boolean);
begin
  if not va_states_set then EnableStates;
  if va_pointers_owner<>self then SetPointers;
  if Indexed then glDrawElements(rendermode, GetIndexCount(_count), GL_UNSIGNED_SHORT, @(fa[first]))
  else glDrawArrays(rendermode, first, _count);
  DisableStates;
end;

procedure TVertexArray.SetPointers;
begin                           
  glVertexPointer(3, GL_FLOAT, 0, @(va[0]));
  if _normals then glNormalPointer(GL_FLOAT, 0, @(na[0]));
  if _textures then glTexCoordPointer(TextureComponents, GL_FLOAT, 0, @(ta[0]));
  if _colors then glColorPointer(ColorComponents, GL_UNSIGNED_BYTE, 0, @(ca[0]));
  va_pointers_owner:=self;
end;

{ TGLFont }

constructor TGLFont.CreateFont(fontName: string; fontSize, _TexSize: longint);
begin
  CreateBMP(fontName, fontSize, _TexSize);
  tex.texture[textureI].Format:=GL_RGBA;
  tex.texture[textureI].intFormat:=GL_RGBA;
  glGenTextures(1, @tex.texture[textureI].index);
  tex.ChangeTexMode3D(false);
  tex.SetTex(textureI, true);
  tex.Restore(@tex.texture[textureI]);
  SetColor(1, 1, 1, 1);
end;

constructor TGLFont.LoadFont(filename: string);
begin
  inherited Load(filename);
  tex.texture[textureI].Format:=GL_RGBA;
  tex.texture[textureI].intFormat:=GL_RGBA;
  glGenTextures(1, @tex.texture[textureI].index);
  tex.ChangeTexMode3D(false);
  tex.SetTex(textureI, true);
  tex.Restore(@tex.texture[textureI]);
  SetColor(1, 1, 1, 1);
end;

procedure TGLFont.Draw(x, y: single; s: string; maxW: integer);
var i,cw,n,x1,y1,wLimit: longint; d1,tx,ty: single;
begin
  wLimit:=0;
  //if UseUTF8 then s:=UTF8toSys(s);
  d1:=1/TexSize; x1:=round(x); y1:=round(y);
  glBegin(GL_QUADS);
    for i:=1 to UTF8Length(s) do begin
      n:=byte(UTFToChr(UTF8Copy(s, i, 1))); cw:=charW[n];
      if maxW>0 then begin
        wLimit:=wLimit+cw;
        if wLimit>maxW then break;
      end;
      n:=n-32; tx:=(n mod 16)*sx*d1+d1; ty:=(n div 16)*sy*d1;
      if n>0 then begin // Skip spaces
        glTexCoord2f(tx,ty); glVertex2f(x1,y1);
        glTexCoord2f(tx,ty+(sy-1)*d1); glVertex2i(x1,y1+sy-1);
        glTexCoord2f(tx+cw*d1,ty+(sy-1)*d1); glVertex2i(x1+cw,y1+sy-1);
        glTexCoord2f(tx+cw*d1,ty); glVertex2i(x1+cw,y1);
      end;
      x1:=x1+cw;
    end;
  glEnd;
end;

procedure TGLFont.DrawScaled(x, y, scaleX, scaleY: single; s: string; maxW: integer);
begin
  glPushMatrix;
  glTranslatef(x, y, 0);
  glScalef(scaleX, scaleY, 1);
  Draw(0, 0, s, maxW);
  glPopMatrix;
end;

procedure TGLFont.DrawCScaled(x, y, scaleX, scaleY: single; s: string; maxW: integer);
begin
  glPushMatrix;
  glTranslatef(x, y, 0);
  glScalef(scaleX, scaleY, 1);
  inherited DrawC(0, 0, s, maxW);
  glPopMatrix;
end;

procedure TGLFont.DrawRotate(x, y, scaleX, scaleY, _angle: single; s: string; maxW: integer);
begin
  glPushMatrix;
  glTranslatef(x, y, 0);
  glRotatef(_angle, 0,0,1);
  glScalef(scaleX, scaleY, 1);
  inherited DrawC(0, 0, s, maxW);
  glPopMatrix;
end;

procedure TGLFont.DrawTextArea(r: TBoundsRect; s: TStrings; x_scroll, y_scroll: longint);
var i,l,cw,n,x1,y1: longint; d1,tx,ty: single; s2: string;
begin
  y1:=r.y; d1:=1/TexSize; //s:=UTF8toSys(s);
  glBegin(GL_QUADS);
  for l:=y_scroll to s.Count-1 do begin
    x1:=r.x-x_scroll; s2:=UTF8toSys(s[l]);
    for i:=1 to length(s2) do begin
      n:=byte(s2[i]);
      if n>31 then begin
        cw:=charW[n];
        if x1+cw>r.x+r.w then break
        else if x1>=r.x then begin
          n:=n-32; tx:=(n mod 16)*sx*d1+d1; ty:=(n div 16)*sy*d1;
          glTexCoord2f(tx,ty); glVertex2f(x1,y1);
          glTexCoord2f(tx,ty+(sy-1)*d1); glVertex2i(x1,y1+sy-1);
          glTexCoord2f(tx+cw*d1,ty+(sy-1)*d1); glVertex2i(x1+cw,y1+sy-1);
          glTexCoord2f(tx+cw*d1,ty); glVertex2i(x1+cw,y1);
        end;
        inc(x1,cw);
      end;
    end;
    inc(y1,height);
    if y1+height>r.y+r.h then break;
  end;
  glEnd;
end;

procedure TGLFont.DrawWrap(r: TBoundsRect; s: TStrings; y_scroll: longint);
var i,l,cw,n,x1,y1: longint; d1,tx,ty: single; s2: string;
begin
  y1:=r.y; d1:=1/TexSize;
  glBegin(GL_QUADS);
  for l:=y_scroll to s.Count-1 do begin
    x1:=r.x; s2:=UTF8toSys(s[l]);
    for i:=1 to length(s2) do begin
      n:=byte(s2[i]);
      if n>31 then begin
        cw:=charW[n];
        if x1+cw>r.x+r.w then begin
          inc(y1,height); x1:=r.x;
          if y1+height>r.y+r.h then begin
            glEnd; exit;
          end;
        end;
        n:=n-32; tx:=(n mod 16)*sx*d1+d1; ty:=(n div 16)*sy*d1;
        glTexCoord2f(tx,ty); glVertex2f(x1,y1);
        glTexCoord2f(tx,ty+(sy-1)*d1); glVertex2i(x1,y1+sy-1);
        glTexCoord2f(tx+cw*d1,ty+(sy-1)*d1); glVertex2i(x1+cw,y1+sy-1);
        glTexCoord2f(tx+cw*d1,ty); glVertex2i(x1+cw,y1);
        inc(x1,cw);
      end;
    end;
    inc(y1,height);
    if y1+height>r.y+r.h then break;
  end;
  glEnd;
end;

function TGLFont.RDraw(renderer: TGLRenderer; x, y, scaleX, scaleY: single; s: string; maxW: single): integer;
var x1, y1, tx, ty, d1, tw, th, cw2, sy2, wLimit: single;
    i, cw, n, quad: longint; UseLimiter: boolean;
    mColor: TfRGBA;
begin
  result:=-1;
  mColor:=MultiplyColor(color, renderer.color);
  with renderer do begin
    if FisBuffer3D or (not FisEnabled) then Enable2D;
    wLimit:=0;
    d1:=1/TexSize; x1:=x; y1:=y;
    sy2:=(sy-1)*scaleY;
    UseLimiter:=maxW>0;
    for i:=1 to UTF8Length(s) do begin
      n:=byte(UTFToChr(UTF8Copy(s, i, 1)));
      cw:=charW[n]; cw2:=cw*scaleX;
      if UseLimiter then begin
        wLimit:=wLimit+cw2;
        if wLimit>maxW then break;
      end;
      n:=n-32; tx:=(n mod 16)*sx*d1+d1; ty:=(n div 16)*sy*d1;
      if n>0 then begin // Skip spaces
        quad:=NewQuad;
        if result<0 then result:=quad;
        tw:=cw*d1; th:=(sy-1)*d1;
        SetVertex(quad,   x1,     y1,     tx,    ty);
        SetVertex(quad+1, x1,     y1+sy2, tx,    ty+th);
        SetVertex(quad+2, x1+cw2, y1+sy2, tx+tw, ty+th);
        SetVertex(quad+3, x1+cw2, y1,     tx+tw, ty);
        with mColor do begin
          SetColor(quad,   r, g, b, a);
          SetColor(quad+1, r, g, b, a);
          SetColor(quad+2, r, g, b, a);
          SetColor(quad+3, r, g, b, a);
        end;
      end;
      x1:=x1+cw2;
    end;
  end;
end;

function TGLFont.RDrawC(renderer: TGLRenderer; x, y, scaleX, scaleY: single; s: string; maxW: single): integer;
var w: single;
begin
  w:=(TextW(s)*0.5)*scaleX;
  if (maxW>0) and (w>maxW/2) then w:=maxW/2;
  result:=RDraw(renderer, x-w, y-CenterH*scaleY, scaleX, scaleY, s, maxW);
end;

procedure TGLFont.SetColor;
begin
  glColor4fv(@color);
end;

procedure TGLFont.SetTexture;
begin
  tex.SetTex(textureI);
end;

{ TDisplayList }

constructor TDisplayList.Create(make: boolean);
begin
  TexRefCount:=0;
  list:=glGenLists(1);
  if make then UpdateList;
end;

destructor TDisplayList.Destroy;
var i: longint;
begin
  for i:=TexRefCount-1 downto 0 do
    tex.RemoveTexture(texRef[i]);
  glDeleteLists(list,1);
  inherited Destroy;
end;

procedure TDisplayList.Draw;
begin
  glCallList(list);
  tex.LastTexIndex:=-5;
end;

procedure TDisplayList.DrawAt(x, y, z: single);
begin
  glPushMatrix;
  glTranslatef(x,y,z); Draw;
  glPopMatrix;
end;

procedure TDisplayList.SetTexRef(TexN: longint; isUsed: boolean);
var i: longint;
begin
  if TexN<0 then exit;
  if isUsed then begin
    for i:=0 to texRefCount-1 do
      if texRef[i]=TexN then exit;
    if TexN<tex.count then begin                
      inc(tex.texture[TexN].RefCount);
      inc(texRefCount); setlength(texRef, texRefCount);
      texRef[texRefCount-1]:=TexN;
    end;
  end else begin
    for i:=0 to texRefCount-1 do
      if texRef[i]=TexN then begin
        dec(tex.texture[TexN].RefCount);
        dec(texRefCount); texRef[i]:=texRef[texRefCount];
        setlength(texRef, texRefCount); exit;
      end;
  end;
end;

procedure TDisplayList.EndList;
begin
  glEndList;
end;

procedure TDisplayList.UpdateList(render: boolean);
var i: longint;
begin
  tex.LastTexIndex:=-6; // Force first texture to show
  if render then glNewList(list, GL_COMPILE_AND_EXECUTE)
  else glNewList(list, GL_COMPILE);
  for i:=0 to TexRefCount-1 do
    dec(tex.texture[texRef[i]].RefCount);
  texRefCount:=0;
end;

{ TGLShader }

destructor TGLShader.Destroy;
var i: integer;
begin
  for i:=0 to high(programs) do programs[i].Free;
  for i:=0 to high(shaders) do shaders[i].Free;
  inherited Destroy;
end;

function TGLShader.AddProgram(const autoAttach: boolean): TShaderProgram;
var i: integer;
begin
  result:=TShaderProgram.Create;
  setlength(programs, length(programs)+1);
  programs[high(programs)]:=result;
  current:=result;
  if autoAttach then
    for i:=high(shaders)-1 to high(shaders) do
      current.AttachShader(shaders[i]);
end;

function TGLShader.AddProgramAndLink: boolean;
begin
  AddProgram; result:=Link;
end;

function TGLShader.AddShader(const s: string; isVertex: boolean): TShaderSource;
begin
  if isVertex then result:=TShaderSource.CreateVertex
  else result:=TShaderSource.CreateFragment;
  setlength(shaders, length(shaders)+1);
  shaders[high(shaders)]:=result;
  result.SetSource(s);
end;

function TGLShader.AddDefault2D: TShaderProgram;
var sl: TStringList;
begin
  sl:=TStringList.Create;
  MakeVShader2D(sl); AddShader(sl.Text, true);
  MakeFShader2D(sl); AddShader(sl.Text, false);
  sl.Free;
  result:=AddProgram(true);
end;

function TGLShader.AddDefault3D(bump: boolean): TShaderProgram;
var sl: TStringList;
begin
  sl:=TStringList.Create;
  MakeVShader3D(sl);       AddShader(sl.Text, true);
  MakeFShader3D(sl, bump); AddShader(sl.Text, false);
  sl.Free;
  result:=AddProgram(true);
end;

procedure TGLShader.AttachShader(index: integer);
begin
  if current<>nil then
    current.AttachShader(shaders[index]);
end;

procedure TGLShader.DeleteProgram(index: integer);
var i: integer;
begin
  current:=nil;
  programs[index].Free;
  for i:=index to high(programs)-1 do
    programs[i]:=programs[i+1];
  setlength(programs, high(programs));
end;

procedure TGLShader.Disable;
begin
  if current<>nil then current.Disable;
end;

procedure TGLShader.Enable;
begin
  if current<>nil then current.Enable;
end;

function TGLShader.GetUniform(uname: string): GLint;
begin
  if current<>nil then result:=current.GetUniform(uname)
  else result:=-1;
end;

function TGLShader.isCompiled: boolean;
begin
  if current<>nil then result:=current.isCompiled
  else result:=false;
end;

function TGLShader.Link: boolean;
begin
  if (current<>nil) and current.isCompiled then result:=current.Link
  else begin
    result:=false;
    if length(programs)=0 then nxSetError('No shader program(s) created')
    else if current<>nil then nxSetError('No shader program selected')
    else nxSetError('Cannot link with shader compile errors');
  end;
end;

function TGLShader.LoadShader(filename: string; isVertex: boolean): TShaderSource;
var sl: TStringList;
begin
  sl:=TStringList.Create;
  FixPath(filename);
  sl.LoadFromFile(filename);
  result:=AddShader(sl.Text, isVertex);
  sl.Free;
end;

procedure TGLShader.SelectProgram(index: integer; doEnable: boolean);
begin
  current:=programs[index];
  if doEnable then current.Enable;
end;

procedure TGLShader.SelectProgram(prog: TShaderProgram; doEnable: boolean);
begin
  current:=prog;
  if doEnable then current.Enable;
end;

{ TCamera }

constructor TCamera.Create;
begin
  setlength(mat, 1); index:=0; Reset(false);
end;

destructor TCamera.Destroy;
var i: longint;
begin
  for i:=0 to high(path) do path[i].Free;
  inherited Destroy;
end;

function TCamera.AddPath: TCameraPath;
begin
  setlength(path, length(path)+1);
  result:=TCameraPath.Create(self);
  path[high(path)]:=result;
end;

procedure TCamera.DeletePath(n: longint);
var i: longint;
begin
  if (n>=0) and (n<length(path)) then begin
    for i:=n to high(path)-1 do
      path[i]:=path[i+1];
    setlength(path, length(path)-1);
  end;
end;

procedure TCamera.GetFromModelView;
begin
  glGetFloatv(GL_MODELVIEW_MATRIX, @mat[index]);
end;

function TCamera.GetMatrix: TMatrix;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result:=mat[index];
end;

function TCamera.GetVector(const axis: integer): TVector;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result:=nxMath3D.GetVector(mat[index], axis);
end;

function TCamera.IndexOfPath(p: TCameraPath): longint;
var i: longint;
begin
  for i:=0 to high(path) do
    if path[i]=p then begin
      result:=i; exit;
    end;
  result:=-1;
end;

procedure TCamera.Interpolate(const mat2: TMatrix; const delta: single; doSet: boolean);
begin
  mat[index]:=nxMath3D.Interpolate(mat[index], mat2, delta);
  if doSet then SetCamera;
end;

procedure TCamera.Load(n: longint; doSet: boolean);
begin
  if (n>=0) and (n<=high(saved)) then begin
    mat[index]:=saved[n];
    if doSet then SetCamera;
  end;
end;

procedure TCamera.LookAt(const eye, target, up: TVector; doSet: boolean);stdcall;{$IFDEF CanInline}inline;{$ENDIF}
begin
  mat[index]:=nxMath3D.LookAt(eye, target, up);
  if doSet then SetCamera;
end;

procedure TCamera.LookAt(const target, up: TVector; doSet: boolean);
begin
  self.LookAt(nxMath3D.GetVector(mat[index], 3), target, up, doSet);
end;

procedure TCamera.LookAt(const target: TVector; doSet: boolean);
begin
  self.LookAt(nxMath3D.GetVector(mat[index], 3), target,
    nxMath3D.GetVector(mat[index], 1), doSet);
end;

procedure TCamera.Multiply(const mat2: TMatrix; doSet: boolean); stdcall;{$IFDEF CanInline}inline;{$ENDIF}
begin
  mat[index]:=nxMath3D.Multiply(mat2, mat[index]);
  if doSet then SetCamera;
end;

procedure TCamera.Multiply(const mat2: TMatrix3f; doSet: boolean); stdcall; {$IFDEF CanInline}inline;{$ENDIF}
begin
  mat[index]:=nxMath3D.Multiply(mat2, mat[index]);
  if doSet then SetCamera;
end;

function TCamera.PathCount: longint;
begin
  result:=length(path);
end;

procedure TCamera.Pop(doSet: boolean);
begin
  if index>0 then begin
    dec(index);
    if doSet then SetCamera;
  end;
end;

procedure TCamera.Push;
begin
  if index<high(index) then begin
    inc(index);
    if index>high(mat) then setlength(mat, index+1);
    mat[index]:=mat[index-1];
  end;
end;

procedure TCamera.Reset(doSet: boolean);
begin
  mat[index]:=NewMatrix;
  if doSet then SetCamera;
end;

procedure TCamera.ResetStack;
begin
  mat[0]:=mat[index]; index:=0; setlength(mat, 1);
end;

procedure TCamera.Rotate(const degrees: single; const axis: TVector; doSet: boolean);
begin
  mat[index]:=nxMath3D.Multiply(CreateMatrix(norm(axis), degrees*toRad), mat[index]);
  if doSet then SetCamera;
end;

procedure TCamera.Rotate(const degrees: single; const ax, ay, az: single; doSet: boolean);
begin
  self.Rotate(degrees, vector(ax, ay, az), doSet);
end;

function TCamera.SaveCount: longint;
begin
  result:=length(saved);
end;

procedure TCamera.Save(n: longint);
begin
  if n>=0 then begin
    if n>high(saved) then setlength(saved, n+1);
    saved[n]:=mat[index];
  end;
end;

procedure TCamera.Scale(const s: single; doSet: boolean);
begin
  mat[index]:=nxMath3D.Scale(mat[index], s);
  if doSet then SetCamera;
end;

procedure TCamera.SetCamera;
begin
  glLoadMatrixf(@mat[index]);
end;

procedure TCamera.Translate(const x, y, z: single; doSet: boolean);
begin
  self.Translate(vector(x, y, z), doSet);
end;

procedure TCamera.Translate(const v: TVector; doSet: boolean);
begin
  mat[index]:=nxMath3D.Multiply(CreateTranslateMatrix(v), mat[index]);
  if doSet then SetCamera;
end;

{ TCameraPath }

constructor TCameraPath.Create(owner: TCamera);
begin
  parent:=owner; pathmode:=pmSmooth;
  wrapping:=true; AutoSetCamera:=false;
end;

procedure TCameraPath.SetPosition(p: single);
var c: longint;
begin
  c:=Count;
  if c=0 then FPosition:=0
  else if wrapping then begin
    if p<0 then FPosition:=p+(((abs(floor(p))-1) div c)+1)*c
    else FPosition:=fmod(p, c);
  end else
    FPosition:=max(0, min(c, p));
  mat:=GetMatrix(FPosition);
  if AutoSetCamera then SetCamera;
end;

function TCameraPath.AddNode(const _mat: TMatrix): longint;
begin
  setlength(node, length(node)+1);
  result:=high(node); node[result]:=_mat;
  if result=0 then mat:=_mat;
end;

function TCameraPath.Count: longint;
begin
  result:=length(node);
end;

function TCameraPath.GetMatrix(const dTime: single): TMatrix;{$IFDEF CanInline}inline;{$ENDIF}
var i, j, m0, m1, m2, m3, c: longint; delta: single;
begin
  c:=Count; m0:=floor(dTime)-1; delta:=dTime-(m0+1);
  if wrapping then begin
    if m0<0 then m0:=m0+(((abs(m0)-1) div c)+1)*c;
    m1:=(m0+1) mod c; m2:=(m0+2) mod c; m3:=(m0+3) mod c;
  end else begin
    m1:=min(c, max(0, m0+1));
    m2:=min(c, max(0, m0+2));
    m3:=min(c, max(0, m0+3));
    m0:=min(c, max(0, m0));
  end;
  for j:=0 to 3 do
    for i:=0 to 3 do begin
      case pathmode of
        pmInterpolate: result[i, j]:=Interpolate(node[m1][i, j], node[m2][i, j], delta);
        pmSmooth: result[i, j]:=Smoothen(node[m1][i, j], node[m2][i, j], delta);
        pmCatmull: result[i, j]:=Catmull(node[m0][i, j], node[m1][i, j],
          node[m2][i, j], node[m3][i, j], delta);
      end;
    end;
end;

procedure TCameraPath.SetCamera;
begin
  if parent<>nil then begin
    parent.mat[parent.index]:=mat;
    parent.SetCamera;
  end;
end;

{ TVBO }

function TVBO.StaticVal: cardinal;
begin
  if FStatic then result:=GL_STATIC_DRAW
  else result:=GL_DYNAMIC_DRAW;
end;

constructor TVBO.Create;
begin
  FStatic:=false; dataIsSet:=false;
  glGenBuffers(1, @bIndex);
  glGenBuffers(1, @bVertex);
  if _normals then glGenBuffers(1, @bNormal);
  if _textures then glGenBuffers(1, @bTexture);
  if _colors then glGenBuffers(1, @bColor);
end;

constructor TVBO.Create(Faces, Vertices: longint; _rendermode: cardinal; textures, normals, colors: boolean);
begin
  inherited Create(Faces, Vertices, _rendermode, textures, normals, colors);
  Create;
end;

constructor TVBO.Create(Faces: longint; _rendermode: cardinal; textures, normals, colors: boolean);
begin
  inherited Create(Faces, _rendermode, textures, normals, colors);
  Create;
end;

destructor TVBO.Destroy;
begin
  glDeleteBuffers(1, @bIndex);
  glDeleteBuffers(1, @bVertex);
  if _normals then glDeleteBuffers(1, @bNormal);
  if _textures then glDeleteBuffers(1, @bTexture);
  if _colors then glDeleteBuffers(1, @bColor);
  inherited Destroy;
end;

function TVBO.GetIndexCount(const faceCount: longint): longint;
begin
  if faceCount>0 then begin
    case rendermode of
      GL_TRIANGLES: result:=faceCount*3;      // TRIANGLES
      GL_QUADS: result:=faceCount*4;          // QUADS
      GL_TRIANGLE_STRIP: result:=faceCount+2; // TRIANGLE_STRIP
      GL_TRIANGLE_FAN: result:=faceCount+1;   // TRIANGLE_FAN
      GL_LINES: result:=faceCount*2;          // LINES
      GL_LINE_LOOP: result:=faceCount;        // LINE_LOOP
      GL_LINE_STRIP: result:=faceCount+1;     // LINE_STRIP
      else result:=faceCount;                 // POINTS
    end;
  end else
    result:=0;
end;

function TVBO.IsStatic: boolean;
begin
  result:=FStatic;
end;

procedure TVBO.Render;
begin
  Render(0, fCount);
end;

procedure TVBO.Render(first, _count: longint);
begin
  SetPointers;
  glDrawElements(rendermode, GetIndexCount(_count), GL_UNSIGNED_SHORT,
    {%H-}pointer(first*sizeof(word)) );
end;

procedure TVBO.SetData;
begin
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, bIndex);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(word)*Count, @fa[0], StaticVal);
  glBindBuffer(GL_ARRAY_BUFFER, bVertex);
  glBufferData(GL_ARRAY_BUFFER, sizeof(TVector)*vCount, @va[0], StaticVal);
  if _normals then begin
    glBindBuffer(GL_ARRAY_BUFFER, bNormal);
    glBufferData(GL_ARRAY_BUFFER, sizeof(TVector)*vCount, @na[0], StaticVal);
  end;
  if _textures then begin
    glBindBuffer(GL_ARRAY_BUFFER, bTexture);
    glBufferData(GL_ARRAY_BUFFER, sizeof(single)*TextureComponents*vCount, @ta[0], StaticVal);
  end;
  if _colors then begin
    glBindBuffer(GL_ARRAY_BUFFER, bColor);
    glBufferData(GL_ARRAY_BUFFER, ColorComponents*vCount, @ca[0], StaticVal);
  end;
  dataIsSet:=true;
end;

procedure TVBO.SetPointers;
begin
  if (not FStatic) or (not dataIsSet) then SetData;
  if UseShader then SetShaderPointers
  else SetDefaultPointers;
  va_pointers_owner:=self;
end;

procedure TVBO.SetDefaultPointers;
begin
  glBindBuffer(GL_ARRAY_BUFFER, bVertex);
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, 0, nil);

  glBindBuffer(GL_ARRAY_BUFFER, bNormal);
  glEnableClientState(GL_NORMAL_ARRAY);
  if _normals then glNormalPointer(GL_FLOAT, 0, nil);

  glBindBuffer(GL_ARRAY_BUFFER, bColor);
  glEnableClientState(GL_COLOR_ARRAY);
  if _colors then glColorPointer(ColorComponents, GL_UNSIGNED_BYTE, 0, nil);

  glBindBuffer(GL_ARRAY_BUFFER, bTexture);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  if _textures then glTexCoordPointer(TextureComponents, GL_FLOAT, 0, nil);
  glClientActiveTexture(GL_TEXTURE0+tex.TextureUnit);
end;

procedure TVBO.SetShaderPointers;
begin
  glBindBuffer(GL_ARRAY_BUFFER, bVertex);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 3, GL_FLOAT, bytebool(GL_FALSE), 0, nil);

  glBindBuffer(GL_ARRAY_BUFFER, bNormal);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 3, GL_FLOAT, bytebool(GL_FALSE), 0, nil);

  glBindBuffer(GL_ARRAY_BUFFER, bColor);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, ColorComponents, GL_UNSIGNED_BYTE, bytebool(GL_FALSE), 0, nil);

  glBindBuffer(GL_ARRAY_BUFFER, bTexture);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, TextureComponents, GL_FLOAT, bytebool(GL_FALSE), 0, nil);
end;

procedure TVBO.SetStatic(enable: boolean);
begin
  FStatic:=enable;
end;

{ TShaderProgram }

constructor TShaderProgram.Create;
begin
  handle:=glCreateProgram();
  if handle=0 then begin
    nxSetError('Failed to create shader program');
  end;
end;

destructor TShaderProgram.Destroy;
begin
  Disable;
  DetachAllShaders;
  if handle<>0 then glDeleteProgram(handle);
  inherited Destroy;
end;

function TShaderProgram.AddAttrib(aname: string): integer;
var i: GLint;
begin
  i:=GetAttrib(aname);
  if i<0 then result:=-1
  else begin
    result:=FindAttrib(aname);
    if result<0 then begin
      result:=length(attrib);
      setlength(attrib, result+1);
      with attrib[result] do begin
        name:=aname; index:=i;
      end;
    end;
  end;
end;

function TShaderProgram.AddUniform(uname: string): integer;
var i: GLint;
begin
  i:=GetUniform(uname);
  if i<0 then result:=-1
  else begin
    result:=FindUniform(uname);
    if result<0 then begin
      result:=length(uniform);
      setlength(uniform, result+1);
      with uniform[result] do begin
        name:=uname; index:=i;
      end;
    end;
  end;
end;

function TShaderProgram.AttachShader(shader: TShaderSource): boolean;
begin
  if (shader=nil) or (shader.handle=0) then begin
    result:=false; exit;
  end;
  setlength(attached, length(attached)+1);
  attached[high(attached)]:=shader;
  glAttachShader(handle, shader.handle);
  result:=true;
end;

procedure TShaderProgram.DetachAllShaders;
var i: integer;
begin
  for i:=0 to high(attached) do
    glDetachShader(handle, attached[i].handle);
  setlength(attached, 0);
end;

procedure TShaderProgram.DetachShader(index: integer);
var i: integer;
begin
  glDetachShader(handle, attached[index].handle);
  for i:=index to high(attached)-1 do
    attached[i]:=attached[i+1];
  setlength(attached, high(attached));
end;

procedure TShaderProgram.Disable;
begin
  glUseProgram(0);
end;

procedure TShaderProgram.Enable;
begin
  glUseProgram(handle);
end;

function TShaderProgram.GetAttrib(aname: string): GLint;
begin
  result:=glGetAttribLocation(handle, PGLChar(aname));
  if result=-1 then nxSetError('Shader attrib "'+aname+'" not found');
end;

function TShaderProgram.GetUniform(uname: string): GLint;
begin
  result:=glGetUniformLocation(handle, PGLChar(uname));
  if result=-1 then nxSetError('Shader uniform "'+uname+'" not found');
end;

function TShaderProgram.FindAttrib(aname: string): integer;
var i: integer;
begin
  for i:=high(attrib) downto 0 do
    if attrib[i].name=aname then begin
      result:=i; exit;
    end;
  result:=-1;
end;

function TShaderProgram.FindUniform(uname: string): integer;
var i: integer;
begin
  for i:=high(uniform) downto 0 do
    if uniform[i].name=uname then begin
      result:=i; exit;
    end;
  result:=-1;
end;

function TShaderProgram.isCompiled: boolean;
var i: integer;
begin
  result:=length(attached)>0;
  for i:=0 to high(attached) do
    result:=result and attached[i].compiled;
end;

function TShaderProgram.Link: boolean;
var isLinked: GLint;
begin
  glLinkProgram(handle);
  isLinked:=GL_FALSE;
  glGetProgramiv(handle, GL_LINK_STATUS, @isLinked);
  FLinked:=isLinked=GL_TRUE;
  result:=FLinked;
  if not FLinked then nxSetError(format(
    'GLSL linking failed with errorID %d!', [isLinked]));
end;

{ TShaderSource }

constructor TShaderSource.Create(_shaderType: GLenum);
begin
  FShaderType:=_shaderType;
  handle:=glCreateShader(FShaderType);
end;

constructor TShaderSource.CreateFragment;
begin
  Create(GL_FRAGMENT_SHADER);
end;

constructor TShaderSource.CreateVertex;
begin
  Create(GL_VERTEX_SHADER);
end;

destructor TShaderSource.Destroy;
begin

  inherited Destroy;
end;

procedure TShaderSource.GetLastErrorInfo(target: TStrings);
var maxLength: GLint; s: string;
begin
  if (target=nil) or (handle=0) then exit;
  glGetShaderiv(handle, GL_INFO_LOG_LENGTH, @maxLength);
  setlength(s, maxLength);
  glGetShaderInfoLog(handle, maxLength, maxLength, @s[1]);
  s:=trim(s);
  if s<>'' then
    case FShaderType of
      GL_VERTEX_SHADER: s:='[Vertex shader compile error]'+#13+#10+s;
      GL_FRAGMENT_SHADER: s:='[Fragment shader compile error]'+#13+#10+s;
    end;
  if (target.Text<>'') and (s<>'') then
    target.Text:=target.Text+#13+#10+s
  else target.Text:=s;
end;

function TShaderSource.LoadDefaultFShader2D: boolean;
var sl: TStringList;
begin
  sl:=TStringList.Create;
  MakeFShader2D(sl);
  result:=SetSource(sl.Text);
  sl.Free;
end;

function TShaderSource.LoadDefaultVShader2D: boolean;
var sl: TStringList;
begin
  sl:=TStringList.Create;
  MakeVShader2D(sl);
  result:=SetSource(sl.Text);
  sl.Free;
end;

function TShaderSource.LoadDefaultFShader3D(bump: boolean): boolean;
var sl: TStringList;
begin
  sl:=TStringList.Create;
  MakeFShader3D(sl, bump);
  result:=SetSource(sl.Text);
  sl.Free;
end;

function TShaderSource.LoadDefaultVShader3D: boolean;
var sl: TStringList;
begin
  sl:=TStringList.Create;
  MakeVShader3D(sl);
  result:=SetSource(sl.Text);
  sl.Free;
end;

function TShaderSource.LoadSource(filename: string): boolean;
var sl: TStringList;
begin
  sl:=TStringList.Create;
  FixPath(filename);
  sl.LoadFromFile(filename);
  result:=SetSource(sl.Text);
  sl.Free;
end;

function TShaderSource.SetSource(const s: string): boolean;
var shader_source: PAnsiChar; isCompiled: GLint;
begin
  shader_source:=PAnsiChar(s+#0);
  glShaderSource(handle, 1, PPGLChar(@shader_source), nil);
  glCompileShader(handle);
  isCompiled:=GL_FALSE;
  glGetShaderiv(handle, GL_COMPILE_STATUS, @isCompiled);
  FCompiled:=isCompiled<>GL_FALSE;
  if not FCompiled then
    nxSetError('Shader compilation failed');
  result:=FCompiled;
end;

{ TGLRenderer }

function TGLRenderer.AddProgram: integer;
begin
  shader.AddProgram(false);
  result:=high(shader.programs);
end;

function TGLRenderer.AddProgram(vert, frag: TShaderSource): integer;
begin
  result:=AddProgram();
  shader.programs[result].AttachShader(vert);
  shader.programs[result].AttachShader(frag);
  shader.programs[result].Link;
end;

function TGLRenderer.AddProgram(vert_file, frag_file: string): integer;
begin
  result:=AddProgram();
  shader.programs[result].AttachShader(shader.LoadShader(vert_file, true));
  shader.programs[result].AttachShader(shader.LoadShader(frag_file, false));
  shader.programs[result].Link;
end;

constructor TGLRenderer.Create;
begin
  Create(300, 300); // multiple of numbers 3 and 4 for triangles and quads
end;

constructor TGLRenderer.Create(buffer2D, buffer3D: word);
var i: longint; solidWhite: TfRGBA; nUp: TVector;
begin
  cam:=TCamera.Create;
  ambient:=fRGBA(0, 0, 0, 1); diffuse:=fRGBA(1, 1, 1, 1); shininess:=10;
  color:=fRGBA(1, 1, 1, 1);
  shader:=TGLShader.Create;
  if buffer2D>0 then begin
    program2D:=shader.AddDefault2D;
    shader.Link;
    setlength(va2D, buffer2D); Length2D:=buffer2D;
    glGenBuffers(1, @buf2D);
    uniPmv2D:=program2D.GetUniform('pmv');
    uniTex2D:=program2D.GetUniform('texture');
    uniDiffuse2D:=program2D.GetUniform('diffuse');
    att_2Dpos:=program2D.GetAttrib('in_position');
    att_2Dtex:=program2D.GetAttrib('in_texCoord');
    att_2Dcol:=program2D.GetAttrib('in_color');
  end;
  if buffer3D>0 then begin
    program3D:=shader.AddProgram(false);
    //shader.AddDefault3D(false);
    //shader.Link;
    setlength(va3D, buffer3D); Length3D:=buffer3D;
    glGenBuffers(1, @buf3D);
    {att_3Dpos:=program3D.GetAttrib('in_position');
    uniAmbient3D:=program3D.GetUniform('ambient');
    uniShininess:=program3D.GetUniform('shininess');}
    //uniLightcount:=program3D.GetUniform('lightcount');
    //uniLights:=program3D.GetUniform('lights');
    att_3Dpos:=program2D.GetAttrib('in_position');
    att_3Dtex:=program2D.GetAttrib('in_texCoord');
    att_3Dcol:=program2D.GetAttrib('in_color');
  end;
  FpolyMode:=GL_QUADS;
  solidWhite:=fRGBA(1, 1, 1, 1);
  nUp:=vector(0, 1, 0);
  for i:=0 to high(va2D) do va2D[i].color:=solidWhite;
  for i:=0 to high(va3D) do begin
    va3D[i].color:=solidWhite; va3D[i].n:=nUp;
  end;
end;

destructor TGLRenderer.Destroy;
begin
  cam.Free;
  shader.Free;
  glDeleteBuffers(1, @buf2D); glDeleteBuffers(1, @buf3D);
  inherited Destroy;
end;

procedure TGLRenderer.Disable;
begin
  Render;
  FisEnabled:=false; shader.Disable;
end;

function TGLRenderer.Draw(x, y: single; pattern: longint): word;
var tx, ty, tw, th, w, h: single;
begin
  if FisBuffer3D or (not FisEnabled) then Enable2D;
  result:=NewQuad;
  tex.GetPatternCoords(tx, ty, tw, th, pattern);
  tex.GetImageSize(w, h);
  SetVertex(result, x, y, tx, ty);
  SetVertex(result+1, x, y+h, tx, ty+th);
  SetVertex(result+2, x+w, y+h, tx+tw, ty+th);
  SetVertex(result+3, x+w, y, tx+tw, ty);
  with color do begin
    SetColor(result, r, g, b, a);
    SetColor(result+1, r, g, b, a);
    SetColor(result+2, r, g, b, a);
    SetColor(result+3, r, g, b, a);
  end;
end;

function TGLRenderer.DrawRotateS(x, y: single; pattern: longint; radians, cx, cy, sizeX, sizeY: single): word;
var tx, ty, tw, th, w, h: single; dx, dy: TVector2f;
begin
  if FisBuffer3D or (not FisEnabled) then Enable2D;
  result:=NewQuad;
  tex.GetPatternCoords(tx, ty, tw, th, pattern);
  tex.GetImageSize(w, h);
  dx.x:=cos(radians); dx.y:=sin(radians);
  dy.x:=-dx.y*sizeY; dy.y:=dx.x*sizeY;
  dx.x:=dx.x*sizeX; dx.y:=dx.y*sizeX;
  x:=x-cx*dx.x-cy*dy.x;
  y:=y-cx*dx.y-cy*dy.y;
  SetVertex(result, x, y, tx, ty);
  SetVertex(result+1, x+dy.x, y+dy.y, tx, ty+th);
  SetVertex(result+2, x+dx.x+dy.x, y+dx.y+dy.y, tx+tw, ty+th);
  SetVertex(result+3, x+dx.x, y+dx.y, tx+tw, ty);
  with color do begin
    SetColor(result, r, g, b, a);
    SetColor(result+1, r, g, b, a);
    SetColor(result+2, r, g, b, a);
    SetColor(result+3, r, g, b, a);
  end;
end;

procedure TGLRenderer.Enable2D(ForceUpdateUniforms: boolean);
begin
  if FisEnabled then begin
    if shader.current<>program2D then begin
      Render; FisBuffer3D:=false;
      shader.SelectProgram(program2D, true);
      SetUniforms;
    end else if ForceUpdateUniforms then
      SetUniforms;
  end else begin
    FisEnabled:=true; FisBuffer3D:=false;
    shader.SelectProgram(program2D, true);
    SetUniforms; curTexture:=tex.LastTexIndex;
  end;
end;

procedure TGLRenderer.Enable3D(ForceUpdateUniforms: boolean);
begin
  if FisEnabled then begin
    if shader.current<>program3D then begin
      Render; FisBuffer3D:=true;
      shader.SelectProgram(program3D);
      SetUniforms;
    end else if ForceUpdateUniforms then
      SetUniforms;
  end else begin
    FisEnabled:=true; FisBuffer3D:=true;
    shader.SelectProgram(program3D);
    SetUniforms; curTexture:=tex.LastTexIndex;
  end;
end;

procedure TGLRenderer.EnableProgram(index: integer; is3D: boolean;
  ForceUpdateUniforms: boolean);
begin
  EnableProgram(shader.programs[index], is3D, ForceUpdateUniforms);
end;

procedure TGLRenderer.EnableProgram(prog: TShaderProgram; is3D: boolean;
  ForceUpdateUniforms: boolean);
begin
  if FisEnabled then begin
    if shader.current<>prog then begin
      Render; FisBuffer3D:=is3D;
      shader.SelectProgram(prog);
      SetUniforms;
    end else if ForceUpdateUniforms then
      SetUniforms;
  end else begin
    FisEnabled:=true; FisBuffer3D:=is3D;
    shader.SelectProgram(prog);
    SetUniforms; curTexture:=tex.LastTexIndex;
  end;
end;

// Returns index of first vertex in quad
function TGLRenderer.NewQuad: word;
begin
  if polyMode<>GL_QUADS then begin
    Render; FpolyMode:=GL_QUADS;
  end else begin
    if isBuffer3D then begin
      if count>length3D-4 then Render;
    end else begin
      if count>length2D-4 then Render;
    end;
  end;
  result:=count; inc(count, 4);
end;

// Returns index of first vertex in triangle
function TGLRenderer.NewTriangle: word;
begin
  if polyMode<>GL_TRIANGLES then begin
    Render; FpolyMode:=GL_TRIANGLES;
  end else begin
    if isBuffer3D then begin
      if count>length3D-3 then Render;
    end else begin
      if count>length2D-3 then Render;
    end;
  end;
  result:=count; inc(count, 3);
end;

procedure TGLRenderer.Render;
var att_pos, att_tex, att_col: GLint;
begin
  if count=0 then exit;
  if FisEnabled then begin
    if shader.current=program2D then begin
      att_pos:=att_2Dpos;
      att_tex:=att_2Dtex;
      att_col:=att_2Dcol;
    end else if shader.current=program3D then begin
      att_pos:=att_3Dpos;
      att_tex:=att_3Dtex;
      att_col:=att_3Dcol;
    end else with shader.current do begin
      att_pos:=attrib[shader.current.FindAttrib('in_position')].index;
      att_tex:=attrib[shader.current.FindAttrib('in_texCoord')].index;
      att_col:=attrib[shader.current.FindAttrib('in_color')].index;
    end;
    if isBuffer3D then begin
      glBindBuffer(GL_ARRAY_BUFFER, buf3D);
      glBufferData(GL_ARRAY_BUFFER, sizeof(T3DVertex)*length3D, @va3D[0], GL_STREAM_DRAW);
    end else begin
      glBindBuffer(GL_ARRAY_BUFFER, buf2D);
      glBufferData(GL_ARRAY_BUFFER, sizeof(T2DVertex)*length2D, @va2D[0], GL_STREAM_DRAW);
    end;
    if att_pos>-1 then begin
      glVertexAttribPointer(att_pos, 2, GL_FLOAT, false, sizeof(T2DVertex), nil);
      glEnableVertexAttribArray(att_pos);
    end;
    if att_tex>-1 then begin
      glVertexAttribPointer(att_tex, 2, GL_FLOAT, false, sizeof(T2DVertex), pointer(8));
      glEnableVertexAttribArray(att_tex);
    end;
    if att_col>-1 then begin
      glVertexAttribPointer(att_col, 4, GL_FLOAT, false, sizeof(T2DVertex), pointer(16));
      glEnableVertexAttribArray(att_col);
    end;
    glDrawArrays(polyMode, 0, count);
    if att_pos>-1 then glDisableVertexAttribArray(att_pos);
    if att_tex>-1 then glDisableVertexAttribArray(att_tex);
    if att_col>-1 then glDisableVertexAttribArray(att_col);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
  end;
  count:=0;
end;

procedure TGLRenderer.Reset;
begin
  count:=0;
end;

procedure TGLRenderer.SetColor(const r, g, b: single; const a: single);
begin
  color.r:=r; color.g:=g; color.b:=b; color.a:=a;
end;

procedure TGLRenderer.SetColor(const index: word; const r, g, b, a: single);
begin
  if isBuffer3D then begin
    va3D[index].color.r:=r;
    va3D[index].color.g:=g;
    va3D[index].color.b:=b;
    va3D[index].color.a:=a;
  end else begin
    va2D[index].color.r:=r;
    va2D[index].color.g:=g;
    va2D[index].color.b:=b;
    va2D[index].color.a:=a;
  end;
end;

procedure TGLRenderer.SetDiffuse(const r, g, b: single; const a: single);
begin
  diffuse.r:=r; diffuse.g:=g; diffuse.b:=b; diffuse.a:=a;
end;

procedure TGLRenderer.SetNormal(const index: word; const nx, ny, nz: single);
begin
  if isBuffer3D then begin
    va3D[index].n.x:=nx;
    va3D[index].n.y:=ny;
    va3D[index].n.z:=nz;
  end;
end;

procedure TGLRenderer.SetTexture(n: longint);
begin
  if curTexture<>n then begin
    Render; curTexture:=n; tex.SetTex(n);
  end;
end;

procedure TGLRenderer.SetTexture(texname: string);
begin
  SetTexture(tex.IndexOf(texname));
end;

procedure TGLRenderer.SetUniforms(camera: TCamera);
var pmv, modelM, projM: TMatrix; uni: GLint;
  //normal: TMatrix3f;
begin
  glGetFloatv(GL_PROJECTION_MATRIX, @projM);
  if camera=nil then begin
    glGetFloatv(GL_MODELVIEW_MATRIX, @modelM);
  end else begin
    //camera.GetMatrix;
  end;
  pmv:=multiply(modelM, projM);

  if isBuffer3D then begin
    //glUniformMatrix4fv(uniModel, 1, bytebool(GL_FALSE), @modelM);
    //glUniformMatrix3fv(uniNormal, 1, bytebool(GL_FALSE), @normal);
    if shader.current=program3D then begin
      glUniformMatrix4fv(uniPmv3D, 1, bytebool(GL_FALSE), @pmv);
      glUniform1i(uniTex3D, 0);
      //with diffuse do glUniform4f(uniDiffuse3D, r, g, b, a);
    end else with shader.current do begin
      uni:=uniform[FindUniform('pmv')].index;
      if uni>-1 then glUniformMatrix4fv(uni, 1, bytebool(GL_FALSE), @pmv);
      uni:=uniform[shader.current.FindUniform('texture')].index;
      if uni>-1 then glUniform1i(uni, 0);
      uni:=uniform[shader.current.FindUniform('diffuse')].index;
      if uni>-1 then
        with diffuse do glUniform4f(uni, r, g, b, a);
    end;

  end else begin
    if shader.current=program2D then begin
      glUniformMatrix4fv(uniPmv2D, 1, bytebool(GL_FALSE), @pmv);
      glUniform1i(uniTex2D, 0);
      with diffuse do glUniform4f(uniDiffuse2D, r, g, b, a);
    end else with shader.current do begin
      uni:=uniform[FindUniform('pmv')].index;
      if uni>-1 then glUniformMatrix4fv(uni, 1, bytebool(GL_FALSE), @pmv);
      uni:=uniform[shader.current.FindUniform('texture')].index;
      if uni>-1 then glUniform1i(uni, 0);
      uni:=uniform[shader.current.FindUniform('diffuse')].index;
      if uni>-1 then
        with diffuse do glUniform4f(uni, r, g, b, a);
    end;
  end;
end;

procedure TGLRenderer.SetVertex(const index: word; const x, y, u, v: single);
begin
  if not isBuffer3D then begin
    va2D[index].v.x:=x;
    va2D[index].v.y:=y;
    va2D[index].uv.x:=u;
    va2D[index].uv.y:=v;
  end;
end;

procedure TGLRenderer.SetVertex(const index: word; const x, y, z, u, v: single);
begin
  if isBuffer3D then begin
    va3D[index].v.x:=x;
    va3D[index].v.y:=y;
    va3D[index].v.z:=z;
    va3D[index].uv.x:=u;
    va3D[index].uv.y:=v;
  end;
end;

initialization

  nx:=TNXGL.Create; nxGraph.nxEngine:=nx;
  tex:=TGLTextureSet.Create; nxGraph.nxTex:=tex;

finalization

  FreeAndNil(tex); nxGraph.nxTex:=nil;
  FreeAndNil(nx); nxGraph.nxEngine:=nil;

end.
