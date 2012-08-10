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

interface

uses Classes, dglOpenGL, Controls, Forms, ExtCtrls,
  {$IFDEF fpc}LCLtype, LCLIntf, FileUtil, LazUTF8,
    {$IFnDEF NX_CUSTOM_WINDOW}OpenGLContext,{$ENDIF}
  {$ELSE}Windows,{$ENDIF} nxModel, nxTypes, nxGraph;

type

  { TGLTextureSet }

  TGLTextureSet = class(TTextureSet)
  public
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
    procedure Restore(tex: PTexture); overload;
    procedure Restore3D(tex: PTexture); overload;
    procedure Restore(tex: integer); overload;
    procedure Restore3D(tex: integer); overload;
    procedure SetByName(name: string; force: boolean = false);
    procedure SetClamp(x_repeat, y_repeat: boolean);
    procedure SetTex(n: integer; force: boolean = false);
  end;

  { TDisplayList }

  TDisplayList = class
  private
    list: TGLuint;
    texRef: array of integer;
    texRefCount: word;
  public
    constructor Create(make: boolean = false);
    destructor Destroy; override;
    procedure UpdateList(render: boolean = false);
    procedure EndList;
    procedure Draw;
    procedure DrawAt(x,y,z: single);
    procedure SetTexRef(TexN: integer; isUsed: boolean);
  end;

  TNXGL = class;

  { TRenderSettings }

  TRenderSettings = class
  private
    parent: TNXGL;
    FAddBlend: array[0..7] of boolean;
    FCullBack: array[0..7] of boolean;
    FCullFront: array[0..7] of boolean;
    FDepthTest: array[0..7] of boolean;
    FLighting: array[0..7] of boolean;
    FSubBlend: array[0..7] of boolean;
    FWireFrame: array[0..7] of boolean;
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
  end;

  { TGLModel }

  TGLModel = class(TTriModel)
  public
    destructor Destroy; override;
    procedure EnableStates;
    procedure CopyToFrame(index: integer);
    procedure DisableStates;
    procedure FreeTextures;
    procedure LoadTextures(path: string);
    procedure MakeDisplayList(var list: TDisplayList);
    procedure Render(Initialize: boolean = true);
    procedure SetFrames(count: integer);
    procedure SetFrame(t: single; fStart,fEnd: integer; loop: boolean);
    procedure SetFrameEx(a,b,c,d: PVertexFrame; delta: single);
    procedure SetPointers;
  end;

  { TGLFont }

  TGLFont = class(TNXFont)
  public
    constructor CreateFont(fontName: string; fontSize, _TexSize: integer);
    procedure Draw(x,y: single; s: string; maxW: integer = 0); override;
    procedure DrawC(x,y: single; s: string; maxW: integer = 0); override;
    procedure DrawCScaled(x,y, scaleX,scaleY: single; s: string; maxW: integer = 0); override;
    procedure DrawRotate(x,y, scaleX,scaleY, _angle: single; s: string; maxW: integer = 0); override;
    procedure DrawScaled(x,y, scaleX,scaleY: single; s: string; maxW: integer = 0); override;
    procedure DrawTextArea(r: TBoundsRect; s: TStrings; x_scroll,y_scroll: integer); override;
    procedure DrawWrap(r: TBoundsRect; s: TStrings; y_scroll: integer = 0); override;
    procedure SetColor; overload;
    procedure SetTexture; override;
  end;

  { TNXGL }

  TNXGL = class(TNXCustomEngine)
  private
    {$IFnDEF NX_CUSTOM_WINDOW}procedure doResize(Sender: TObject);{$ENDIF}
    function GetFFPS: integer;
    function NewFontName(base: string): string;
  public
    {$IFDEF fpc}{$IFnDEF NX_CUSTOM_WINDOW}window: TOpenGLControl;{$ENDIF}{$ENDIF}
    rs: TRenderSettings;
    property FPS: integer read GetFFPS;
    function AllOK: boolean;
    function CanRender: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Clear(color,depth: boolean);
    procedure ClearFonts;
    procedure CreateBasicFont;
    function CreateFont(fontName: string; fontSize, TexSize: integer): integer;
    {$IFnDEF NX_CUSTOM_WINDOW}
    function CreateGlWindow(hWindow: TWinControl): boolean;
    {$ENDIF}
    procedure DefaultLights(Enable: boolean = true);
    procedure DeleteFont(index: integer);
    procedure Disable2D;
    procedure Draw(x,y: integer; pattern: word = 0);
    procedure DrawRotate(x,y: single; pattern: word; a,cx,cy: single);
    procedure DrawRotateS(x,y: single; pattern: word; a,cx,cy,
      sizeX,sizeY: single);
    procedure DrawScaled(x,y,sizeX,sizeY: single; pattern: word = 0);
    procedure DrawSections(x,y,sizeX,sizeY,pattern: integer; bdSize: single = 0.25);
    procedure DrawTexRepeat(x,y,sizeX,sizeY: integer);
    procedure Enable2D(flip: boolean = true); overload;
    procedure Enable2D(X, Y, _Width, _Height: integer; flip: boolean = true); overload;
    {$IFnDEF NX_CUSTOM_WINDOW}
    procedure Flip;
    {$ENDIF}
    procedure FontFromImage(TexSize: integer);
    function GetEXT: string;
    procedure GetMouseRay(const mx, my: single; const p, normal: PVector;
      rayMove: single = -100);
    function GLInfo(ext: array of string): boolean; overload;
    function GLInfo(ext: string): boolean; overload;
    function Initialized: boolean;
    {$IFnDEF NX_CUSTOM_WINDOW}
    procedure KillGLWindow;
    {$ENDIF}
    procedure Line(x,y,x2,y2: integer);
    function MouseRayAtPlane(const mx,my: single; const planePos,planeNormal: TVector): TVector;
    function MouseRayAtXZPlane(const mx,my: single): TVector;
    procedure OutLine(x,y,_width,_height: integer);
    procedure Perspective(Stretch: boolean = false);
    function ProgDir: string;
    procedure RectT(x1,y1,x2,y2: single);
    procedure RectZT(x1,z1,x2,z2,y: single);
    procedure SetClearColor(r,g,b: single; a: single=0);
    procedure SetColor(r,g,b: single); overload;
    procedure SetColor(r,g,b,a: single); overload;
    procedure SetColor(const rgb: TRGB); overload;
    procedure SetColor(const rgba: TRGBA); overload;
    procedure SetFont(index: integer);
    procedure SetLight(light, pname: TGLenum; x,y,z: single; w: single=1);
    procedure SetPixel(x,y: integer);
    procedure SetSpecular(Enable: boolean; r,g,b,shininess: single);
    procedure SetView(X, Y, _Width, _Height: integer);
    procedure SetWireframe(enable: boolean = true);
  end;

  TNXGLShaders = class(TNXGL)

  end;

  { TVertexArray }

  TVertexArray = class
    fCount,vCount: integer;
    rendermode: TGLenum;
    fa: array of word; // Faces
    va,na: array of TVector; // Vertices, Normals
    ta,ca: array of single; // Textures, Colors
  private
    _textures,_normals,_colors,_AlphaColors,_3Dtextures: boolean;
  public
    constructor Create(Faces,Vertices: integer; _rendermode: cardinal; textures,normals,colors: boolean);
    destructor Destroy; override;
    procedure DisableStates;
    procedure EnableStates;
    procedure MakeDisplayList(var list: TDisplayList; first,count: integer; Indexed: boolean);
    procedure Render(first,count: integer; Indexed: boolean);
    procedure Set3DTextures;
    procedure SetAlphaColors;
    procedure SetPointers;
  end;

  { TFrameBuffer }

  TFrameBuffer = class
  private
    fb, rb: cardinal;
    depth: boolean;
    function GetWidth: integer;
    function GetHeight: integer;
  public
    texture: integer;
    constructor Create(const _texture: integer; _depth: boolean = false); overload;
    constructor Create(const width, height: integer; transparency: boolean;
      _depth: boolean = false); overload;
    destructor Destroy; override;
    procedure Bind;
    procedure SetTexture(const _texture: integer; _unbind: boolean = true);
    procedure UnBind;
    property Height: integer read GetHeight;
    property Width: integer read GetWidth;
  end;

var
  nxFARZ: single = 1000;
  nxNEARZ: single = 0.1;
  nxFov: single = 45;
  nxAspectRatio: single = 1.33;
  nx: TNXGLShaders;
  tex: TGLTextureSet;
  va_states_set, va_pointers_set: boolean;

  procedure nxInitGL;
  procedure nxFreeModelTextures(model: T3DModel);
  procedure nxLoadModelTextures(model: T3DModel; path: string);

implementation

uses SysUtils, nxMath3D, nxStrings;

procedure nxInitGL;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(nxFov, nxAspectRatio, nxNEARZ, nxFARZ);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glClearColor(0.0, 0.0, 0.0, 0.0); //glClearDepth(1.0);
  glColor3f(1,1,1);
  glEnable(GL_TEXTURE_2D);
  nx.rs.CullBack:=true;
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GREATER,0.05);
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
var i, n: integer;
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

constructor TFrameBuffer.Create(const _texture: integer; _depth: boolean);
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
var n: integer;
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

procedure TFrameBuffer.SetTexture(const _texture: integer; _unbind: boolean);
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
  if StackLevel<7 then begin
    inc(StackLevel);
    FAddBlend[StackLevel]:=FAddBlend[StackLevel-1];
    FCullBack[StackLevel]:=FCullBack[StackLevel-1];
    FCullFront[StackLevel]:=FCullFront[StackLevel-1];
    FDepthTest[StackLevel]:=FDepthTest[StackLevel-1];
    FLighting[StackLevel]:=FLighting[StackLevel-1];
    FSubBlend[StackLevel]:=FSubBlend[StackLevel-1];
    result:=true;
  end else result:=false;
end;

function TRenderSettings.Pop: boolean;
begin
  if StackLevel>0 then begin
    AddBlend:=FAddBlend[StackLevel-1];
    CullBack:=FCullBack[StackLevel-1];
    CullFront:=FCullFront[StackLevel-1];
    DepthTest:=FDepthTest[StackLevel-1];
    Lighting:=FLighting[StackLevel-1];
    SubBlend:=FSubBlend[StackLevel-1];
    dec(StackLevel);
    result:=true;
  end else result:=false;
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
    if FAddBlend[stackLevel] and FSubBlend[stackLevel] then
      FSubBlend[stackLevel]:=false;
    SetBlend;
  end;
end;

procedure TRenderSettings.SetBlend;
begin
  if SubBlend then begin
    glBlendEquationEXT(GL_FUNC_REVERSE_SUBTRACT_EXT);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  end else begin
    glBlendEquationEXT(GL_FUNC_ADD_EXT);
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

procedure TNXGL.Clear(color,depth: boolean);
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
  for i:=0 to FontCount-1 do font[i].Free;
  FontCount:=0; setlength(font,0);
end;

procedure TNXGL.CreateBasicFont;
begin
  if FontCount=0 then CreateFont('Arial',9,256);
end;

function TNXGL.CreateFont(fontName: string; fontSize, TexSize: integer): integer;
begin
  result:=fontCount;
  inc(FontCount); setlength(font,fontcount);
  font[result]:=TGLFont.CreateFont(fontName,fontSize,TexSize);
  font[result].name:=NewFontName(fontName);
end;

procedure TNXGL.FontFromImage(TexSize: integer);
begin
  inc(FontCount); setlength(font,fontcount);
  font[fontcount-1]:=TGLFont.Create(TexSize);
end;

{$IFnDEF NX_CUSTOM_WINDOW}
function TNXGL.CreateGlWindow(hWindow: TWinControl): boolean;
var
{$IFnDEF fpc}pfd: TPIXELFORMATDESCRIPTOR; pf: integer;{$ENDIF}
  err: cardinal;
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
    //window.SetBounds(0,0,hWindow.Width,hWindow.Height);
    window.OnResize:=@doResize;
    window.Align:=alClient;
    window.MakeCurrent(false);
    width:=window.Width; height:=window.Height;
    ReadExtensions; ReadImplementationProperties;
    if hWindow is TForm then with TForm(hWindow) do begin
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

  if nxHWND is TForm then
    if not assigned(TForm(nxHWND).OnResize) then
      TForm(nxHWND).OnResize:=doResize;
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
    glEnable(GL_LIGHT0); // Point
    glEnable(GL_LIGHT1); // Ambient
    nx.SetLight(1, GL_AMBIENT, 0.6, 0.6, 0.6);
    glEnable(GL_COLOR_MATERIAL);
    c.r:=0; c.g:=0; c.b:=0; c.a:=1;
    glMaterialfv(GL_FRONT, GL_SPECULAR, PGLFloat(@c));
    glColor3f(0.7,0.7,0.7);
  end else
    rs.Lighting:=false;
end;

procedure TNXGL.DeleteFont(index: integer);
begin
  font[index].Free;
  dec(FontCount); setlength(font,fontcount);
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

procedure TNXGL.Draw(x, y: integer; pattern: word);
var w,h,cols,rows,mult: integer; tx,ty,tw,th,sx,sy: single;
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

procedure TNXGL.DrawRotate(x, y: single; pattern: word; a, cx, cy: single);
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
  glRotatef(a,0,0,1);
  glTranslatef(-cx*w,-cy*h,0);
  glBegin(GL_QUADS);
    glTexCoord2f(tx,ty); glVertex2f(0,0);
    glTexCoord2f(tx,ty+th); glVertex2f(0,h);
    glTexCoord2f(tx+tw,ty+th); glVertex2f(w,h);
    glTexCoord2f(tx+tw,ty); glVertex2f(w,0);
  glEnd;
  glPopMatrix;
end;

procedure TNXGL.DrawRotateS(x, y: single; pattern: word; a, cx, cy, sizeX,
  sizeY: single);
var tx,ty,tw,th: single;
begin
  {$HINTS OFF}tex.GetPatternCoords(tx,ty,tw,th,pattern);{$HINTS ON}
  glPushMatrix;
  glTranslatef(x,y,0);
  glRotatef(a,0,0,1);
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
  {$HINTS OFF}tex.GetPatternCoords(tx,ty,tw,th,pattern);{$HINTS ON}
  glPushMatrix;
  glTranslatef(x,y,0);
  glBegin(GL_QUADS);
    glTexCoord2f(tx,   ty);    glVertex2f(0,    0);
    glTexCoord2f(tx,   ty+th); glVertex2f(0,    sizeY);
    glTexCoord2f(tx+tw,ty+th); glVertex2f(sizeX,sizeY);
    glTexCoord2f(tx+tw,ty);    glVertex2f(sizeX,0);
  glEnd;
  glPopMatrix;
end;

procedure TNXGL.DrawTexRepeat(x, y, sizeX,sizeY: integer);
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

procedure TNXGL.DrawSections(x, y, sizeX, sizeY, pattern: integer; bdSize: single);
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

procedure TNXGL.GetMouseRay(const mx, my: single; const p, normal: PVector;
  rayMove: single);
var viewport: TGLVectori4;
    modelM,projM: TGLMatrixd4;
    x1,y1,z1,x2,y2,z2: double;
    n: TVector;
begin
  glGetIntegerv(GL_VIEWPORT,@viewPort);
  glGetDoublev(GL_PROJECTION_MATRIX,@projM);
  glGetDoublev(GL_MODELVIEW_MATRIX,@modelM);
  {$HINTS OFF} // hints about x1..z2 not initialized
  gluUnProject(mx,my,modelM[2,3],modelM,projM,viewport,@x1,@y1,@z1);
  gluUnProject(mx,my,modelM[2,3]-1,modelM,projM,viewport,@x2,@y2,@z2);
  {$HINTS ON}
  n.x:=x1-x2; n.y:=y1-y2; n.z:=z1-z2; Norm(n);
  if p<>nil then begin
    p^.x:=x1+n.x*rayMove;
    p^.y:=y1+n.y*rayMove;
    p^.z:=z1+n.z*rayMove;
  end;
  if normal<>nil then normal^:=n;
end;

function TNXGL.GLInfo(ext: array of string): boolean;
var s: string; i: integer;
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

{$IFnDEF NX_CUSTOM_WINDOW}
procedure TNXGL.KillGLWindow;
begin
  {$IFnDEF fpc}
  if Initialized then DeactivateRenderingContext;
  if nxRC>0 then wglDeleteContext(nxRC);
  if (nxHWND.Handle>0) and (nxDC>0) then
    ReleaseDC(nxHWND.Handle,nxDC);
  nxDC:=0; nxRC:=0;
  {$ENDIF}
  nxHWND:=nil;
end;
{$ENDIF}

procedure TNXGL.Line(x, y, x2, y2: integer);
const d = 0.5;
begin
  glBegin(GL_LINES);
    glVertex2f(x+d,y+d); glVertex2f(x2+d,y2+d);
  glEnd;
end;

function TNXGL.MouseRayAtPlane(const mx,my: single; const planePos,planeNormal: TVector): TVector;
var rayOrigin, rayDirection: TVector;
begin
  GetMouseRay(mx,my,@rayOrigin,@rayDirection);
  rayPlaneIntersect(rayOrigin,rayDirection,planePos,planeNormal,@result);
end;

function TNXGL.MouseRayAtXZPlane(const mx, my: single): TVector;
begin
  result:=MouseRayAtPlane(mx,my,Vector(0,0,0),Vector(0,1,0));
end;

function TNXGL.NewFontName(base: string): string;
var i,n: integer; h: boolean;
begin
  result:=base+'1'; n:=1;
  repeat
    h:=true;
    for i:=0 to FontCount-1 do
      if result=font[i].name then begin
        h:=false; inc(n); result:=base+inttostr(n);
      end;
  until h;
end;

function TNXGL.AllOK: boolean;
begin
  result:=Initialized and (nxError='');
end;

function TNXGL.CanRender: boolean;
begin
  result:=glCheckFramebufferStatus(GL_FRAMEBUFFER)=GL_FRAMEBUFFER_COMPLETE;
end;

constructor TNXGL.Create;
begin
  rs:=TRenderSettings.Create(self);
  FrameTick:=GetTick; secTick:=FrameTick+1000;
end;

procedure TNXGL.OutLine(x, y, _width, _height: integer);
const d = 0.5;
begin
  glBegin(GL_LINE_LOOP);
    glVertex2f(x+d,y+d); glVertex2f(x+d,y+d+_height-1);
    glVertex2f(x+d+_width-1,y+d+_height-1); glVertex2f(x+d+_width-1,y+d);
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
    c.r:=r; c.g:=g; c.b:=b; c.a:=0;
    glMaterialfv(GL_FRONT, GL_SPECULAR, @c);
    glMaterialf(GL_FRONT, GL_SHININESS, shininess);
  end else glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, GL_SINGLE_COLOR);
end;

procedure TNXGL.SetView(X, Y, _Width, _Height: integer);
begin
  Left:=X; Top:=Y; Width:=_Width; Height:=_Height;
  nxAspectRatio:=_Width/_Height;
  if Initialized then begin
    glViewport(X,Y,_Width,_Height);
    {$IFDEF fpc}window.SetBounds(X,Y,_Width,_Height);{$ENDIF}
  end;
end;

procedure TNXGL.SetColor(r, g, b, a: single);
begin
  glColor4f(r,g,b,a);
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
  TGLFont(Font[index]).SetTexture;
end;

procedure TNXGL.SetColor(r, g, b: single);
begin
  glColor3f(r,g,b);
end;

procedure TNXGL.SetWireframe(enable: boolean = true);
begin
  if enable then glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
  else glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
end;

// Use light index starting from 0
procedure TNXGL.SetLight(light, pname: TGLenum; x, y, z: single; w: single);
var v: TVector4f;
begin
  v.x:=x; v.y:=y; v.z:=z; v.w:=w;
  glLightfv(GL_LIGHT0+light, pname, @v);
end;

procedure TNXGL.SetPixel(x, y: integer);
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
var i: integer;
begin
  if Initialize then begin
    SetPointers; EnableStates;
  end;
  nx.rs.Push;
  for i:=0 to groups-1 do
    with grp[i] do begin
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

procedure TGLModel.SetFrame(t: single; fStart, fEnd: integer; loop: boolean);
var delta: single; a,b,c,d,count: integer;
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
var i: integer;
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
  transparency: boolean): integer;
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
      if Data<>nil then FreeMem(Data);
      size:=sizeX*sizeY*values;
      Data:=AllocMem(size);
      fillchar(data[0],size,0);
      if values=4 then begin
        for i:=0 to (size-1) div 4 do
          Data[i*4+3]:=255; // Full alpha
      end;
    end;
    Restore(result);
  end;
end;

function TGLTextureSet.AddTexture(name,filename: string; transparency: boolean): integer;
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

function TGLTextureSet.Add3DTexture(name, filename: string; cols, rows: integer; transparency: boolean): integer;
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
var i: integer;
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

procedure TGLTextureSet.ReloadTexture(n: integer; _filename: string;
  transparency: boolean);
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
      FreeMem(texture[n].Data); texture[n].Data:=nil;
    end;
    if not LoadTextureData(@texture[n],_filename) then begin
      if texture[n].Data<>nil then begin
        FreeMem(texture[n].Data); texture[n].Data:=nil;
      end;
      exit;
    end;
  end;
  if not (toNoLoad in Options) then
    if not texture[n].tex3D then Restore(@texture[n])
    else Restore3D(@texture[n]);
end;

procedure TGLTextureSet.RemoveTexture(n: integer; force: boolean);
begin
  dec(texture[n].RefCount);
  if force or (texture[n].RefCount=0) then begin
    if texture[n].index>0 then
      glDeleteTextures(1,@texture[n].index);
    inherited RemoveTexture(n);
  end;
end;

procedure TGLTextureSet.Restore(tex: PTexture);
var i: integer;
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
          tex^.Format, GL_UNSIGNED_BYTE, tex^.Data);
      end else begin
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
        gluBuild2DMipmaps(GL_TEXTURE_2D, tex^.Values, tex^.sizeX, tex^.sizeY,
          tex^.Format, GL_UNSIGNED_BYTE, tex^.Data);
      end;
    end;
    if not (toKeepData in Options) then begin
      freemem(tex^.data); tex^.data:=nil;
    end;
    nx.RenderThreadReserved:=false;
  end;
end;

procedure TGLTextureSet.Restore(tex: integer);
begin
  if tex>=0 then Restore(@texture[tex]);
end;

procedure TGLTextureSet.Restore3D(tex: PTexture);
var temp: TTexture; i,j,k,rs,dest,src: integer;
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
        {$IFDEF fpc}tex^.Data:={$ENDIF}reallocmem(tex^.Data,
          tex^.sizeX*tex^.sizeY*tex^.sizeZ*tex^.values);
        dest:=0;
        for k:=0 to tex^.sizeZ-1 do
          for j:=0 to tex^.sizeY-1 do begin
            src:=(k mod cols3D+j*cols3D+(k div cols3D)*tex^.sizeY*cols3D)*rs;
            move(temp.Data[src], tex^.Data[dest], rs);
            dest:=dest+rs;
          end;
      finally
        freemem(temp.data);
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
          0, tex^.Format, GL_UNSIGNED_BYTE, tex^.Data);
      //end else begin

        // insert 3D MipMapping here

        //glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
        //gluBuild3DMipmaps(GL_TEXTURE_3D, tex^.values, tex^.sizeX, tex^.sizeY, tex^.sizeZ,
        //  tex^.Format, GL_UNSIGNED_BYTE, tex^.Data);
      //end;
      if not (toKeepData in Options) then begin
        freemem(tex^.data); tex^.data:=nil;
      end;
    end;
    nx.RenderThreadReserved:=false;
  end;
end;

procedure TGLTextureSet.SetByName(name: string; force: boolean);
var i: integer;
begin
  i:=indexof(name);
  if i>=0 then settex(i,force);
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

procedure TGLTextureSet.Restore3D(tex: integer);
begin
  if tex>=0 then Restore3D(@texture[tex]);
end;

procedure TGLTextureSet.SetTex(n: integer; force: boolean);
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

{ TVertexArray }

constructor TVertexArray.Create(Faces,Vertices: integer; _rendermode: cardinal;
  textures,normals,colors: boolean);
var n: integer;
begin
  fCount:=Faces; vCount:=Vertices;
  self.rendermode:=_rendermode;
  _textures:=textures; _normals:=normals; _colors:=colors;
  if faces>0 then begin
    if rendermode=GL_QUADS then n:=fCount*4
    else if rendermode=GL_TRIANGLE_STRIP then n:=fCount+2
    else if rendermode=GL_TRIANGLE_FAN then n:=fCount+1
    else n:=fCount*3; // GL_TRIANGLES
    setlength(fa,n);
  end;
  setlength(va,vCount);
  if textures then setlength(ta,vCount*2);
  if normals then setlength(na,vCount);
  if colors then setlength(ca,vCount*3);
end;

destructor TVertexArray.Destroy;
begin
  setlength(va,0);
  if fCount>0 then setlength(fa,0);
  if _textures or _3Dtextures then setlength(ta,0);
  if _normals then setlength(na,0);
  if _colors or _AlphaColors then setlength(ca,0);
  inherited;
end;

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

procedure TVertexArray.MakeDisplayList(var list: TDisplayList; first,count: integer; Indexed: boolean);
begin
  SetPointers;
  if (list=nil) or (not assigned(list)) then
    list:=TDisplayList.Create(true)
  else list.UpdateList;
  EnableStates; Render(first,count,indexed);
  list.EndList;
end;

procedure TVertexArray.Render(first,count: integer; Indexed: boolean);
begin
  if not va_states_set then EnableStates;
  if not va_pointers_set then SetPointers;
  if Indexed then glDrawElements(rendermode,count,GL_UNSIGNED_SHORT,@(fa[first]))
  else glDrawArrays(rendermode,first,count);
  DisableStates;
end;

procedure TVertexArray.Set3DTextures;
begin
  _3Dtextures:=true; setlength(ta,vCount*3);
end;

procedure TVertexArray.SetAlphaColors;
begin
  _AlphaColors:=true; setlength(ca,vCount*4);
end;

procedure TVertexArray.SetPointers;
begin                           
  glVertexPointer(3,GL_FLOAT,0,@(va[0]));
  if _normals then glNormalPointer(GL_FLOAT,0,@(na[0]));
  if _3Dtextures then glTexCoordPointer(3,GL_FLOAT,0,@(ta[0]))
  else if _textures then glTexCoordPointer(2,GL_FLOAT,0,@(ta[0]));
  if _AlphaColors then glColorPointer(4,GL_FLOAT,0,@(ca[0]))
  else if _colors then glColorPointer(3,GL_FLOAT,0,@(ca[0]));
  va_pointers_set:=true;
end;

{ TGLFont }

constructor TGLFont.CreateFont(fontName: string; fontSize, _TexSize: integer);
var temp: TTextureLoadOptions; tempC: TRGB;
begin
  temp:=tex.Options; tempC:=tex.TransparentColor;
  CreateBMP(fontName,fontSize,_TexSize);
  tex.texture[textureI].Format:=GL_RGBA;
  tex.texture[textureI].intFormat:=GL_RGBA;
  glGenTextures(1,@tex.texture[textureI].index);
  tex.ChangeTexMode3D(false);
  tex.SetTex(textureI,true);
  tex.Restore(@tex.texture[textureI]);
  tex.Options:=temp;
  tex.TransparentColor:=tempC;
  SetColor(1,1,1,1);
end;

procedure TGLFont.Draw(x, y: single; s: string; maxW: integer);
var i,cw,n,x1,y1,wLimit: integer; d1,tx,ty: single;
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

procedure TGLFont.DrawC(x, y: single; s: string; maxW: integer);
var w: single;
begin
  w:=TextW(s)/2;
  if (maxW>0) and (w>maxW/2) then w:=maxW/2;
  Draw(x-w, y-CenterH, s, maxW);
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
  DrawC(0, 0, s, maxW);
  glPopMatrix;
end;

procedure TGLFont.DrawRotate(x, y, scaleX, scaleY, _angle: single; s: string; maxW: integer);
begin
  glPushMatrix;
  glTranslatef(x, y, 0);
  glRotatef(_angle, 0,0,1);
  glScalef(scaleX, scaleY, 1);
  DrawC(0, 0, s, maxW);
  glPopMatrix;
end;

procedure TGLFont.DrawTextArea(r: TBoundsRect; s: TStrings; x_scroll, y_scroll: integer);
var i,l,cw,n,x1,y1: integer; d1,tx,ty: single; s2: string;
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

procedure TGLFont.DrawWrap(r: TBoundsRect; s: TStrings; y_scroll: integer);
var i,l,cw,n,x1,y1: integer; d1,tx,ty: single; s2: string;
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
var i: integer;
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

procedure TDisplayList.SetTexRef(TexN: integer; isUsed: boolean);
var i: integer;
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
var i: integer;
begin
  tex.LastTexIndex:=-6; // Force first texture to show
  if render then glNewList(list, GL_COMPILE_AND_EXECUTE)
  else glNewList(list, GL_COMPILE);
  for i:=0 to TexRefCount-1 do
    dec(tex.texture[texRef[i]].RefCount);
  texRefCount:=0;
end;

initialization

  nx:=TNXGLShaders.Create; nxGraph.nxEngine:=nx;
  tex:=TGLTextureSet.Create; nxGraph.nxTex:=tex;

finalization

  FreeAndNil(tex); nxGraph.nxTex:=nil;
  FreeAndNil(nx); nxGraph.nxEngine:=nil;

end.
