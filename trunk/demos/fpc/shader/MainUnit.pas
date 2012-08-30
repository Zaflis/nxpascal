unit MainUnit;

{$mode objfpc}{$H+}

{ == Links on GLSL ==
  http://oss.sgi.com/projects/ogl-sample/registry/ARB/fragment_shader.txt
  http://en.wikipedia.org/wiki/OpenGL_Shading_Language#A_sample_trivial_GLSL_fragment_shader
  http://nehe.gamedev.net/article/glsl_an_introduction/25007/
  http://en.wikibooks.org/wiki/GLSL_Programming/Unity/Lighting_of_Bumpy_Surfaces
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, math, dglOpenGL, nxMath3D, nxGL, nxTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    model: TGLModel;
    shader: TGLShader;
    locColorMap, locNormalMap: GLint;
    rotX, rotY: single;
    mx, my: integer;
  public
  end;

var
  Form1: TForm1;

procedure MakeBumpTexture(index: integer; dest: TBitmap);

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var v4: TVector4f; //bmp: TBitmap;
    //errorStr: TStringList;
begin
  clientwidth:=800; clientheight:=600;
  if not nx.CreateGlWindow(self) then begin
    showmessage('Cannot initialize OpenGL'); exit;
  end;

  // Initialize shader
  shader:=TGLShader.Create(true, true);
  if nx.LastError<>'' then begin
    showmessage(nx.LastError); exit;
  end;

  // Load sources
  //errorStr:=TStringList.Create;
  //shader.LoadVertexSource('shader\vertex.txt');
  //shader.LoadFragmentSource('shader\fragment.txt');
  shader.LoadDefaultVShader3D;
  shader.LoadDefaultFShader3D(true);

  //errorStr.Free;

  // Link shader program
  if not shader.Link then begin
    showmessage(nx.LastError); exit;
  end;

  // Read shader variables
  locColorMap:=shader.GetUniform('colorMap');
  if not shader.LastUniformValid then showmessage(nx.LastError);

  locNormalMap:=shader.GetUniform('normalMap');
  if not shader.LastUniformValid then showmessage(nx.LastError);
  nx.ClearError;

  // Use shader program
  shader.Enable;

  // Initialize other OpenGL
  nx.DefaultLights;
  nx.SetSpecular(true, 1, 1, 1, 10);
  nx.rs.DepthTest:=true;

  tex.Options:=tex.Options+[toMipMap];
  tex.AddTexture('texture', 'data\shiphull.png');
  tex.AddTexture('normalmap', 'data\shiphull_n.png');

  model:=TGLModel.CreateCube;

  v4:=vector4f(0.4, 0.6, 0.8);
  glMaterialfv(GL_FRONT, GL_SPECULAR, @v4);

  if nx.LastError<>'' then begin
    showmessage(nx.LastError); exit;
  end;

  tex.SetTextureUnit(0); tex.SetByName('texture');
  tex.SetTextureUnit(1); tex.SetByName('normalmap');

  // Tell shader that texture-unit 0 is texture, and 1 normalmap
  if locColorMap>=0 then glUniform1i(locColorMap, 0);
  if locNormalMap>=0 then glUniform1i(locNormalMap, 1);

  Timer1.Enabled:=true;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  timer1.Enabled:=false;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  model.Free;
  shader.Free;
  nx.KillGLWindow;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if shift <> [] then begin
    rotX:=rotX+(x-mx)*0.5;
    rotY:=rotY+(y-my)*0.5;
  end;
  mx:=x; my:=y;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  nx.Clear(true, true);

  glLoadIdentity;
  glTranslatef(0, 0, -2);
  glRotatef(rotY, 1, 0, 0);
  glRotatef(rotX, 0, 1, 0);

  glColor3f(0.5, 0.5, 0.5);
  model.Render;

  nx.Flip;
end;

// Optional procedure you can use to make
// normalmap texture
procedure MakeBumpTexture(index: integer; dest: TBitmap);
  function GetBr(x, y: integer): single;
  var d: integer;
  begin
    // Luminocity of pixel
    with tex.texture[index] do begin
      d:=values*(sizeX*y+x);
      result:=data[d]*0.2989 + data[d+1]*0.5870 + data[d+2]*0.1140;
    end;
  end;
var i, j: integer; br: array[-1..1,-1..1] of single;
    c: TVector;
begin
  if (index>=tex.count) then exit;
  if tex.texture[index].Data=nil then exit;
  with tex.texture[index] do begin
    dest.PixelFormat:=pf32bit;
    dest.SetSize(width, height);
    for j:=0 to height-1 do
      for i:=0 to width-1 do begin
        br[0,0]:=GetBr(i,j);
        if i>0 then        br[-1,0]:=GetBr(i-1, j)
        else               br[-1,0]:=br[0, 0];
        if j>0 then        br[0,-1]:=GetBr(i, j-1)
        else               br[0,-1]:=br[0, 0];
        if i<width-1 then  br[1,0]:=GetBr(i+1, j)
        else               br[1,0]:=br[0, 0];
        if j<height-1 then br[0,1]:=GetBr(i, j+1)
        else               br[0,1]:=br[0, 0];
        c.x:=((br[0,0]-br[-1,0]) + (br[1,0]-br[0,0]))/2;
        c.y:=-((br[0,0]-br[0,-1]) + (br[0,1]-br[0,0]))/2;
        c.z:=20;
        c:=Norm(c);
        c.x:=0.5+c.x*0.5;
        c.y:=0.5+c.y*0.5;
        c.z:=0.5+c.z*0.5;
        c.x:=max(0, min(255, c.x));
        c.y:=max(0, min(255, c.y));
        c.z:=max(0, min(255, c.z));
        dest.Canvas.Pixels[i,j]:=RGBToColor(
          round(c.x*255), round(c.y*255), round(c.z*255));
      end;
  end;
end;

end.

