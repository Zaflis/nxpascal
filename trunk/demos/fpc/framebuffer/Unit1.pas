unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, nxGL, dglOpenGL, nxTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fb,fb2: TFrameBuffer;
    cube: TDisplayList;
  public
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var model: TGLModel; err: GLenum;
begin
  nx.CreateGlWindow(self);
  nx.rs.DepthTest:=true; nx.rs.CullBack:=false;
  nx.DefaultLights;
  nx.SetSpecular(true, 0.5,0.5,0.5, 10);

  fb:=TFrameBuffer.Create(512, 512, false, true);
  fb2:=TFrameBuffer.Create(512, 512, false, false);
  model:=TGLModel.Create;
  model.LoadFromW3D('cube.w3d');
  model.UseMaterials:=false;
  model.MakeDisplayList(cube);
  model.Free;

  err:=glGetError();
  if err>0 then ShowMessage(format('glGetError, code: %d',[err]));
  // Seen code 1286, which by some sources isn't actual error

  if nx.LastError<>'' then ShowMessage(nx.LastError)
  else timer1.Enabled:=true;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  timer1.Enabled:=false;
  nx.KillGLWindow;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Draw cube to framebuffer, using framebuffer2 as texture
  fb.Bind;
  glClearColor(0.2,0.4,1,1);
  nx.Clear(true,true);
  glLoadIdentity;
  glTranslatef(0,0,-2.5);
  glRotatef(nx.GetTick2(360,0.02),0.2,1,0.4);
  tex.SetTex(fb2.texture);
  nx.SetColor(0.3,0.3,0.6);
  cube.Draw;
  fb.UnBind;

  // Draw framebuffer on framebuffer2
  fb2.Bind;
  nx.Enable2D;
  nx.SetColor(1,1,1);
  tex.SetTex(fb.texture);
  nx.Draw(0,0,0);
  nx.Disable2D;
  fb2.UnBind;

  // Draw framebuffer2 on screen
  glLoadIdentity;
  glClearColor(0,0,0,0.1);
  nx.Clear(true,true);
  glTranslatef(0,0,-2.5);
  nx.SetColor(0.6,0.6,0.6);
  glRotatef(nx.GetTick2(360,0.01),0.2,1,0.4);
  tex.SetTex(fb2.texture);
  cube.Draw;

  nx.Flip;
end;

end.

