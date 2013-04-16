unit Unit1;

interface

uses
  Windows, Classes, Forms, SysUtils, Controls, ExtCtrls, Dialogs,
  nxGL, AppEvnts, math, dglOpenGL, nxGLExtra, nxTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fb, fb2: TFrameBuffer;
    model: TGLModel;
  public
  end;

var
  Form1: TForm1; 

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var err: GLenum;
begin
  nx.CreateGlWindow(self);

  nx.rs.DepthTest:=true; // Test depth-property in framebuffer
  //nx.rs.CullBack:=true; // Use culling when not use depth

  nx.DefaultLights;
  nx.SetSpecular(true, 0.5,0.5,0.5, 10);

  fb:=TFrameBuffer.Create(512, 512, true, true);
  fb2:=TFrameBuffer.Create(512, 512, true, true);

  model:=TGLModel.CreateCube;

  err:=glGetError();
  if err>0 then ShowMessage(format('glGetError, code: %d',[err]));

  if nx.LastError<>'' then ShowMessage(nx.LastError)
  else timer1.Enabled:=true;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  timer1.Enabled:=false;
  model.Free;
  nx.KillGLWindow;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Draw cube to framebuffer, using framebuffer2 as texture
  nx.SetView(0, 0, 512, 512);
  nx.Perspective(false);
  fb.Bind;
  glClearColor(0.2,0.4,1,1);
  nx.Clear(true,true);
  glLoadIdentity;
  glTranslatef(0,0,-2.5);
  glRotatef(nx.GetTick(360,0.02),0.2,1,0.4);
  tex.SetTex(fb2.texture);
  nx.SetColor(0.3,0.3,0.6);
  model.Render;
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
  nx.SetView(0, 0, ClientWidth, ClientHeight);
  nx.Perspective(false);
  glLoadIdentity;
  glClearColor(0,0,0,0.1);
  nx.Clear(true,true);
  glTranslatef(0,0,-2.5);
  nx.SetColor(0.6,0.6,0.6);
  glRotatef(nx.GetTick(360,0.01),0.2,1,0.4);
  tex.SetTex(fb2.texture);
  model.Render;

  nx.Flip;
end;

end.
