unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Forms, Controls, SysUtils, ExtCtrls, Dialogs,
  dglOpenGL, nxGL, nxTypes, nxMath3D, nxGLExtra;

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FormMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    ship, arrow: TGLModel;
    mx, my, mb: integer;
    pt: TParticleEngine;
  public
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var c: TfRGBA; err: GLenum;
begin
  ClientWidth:=800; ClientHeight:=600;
  nx.CreateGlWindow(self);
  nx.DefaultLights;
  c.r:=0; c.g:=c.r; c.b:=c.r; c.a:=1;
  glLightfv(GL_LIGHT1, GL_AMBIENT, @c);
  nx.rs.DepthTest:=true;

  pt:=TParticleEngine.Create;

  // Load particle texture and set it to new particle type
  pt.AddPType(tex.AddTexture('glow','textures\glow.png'), 0.5, -0.01, 0.0001);

  // Load arrow model
  arrow:=TGLModel.Create;
  arrow.LoadFromW3D('objects\arrow.w3d');
  arrow.Scale(0.5,0.5,0.5); // Make arrow smaller
  arrow.LoadTextures('textures');

  // Load ship model
  ship:=TGLModel.Create;
  ship.LoadFromFile('objects\ship.w3d');
  ship.LoadTextures('textures');
  ship.UseColors:=false; // Can ignore color materials from file (optional)

  err:=glGetError();
  if err>0 then ShowMessage(format('glGetError, code: %d',[err]));

  if nx.LastError<>'' then showmessage(nx.LastError)
  else timer1.Enabled:=true;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var cam,arrM: TMatrix; mp,dir,intersect,normal: TVector;
    n: integer; camA,d: single; launcher: pParticleLauncher;
begin
  if not nx.AllOK then exit;
  nx.Clear(true, true);

  // Rotate camera around Y-axis
  camA:=nx.GetTick(360, 0.02)*toRad;
  cam:=LookAt(vector(2*cos(camA),-3,2*sin(camA)), vector(0,0,0), vector(0,1,0));

  // Add blue trail
  d:=(180-nx.GetTick(360,0.043))*toRad;
  if pt.tCount>0 then
    pt.SetTrailPos(@pt.trail[0], 1.5*cos(d), 1.0*sin(d), 0.3*sin(camA))
  else
    pt.SetTrailColor(pt.AddTrail(tex.IndexOf('glow'), 1000, 0.1,
      1.5*cos(d), 1*sin(d), 0.3*sin(camA)), 0.1,0.5,1);

  // Set camera
  glLoadIdentity;
  glMultMatrixf(@cam);

  // Rotate ship around Z-axis
  glRotatef(nx.GetTick(360, 0.0131), 0, 0, 1);
  glColor3f(0.4, 0.4, 1.0); // Use blue color
  // Render ship
  ship.Render;

  nx.GetMouseRay(mx, my, @mp, @dir);

  // Find mouse-ray - model intersection point
  n:=ship.rayIntersect(mp, dir, true, @intersect, @normal);
  if n>=0 then begin
    // Create rotation+position matrix for arrow
    arrM:=MatrixOnPlane(intersect, normal, nx.GetTick(360, 0.01));

    // Render arrow
    glPushMatrix;
    glMultMatrixf(@arrM);
    arrow.Render;
    glPopMatrix;

    // Create particle when mouse is held down
    if mb>0 then begin

      // * This code would manually add a particle *
      //pt.AddParticle(0, 1,0.5+random*0.5,0.3, 0.07, intersect.x, intersect.y, intersect.z)
      //  ^.velocity:=vector(normal.x*0.01, normal.y*0.01, normal.z*0.01);

      launcher:=pt.AddLauncher(0, 1, 0.02,0.07, 0.005,0.006, 25, normal, 500);
      pt.SetLauncherColor(launcher, 1,1, 0,1, 0,0.3);
      pt.SetLauncherPos(launcher, intersect);
    end;
  end;

  // Move and remove old particles
  pt.Move3D;

  // Draw particles
  nx.rs.Push;
  nx.rs.AddBlend:=true;
  pt.Draw3D;
  nx.rs.Pop;

  nx.Flip;

  // Show FPS
  caption:=format('Model demo - FPS: %d, Particles: %d',[nx.FPS, pt.pCount]);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  timer1.Enabled:=false;
  pt.Free; ship.Free; arrow.Free;
  nx.KillGLWindow;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mb:=1;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  mx:=X; my:=Y;
  if ssLeft in Shift then mb:=1;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mb:=0;
end;

end.

