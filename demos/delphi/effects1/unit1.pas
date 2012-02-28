unit unit1;

interface

uses
  Windows, Classes, Forms, SysUtils, Controls, ExtCtrls, Dialogs,
  AppEvnts, math, dglOpenGL, nxGL, nxTypes, nxGLExtra, nxBass;

const GS = 50; // Grid size (GS x GS)

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    ApplicationEvents1: TApplicationEvents;
    MusicTimer: TTimer;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure MusicTimerTimer(Sender: TObject);
  private
    mx, my: integer;
    DoWireFrame: boolean;
    lag, LastTick: cardinal;
    qt: TQuadTexture;
    pt: TParticleEngine;
    follower: array[0..2] of TParticle;
    fb: TFrameBuffer;
    grid: TVertexArray;
    bassSound: TBassEngine;
  public
    procedure GameLoop;
  end; 

var Form1: TForm1;

implementation

uses nxMath3D, nxMath;

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var i,j: integer; launcher: pParticleLauncher;
begin
  clientwidth:=800; clientheight:=600;
  if not nx.CreateGlWindow(self) then begin
    Showmessage(nx.LastError); halt;
  end;
  Randomize;

  fb:=TFrameBuffer.Create(clientWidth, clientHeight, false);

  // Initialize grid
  grid:=TVertexArray.Create(GS*GS*4, (GS+1)*(GS+1), GL_QUADS, true, false, false);
  for j:=0 to GS do
    for i:=0 to GS do begin
      // Texture coordinates
      grid.ta[2*(i+j*(GS+1))]:=i/GS;
      grid.ta[2*(i+j*(GS+1))+1]:=j/GS;

      // Vertex Z coordinate
      grid.va[i+j*(GS+1)].z:=0;
    end;
  // Face vertex indexes
  for j:=0 to GS-1 do
    for i:=0 to GS-1 do
      if i mod 2=0 then begin
        grid.fa[4*(i+j*GS)]:=i+j*(GS+1);
        grid.fa[4*(i+j*GS)+1]:=i+(j+1)*(GS+1);
        grid.fa[4*(i+j*GS)+2]:=i+1+(j+1)*(GS+1);
        grid.fa[4*(i+j*GS)+3]:=i+1+j*(GS+1);
      end else begin
        // Improve grid drawing by flipping every second quad
        grid.fa[4*(i+j*GS)]:=i+(j+1)*(GS+1);
        grid.fa[4*(i+j*GS)+1]:=i+1+(j+1)*(GS+1);
        grid.fa[4*(i+j*GS)+2]:=i+1+j*(GS+1);
        grid.fa[4*(i+j*GS)+3]:=i+j*(GS+1);
      end;

  qt:=TQuadTexture.Create('textures'+PathChar+'test.jpg');
  tex.AddTexture('glow', 'textures'+PathChar+'glow.png');
  tex.AddTexture('snow', 'textures'+PathChar+'snow.png', true);

  pt:=TParticleEngine.Create;
  pt.AddPType(tex.IndexOf('snow'), 1,-0.01, 0);
  pt.AddPType(tex.IndexOf('glow'), 1,-0.02, 0.2);

  launcher:=pt.AddLauncher(1, 5, 8,9, 4.5,5.0, 6, norm2(vector(1,-1)), -1);
  pt.SetLauncherColor(launcher, 1,1, 0.5,1,0.2,0.7);
  pt.SetLauncherPos(launcher, 0, ClientHeight-1);

  pt.CloneLauncher(launcher, norm2(vector(-1,-1)), ClientWidth-1, ClientHeight-1);

  for i:=0 to 2 do
    with follower[i] do begin
      x:=ClientWidth*(0.3+0.4*random);
      y:=ClientHeight*(0.3+0.4*random);
      pt.AddTrail(tex.IndexOf('glow'), 40, 10, x, y);
    end;
  pt.SetTrailColor(@pt.trail[0], 1, 0, 0);
  pt.SetTrailColor(@pt.trail[1], 0, 1, 0);
  pt.SetTrailColor(@pt.trail[2], 0, 0, 1);

  bassSound:=TBassEngine.Create(handle);
  if not bassSound.CheckErrors then begin
    bassSound.Add('music', '..'+PathChar+'..'+PathChar+'media'+PathChar+
      'test.s3m').Play;
    bassSound.sound[0].looped:=true;
  end;

  LastTick:=nx.GetTick;

  if nx.LastError<>'' then Showmessage(nx.LastError)
  else Timer1.Enabled:=true;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  qt.Free; pt.Free; grid.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_SPACE: DoWireFrame:=not DoWireFrame;
    VK_ESCAPE: close;
  end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var dx, dy: single; i: integer;
begin
  if not nx.AllOK then exit;
  dx:=x-mx; dy:=y-my; // Cursor movement delta
  mx:=x; my:=y; // Current cursor position

  // Add a snowflake particles
  for i:=1 to 10 do
    pt.AddParticle(0, random,1,1, 3+10*random, x,y)^.velocity:=
      vector(dx*0.5*random, dy*0.5*random);
end;

procedure TForm1.MusicTimerTimer(Sender: TObject);
begin
  if bassSound<>nil then
    bassSound.Update; // Music looping
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  timer1.Enabled:=false;
  if bassSound.count>0 then begin
    bassSound.sound[0].FadeVolume(0, 50);
    sleep(50);
  end;
  fb.Free; FreeAndNil(bassSound);
  nx.KillGLWindow;
end;

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  if timer1.Enabled then begin
    Timer1Timer(nil);
    Done:=false;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var i,j: integer; d, h, x1,y1: single; vec: TVector; t: cardinal;
begin
  if not nx.AllOK then exit;

  t:=nx.GetTick;
  lag:=lag+(t-LastTick);
  LastTick:=t;

  if lag>=15 then begin
    lag:=lag-15;
    GameLoop;
  end;

  nx.Enable2D(false);
  fb.Bind; // Start drawing to framebuffer

  nx.Clear(true, false);
  nx.SetColor(1, 1, 1);

  // Draw Quad texture
  qt.DrawScaled(0, 0, nx.Width, nx.Height);

  // Draw particle engine with blending
  nx.rs.AddBlend:=true;
  pt.Draw2D;
  nx.rs.AddBlend:=false;

  fb.UnBind; // Resume to normal rendering
  nx.Disable2D;

  // Vertex X and Y coordinates
  for j:=0 to GS do begin
    y1:=j*ClientHeight/GS;
    for i:=0 to GS do
      with grid.va[i+j*(GS+1)] do begin
        x1:=i*ClientWidth/GS;

        // Bend towards blue trail[2]
        vec:=VectorSub2(vector(x1, y1), vector(pt.trail[2].x, pt.trail[2].y));

        h:=hypot(vec.x, vec.y);
        d:=5-h*0.02;
        d:=d*abs(d);
        if d<=0 then d:=0
        else if d>h then d:=h;
        vec:=norm2(vec);
        x:=x1 -vec.x*d;
        y:=y1 -vec.y*d;
      end;
  end;
  nx.Enable2D;
  nx.Clear(true, false);
  tex.SetTex(fb.texture);
  nx.SetColor(1, 1, 1);
  grid.EnableStates;
  grid.SetPointers;
  nx.rs.AddBlend:=true;
  nx.rs.WireFrame:=DoWireFrame;
  grid.Render(0, grid.fCount, true); // Draw grid (or wireframe)
  nx.rs.WireFrame:=false;
  nx.rs.AddBlend:=false;
  grid.DisableStates;

  if DoWireFrame then begin
    // Draw partially transparent image
    glBegin(GL_QUADS);
      glColor3f(1,1,1);
      glTexCoord2f(1,0); glVertex2f(ClientWidth, 0);
      glTexCoord2f(0,0); glVertex2f(0, 0);
      glTexCoord2f(0,1); glVertex2f(0, ClientHeight);
      glColor4f(0,0,0,0);
      glTexCoord2f(1,1); glVertex2f(ClientWidth, ClientHeight);
    glEnd;
  end;

  nx.Disable2D;

  nx.Flip;

  caption:=format('Effects demo 1 - FPS: %d', [nx.FPS]);
end;

procedure TForm1.GameLoop;
var i: integer;
begin
  for i:=0 to 2 do
    with follower[i] do begin
      velocity.x:=velocity.x+(mx-x)*0.002; velocity.y:=velocity.y+(my-y)*0.002;
      x:=x+velocity.x; y:=y+velocity.y;

      // Collide to window borders
      if x<0 then velocity.x:=abs(velocity.x*0.5)
      else if x>=ClientWidth then velocity.x:=-abs(velocity.x*0.5);
      if y<0 then velocity.y:=abs(velocity.y*0.5)
      else if y>=ClientHeight then velocity.y:=-abs(velocity.y*0.5);

      pt.SetTrailPos(@pt.trail[i], x, y);
    end;

  // Apply gravity on snowflakes
  pt.ApplyForce(0, 0, 0.1);
  pt.Move2D; // Process particle engine tick
end;

end.
