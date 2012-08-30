unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Forms, SysUtils, Controls, ExtCtrls, Dialogs,
  nxGL, nxTypes, math, dglOpenGL, LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Timer1: TTimer;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
  private
  public
    procedure Render;
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ClientWidth:=800; ClientHeight:=600;
  nx.CreateGlWindow(self);

  tex.AddTexture('circle', 'test.png', true);
  tex.AddTexture('hint', 'hint.png', true);

  nx.CreateBasicFont;
  nx.CreateFont('Courier', 10, 256);

  if nx.LastError<>'' then showmessage(nx.LastError)
  else timer1.Enabled:=true;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Press space to toggle between Timer1 and AppOnIdle
  if Key=VK_SPACE then begin
    timer1.Enabled:=not timer1.Enabled;
    if timer1.Enabled then caption:='Timer enabled'
    else caption:='OnIdle enabled';
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Render;
end;

procedure TForm1.Render;
var r, turnAngle, col, x, sc: single; i: integer;
begin
  if not nx.AllOK then exit;
  nx.Clear(true, false); // Clear only color, no depth buffer

  nx.Enable2D; // This includes glLoadidentity

  tex.SetTex(0);
  nx.SetColor(1, 1, 1); // Set white color

  // Find r that makes circle fit window
  r:=min(nx.Width, nx.Height) * 0.5;

  turnAngle:= -nx.GetTick(360,0.02);

  glTranslatef(nx.Width/2, nx.Height/2, 0);
  glRotatef(turnAngle, 0, 0, 1);
  nx.DrawScaled(-r, -r, r*2, r*2);

  // Rotated and scaled drawing:
  // (x, y, pattern, angle, centerX,centerY, sizeX,sizeY)
  // nx.DrawRotateS(-r*0.5, 0, 0, turnAngle, 0.5,0.5, r*0.75,r*0.75);

  for i:=1 to 4 do begin
    col:=i/4;
    nx.SetColor(1-col,1-col,1);
    glTranslatef(-r*0.5,0,0);
    r:=r*0.377;
    glRotatef(turnAngle, 0, 0, 1);
    nx.DrawScaled(-r, -r, r*2, r*2);
  end;

  // Show hint text
  glLoadIdentity;
  nx.SetColor(1,1,1);
  tex.SetTex(1);
  nx.Draw((nx.Width-tex.texture[1].Width) div 2,
    nx.Height-10-tex.texture[1].Height);

  nx.SetFont(1);
  nx.Font[1].Draw(5, 5, 'FPS: '+inttostr(nx.FPS));

  nx.SetFont(0);
  nx.rs.AddBlend:=true;
  for i:=0 to 10 do begin
    nx.SetColor(i/10, i/20, i/30);
    x:=70+20*sin((nx.GetTick(360, 0.1)+i*10)*toRad);
    glPushMatrix;
    sc:=1+i*0.1;
    glScalef(sc, sc, sc);
    glTranslatef(x, 20+i*3, 0);
    glRotatef(80*sin((nx.GetTick(360, 0.1)+i*10+90)*toRad), 0,0,1);
    nx.Font[0].DrawC(0, 0, 'Texture demo');
    glPopMatrix;
  end;
  nx.rs.AddBlend:=false;

  nx.Disable2D;

  nx.Flip;
end;

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  // Loop activates after pressing Space
  if not timer1.Enabled then begin
    Render;

    // Save CPU a little? Limits fps to few thousands (varies)
    sleep(1); // Comment out for full performance

    Done:=false;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  timer1.Enabled:=false;
  nx.KillGLWindow;
end;

end.

