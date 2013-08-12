unit GraphicsUnit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Dialogs, dglOpenGL, nxGL, math, nxMath, nxMath3D,
  nxTypes, GameUnit;

type

  { TGraphicalGame }

  TGraphicalGame = class(TGame)
  public
    constructor Create;
    procedure Draw; override;
  end;

implementation

{ TGame }

constructor TGraphicalGame.Create;
var n: integer;
begin
  inherited Create;
  // Load graphics
  nx.SetClearColor(0.1, 0.2, 0.4);
  nx.Clear(true, true);
  nx.Flip;

  nx.CreateFont('Courier', 8, 256);
  nx.rs.CullBack:=false;
  tex.Options:=[toFitScale];

  n:=tex.AddTexture('tile', GetPath('textures\tile.png'));
  n:=tex.AddTexture('spark', GetPath('textures\spark.png'));
  pt.AddPType(n, 0.5, -1.0, 0.2);

  tex.TextureQuality:=GL_NEAREST;
  n:=tex.AddTexture('walker', GetPath('textures\walker.png'), true);
  tex.SetPattern(n, 17, 26, 1, 1);

  // Last operations
  if nx.LastError<>'' then ShowMessage(nx.LastError);
  ResetTick; Initialized:=true;
end;

procedure TGraphicalGame.Draw;
var i, j: integer;
begin
  // Draw game tick
  nx.Clear(true, false);
  nx.Enable2D;

  glPushMatrix;
  i:=(nx.Width-nx.Height*4 div 3) div 2;
  if i>0 then glTranslatef(i, 0, 0);
  glScalef(nx.Height/300, nx.Height/300, 1);

  tex.SetByName('tile');
  nx.SetColor(0.5, 0.6, 0.7);
  // Draw tiles
  for j:=0 to 7 do
    for i:=-2 to 9 do
      nx.Draw(i*55, j*38);

  // Draw walkers
  tex.SetByName('walker');
  nx.SetColor(1, 1, 1);
  for i:=0 to high(walker) do
    with walker[i] do begin
      nx.SetColor(color);
      if goLeft then
        nx.DrawRotateS(position.x, position.y, trunc(anim), 0, 0.5, 20/27,
          -17, 26)
      else
        nx.DrawRotate(position.x, position.y, trunc(anim), 0, 0.5, 20/27);
    end;

  // Draw particles
  nx.rs.Push;
  nx.rs.AddBlend:=true;
  pt.Draw2D;
  nx.rs.Pop;

  glPopMatrix; // Revert screen scaling

  nx.SetFont(0); nx.SetColor(1, 1, 1);
  nx.Font[0].Draw(1, 1, format('FPS: %d', [nx.FPS]));
  nx.Font[0].Draw(1, 12, format('Walkers: %d', [length(walker)]));
  nx.Font[0].Draw(1, 23, format('Physics loops: %d', [physLoops]));
  nx.Font[0].Draw(1, 34, format('Particles: %d', [pt.pCount]));

  nx.SetFont(0); nx.SetColor(1, 0.3, 0.3);
  nx.Font[0].DrawC(nx.Width/2, 15, '- Press F1 to reset -');

  nx.Disable2D;
  nx.Flip;
end;

end.

