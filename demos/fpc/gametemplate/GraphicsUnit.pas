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
begin
  inherited Create;
  // Load graphics
  nx.SetClearColor(0.1, 0.2, 0.4);
  nx.Clear(true, true);
  nx.Flip;

  nx.CreateFont('Courier', 8, 256);
  tex.AddTexture('glow', GetPath('textures\glow.png'));

  // Last operations
  if nx.LastError<>'' then ShowMessage(nx.LastError);
  ResetTick; Initialized:=true;
end;

procedure TGraphicalGame.Draw;
begin
  // Draw game tick
  nx.Clear(true, false);
  nx.Enable2D;

  // Draw "player"
  nx.rs.Push;
  nx.rs.AddBlend:=true;
  tex.SetByName('glow');
  // Change red color if click button, otherwise light-green
  if mb[1] then nx.SetColor(1, 0, 0)
  else nx.SetColor(0.5, 1, 0.5);
  nx.DrawRotate(playerPos.x, playerPos.y, 0, t*0.1, 0.5, 0.5);
  nx.rs.Pop;

  // Write some text
  nx.SetFont(0); nx.SetColor(1, 1, 1);
  nx.Font[0].Draw(1, 1, format('FPS: %d', [nx.FPS]));
  nx.Font[0].Draw(1, 12, format('Mouse (%.1f : %.1f)', [mp.x, mp.y]));
  nx.Font[0].Draw(1, 23, format('Player (%.1f : %.1f)', [playerPos.x, playerPos.y]));

  nx.SetFont(0); nx.SetColor(1, 0, 0);
  nx.Font[0].DrawC(nx.Width/2, 15, '- Press ESC to quit -');
  nx.SetColor(1, 1, 1, 0.5);
  nx.Font[0].DrawC(nx.Width/2, 30, '- or Space to toggle mouse mode -');

  nx.Disable2D;
  nx.Flip;
end;

end.

