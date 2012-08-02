unit GraphicsUnit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, dglOpenGL, nxGL, math, nxMath, nxMath3D, nxTypes, GameUnit;

type

  { TGame }

  TGame = class(TCustomGame)
  public
    constructor Create;
    procedure Draw; override;
  end;

implementation

{ TGame }

constructor TGame.Create;
begin
  inherited Create;
  // Load graphics
  nx.CreateFont('Courier', 8, 256);

  tex.AddTexture('glow', GetPath('textures\glow.png'));

  // Last operations
  ResetTick; Initialized:=true;
end;

procedure TGame.Draw;
begin
  // Draw game tick
  nx.Clear(true, false);
  nx.Enable2D;

  // Draw "player"
  tex.SetByName('glow'); nx.SetColor(0.5, 1, 0.5);
  nx.DrawRotate(playerPos.x, playerPos.y, 0, t*0.1, 0.5, 0.5);

  // Write some text
  nx.SetFont(0); nx.SetColor(1, 1, 1);
  nx.Font[0].Draw(1, 1, format('FPS: %d', [nx.FPS]));
  nx.Font[0].Draw(1, 12, format('Mouse (%.1f : %.1f)', [mp.x, mp.y]));
  nx.Font[0].Draw(1, 23, format('Player (%.1f : %.1f)', [playerPos.x, playerPos.y]));

  nx.SetFont(0); nx.SetColor(1, 0, 0);
  nx.Font[0].DrawC(nx.Width/2, 15, '- Press ESC to quit -');

  nx.Disable2D;
  nx.Flip;
end;

end.

