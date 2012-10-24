unit GameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, SysUtils, LCLType, math, nxMath, nxMath3D, nxGL,
  nxGame, nxTypes;

type

  { TGame }

  TGame = class(TGameHandler)
  public
    playerPos: TVector2f;
    constructor Create;
    procedure GameLoop; override;
  end;

implementation

{ TCustomGame }

constructor TGame.Create;
begin
  inherited Create;
  // Initialize and load game variables

  modpath:='testMod'; // Use custom mod in this path
  playerPos:=vector2f(nx.Width/2, nx.Height/2);
  mouseXSpeed:=1; mouseYSpeed:=mouseXSpeed;
  CenterMouse(true);
end;

procedure TGame.GameLoop;
begin
  // Process game tick

  // Make "player" to move by cursor position
  if isMouseCentered then begin
    playerPos.x:=playerPos.x+mDelta.x;
    playerPos.y:=playerPos.y+mDelta.y;
    // Wrap on borders
    if playerPos.x<0 then playerPos.x:=playerPos.x+nx.Width
    else if playerPos.x>=nx.Width then playerPos.x:=playerPos.x-nx.Width;
    if playerPos.y<0 then playerPos.y:=playerPos.y+nx.Height
    else if playerPos.y>=nx.Height then playerPos.y:=playerPos.y-nx.Height;
  end else begin
    playerPos:=mp;
  end;
end;

end.

