unit GameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, SysUtils, LCLType, math, nxMath, nxMath3D, nxGL,
  nxGame, nxTypes;

type

  { TCustomGame }

  TCustomGame = class(TGameHandler)
  public
    playerPos: TVector2f;
    constructor Create;
    procedure GameLoop; override;
  end;

implementation

{ TCustomGame }

constructor TCustomGame.Create;
begin
  inherited Create;
  // Initialize and load game variables

  modpath:='testMod'; // Use custom mod in this path
  playerPos:=vector2f(nx.Width/2, nx.Height/2);
  mouseXSpeed:=1; mouseYSpeed:=mouseXSpeed;
  CenterMouse(true);
end;

procedure TCustomGame.GameLoop;
begin
  // Process game tick

  // Make "player" to move by cursor position
  playerPos.x:=playerPos.x+mDelta.x;
  playerPos.y:=playerPos.y+mDelta.y;
end;

end.

