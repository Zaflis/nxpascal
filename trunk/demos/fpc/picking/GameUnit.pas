unit GameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, SysUtils, LCLType, math, nxMath, nxMath3D, nxGL,
  nxGame, nxTypes;

type

  TPlayer = record
    position, movement: TVector;
    rotation: TMatrix;
  end;

  TFloatingObject = record
    position, rotAxis: TVector;
    color: TRGB;
    rotation: TMatrix;
    model: byte;
  end;

  { TGame }

  TGame = class(TGameHandler)
  public
    pl: TPlayer;
    cam: TMatrix;
    obj: array[0..40] of TFloatingObject;
    constructor Create;
    procedure GameLoop; override;
    procedure SetCamBehind;
  end;

implementation

{ TCustomGame }

constructor TGame.Create;
var i: integer;
begin
  inherited Create;
  mouseXSpeed:=0.5; mouseYSpeed:=mouseXSpeed;
  CenterMouse(true);

  pl.rotation:=NewMatrix;
  pl.position:=vector(1, 5, 3);
  cam:=NewMatrix;
  SetCamBehind;

  for i:=0 to high(obj) do
    with obj[i] do begin
      model:=random(2)+1;
      position.x:=random*40-20;
      position.y:=random*40-20;
      position.z:=random*40-20;
      rotAxis:=vector(random*2-1, random*2-1, random*2-1);
      norm(rotAxis);
      rotation:=multiply(
        CreateMatrix(vector(0, 1, 0), random*2*pi),
        CreateMatrix(vector(1, 0, 0), random*2*pi));
      color:=rgb(100+random(156), 100+random(156), 100+random(156));
    end;
end;

procedure TGame.GameLoop;
var i: integer;
begin
  // Accelerate with W-key
  if keys[VK_W] then begin
    // Read Z-direction vector from player rotation matrix,
    // scale it down, and interpolate movement vector with it.
    pl.movement:=interpolate(pl.movement, scale(GetVector(pl.rotation, 2),
      0.1), // Max movement speed
      0.01); // Acceleration
  end;
  // Backwards
  if keys[VK_S] then begin
    pl.movement:=interpolate(pl.movement, scale(GetVector(pl.rotation, 2),
      -0.1), // Max movement speed
      0.01); // Acceleration
  end;
  // Strafing
  if keys[VK_A] then begin
    pl.movement:=interpolate(pl.movement, scale(GetVector(pl.rotation, 0),
      0.05), // Max movement speed
      0.01); // Acceleration
  end;
  if keys[VK_D] then begin
    pl.movement:=interpolate(pl.movement, scale(GetVector(pl.rotation, 0),
      -0.05), // Max movement speed
      0.01); // Acceleration
  end;
  // Rolling sideways
  if keys[VK_Q] then begin
    rotate(pl.rotation, norm2(GetVector(pl.rotation, 2)),
      3*toRad, false);
  end;
  if keys[VK_E] then begin
    rotate(pl.rotation, norm2(GetVector(pl.rotation, 2)),
      -3*toRad, false);
  end;

  // Rotate ship with mouse
  if abs(mDelta.x)>50 then mDelta.x:=0;
  if abs(mDelta.y)>50 then mDelta.y:=0;
  if mDelta.x<>0 then
    rotate(pl.rotation, norm2(GetVector(pl.rotation, 1)),
      2*mDelta.x*toRad, false);
  if mDelta.y<>0 then
    rotate(pl.rotation, norm2(GetVector(pl.rotation, 0)),
      -2*mDelta.y*toRad, false);

  // Move ship
  VectorAdd(pl.position, pl.movement);

  SetCamBehind;

  // Rotate floating objects
  for i:=0 to high(obj) do
    with obj[i] do begin
      Rotate(rotation, rotAxis, 0.1*toRad, false);
    end;
end;

procedure TGame.SetCamBehind;
begin
  cam:=pl.rotation;
  SetVector(cam, pl.position, 3);
  Invert(cam);
  Rotate(cam, 0, PI); // Rotate 180 degrees to ship behind
end;

end.

