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
    xRot, yRot: single;
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
    shipCam: TMatrix;
    camera: TCamera;
    obj: array[0..40] of TFloatingObject;
    focus: integer;
    constructor Create;
    destructor Destroy; override;
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

  camera:=TCamera.Create;
  pl.rotation:=NewMatrix;
  pl.position:=vector(1, 5, 3);
  shipCam:=NewMatrix;
  SetCamBehind;

  for i:=0 to high(obj) do
    with obj[i] do begin
      model:=random(4)+2;
      position.x:=random*40-20;
      position.y:=random*40-20;
      position.z:=random*40-20;
      rotAxis:=vector(random*2-1, random*2-1, random*2-1);
      rotAxis:=norm(rotAxis);
      rotation:=NewMatrix;
      rotation:=multiply(
        CreateMatrix(vector(0, 1, 0), random*2*pi),
        CreateMatrix(vector(1, 0, 0), random*2*pi));
      color:=rgb(100+random(156), 100+random(156), 100+random(156));
    end;
end;

destructor TGame.Destroy;
begin
  camera.Free;
  inherited Destroy;
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
    pl.rotation:=rotate(pl.rotation, norm(GetVector(pl.rotation, 2)),
      -3*toRad, false);
  end;
  if keys[VK_E] then begin
    pl.rotation:=rotate(pl.rotation, norm(GetVector(pl.rotation, 2)),
      3*toRad, false);
  end;

  // Rotate ship with mouse
  if abs(mDelta.x)>50 then mDelta.x:=0;
  if abs(mDelta.y)>50 then mDelta.y:=0;
  pl.xRot:=pl.xRot-mDelta.x*0.05;
  pl.yRot:=pl.yRot-mDelta.y*0.05;
  pl.xRot:=interpolate(pl.xRot, 0, 0.05);
  pl.yRot:=interpolate(pl.yRot, 0, 0.05);
  if pl.xRot<>0 then begin
    pl.rotation:=rotate(pl.rotation, norm(GetVector(pl.rotation, 1)),
      2*pl.xRot*toRad, false);
  end;
  if pl.yRot<>0 then begin
    pl.rotation:=rotate(pl.rotation, norm(GetVector(pl.rotation, 0)),
      -2*pl.yRot*toRad, false);
  end;

  // Move ship
  pl.position:=pl.position + pl.movement; // Operator overloading
  //pl.position:=VectorAdd(pl.position, pl.movement);

  SetCamBehind;

  // Rotate floating objects
  for i:=0 to high(obj) do
    with obj[i] do begin
      rotation:=Rotate(rotation, rotAxis, 0.1*toRad, false);
    end;
end;

procedure TGame.SetCamBehind;
var temp: TMatrix;
begin
  temp:=pl.rotation;
  SetVector(temp, pl.position, 3);
  temp:=Invert(temp);
  temp:=Rotate(temp, 0, PI);
  shipCam:=Interpolate(shipCam, temp, 0.3);
end;

end.

