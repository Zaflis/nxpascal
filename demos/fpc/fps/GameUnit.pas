unit GameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, SysUtils, LCLType, math, nxMath, nxMath3D,
  nxGL, nxGame, nxTypes;

const
  MoveSpeed = 0.023;
  EyeHeight = 1.5;

type
  TPlayer = record
    position, movement: TVector;
    rotX, lookY: single;
    grounded: boolean;
  end;

  { TGame }

  TGame = class(TGameHandler)
  public
    cam: TCamera;
    player: TPlayer;
    world: TGLModel;
    cVec, mVec: TVector;
    ray: TMouseRay;
    constructor Create;
    destructor Destroy; override;
    procedure GameLoop; override;
  end;

implementation

{ TCustomGame }

constructor TGame.Create;
begin
  inherited Create;
  // Initialize and load game variables

  cam:=TCamera.Create;
  mouseXSpeed:=0.15; mouseYSpeed:=mouseXSpeed;
  CenterMouse(true);
  player.position:=vector(0, 1, 0);
  player.rotX:=280;
end;

destructor TGame.Destroy;
begin
  cam.Free;
  inherited Destroy;
end;

procedure TGame.GameLoop;
begin
  with player do begin
    rotX:=fmod(rotX+mDelta.x, 0, 360);
    lookY:=MinMax(lookY+mDelta.y, -70, 90);

    movement.x:=interpolate(movement.x, 0, 0.3);
    movement.z:=interpolate(movement.z, 0, 0.3);
    if not grounded then movement.y:=movement.y-0.008;

    if keys[VK_W] then
      movement:=VectorAdd(movement, VectorXZFromAngle(rotX*toRad, MoveSpeed));
    if keys[VK_S] then
      movement:=VectorSub(movement, VectorXZFromAngle(rotX*toRad, MoveSpeed));
    if keys[VK_A] then
      movement:=VectorAdd(movement, VectorXZFromAngle((rotX-90)*toRad, MoveSpeed));
    if keys[VK_D] then
      movement:=VectorAdd(movement, VectorXZFromAngle((rotX+90)*toRad, MoveSpeed));
    if keys[VK_SPACE] and grounded then begin
      grounded:=false;
      movement.y:=0.1;
    end;

    position:=VectorAdd(position, movement);
    if position.y<0 then begin
      grounded:=true;
      position.y:=0; movement:=nullVector;
    end;

    cam.Reset(false);
    cam.Rotate(lookY, 1, 0, 0, false);
    cam.Rotate(rotX+90, 0, 1, 0, false);
    with position do cam.Translate(-x, -y-EyeHeight, -z);
    nx.GetMouseRay(nx.Width/2, nx.Height/2, @ray, 0);
    world.rayIntersect(ray.start, ray.dir, true, @mVec, nil);
  end;

end;

end.

