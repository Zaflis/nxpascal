unit GameUnit;

interface

uses
  Windows, Classes, Dialogs, SysUtils, math, nxMath, nxMath3D,
  nxGL, nxGame, nxTypes, nxBass;

const
  MoveSpeed = 0.083;

type
  TPlayer = record
    position, movement, gunhole: TVector;
    rotX, lookY, groundDist, eyeHeight: single;
    shootDelay: word;
    weapon: shortint;
  end;

  TBullet = record
    position, movement: TVector;
    bounces: word;
  end;

  TPickableType = (ptWeapon, ptHealth, ptAmmo);

  TPickableObject = record
    position, movement: TVector;
    kind: TPickableType;
    index: smallint;
    quantity: word;
    stopped: boolean;
  end;
  PPickableObject = ^TPickableObject;

  { TWorldModel }

  TWorldModel = class(TGLModel)
  public
    forcefield: TGLModel;
    constructor Create(filename: string);
    destructor Destroy; override;
    procedure Render(Initialize: boolean = true);
  end;

  { TGame }

  TGame = class(TGameHandler)
  public
    cam: TCamera;
    player: TPlayer;
    world: TWorldModel;
    cVec, mVec: TVector;
    ray: TMouseRay;
    jumping, crouching: boolean;
    bullet: array of TBullet;
    obj: array of TPickableObject;
    holdAngleY: single;
    sound: TBassEngine;
    constructor Create;
    destructor Destroy; override;
    function AddPickable(const kind: TPickableType;
      const position: TVector): PPickableObject;
    procedure GameLoop; override;
    procedure RemovePickable(index: integer);
  end;

implementation

{ TCustomGame }

constructor TGame.Create;
begin
  inherited Create;
  // Initialize and load game variables

  cam:=TCamera.Create;
  player.position:=vector(0, 2, 0);
  player.rotX:=280;
  player.weapon:=-1;

  AddPickable(ptWeapon, vector(-4.6, -0.25, 14.3));

  mouseXSpeed:=0.15; mouseYSpeed:=mouseXSpeed;
  CenterMouse(true);
end;

destructor TGame.Destroy;
begin
  cam.Free;
  sound.Free;
  inherited Destroy;
end;

function TGame.AddPickable(const kind: TPickableType;
  const position: TVector): PPickableObject;
begin
  setlength(obj, length(obj)+1);
  result:=@obj[high(obj)];
  result^.position:=position;
  result^.kind:=kind;
  result^.movement:=nullVector;
  result^.index:=0;
  result^.quantity:=1;
  result^.stopped:=false;
end;

procedure TGame.GameLoop;
var nearest, normal, v: TVector; d: single;
    i, loops: integer;
begin
  with player do begin
    world.rayIntersect(position, vector(0, -1, 0), true, @v, nil);
    groundDist:=vectordist(position, v);

    rotX:=fmod(rotX+mDelta.x, 0, 360);
    lookY:=MinMax(lookY+mDelta.y, -70, 90);

    movement.x:=0;
    movement.z:=0;
    movement.y:=movement.y-0.008;

    if keys[ord('W')] then
      movement:=VectorAdd(movement, VectorXZFromAngle(rotX*toRad, MoveSpeed));
    if keys[ord('S')] then
      movement:=VectorSub(movement, VectorXZFromAngle(rotX*toRad, MoveSpeed));
    if keys[ord('A')] then
      movement:=VectorAdd(movement, VectorXZFromAngle((rotX-90)*toRad, MoveSpeed));
    if keys[ord('D')] then
      movement:=VectorAdd(movement, VectorXZFromAngle((rotX+90)*toRad, MoveSpeed));
    if (groundDist<0.5) and keys[VK_SPACE] and (not jumping) then begin
      movement.y:=0.15;
      jumping:=true;
    end else if jumping and (not keys[VK_SPACE]) then
      jumping:=false;

    if keys[VK_CONTROL] and (not crouching) then
      crouching:=true
    else if crouching and (not keys[VK_CONTROL]) then
      crouching:=false;

    if crouching then
      eyeHeight:=max(0.4, interpolate(eyeHeight, 0.4, 0.3))
    else
      eyeHeight:=min(1.1, interpolate(eyeHeight, 1.1, 0.3));

    position:=VectorAdd(position, movement);

    loops:=0;
    repeat
      world.FindNearest(position, @nearest, nil);
      normal:=norm(VectorSub(position, nearest));
      d:=VectorDist(position, nearest);
      if d<0.4 then begin
        position:=VectorAdd(position, scale(normal, 0.4-d));
        movement.y:=movement.y*(1-abs(normal.y));
      end else
        break;
      inc(loops);
    until loops>9;

    sound.listener.SetLocation(position);
    sound.listener.SetOrientation(
      VectorXZFromAngle(rotX*toRad), vector(0, 1, 0));

    if holdAngleY<0 then holdAngleY:=interpolate(holdAngleY, 0, 0.2);

    gunhole:=vector(0.4, -0.07, 0.05);
    gunhole:=rotate(gunhole, -lookY*toRad, vector(0, 0, 1));
    gunhole:=rotate(gunhole, -rotX*toRad, vector(0, 1, 0));
    gunhole:=VectorAdd(VectorAdd(gunhole, position),
      vector(0, player.EyeHeight, 0));

    for i:=high(obj) downto 0 do
      if VectorDist(position, obj[i].position)<0.4 then begin
        if obj[i].kind=ptWeapon then begin
          weapon:=obj[i].index;
          holdAngleY:=-30;
        end;
        RemovePickable(i);
      end;

    cam.Reset(false);
    cam.Rotate(lookY, 1, 0, 0, false);
    cam.Rotate(rotX+90, 0, 1, 0, false);
    with position do cam.Translate(-x, -y-EyeHeight, -z);
    nx.GetMouseRay(nx.Width/2, nx.Height/2, @ray, 0);
    world.rayIntersect(ray.start, ray.dir, true, @mVec, nil);
  end;

  for i:=high(bullet) downto 0 do
    with bullet[i] do begin
      position:=VectorAdd(position, movement);
      world.FindNearest(position, @nearest, @normal);
      d:=VectorDist(position, nearest);
      if d<0.1 then begin
        position:=VectorSub(position, movement);
        movement:=Reflect(movement, normal);
        inc(bounces);
        sound.Find('bounce').SetLocation(position);
        sound.Find('bounce').SetVelocity(movement, nx.FPS);
        sound.Find('bounce').SetOrientation(norm(movement));
        sound.Find('bounce').Play;
      end else begin
        world.forcefield.FindNearest(position, @nearest, @normal);
        d:=VectorDist(position, nearest);
        if d<0.2 then begin
          position:=VectorSub(position, movement);
          movement:=Reflect(movement, normal);
          inc(bounces);
          sound.Find('bounce').SetLocation(position);
          sound.Find('bounce').SetVelocity(movement, nx.FPS);
          sound.Find('bounce').SetOrientation(norm(movement));
          sound.Find('bounce').Play;
        end;
      end;
      if bounces>10 then begin
        bullet[i]:=bullet[high(bullet)];
        setlength(bullet, high(bullet));
      end;
    end;

  if player.shootDelay>0 then dec(player.shootDelay);
  if mb[1] and (player.shootDelay=0) and (player.weapon=0) then begin
    player.shootDelay:=10;
    setlength(bullet, length(bullet)+1);
    with bullet[high(bullet)] do begin
      position:=player.gunhole;
      movement:=scale(norm(VectorSub(mVec, player.gunhole)), 0.2);
      bounces:=0;
      //sound.Find('laser').Stop;
      sound.Find('laser').SetLocation(position);
      sound.Find('laser').SetVelocity(movement, nx.FPS);
      sound.Find('laser').SetOrientation(norm(movement));
      sound.Find('laser').Play;
    end;
  end;

  sound.Update;
end;

procedure TGame.RemovePickable(index: integer);
begin
  obj[index]:=obj[high(obj)];
  setlength(obj, high(obj));
end;

{ TWorldModel }

constructor TWorldModel.Create(filename: string);
var n, i: integer;
begin
  inherited Create(filename);
  forcefield:=TGLModel.CreateCube;
  forcefield.UseMaterials:=true;
  forcefield.UseColors:=false;
  n:=forcefield.NewMaterial(tex.IndexOf('forcefield'));
  forcefield.mat[n].addMode:=true;
  forcefield.mat[n].specular:=0;
  forcefield.mat[n].shininess:=0;
  for i:=0 to forcefield.groups-1 do
    forcefield.grp[i].matIndex:=n;
  forcefield.Scale(1, 2, 0);
  forcefield.Translate(0.1, 1, 3.1);
end;

destructor TWorldModel.Destroy;
begin
  forcefield.Free;
  inherited Destroy;
end;

procedure TWorldModel.Render(Initialize: boolean);
begin
  inherited Render(Initialize);
  nx.rs.Push;
  nx.rs.Lighting:=false;
  nx.SetColor(1, 0, 0, 0.9+random*0.1); // Flashy red color
  forcefield.Render;
  nx.rs.Pop;
end;

end.

