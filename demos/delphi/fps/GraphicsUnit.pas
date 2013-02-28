unit GraphicsUnit;

interface

uses
  SysUtils, Dialogs, dglOpenGL, nxGL, math, nxMath, nxMath3D,
  nxTypes, GameUnit;

type

  { TGraphicalGame }

  TGraphicalGame = class(TGame)
  public
    weapon, sphere: TGLModel;
    lightpos: array[0..0] of TVector;
    constructor Create;
    destructor Destroy; override;
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

  nx.rs.DepthTest:=true;
  nx.DefaultLights;
  //nx.SetLight(0, GL_AMBIENT, 0.2, 0.2, 0.2);
  nx.SetLight(0, GL_AMBIENT, 0, 0, 0);
  nx.SetLight(0, GL_DIFFUSE, 0, 0, 0);

  nx.SetLight(1, GL_DIFFUSE, 1, 0, 0);

  nx.CreateFont('Courier', 8, 256);
  tex.AddTexture('glow', 'data\glow.png');
  tex.AddTexture('cursor', 'data\cursor.png');

  world:=TGLModel.Create('data\world.w3d');
  world.Scale(10, 10, 10);
  world.LoadTextures('data');
  weapon:=TGLModel.Create('data\weapon.w3d');
  weapon.Scale(0.5, 0.5, 0.5);
  weapon.LoadTextures('data');
  weapon.mat[4].addMode:=true;

  sphere:=TGLModel.CreateSphere(16, 16);
  sphere.ScaleTo(0.05, 0.05, 0.05);

  lightpos[0]:=vector(0, 3, 0);

  // Last operations
  if nx.LastError<>'' then ShowMessage(nx.LastError);
  ResetTick; Initialized:=true;
end;

destructor TGraphicalGame.Destroy;
begin
  world.Free;
  weapon.Free;
  inherited Destroy;
end;

procedure TGraphicalGame.Draw;
var spriteM: TMatrix; i: integer;
begin
  // Draw game tick
  nx.Clear(true, true);

  spriteM:=GetRotation(invert(cam.GetMatrix));

  for i:=0 to high(lightpos) do
    with lightpos[i] do nx.SetLight(i, GL_POSITION, x, y, z);

  world.Render;

  // Set blend effects for bullets and objects
  nx.rs.Push;
  tex.SetByName('glow');
  nx.rs.AddBlend:=true;
  nx.rs.Lighting:=false;

  // Bullets
  glDepthMask(false);
  nx.SetColor(1, 1, 0);
  for i:=0 to high(bullet) do begin
    cam.Push;
    cam.Translate(bullet[i].position);
    cam.Multiply(spriteM);
    nx.DrawScaled(0.2, -0.2, -0.4, 0.4);
    cam.Pop(false);
  end;

  // Object glow
  nx.SetColor(1, 1, 1, 0.5);
  for i:=0 to high(obj) do
    with obj[i] do begin
      cam.Push;
      with position do
        cam.Translate(x, y+0.1+0.03*sin(toRad*nx.GetTick(360, 0.14)), z);
      cam.Multiply(spriteM);
      nx.DrawScaled(0.3, -0.3, -0.6, 0.6);
      cam.Pop;
    end;
  glDepthMask(true);
  nx.rs.Pop;

  for i:=0 to high(obj) do
    with obj[i] do begin
      // Weapon model
      cam.Push;
      with position do
        cam.Translate(x, y+0.1+0.03*sin(toRad*nx.GetTick(360, 0.14)), z, false);
      cam.Rotate(nx.GetTick(360, 0.1), 0, 1, 0);
      if kind=ptWeapon then weapon.Render;
      cam.Pop;
    end;

  cam.Pop;

  if player.weapon>=0 then begin
    // Mouseray intersection (laser dot)
    cam.Push;
    tex.Disable;
    nx.rs.Push;
    nx.rs.AddBlend:=true;
    nx.rs.Lighting:=false;
    nx.rs.DepthTest:=false;
    cam.Translate(mVec);
    nx.SetColor(1, 0, 0);
    sphere.Render;
    nx.rs.Pop;
    tex.Enable;
    cam.Pop;

    // Equipped weapon
    cam.Push;
    cam.Reset(false);
    cam.Translate(0.05, -0.1+holdAngleY*0.002, -0.27);
    cam.Rotate(180, 0, 1, 0);
    cam.Rotate(-holdAngleY, 1, 0, 0);
    weapon.Render;
    cam.Pop;
  end;

  nx.Enable2D;

  if player.weapon>=0 then begin
    // Crosshair
    nx.rs.Push;
    tex.SetByName('cursor');
    nx.rs.AddBlend:=true;
    nx.SetColor(1, 0, 0);
    nx.DrawRotateS(nx.Width/2, nx.Height/2, 0, 0, 0.5, 0.5,
      nx.Height*0.08, nx.Height*0.08);
    nx.rs.Pop;
  end;

  // Write some text
  nx.SetFont(0);
  nx.SetColor(1, 1, 1);
  nx.Font[0].Draw(1, 1, format('FPS: %d', [nx.FPS]));
  nx.SetColor(1, 1, 1, 0.8);
  //nx.Font[0].Draw(1, 15, format('look angle: %.1f', [player.rotX]));
  //nx.Font[0].Draw(1, 30, format('Ground distance: %.2f', [player.groundDist]));
  with player.position do
    nx.Font[0].Draw(1, 45, format('Coords: %.1f, %.1f, %.1f', [x, y, z]));

  nx.SetColor(1, 1, 1, 0.3);
  nx.Font[0].DrawC(nx.Width/2, 15, '- Press ESC to quit -');

  nx.Disable2D;
  nx.Flip;
end;

end.

