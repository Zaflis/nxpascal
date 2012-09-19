unit GraphicsUnit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Dialogs, dglOpenGL, nxGL, math, nxMath, nxMath3D,
  nxTypes, GameUnit;

type

  { TGraphicalGame }

  TGraphicalGame = class(TGame)
  protected
    mdl: array[0..5] of TGLModel;
  public
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
  nx.SetClearColor(0, 0, 0);
  nx.DefaultLights;
  nx.rs.DepthTest:=true;

  mdl[0]:=TGLModel.Create;
  mdl[0].LoadFromFile(GetPath('objects\ship.w3d'));
  mdl[0].LoadTextures('textures');

  mdl[1]:=TGLModel.Create;
  mdl[1].LoadFromFile(GetPath('objects\cell.w3d'));
  mdl[1].LoadTextures('textures');

  mdl[2]:=TGLModel.Create;
  mdl[2].LoadFromFile(GetPath('objects\object.w3d'));
  mdl[2].LoadTextures('textures');
  mdl[2].UseColors:=false;

  mdl[3]:=TGLModel.CreateCube;
  mdl[3].NewMaterial(mdl[2].mat[0].texIndex);

  mdl[4]:=TGLModel.CreateSphere(20, 20);
  mdl[4].NewMaterial(mdl[0].mat[0].texIndex);
  mdl[4].Scale(2, 2, 2);

  mdl[5]:=TGLModel.CreateTorus(20, 20, 0.15);
  mdl[5].NewMaterial(mdl[2].mat[0].texIndex);
  mdl[5].Scale(3, 2, 2);
  mdl[5].ScaleUV(3, 3);

  // Last operations
  if nx.LastError<>'' then ShowMessage(nx.LastError);
  ResetTick; Initialized:=true;
end;

destructor TGraphicalGame.Destroy;
var i: integer;
begin
  for i:=0 to high(mdl) do mdl[i].Free;
  inherited Destroy;
end;

procedure TGraphicalGame.Draw;
var i: integer; rayPos, rayDir, intersect: TVector;
    d: single;
begin
  nx.Clear(true, true);

  // Make torus "spinning", by moving UV-map
  mdl[5].TranslateUV(0.05, 0);

  // Set camera
  camera.Reset(false); //glLoadIdentity;
  // Bit further behind and upwards
  camera.Translate(0, -0.5, -3, false); //glTranslatef(0, -0.5, -3);
  // Apply behind-ship-matrix
  camera.Multiply(shipCam); //glMultMatrixf(@shipCam);

  // Get mouse-ray on cursor
  if isMouseCentered then
    nx.GetMouseRay(nx.Width/2, nx.Height/2, @rayPos, @rayDir)
  else
    nx.GetMouseRay(mp.x, mp.y, @rayPos, @rayDir);

  // Render player ship
  camera.Push; //glPushMatrix;
  with pl.position do
    camera.Translate(x, y, z); //glTranslatef(x, y, z);
  camera.Multiply(pl.rotation); //glMultMatrixf(@pl.rotation);
  mdl[0].Render;
  camera.Pop; //glPopMatrix;

  focus:=-1;

  // Render floating objects
  for i:=0 to high(obj) do
    with obj[i] do begin
      camera.Push; //glPushMatrix;

      // Test mouse-ray intersect with object
      if mdl[model].rayIntersect(rayPos, rayDir, false,
         position, rotation, nil, nil)>=0 then begin
        // Set glowing transparent red
        nx.SetColor(1, 0, 0, 0.8+0.2*sin(toRad*nx.GetTick(360, 0.5)));
        if (focus<0) or (VectorDist(position, pl.position)<
             VectorDist(obj[focus].position, pl.position)) then begin
          // Change focus if it is closest focused object to player ship
          focus:=i;
        end;
      end else
        nx.SetColor(color); // Use objects own color

      with position do
        camera.Translate(x, y, z); //glTranslatef(x, y, z);
      camera.Multiply(rotation); //glMultMatrixf(@rotation);
      mdl[model].Render;
      camera.Pop; //glPopMatrix;
    end;

  // Don't write in depth-buffer, but we can still use depth-test
  glDepthMask(false);
  nx.rs.Push;           // Take backup of old render settings
  nx.rs.AddBlend:=true; // Use additive blending
  nx.rs.Lighting:=false; // Disable lighting

  // Render grid
  tex.Disable; // Disable textures
  nx.SetColor(0.4, 1, 0.4, 0.2); // Transparent light-green
  glBegin(GL_LINES);
  glVertex3f(0, 0, 0); glVertex3f(0, 40, 0);
  for i:=-40 to 40 do begin
    glVertex3f(i, 0, -40); glVertex3f(i, 0, 40);
    glVertex3f(-40, 0, i); glVertex3f(40, 0, i);
  end;
  glEnd;
  tex.Enable;

  // Render cell focus
  d:=RayPlaneIntersect(rayPos, rayDir,
    vector(0,0,0), vector(0,1,0), @intersect);
  if d>=0 then begin
    camera.Push; //glPushMatrix;
    with intersect do
      camera.Translate(floor(x), 0, floor(z)); //glTranslatef(floor(x), 0, floor(z));
    mdl[1].Render;
    camera.Pop; //glPopMatrix;
  end;

  nx.rs.Pop; // Recall earlier render settings
  glDepthMask(true); // Write in depth-buffer again

  nx.Flip;
end;

end.

