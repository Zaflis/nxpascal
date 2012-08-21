unit GraphicsUnit;

interface

uses
  SysUtils, Dialogs, dglOpenGL, nxGL, math, nxMath, nxMath3D,
  nxTypes, GameUnit;

type

  { TGraphicalGame }

  TGraphicalGame = class(TGame)
  protected
    mdl: array[0..3] of TGLModel;
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
  mdl[1].LoadFromFile(GetPath('objects\cube.w3d'));
  mdl[1].LoadTextures('textures');
  mdl[1].UseColors:=false;

  mdl[2]:=TGLModel.Create;
  mdl[2].LoadFromFile(GetPath('objects\object.w3d'));
  mdl[2].LoadTextures('textures');
  mdl[2].UseColors:=false;

  mdl[3]:=TGLModel.Create;
  mdl[3].LoadFromFile(GetPath('objects\cell.w3d'));
  mdl[3].LoadTextures('textures');

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

  // Set camera
  glLoadIdentity;
  glTranslatef(0, 0, -3); // Bit further behind
  glTranslatef(0, -0.5, 0); // bit upwards
  glMultMatrixf(@cam); // Move and rotate where ship is

  nx.GetMouseRay(nx.Width/2, nx.Height/2, @rayPos, @rayDir);

  // Render player ship
  glPushMatrix;
  with pl.position do glTranslatef(x, y, z);
  glMultMatrixf(@pl.rotation);
  mdl[0].Render;
  glPopMatrix;


  // Render floating objects
  for i:=0 to high(obj) do
    with obj[i] do begin
      glPushMatrix;

      // Test mouse-ray intersect with object
      if mdl[model].rayIntersect(rayPos, rayDir, false,
         position, rotation, nil, nil)>=0 then begin
        nx.SetColor(1, 0, 0, 0.8+0.2*sin(toRad*nx.GetTick2(360, 0.5)));
      end else
        nx.SetColor(color);

      with position do glTranslatef(x, y, z);
      glMultMatrixf(@rotation);
      mdl[model].Render;
      glPopMatrix;
    end;

  // Don't write in depth-buffer, but we can still use depth-test
  glDepthMask(false);
  // Render grid
  nx.rs.Push;
  nx.rs.AddBlend:=true;
  nx.rs.Lighting:=false;
  tex.Disable;
  nx.SetColor(0.4, 1, 0.4, 0.2);
  glBegin(GL_LINES);
  glVertex3f(0, 0, 0); glVertex3f(0, 40, 0);
  for i:=-40 to 40 do begin
    glVertex3f(i, 0, -40); glVertex3f(i, 0, 40);
    glVertex3f(-40, 0, i); glVertex3f(40, 0, i);
  end;
  glEnd;
  tex.Enable;

  d:=RayPlaneIntersect(rayPos, rayDir,
    vector(0,0,0), vector(0,1,0), @intersect);

  // Render cell focus
  if d>=0 then begin
    glPushMatrix;
    with intersect do glTranslatef(floor(x), 0, floor(z));
    mdl[3].Render;
    glPopMatrix;
  end;
  nx.rs.Pop;
  glDepthMask(true);

  nx.Flip;
end;

end.

