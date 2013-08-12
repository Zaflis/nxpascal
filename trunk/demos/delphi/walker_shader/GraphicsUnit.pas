unit GraphicsUnit;

interface

uses
  SysUtils, Dialogs, dglOpenGL, nxGL, math, nxMath, nxMath3D,
  nxTypes, GameUnit;

type

  { TGraphicalGame }

  TGraphicalGame = class(TGame)
  public
    renderer: TGLRenderer;
    constructor Create;
    procedure Draw; override;
    destructor Destroy; override;
  end;

implementation

{ TGame }

constructor TGraphicalGame.Create;
var n, i: integer;
begin
  inherited Create;
  // Load graphics
  nx.SetClearColor(0.1, 0.2, 0.4);
  nx.Clear(true, true);
  nx.Flip;

  nx.CreateFont('Courier', 8, 256);
  tex.Options:=[toFitScale];
  nx.rs.CullBack:=false;

  tex.AddTexture('tile', GetPath('textures\tile.png'));
  n:=tex.AddTexture('spark', GetPath('textures\spark.png'));
  pt.AddPType(n, 0.5, -1.0, 0.2);

  tex.TextureQuality:=GL_NEAREST;
  n:=tex.AddTexture('walker', GetPath('textures\walker.png'), true);
  tex.SetPattern(n, 17, 26, 1, 1);

  renderer:=TGLRenderer.Create(100, 10);
  renderer.AddProgram(GetPath('shaders\vertex.glsl'),
    GetPath('shaders\frag_red.glsl'));
  renderer.AddProgram(GetPath('shaders\vertex.glsl'),
    GetPath('shaders\frag_green.glsl'));
  renderer.AddProgram(GetPath('shaders\vertex.glsl'),
    GetPath('shaders\frag_blue.glsl'));
  renderer.AddProgram(GetPath('shaders\vertex_wave.glsl'),
    GetPath('shaders\frag.glsl'));
  for i:=2 to 5 do
    with renderer.shader.programs[i] do begin
      AddUniform('pmv');
      AddUniform('texture');
      AddUniform('diffuse');
      AddAttrib('in_position');
      AddAttrib('in_texCoord');
      AddAttrib('in_color');
      if i=5 then AddUniform('wave');
    end;

  // Last operations
  if nx.LastError<>'' then ShowMessage(nx.LastError);
  ResetTick; Initialized:=true;
end;

procedure TGraphicalGame.Draw;
var i, j: integer;
begin
  // Draw game tick
  nx.Clear(true, false);
  nx.Enable2D;

  glPushMatrix;
  i:=(nx.Width-nx.Height*4 div 3) div 2;
  if i>0 then glTranslatef(i, 0, 0);
  glScalef(nx.Height/300, nx.Height/300, 1);

  renderer.EnableProgram(5, false);
  with renderer.shader.programs[5] do begin
    i:=FindUniform('wave');
    if i>-1 then glUniform1f(uniform[i].index, nx.GetTick(360, 0.3)*toRad)
    else nxSetError('Cannot set wave uniform');
  end;

  renderer.Enable2D;
  renderer.SetTexture('tile');
  renderer.SetColor(0.5, 0.6, 0.7);
  // Draw tiles
  for j:=0 to 7 do
    for i:=-2 to 9 do
      renderer.Draw(i*55, j*38);

  // Draw walkers
  renderer.SetTexture('walker');
  renderer.SetColor(1, 1, 1);
  for i:=0 to high(walker) do
    with walker[i] do begin
      renderer.EnableProgram(renderProgram, false);
      if goLeft then
        renderer.DrawRotateS(position.x, position.y, trunc(anim), 0, 0.5, 20/27,
          -17, 26)
      else
        renderer.DrawRotateS(position.x, position.y, trunc(anim), 0, 0.5, 20/27,
          17, 26);
    end;

  renderer.Disable;

  // Draw particles
  nx.rs.Push;
  nx.rs.AddBlend:=true;
  pt.Draw2D;
  nx.rs.Pop;

  glPopMatrix;

  // Write some text
  nx.SetFont(0);
  renderer.Enable2D;
  renderer.SetColor(1, 1, 1);
  nx.Font[0].RDraw(renderer, 1, 1, 1, 1, format('FPS: %d', [nx.FPS]));
  nx.Font[0].RDraw(renderer, 1, 12, 1, 1, format('Walkers: %d', [length(walker)]));
  nx.Font[0].RDraw(renderer, 1, 23, 1, 1, format('Physics loops: %d', [physLoops]));
  if debugstr<>'' then
    nx.Font[0].RDraw(renderer, 1, 34, 1, 1, format('debug: %s', [debugstr]));
  if nx.LastError<>'' then
    nx.Font[0].RDraw(renderer, 1, 45, 1, 1, format('error: %s', [nx.LastError]));

  renderer.SetColor(1, 0.3, 0.3);
  nx.Font[0].RDrawC(renderer, nx.Width/2, 15, 1, 1, '- Press F1 to reset -');

  renderer.Disable;
  nx.Disable2D;
  nx.Flip;
end;

destructor TGraphicalGame.Destroy;
begin
  renderer.Free;
  inherited;
end;

end.

