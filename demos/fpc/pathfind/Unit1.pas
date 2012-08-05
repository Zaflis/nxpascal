unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, dglOpenGL, nxGL, nxTypes, nxPath, Types;

type
  TMover = record
    p: TPoint;
    path: TPath;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    cMap: PathMap;
    pathfinder: TTilePath;
    mover: array[0..19] of TMover;
    movers,W,H,mx,my: integer;
    back: TDisplayList;
    XS,YS: single;
    doUpdate: boolean;
  public
    procedure UpdatePath;
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.UpdatePath;
var i: integer;
begin
  for i:=0 to movers-1 do
    //pathfinder.GetPath(mover[i].p, point(mx,my), mover[i].path);
    pathfinder.GetBetterPath(mover[i].p, point(mx,my), mover[i].path);

  doUpdate:=false;
end;

procedure TForm1.FormCreate(Sender: TObject);
var b: TBitmap; i,j: integer; err: GLenum;
begin
  ClientWidth:=800; ClientHeight:=600;
  if nx.CreateGlWindow(self) then begin
    // Load textures
    tex.AddTexture('tile','tex.png',true);
    tex.SetPattern(tex.IndexOf('tile'),32,32,0,0);

    tex.Options:=tex.Options+[toAlphaColor];
    tex.AddTexture('glow','glow.png',true);

    // Initialize path finder
    pathfinder:=TTilePath.Create;
    b:=TBitmap.Create;
    b.LoadFromFile('maze.bmp');
    W:=b.Width;
    H:=b.Height;
    pathfinder.SetMap(cMap, W, H);
    for j:=0 to H-1 do
      for i:=0 to W-1 do begin
        if b.Canvas.Colors[i,j].Blue > 30000 then
          pathfinder.SetCell(i, j, 0) // floor
        else if b.Canvas.Colors[i, j].Red > 30000 then begin
          // Add mover
          pathfinder.SetCell(i, j, 0); // floor
          inc(movers);
          with mover[movers-1] do begin
            p.x:=i;
            p.y:=j;
          end;
        end else
          pathfinder.SetCell(i, j, 1); // wall
      end;
    b.Free;

    // Create displaylist for tiles
    back:=TDisplayList.Create(true);
    tex.SetByName('tile',true);
    nx.SetColor(1,1,1); // Set white
    for j:=0 to H-1 do
      for i:=0 to W-1 do
        nx.Draw(i*32,j*32,cMap[i+j*W]);
    back.EndList;

    err:=glGetError();
    if err>0 then ShowMessage(format('glGetError, code: %d',[err]));

    if nx.LastError<>'' then showmessage(nx.LastError)
    else begin
      doUpdate:=true;
      timer1.Enabled:=true;
    end;
  end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  mx:=(x*W) div nx.Width;
  my:=(y*H) div nx.Height;
  doUpdate:=true;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var i,j,t: integer; c: single;
begin
  if not nx.AllOK then exit;

  // Mouse has moved - refresh paths
  if doUpdate then UpdatePath;

  t:=nx.GetTick mod 1000;

  nx.Clear(true,false);
  nx.Enable2D;

  XS:=nx.Width/(W*32);
  YS:=nx.Height/(H*32);
  glScalef(XS,YS,1);

  back.Draw; // Draw tiles

  tex.SetTex(1); // Glow texture
  glScalef(32,32,1);
  nx.rs.Push; // Save render settings
  nx.rs.AddBlend:=true; // Additive blend
  for j:=0 to movers-1 do
    with mover[j].path do
      if nodes>0 then begin
        glColor3f(0.3,0.3,1);
        with mover[j].p do nx.RectT(x-2,y-2,x+3,y+3);
        for i:=0 to nodes-1 do begin
          c:=1-(t+(nodes-i)*50) mod 1000/500;
          if c<0 then c:=0;
          if j mod 3=0 then nx.SetColor(c,0,0)
          else if j mod 3=1 then nx.SetColor(0,c,0)
          else if j mod 3=2 then nx.SetColor(0,c,c);
          with node[i] do nx.RectT(x-1,y-1,x+2,y+2);
        end;
        nx.SetColor(0.5,0.5,0.5);
        if nodes>5 then
          with node[nodes-1] do nx.RectT(x-1,y-1,x+2,y+2);
      end;
  nx.rs.Pop; // Recall previous render settings

  nx.SetColor(0,1,0); // Set green
  nx.RectT(mx-1,my-1,mx+2,my+2); // Show cursor

  nx.Disable2D;
  nx.Flip;

  // Show FPS
  caption:='Path finding demo - FPS: '+inttostr(nx.FPS);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  back.Free;
  // Pathfinder class allows use of several custom maps
  // therefore they have to be freed manually.
  // Consider unit specific collision map, or layers.
  pathfinder.FreeMap(cMap);
  pathfinder.Free;
end;

end.

