unit ModelUnit;

{$mode objfpc}{$H+}

interface

uses SysUtils, dglOpenGL, nxModel, nxGL,
  {%H-}nxMath, nxMath3D, nxTypes;

const DefaultZoom = 4;

type
  { TEditModel }

  TEditModel = class(TPolyModel)
  public
    procedure DeleteFace(index: integer);
    procedure DeleteVertex(index: integer);
    function GetFaceMiddle(index: integer): TVector;
    procedure Optimize;
    function rayIntersect2(const rayStart, rayDir: TVector; findClosest: boolean;
      const intersect: PVector=nil; const normal: PVector=nil): integer; overload;
    function rayIntersect2(rayStart, rayDir: TVector; findClosest: boolean;
      const position: TVector; rotation: TMatrix;
      const intersect: PVector=nil; const normal: PVector=nil): integer; overload;
    function RayPolyIntersect2(const rayStart, rayDirection: TVector;
      const vi: PWordArray; Count: integer; BothSided: boolean;
      intersect: PVector; intersectNormal: PVector): Boolean;
  end;

  { TInstancedModel }

  TInstancedModel = class
  public
    obj: TEditModel;
    visible: boolean;
    position, radius: TVector;
    rotation: TMatrix3f;
    procedure GetSize;
    procedure SetMaterial(index: integer);
  end;

  { T3DScene }

  T3DScene = class
  public
    o: array of TInstancedModel;
    cam: TCamera;
    ax, ay, zoom, arrowAlpha: single;
    camPos: TVector;
    selObject: TEditModel;
    selIndex: integer;
    wireFrame: byte;
    showGrid, showNormals, bothSided: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Render;
    procedure RenderObjects;
    procedure Add(obj: TEditModel);
    procedure Clear;
    procedure Delete(objIndex: integer);
    procedure DeSelect(index: integer);
    procedure GetSelIndices;
    procedure GetVertIndices;
    procedure SelectObj(index: integer);
    procedure SetCamera(addVec: PVector);
    procedure TryHighSel(index: integer);
  end;

  TSceneMode = (smObject, smFace, smVertex, smBone, smAnim);
  TDragState = (dsNone, dsMove, dsMoveY, dsRotate, dsMoveCam,
    dsRotateCam, dsArea);

var
  mode: TSceneMode;
  ds: TDragState;
  selarray: array[0..high(word)] of byte;
  focus, selCount, highSel: integer;
  ray: TMouseRay;
  mp, mp2: TVector2f;
  selStart, selEnd, movePPos, camAdd,
    intersect, normVec: TVector;
  selIndices: array of word;
  movePRot: TMatrix3f;
  eobj: array[0..2] of TGLModel;
  DEBUGMODE: boolean = false;

implementation

{ T3DScene }

constructor T3DScene.Create;
begin
  cam:=TCamera.Create; ax:=-30; ay:=30; zoom:=DefaultZoom;
  showGrid:=true; selIndex:=-1;
end;

destructor T3DScene.Destroy;
begin
  Clear; cam.Free;
  inherited Destroy;
end;

procedure T3DScene.Render;
var i: integer; d: single; v: TVector;
begin
  if (ds=dsNone) and (arrowAlpha>0) then begin
    arrowAlpha:=arrowAlpha-(0.11-0.1*arrowAlpha);
    if arrowAlpha<0 then arrowAlpha:=0;
  end;

  if ds=dsMoveCam then SetCamera(@camAdd)
  else SetCamera(nil);

  nx.rs.Push;
  nx.rs.CullBack:=not bothSided;
  nx.rs.WireFrame:=wireframe=1;

  // Render models
  RenderObjects;
  if wireframe=2 then begin
    nx.rs.WireFrame:=true; RenderObjects;
  end;
  nx.rs.WireFrame:=false;
  nx.rs.Pop;

  // Grid
  if showGrid then begin
    tex.Disable; nx.rs.Push;
    nx.rs.Lighting:=false;
    glBegin(GL_LINES);
    for i:=-10 to 10 do begin
      if abs(i) mod 5=0 then nx.SetColor(0.5, 1, 0.5, 0.2)
      else nx.SetColor(0.5, 1, 0.5, 0.1);
      glVertex3f(i*0.2, 0, -2.2); glVertex3f(i*0.2, 0, 2.2);
      glVertex3f(-2.2, 0, i*0.2); glVertex3f(2.2, 0, i*0.2);
    end;
    glVertex3f(0, 0, 0); glVertex3f(0, 2.2, 0);
    glVertex3f(-0.2, 1, 0); glVertex3f(0.2, 1, 0);
    glVertex3f(-0.2, 2, 0); glVertex3f(0.2, 2, 0);
    glEnd;
    nx.rs.Pop; tex.Enable;
  end;

  // Selected/Focused objects
  if mode=smObject then begin
    for i:=0 to high(o) do
      if (selArray[i]>0) or (focus=i) then
        with o[i] do begin
          cam.Push;
          cam.Translate(position, false);
          cam.Multiply(rotation, true);
          eObj[1].ScaleTo(radius.x*2, radius.y*2, radius.z*2);
          if focus=i then d:=1.05+0.05*sin(nx.GetTick(360, 0.2)*toRad)
          else d:=1.02;
          case ds of
            dsMove, dsMoveY, dsRotate: nx.SetColor(1, 1, 1, 0.8);
            else nx.SetColor(0.5, 1, 0.5, 0.6);
          end;
          eobj[1].Scale(d, d, d);
          eObj[1].Render;

          // Intersection and normal
          if focus=i then begin
            tex.Disable;
            nx.SetColor(0, 1, 0);
            glPointSize(6);
            glBegin(GL_POINTS);
            glVertex3fv(@intersect);
            glEnd;
            glBegin(GL_LINES);
            with intersect do begin
              glVertex3f(x, y, z);
              glVertex3f(x+normVec.x*0.2, y+normVec.y*0.2, z+normVec.z*0.2);
            end;
            glEnd;
            tex.Enable;
          end;

          cam.Pop(false);
        end;
  end else if (mode=smVertex) and (selObject<>nil) then begin
    cam.Push; nx.rs.Push; tex.Disable;
    nx.rs.DepthTest:=false; nx.rs.Lighting:=false;
    //nx.rs.AddBlend:=true;
    glPointSize(4);
    with o[selIndex] do begin
      cam.Translate(position, false);
      cam.Multiply(rotation);
      glBegin(GL_POINTS);
      for i:=0 to selObject.vCount-1 do
        with selObject.va[i] do begin
          if focus=i then nx.SetColor(0, 1, 0)
          else if selArray[i]=255 then nx.SetColor(1, 1, 1)
          else if selArray[i]>0 then
            nx.SetColor(1, selArray[i]/255, selArray[i]/255)
          else nx.SetColor(0.2, 0.2, 1);
          glVertex3f(x, y, z);
        end;
      glEnd;
    end;
    nx.rs.Pop; cam.Pop(false); tex.Enable;
  end;
  cam.SetCamera;

  nx.Enable2D;
  nx.SetFont(0); nx.SetColor(1, 1, 1);
  //if length(o)>0 then
  //  nx.Font[0].Draw(1, 0, format('vCount: %d', [o[0].obj.groups]));
  //with camAdd do
  //  nx.Font[0].Draw(1, 15, format('%.2f %.2f %.2f', [x, y, z]));
  //with camPos do
  //  nx.Font[0].Draw(1, 30, format('%.1f %.1f %.1f', [x, y, z]));
  //with ray.dir do
  //  nx.Font[0].Draw(1, 45, format('%.3f %.3f %.3f', [x, y, z]));
  //nx.Font[0].Draw(1, 60, format(
  //  'x %f y %f', [mp.x, mp.y]));

  // Select area
  if ds=dsArea then begin
    nx.SetColor(0, 1, 0);
    tex.Disable;
    glBegin(GL_LINE_LOOP);
      glVertex2f(selStart.x, selStart.y);
      glVertex2f(selEnd.x, selStart.y);
      glVertex2f(selEnd.x, selEnd.y);
      glVertex2f(selStart.x, selEnd.y);
    glEnd;
    tex.Enable;
  end;

  nx.Disable2D;

  nx.Clear(false, true); // Clear depth-buffer

  // Axis's of moving plane
  //if ds in [dsMove, dsMoveY, dsRotate, dsMoveCam, dsRotateCam] then begin
  if arrowAlpha>0 then begin
    cam.Push;
    v:=movePPos+camAdd;
    with v do cam.Translate(x, y, z, false);
    cam.Multiply(movePRot);
    cam.Scale(0.4);
    nx.SetColor(0.8, 0.2, 0.2, arrowAlpha); eObj[0].Render; // X
    cam.Rotate(-90, 1, 0, 0);
    nx.SetColor(0.2, 0.8, 0.2, arrowAlpha); eObj[0].Render; // Y
    cam.Rotate(-90, 0, 1, 0);
    nx.SetColor(0.2, 0.2, 0.8, arrowAlpha); eObj[0].Render; // Z
    cam.Pop(false);
  end;

  // Axis-arrows
  cam.Push;
  nx.SetView(0, 0, 80, 80); nx.Perspective(true);
  cam.Reset(false);
  cam.Translate(0, 0, -1, false); cam.Rotate(ay, 1, 0, 0, false);
  cam.Rotate(ax+90, 0, 1, 0);
  nx.SetColor(0.8, 0.2, 0.2); eObj[0].Render; // X
  cam.Rotate(-90, 1, 0, 0);
  nx.SetColor(0.2, 0.8, 0.2); eObj[0].Render; // Y
  cam.Rotate(-90, 0, 1, 0);
  nx.SetColor(0.2, 0.2, 0.8); eObj[0].Render; // Z
  nx.SetView(0, 0, nx.window.Width, nx.window.Height);
  nx.Perspective(false);
  cam.Pop;

  if ds=dsMoveCam then SetCamera(nil);
end;

procedure T3DScene.RenderObjects;
var i, j, f, g, n, lastC, lastM: integer; IgnoreMat: boolean;
begin
  nx.rs.Push;
  nx.rs.DepthTest:=not nx.rs.WireFrame;
  nx.rs.Lighting:=not nx.rs.WireFrame;
  nx.SetColor(1, 1, 1);
  IgnoreMat:=nx.rs.WireFrame;
  if nx.rs.WireFrame then begin
    tex.Disable;
  end else begin
    tex.Enable;
  end;
  for i:=0 to high(o) do
    with o[i] do begin
      cam.Push;
      cam.Translate(position, false);
      cam.Multiply(rotation, true);

      lastC:=0;
      if obj.groups>0 then begin
        lastM:=obj.grp[0].matIndex;
        if not IgnoreMat then
          SetMaterial(lastM);
      end else
        lastM:=-1;
      for g:=0 to obj.groups-1 do
        with obj.grp[g] do begin
          if lastM<>obj.grp[g].matIndex then begin
            if lastC>0 then begin
              glEnd; lastC:=0;
            end;
            lastM:=obj.grp[g].matIndex;
            if not IgnoreMat then SetMaterial(lastM);
          end;

          for f:=first to integer(first)+count-1 do begin
            if obj.fa[f].count<>lastC then begin
              if lastC>0 then glEnd;
              lastC:=obj.fa[f].count;
              case obj.fa[f].count of
                0: ;
                1: glBegin(GL_POINTS);
                2: glBegin(GL_LINES);
                3: glBegin(GL_TRIANGLES);
                4: glBegin(GL_QUADS);
                else glBegin(GL_POLYGON);
              end;
            end;

            for j:=0 to integer(obj.fa[f].count)-1 do begin
              n:=obj.fa[f].index[j];
              glTexCoord2fv(@obj.ta[n]);
              glNormal3fv(@obj.na[n]);
              glVertex3fv(@obj.va[n]);
            end;

            if lastC>4 then begin
              glEnd; lastC:=0;
            end;
          end;
        end;
      if lastC>0 then glEnd;
      cam.Pop;
    end;
  nx.rs.Pop;
  tex.Enable;
end;

procedure T3DScene.Add(obj: TEditModel);
var n: integer;
begin
  if obj=nil then exit;
  n:=length(o); setlength(o, n+1);
  o[n]:=TInstancedModel.Create;
  o[n].obj:=obj;
  o[n].position:=nullVector;
  o[n].rotation:=newMatrix3f;
  o[n].GetSize;
end;

procedure T3DScene.Clear;
var i: integer;
begin
  for i:=0 to high(o) do o[i].Free;
  setlength(o, 0);
end;

procedure T3DScene.Delete(objIndex: integer);
var i: integer;
begin
  o[objIndex].Free;
  for i:=objIndex to high(o)-1 do o[i]:=o[i+1];
  setlength(o, high(o));
end;

procedure T3DScene.DeSelect(index: integer);
var i: integer;
begin
  if selArray[index]>0 then begin
    selArray[index]:=0; dec(selCount);
  end;
  if (mode=smObject) and (index=selIndex) then begin
    selIndex:=-1; selObject:=nil;
  end;
  if index>=highSel then
    for i:=index-1 downto 0 do begin
      dec(highSel);
      if selArray[i]>0 then exit;
    end;
end;

procedure T3DScene.GetSelIndices;
var i, n: integer;
begin
  setlength(selIndices, selCount);
  n:=0;
  for i:=0 to highsel do
    if selarray[i]>0 then begin
      selIndices[n]:=i; inc(n);
    end;
end;

procedure T3DScene.GetVertIndices;
var i, j, n, vc: integer;
    va2: array of byte;
begin
  if selObject=nil then exit;
  vc:=0;
  setlength(va2, selObject.vCount);
  fillchar(va2[0], selObject.vCount, 0);
  for i:=0 to highsel do
    if selarray[i]>0 then
      for j:=0 to selObject.fa[i].count-1 do begin
        n:=selObject.fa[i].index[j];
        if va2[n]=0 then begin
          va2[n]:=1; inc(vc);
        end;
      end;
  setlength(selIndices, vc); n:=0;
  for i:=0 to selObject.vCount-1 do
    if va2[i]>0 then begin
      selIndices[n]:=i; inc(n);
    end;
end;

procedure T3DScene.SelectObj(index: integer);
begin
  if index<0 then exit;
  selIndex:=index;
  if index<0 then selObject:=nil
  else selObject:=o[index].obj;
end;

procedure T3DScene.SetCamera(addVec: PVector);
var v: TVector;
begin
  cam.Reset(false);
  cam.Translate(0, 0, -zoom, false);
  cam.Rotate(ay, 1, 0, 0, false);
  cam.Rotate(ax, 0, 1, 0, false);
  if addVec<>nil then v:=camPos+addVec^
  else v:=camPos;
  with v do cam.Translate(-x, -y, -z);
  {if (mode<>smObject) and (selObject<>nil) then begin
    with o[selIndex] do begin
      cam.Multiply(invert(matrix(rotation)));
      with position do cam.Translate(-x, -y, -z);
    end;
  end;}
end;

procedure T3DScene.TryHighSel(index: integer);
begin
  if index>highSel then highSel:=index;
end;

{ TInstancedModel }

procedure TInstancedModel.GetSize;
begin
  radius:=vector(obj.GetRadiusX(), obj.GetRadiusY(), obj.GetRadiusZ());
end;

procedure TInstancedModel.SetMaterial(index: integer);
begin
  if (obj.mCount>0) and (index>=0) then
    with obj.mat[index] do begin
      tex.SetTex(texIndex);
      nx.SetSpecular(specular>0.01, specular,specular,specular, shininess);
      nx.rs.AddBlend:=addMode;
      if obj.UseColors then glColor4ubv(@color);
    end;
end;

{ TEditModel }

procedure TEditModel.DeleteFace(index: integer);
//var i: integer;
begin
  //

end;

procedure TEditModel.DeleteVertex(index: integer);
//var i: integer;
begin
  //

end;

function TEditModel.GetFaceMiddle(index: integer): TVector;
var i: integer;
begin
  result:=nullVector;
  for i:=0 to fa[index].count-1 do
    result:=result+va[fa[index].index[i]];
  result:=result/fa[index].count;
end;

procedure TEditModel.Optimize;
var i, j, g: integer;
begin
  // Sort groups by material
  if groups>1 then
    for i:=0 to groups-2 do
      for j:=i+1 to groups-1 do
        if grp[i].matIndex>grp[j].matIndex then
          switchvar(@grp[i], @grp[j], sizeof(TFaceGroup));

  // Sort faces by vertex count
  for g:=0 to groups-1 do
    with grp[g] do begin
      if count>1 then
        for i:=first to integer(first)+count-2 do
          for j:=i+1 to integer(first)+count-1 do
            if fa[i].count>fa[j].count then
              switchvar(@fa[i], @fa[j], sizeof(TPolyFaceIndices));
    end;
end;

function TEditModel.rayIntersect2(const rayStart, rayDir: TVector; findClosest: boolean; const intersect: PVector; const normal: PVector): integer;
var g, i: integer; d: single; nearest: single;
    vI,vN: TVector;
begin
  result:=-1; nearest:=-2;
  for g:=0 to groups-1 do
    if grp[g].visible then with grp[g] do
      for i:=first to integer(first)+Count-1 do
        if RayPolyIntersect2(rayStart, RayDir,
           @fa[i].index[0], fa[i].count,
           false, intersect, normal) then begin
          if (not findClosest) or (intersect=nil) then begin
            result:=i; exit;
          end else begin
            d:=VectorDist(rayStart, intersect^);
            if (d<nearest) or (nearest<-1) then begin
              nearest:=d; vI:=intersect^;
              if normal<>nil then vN:=normal^;
              result:=i;
            end;
          end;
        end;
  if nearest>-1 then begin
    intersect^:=vI;
    if normal<>nil then normal^:=vN;
  end;
end;

function TEditModel.rayIntersect2(rayStart, rayDir: TVector;
  findClosest: boolean; const position: TVector; rotation: TMatrix;
  const intersect: PVector; const normal: PVector): integer;
begin
  // Correct ray
  rayDir:=Multiply(rayDir, Invert(rotation));
  // Correct position
  SetVector(rotation, position, 3);
  rotation:=invert(rotation);
  rayStart:=Multiply(rayStart, rotation);
  result:=rayIntersect2(rayStart, rayDir, findClosest, intersect,
    normal);
end;

function TEditModel.RayPolyIntersect2(const rayStart, rayDirection: TVector;
  const vi: PWordArray; Count: integer; BothSided: boolean;
  intersect: PVector; intersectNormal: PVector): Boolean;
var i: integer;
begin
  result:=false;
  for i:=0 to Count-3 do begin
    result:=nxMath3D.RayTriangleIntersect(rayStart, rayDirection,
      va[vi^[0]], va[vi^[i+1]], va[vi^[i+2]],
      BothSided, intersect, intersectNormal);
    if result then exit;
  end;
end;

end.

