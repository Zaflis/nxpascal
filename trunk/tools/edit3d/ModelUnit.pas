unit ModelUnit;

{$mode objfpc}{$H+}

interface

uses SysUtils, nxModel, nxGL, {%H-}nxMath, nxMath3D, nxTypes,
  dglOpenGL;

type
  TSceneMode = (smObject, smFace, smVertex, smBone, smAnim);

  { TEditModel }

  TEditModel = class(TPolyModel)
  public
    procedure Optimize;
  end;

  { TInstancedModel }

  TInstancedModel = class
  public
    obj: TEditModel;
    visible: boolean;
    position: TVector;
    rotation: TMatrix3f;
    procedure SetMaterial(index: integer);
  end;

  { T3DScene }

  T3DScene = class
  public
    o: array of TInstancedModel;
    cam: TCamera;
    ax, ay, zoom: single;
    cp: TVector;
    constructor Create;
    destructor Destroy; override;
    procedure Render;
    procedure Add(obj: TEditModel);
    procedure Clear;
    procedure Delete(objIndex: integer);
  end;

implementation

{ T3DScene }

constructor T3DScene.Create;
begin
  cam:=TCamera.Create;
  cam.Translate(0, 0, -5);
  ax:=-30; ay:=30;
end;

destructor T3DScene.Destroy;
begin
  Clear; cam.Free;
  inherited Destroy;
end;

procedure T3DScene.Render;
var i, j, g, f, n, lastM: integer; lastC: word;
begin
  cam.Reset(false);
  cam.Translate(0, 0, -5);
  cam.Rotate(ay, 1, 0, 0);
  cam.Rotate(ax, 0, 1, 0);

  for i:=0 to high(o) do
    with o[i] do begin
      cam.Push;
      cam.Translate(position, false);
      cam.Multiply(rotation, true);
      //cam.Rotate(nx.GetTick(360, 0.0511), 0, 1, 0, false);
      //cam.Rotate(nx.GetTick(360, 0.02877), 1, 0, 0, true);

      lastC:=0;
      if obj.groups>0 then begin
        lastM:=obj.grp[0].matIndex; SetMaterial(lastM);
      end else
        lastM:=-1;
      for g:=0 to obj.groups-1 do
        with obj.grp[g] do begin
          if lastM<>obj.grp[g].matIndex then begin
            if lastC>0 then begin
              glEnd; lastC:=0;
            end;
            lastM:=obj.grp[g].matIndex; SetMaterial(lastM);
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

  nx.Enable2D;
  nx.SetFont(0); nx.SetColor(1, 1, 1);
  //if length(o)>0 then
  //  nx.Font[0].Draw(1, 1, format('vCount: %d', [o[0].obj.groups]));
  nx.Disable2D;
end;

procedure T3DScene.Add(obj: TEditModel);
var n: integer;
begin
  n:=length(o); setlength(o, n+1);
  o[n]:=TInstancedModel.Create;
  o[n].obj:=obj;
  o[n].position:=nullVector;
  o[n].rotation:=newMatrix3f;
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

{ TInstancedModel }

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

end.

