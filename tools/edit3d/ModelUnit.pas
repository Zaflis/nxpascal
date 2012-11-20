unit ModelUnit;

{$mode objfpc}{$H+}

interface

uses
  nxModel, nxGL, nxMath, nxMath3D, nxTypes;

type
  TEditModel = class(TPolyModel)
  public

  end;

  TInstancedModel = class
  public
    obj: TEditModel;
    visible: boolean;
    position: TVector;
    rotation: TMatrix3f;
  end;

  { T3DScene }

  T3DScene = class
  public
    o: array of TInstancedModel;
    destructor Destroy; override;
    procedure Add(obj: TEditModel);
    procedure Clear;
    procedure Render;
  end;

implementation

{ T3DScene }

destructor T3DScene.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure T3DScene.Add(obj: TEditModel);
var n: integer;
begin
  n:=length(o);
  setlength(o, n+1);
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

procedure T3DScene.Render;
begin

end;

end.

