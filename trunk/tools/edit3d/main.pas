unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, LCLType, math,
  dglOpenGL, nxGL, nxTypes, ModelUnit, nxMath, nxMath3D, types;

type

  { TForm1 }

  TForm1 = class(TForm)
    AppProperties: TApplicationProperties;
    btnAddObject: TButton;
    btnCreateObj: TButton;
    Button1: TButton;
    mnuResetRotation: TMenuItem;
    val1: TLabeledEdit;
    mnuSaveObjAs: TMenuItem;
    MenuItem11: TMenuItem;
    mnuAddToScene: TMenuItem;
    mnuDeleteObj: TMenuItem;
    objlistPopup: TPopupMenu;
    Panel4: TPanel;
    sbar: TPanel;
    saveD: TSaveDialog;
    selMode: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    objlist: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnuExit: TMenuItem;
    mnuSaveObject: TMenuItem;
    mnuSaveWorkspaceAs: TMenuItem;
    MenuItem13: TMenuItem;
    mnuUndo: TMenuItem;
    MenuItem15: TMenuItem;
    mnuAddObject: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    mnuSmoothen: TMenuItem;
    MenuItem2: TMenuItem;
    mnuListExt: TMenuItem;
    mnuMemory: TMenuItem;
    MenuItem22: TMenuItem;
    mnuCopy: TMenuItem;
    mnuPaste: TMenuItem;
    mnuDelete: TMenuItem;
    MenuItem26: TMenuItem;
    mnuSelAll: TMenuItem;
    mnuCamTop: TMenuItem;
    mnuCamFront: TMenuItem;
    MenuItem3: TMenuItem;
    mnuCamRight: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    mnuScale: TMenuItem;
    MenuItem36: TMenuItem;
    mnuLoadImage: TMenuItem;
    mnuWire: TMenuItem;
    mnuGrid: TMenuItem;
    MenuItem4: TMenuItem;
    mnuBothSided: TMenuItem;
    mnuNormals: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    mnuFlip: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    mnuCirculate: TMenuItem;
    mnuNoise: TMenuItem;
    mnuSpiral: TMenuItem;
    MenuItem5: TMenuItem;
    mnuMirrorX: TMenuItem;
    mnuPolyToTri: TMenuItem;
    mnuSplit4: TMenuItem;
    mnuSplit3: TMenuItem;
    mnuMirrorY: TMenuItem;
    mnuMirrorZ: TMenuItem;
    mnuFlattenNorm: TMenuItem;
    mnuSmoothNorm: TMenuItem;
    mnuSnapVert: TMenuItem;
    MenuItem6: TMenuItem;
    mnuSaveWorkspace: TMenuItem;
    mnuHelp: TMenuItem;
    mnuNewWorkspace: TMenuItem;
    mnuOpenWorkspace: TMenuItem;
    openD: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    createType: TComboBox;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Timer1: TTimer;
    val2: TLabeledEdit;
    procedure btnAddObjectClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormMouseDown(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FormMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FormMouseWheel(Sender: TObject; {%H-}Shift: TShiftState; WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure FormPaint(Sender: TObject);
    procedure mnuResetRotationClick(Sender: TObject);
    procedure mnuBothSidedClick(Sender: TObject);
    procedure mnuDeleteClick(Sender: TObject);
    procedure mnuGridClick(Sender: TObject);
    procedure mnuNormalsClick(Sender: TObject);
    procedure mnuUndoClick(Sender: TObject);
    procedure mnuCamTopClick(Sender: TObject);
    procedure mnuCamFrontClick(Sender: TObject);
    procedure mnuCamRightClick(Sender: TObject);
    procedure mnuOpenWorkspaceClick(Sender: TObject);
    procedure mnuPasteClick(Sender: TObject);
    procedure mnuSaveObjAsClick(Sender: TObject);
    procedure mnuDeleteObjClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuListExtClick(Sender: TObject);
    procedure mnuMemoryClick(Sender: TObject);
    procedure mnuAddObjectClick(Sender: TObject);
    procedure mnuNewWorkspaceClick(Sender: TObject);
    procedure mnuSaveWorkspaceAsClick(Sender: TObject);
    procedure mnuSaveWorkspaceClick(Sender: TObject);
    procedure mnuWireClick(Sender: TObject);
    procedure selModeChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    obj: array of TEditModel;
    modified: boolean;
    scene: T3DScene;
    mb: byte;
    wsFilename: string;
    function AddFile(filename: string): TEditModel;
    procedure AddSelect(index: integer; value: byte = 255);
    procedure ClearSelection;
    procedure FreeObjects;
    procedure LoadGLData;
    procedure PrepareMove;
    procedure ResetMode;
    procedure SetStatus(statusText: string);
    procedure SetPlaneXZ(p: TVector);
    procedure SetPlaneY(p: TVector);
    procedure SetPlaneFree(p: TVector);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
//var m, mi, m1: TMatrix3f;
begin
  if paramstr(1)='-debug' then DEBUGMODE:=true;

  {m[0,0]:=1;
  m[1,0]:=3;
  m[2,0]:=3;
  m[0,1]:=1;
  m[1,1]:=4;
  m[2,1]:=3;
  m[0,2]:=1;
  m[1,2]:=3;
  m[2,2]:=4;
  mi:=invert(m);
  m1:=multiply(mi, m);
  halt; }

  width:=screen.Width*3 div 4;
  height:=screen.Height*3 div 4;
  if not nx.CreateGlWindow(self) then begin
    showmessage('Cannot initialize OpenGL'); exit;
  end;
  scene:=T3DScene.Create;
  nx.DefaultLights;
  nx.rs.DepthTest:=true;
  nx.rs.CullBack:=true;
  mode:=smObject; highsel:=-1; focus:=-1;
  SetPlaneXZ(nullVector);
  if nx.LastError<>'' then begin
    showmessage(nx.LastError); nx.ClearError;
  end;
end;

procedure TForm1.LoadGLData;
begin
  nx.CreateBasicFont;
  eObj[0]:=TGLModel.Create('data\Arrow.w3d');
  eObj[0].ScaleTo(0.1);
  eObj[0].UseColors:=false;
  eObj[1]:=TGLModel.Create('data\Select.w3d');
  eObj[1].UseColors:=false;
  eObj[2]:=TGLModel.Create('data\Bone.w3d');
  SetStatus('Done');
end;

procedure TForm1.ResetMode;
begin
  selMode.ItemIndex:=0; mode:=smObject; modified:=false;
  ClearSelection; focus:=-1; highsel:=-1;
  ds:=dsNone; mb:=0; scene.arrowAlpha:=0;
  SetPlaneXZ(nullVector);
  mnuCamTopClick(nil);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var s: string;
begin
  if nx.LastError<>'' then begin
    s:=nx.LastError; nx.ClearError;
    showmessage(s); exit;
  end;

  nx.Clear(true, true);
  scene.Render;
  nx.Flip;
end;

procedure TForm1.FormPaint(Sender: TObject);
var s: string;
begin
  if (tag=0) and nx.AllOK then begin
    OnPaint:=nil; tag:=1;
    LoadGLData;

    if DEBUGMODE and (timer1.Tag=0) then begin
      timer1.Tag:=1;
      scene.Add(AddFile('objects\ship.w3d'));
      //AddFile('objects\extrude.w3d');
      AddFile('objects\test.w3d');
      //btnAddObjectClick(nil);
      modified:=false;
    end;

    timer1.Enabled:=true;
  end else if (tag=0) and (nx.LastError<>'') then begin
    s:=nx.LastError; nx.ClearError;
    showmessage(s);
  end;
end;

procedure TForm1.mnuResetRotationClick(Sender: TObject);
var i: integer;
begin
  if mode=smObject then
    for i:=0 to highSel do
      if selArray[i]>0 then
        with scene.o[i] do rotation:=NewMatrix3f;
end;

procedure TForm1.mnuBothSidedClick(Sender: TObject);
begin
  scene.bothSided:=mnuBothSided.Checked;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Timer1.Enabled then exit;
  if mb>0 then exit;
  mp:=vector2f(x, y);
  if button=mbLeft then begin
    mb:=1;
    if focus<0 then begin
      if ssCtrl in Shift then ds:=dsRotateCam
      else begin
        if not (ssShift in Shift) then ClearSelection;
        ds:=dsArea;
      end;
    end else begin
      if (not (ssShift in Shift)) and (selCount=1) then
        ClearSelection;
      AddSelect(focus);
      if ssCtrl in Shift then begin
        ds:=dsRotate;
      end else begin
        if ssShift in Shift then begin
          ds:=dsMoveY;
        end else begin
          ds:=dsMove;
        end;
      end;
    end;
  end else begin
    mb:=2;
    if ssCtrl in Shift then ds:=dsRotateCam
    else begin
      ds:=dsMoveCam;
    end;
  end;
  PrepareMove;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var dx, dy, d, nearest: single; i, n: integer; dV, v1: TVector;
    m, m2: TMatrix; oldmp: TVector2f; view: TGLViewDetails;
    p: TVector3d;
begin
  if not Timer1.Enabled then exit;
  if (not (ssRight in Shift)) and (mb=2) then begin
    FormMouseUp(nil, mbRight, Shift, round(mp.x), round(mp.y));
  end;

  {if (ds=dsMoveCam) and (ssCtrl in Shift) then begin
    scene.camPos:=scene.camPos+camAdd;
    ds:=dsRotateCam; PrepareMove;
  end else if (ds=dsRotateCam) and (not (ssCtrl in Shift)) then begin
    ds:=dsMoveCam; PrepareMove;
  end;}

  oldmp:=mp;
  dx:=x-oldmp.x; dy:=y-oldmp.y;
  mp:=vector2f(x, y);
  mp2:=vector2f(x, nx.Height-1-y);
  nx.GetMouseRay(x, y, @ray);

  if ds in [dsMove, dsMoveY, dsMoveCam] then begin
    nxMath3D.RayPlaneIntersect(ray.start, ray.dir, movePPos,
      GetVector(movePRot, 1), @selEnd);
    dv:=selEnd-selStart;
    if ds=dsMoveY then dv:=vector(0, dv.y, 0);
  end;
  case ds of
    dsNone: if mb=0 then begin
      focus:=-1; nearest:=9999;
      case mode of
        smObject: begin
          for i:=0 to high(scene.o) do begin
            //if scene.o[i].obj.rayIntersect2(ray.start, ray.dir,
            if scene.o[i].obj.rayIntersect(ray.start, ray.dir,
               true, scene.o[i].position, Matrix(scene.o[i].rotation),
               @v1, @normVec)>=0 then begin
              d:=VectorDist(v1, ray.start);
              if d<nearest then begin
                focus:=i;
                nearest:=d; intersect:=v1;
              end;
            end;
          end;

        end;
        smFace: if scene.selObject<>nil then begin
          focus:=scene.selObject.rayIntersect(ray.start, ray.dir,
            true, @intersect, @normVec);
        end;
        smVertex: if scene.selObject<>nil then begin
          nearest:=12;
          scene.cam.Push;
          with scene.o[scene.selIndex] do begin
            scene.cam.Translate(position, false);
            scene.cam.Multiply(rotation, true);
          end;
          nx.GetViewDetails(@view);
          for i:=0 to scene.selObject.vCount-1 do
            with scene.selObject.va[i] do begin
              gluProject(x, y, z,
                view.modelM, view.projM, view.viewport, @p.x, @p.y, @p.z);
              d:=hypot(p.x-mp2.x, p.y-mp2.y);
              if d<nearest then begin
                focus:=i; nearest:=d;
              end;
            end;
          scene.cam.Pop;
        end;
        //smBone:
        //smAnim:
      end;
    end; // end dsNone
    dsMove, dsMoveY: begin
      n:=selCount;
      for i:=0 to highSel do begin
        if selArray[i]>0 then begin
          case mode of
            smObject: begin
              scene.o[i].position:=scene.o[i].position+dV;
            end;
            smVertex: begin
              scene.selObject.va[i]:=scene.selObject.va[i]+dV;
            end;
            //smFace:
            //smBone:
            //smAnim:
          end;
          dec(n);
          if n<1 then break;
        end;
      end;
      selStart:=selEnd;
    end;
    dsRotate: begin
      n:=selCount;
      for i:=0 to highSel do begin
        if selArray[i]>0 then begin
          case mode of
            smObject: begin
              m:=Matrix(scene.o[i].rotation);
              m2:=CreateMatrix(vector(0, 1, 0), dx*toRad);

              m:=Multiply(m, m2);
              //v1:=scene.cam.GetVector(1);
              d:=scene.ax*toRad;
              v1:=vector(cos(d), 0, sin(d));
              m2:=CreateMatrix(v1, dy*toRad);
              m:=Multiply(m, m2);

              scene.o[i].rotation:=GetRotation3(m);
            end;
            smVertex: begin
              scene.selObject.va[i]:=scene.selObject.va[i]+dV;
            end;
            //smFace:
            //smBone:
            //smAnim:
          end;
          dec(n);
          if n<1 then break;
        end;
      end;
      selStart:=selEnd;
    end; // end dsRotate
    dsMoveCam: begin
      camAdd:=camAdd-dV;
      selStart:=selEnd;
    end;
    dsRotateCam: with scene do begin
      ax:=ax+dx;
      ay:=ay+dy;
      if ax<0 then ax:=ax+360
      else if ax>=360 then ax:=ax-360;
      if ay>90 then ay:=90
      else if ay<-90 then ay:=-90;
    end;
    dsArea: begin
      selEnd.x:=x; selEnd.y:=y;
    end;
  end;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer; p: TVector3d; r: TRectf;
    view: TGLViewDetails;
begin
  if not Timer1.Enabled then exit;
  case ds of
    dsArea: begin
      scene.SetCamera(nil);
      r.x1:=selStart.x; r.y1:=nx.Height-1-selStart.y;
      r.x2:=selEnd.x; r.y2:=nx.Height-1-selEnd.y;
      scene.cam.Push;
      case mode of
        smObject: begin
          nx.GetViewDetails(@view);
          for i:=0 to high(scene.o) do
            with scene.o[i] do begin
              gluProject(position.x, position.y, position.z,
                view.modelM, view.projM, view.viewport, @p.x, @p.y, @p.z);
              //p.y:=nx.Height-1-p.y;
              if PointInRect(p.x, p.y, r) then begin
                AddSelect(i);
              end;
            end;
        end;
        smVertex: if scene.selObject<>nil then begin
          with scene.o[scene.selIndex] do begin
            scene.cam.Translate(position, false);
            scene.cam.Multiply(rotation, true);
          end;
          nx.GetViewDetails(@view);
          for i:=0 to scene.selObject.vCount-1 do
            with scene.selObject.va[i] do begin
              gluProject(x, y, z,
                view.modelM, view.projM, view.viewport, @p.x, @p.y, @p.z);
              //p.y:=nx.Height-1-p.y;
              if PointInRect(p.x, p.y, r) then begin
                AddSelect(i);
              end;
            end;
        end;
        //smFace:
        //smBone:
        //smAnim:
      end;
      scene.cam.Pop;
    end;
    dsMoveCam: begin
      scene.camPos:=scene.camPos+camAdd;
      camAdd:=nullVector;
      SetPlaneXZ(nullVector); scene.arrowAlpha:=0;
    end;
    dsMove, dsRotate: begin
      scene.arrowAlpha:=0;
      if (mode in [smVertex, smFace]) and (scene.selIndex>=0) then begin
        scene.o[scene.selIndex].GetSize;
      end;
    end;
  end;
  mb:=0; ds:=dsNone;
  FormMouseMove(nil, Shift, x, y);
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ds=dsNone then begin
    scene.zoom:=scene.zoom-(WheelDelta div abs(WheelDelta))*0.5;
    if scene.zoom<0 then scene.zoom:=0
    else if scene.zoom>20 then scene.zoom:=20;
    scene.arrowAlpha:=1;
    if ds=dsNone then SetPlaneXZ(nullVector);
  end;
end;

procedure TForm1.mnuDeleteClick(Sender: TObject);
var i, n: integer;
begin
  if selCount>0 then begin
    n:=selCount;
    for i:=highSel downto 0 do
      if selArray[i]>0 then begin
        case mode of
          smObject: begin
            scene.Delete(i);
          end;
          smFace: begin
            scene.selObject.DeleteFace(i);
          end;
          smVertex: begin
            scene.selObject.DeleteVertex(i);
          end;
          smBone: begin

          end;
          smAnim: begin

          end;
        end;
        dec(n);
        if n<1 then break;
      end;
    ClearSelection; focus:=-1; mb:=0; ds:=dsNone;
  end;
end;

procedure TForm1.mnuGridClick(Sender: TObject);
begin
  scene.showGrid:=mnuGrid.Checked;
end;

procedure TForm1.mnuNormalsClick(Sender: TObject);
begin
  scene.showNormals:=mnuNormals.Checked;
end;

procedure TForm1.mnuUndoClick(Sender: TObject);
begin


  modified:=true;
end;

function TForm1.AddFile(filename: string): TEditModel;
var n: integer;
begin
  result:=nil;
  if not fileexists(filename) then begin
    showmessage(filename+' doesn''t exist.'); exit;
  end;
  SetStatus('Loading '+filename+'...');
  n:=length(obj);
  setlength(obj, n+1);
  obj[n]:=TEditModel.Create;
  obj[n].LoadFromFile(filename);
  result:=obj[n];
  objList.Items.Add(extractfilename(filename));
  objList.ItemIndex:=objList.Items.Count-1;
  modified:=true;

  SetStatus('Loading textures...');
  nxLoadModelTextures(obj[n], 'textures');

  SetStatus('Done');
end;

procedure TForm1.AddSelect(index: integer; value: byte);
begin
  if index<0 then exit;
  if selArray[index]=0 then inc(selCount);
  selArray[index]:=value;
  scene.TryHighSel(index);
  if mode=smObject then scene.SelectObj(index);
end;

procedure TForm1.ClearSelection;
var i: integer;
begin
  if mode=smObject then begin
    scene.selIndex:=-1; scene.selObject:=nil;
  end;
  for i:=0 to highSel do
    if selArray[i]>0 then selArray[i]:=0;
  highSel:=-1; selCount:=0;
end;

procedure TForm1.mnuCamTopClick(Sender: TObject);
begin
  scene.ax:=0; scene.ay:=90; scene.zoom:=DefaultZoom;
  scene.camPos:=nullVector;
end;

procedure TForm1.mnuCamFrontClick(Sender: TObject);
begin
  scene.ax:=0; scene.ay:=0; scene.zoom:=DefaultZoom;
  scene.camPos:=nullVector;
end;

procedure TForm1.mnuCamRightClick(Sender: TObject);
begin
  scene.ax:=270; scene.ay:=0; scene.zoom:=DefaultZoom;
  scene.camPos:=nullVector;
end;

procedure TForm1.mnuOpenWorkspaceClick(Sender: TObject);
begin
  openD.Filter:='Workspaces (NXW, W3D)|*.nxw;*.w3d|All files|*.*';
  if openD.Execute then begin
    wsFilename:=openD.FileName;

    ResetMode;
  end;
end;

procedure TForm1.mnuPasteClick(Sender: TObject);
begin


  modified:=true;
end;

procedure TForm1.mnuSaveObjAsClick(Sender: TObject);
begin
  if (objlist.Items.Count>0) and (objlist.ItemIndex>=0) then begin
    saveD.DefaultExt:='.nxw';
    saveD.Filter:='NXW|*.nxw|OBJ|*.obj|W3D|*.w3d|Milkshape ascii|*.ms3d|All files|*.*';
    if saveD.Execute then begin
      obj[objlist.ItemIndex].SaveToFile(saveD.FileName);
    end;
  end;
end;

procedure TForm1.mnuDeleteObjClick(Sender: TObject);
var i, n: integer;
begin
  if (objlist.Items.Count>0) and (objlist.ItemIndex>=0) then begin
    n:=objlist.ItemIndex;
    objlist.Items.Delete(n);
    for i:=high(scene.o) downto 0 do
      if scene.o[i].obj=obj[n] then scene.Delete(i);
    obj[n].Free;
    for i:=n to high(obj)-1 do obj[i]:=obj[i+1];
    setlength(obj, high(obj));
  end;
end;

procedure TForm1.SetStatus(statusText: string);
begin
  sbar.Caption:=statusText;
  application.ProcessMessages;
end;

procedure TForm1.PrepareMove;
var p: TVector;
begin
  p:=nullVector; camAdd:=nullVector; scene.arrowAlpha:=1;
  if ds in [dsMoveCam, dsRotateCam] then begin
    p:=scene.camPos; focus:=-1;
  end else if focus>=0 then
    case mode of
      smObject: p:=scene.o[focus].position;
      smFace: begin
        p:=scene.selObject.GetFaceMiddle(focus);
        //scene.arrowAlpha:=0;
        scene.GetVertIndices;
        //p:=p+scene.o[scene.selIndex].position;
      end;
      smVertex: begin
        p:=scene.selObject.va[focus];
        //scene.arrowAlpha:=0;
        scene.GetSelIndices;
      end;
      //smBone:
      //smAnim:
    end;
  case ds of
    dsMove: SetPlaneXZ(p);
    dsMoveY: SetPlaneY(p);
    dsMoveCam: SetPlaneFree(p);
    dsArea: begin
      selStart.x:=mp.x; selStart.y:=mp.y; selStart.z:=0;
      selEnd:=selStart;
      exit;
    end
    else SetPlaneXZ(p);
  end;
  if (mode<>smObject) and (scene.selObject<>nil) and
     (ds in [dsMove, dsMoveY]) then begin
    with scene.o[scene.selIndex] do begin
      //scene.cam.Translate(position, false);
      //scene.cam.Multiply(rotation, true);
      movePPos:=movePPos+position;
      movePRot:=multiply(rotation, movePRot);
    end;
  end;
  nxMath3D.RayPlaneIntersect(ray.start, ray.dir, movePPos,
    GetVector(movePRot, 1), @selStart);
  selEnd:=selStart;
end;

procedure TForm1.SetPlaneXZ(p: TVector);
begin
  movePPos:=p;
  if (mode<>smObject) and (scene.selObject<>nil) then begin
    movePRot:=invert(scene.o[scene.selIndex].rotation);
  end else
    movePRot:=NewMatrix3f;
end;

procedure TForm1.SetPlaneY(p: TVector);
var m: TMatrix;
begin
  movePPos:=p;
  m:=CreateMatrix(vector(0, 1, 0), 0);
  m:=nxMath3D.Rotate(m, vector(1, 0, 0), -pi/2, false);
  m:=nxMath3D.Rotate(m, vector(0, 1, 0), -(scene.ax-180)*toRad, false);
  movePRot:=GetRotation3(m);
end;

procedure TForm1.SetPlaneFree(p: TVector);
var m, m2: TMatrix;
begin
  movePPos:=p;
  m:=scene.cam.GetMatrix;
  m:=invert(m);
  m2:=CreateMatrix(vector(1, 0, 0), 90*toRad);
  m:=Multiply(m2, m);
  movePRot:=GetRotation3(m);
end;

procedure TForm1.mnuListExtClick(Sender: TObject);
begin
  showmessage(nx.GetEXT);
end;

procedure TForm1.mnuMemoryClick(Sender: TObject);
var m, tm: cardinal;
begin
  tm:=tex.GetMemoryUsage;
  m:=tm;
  showmessage(format(' - Memory usage -'+#13+
    'Textures = %d'+#13+
    'Objects = %d'+#13+
    'Instanced Objects = %d'+#13+
    'Total = %d',[tm, length(obj), length(scene.o), m]));
end;

procedure TForm1.mnuAddObjectClick(Sender: TObject);
begin
  openD.Filter:='Supported models (NXW, OBJ, W3D, Milkshape)|*.nxw;*.obj;*.w3d;*.ms3d|All files|*.*';
  if openD.Execute then begin
    AddFile(openD.FileName);
  end;
end;

procedure TForm1.mnuNewWorkspaceClick(Sender: TObject);
var dr: integer;
begin
  if modified then begin
    dr:=application.MessageBox('Discard changes without saving?',
      'Workspace was modified', MB_ICONQUESTION + MB_YESNOCANCEL);
    if dr<>IDYES then exit;
  end;
  objlist.Items.Clear; FreeObjects;
  wsFilename:=''; ResetMode;
  modified:=false;
end;

procedure TForm1.mnuSaveWorkspaceAsClick(Sender: TObject);
begin
  saveD.DefaultExt:='.nxw';
  saveD.Filter:='NXW|*.nxw|W3D (limited features)|*.w3d|All files|*.*';
  if saveD.Execute then begin
    wsFilename:=saveD.FileName;
    mnuSaveWorkspaceClick(nil);
  end;
end;

procedure TForm1.mnuSaveWorkspaceClick(Sender: TObject);
begin
  if wsFilename<>'' then begin
    // Save workspace


    modified:=false;
  end else // Save as...
    mnuSaveWorkspaceAsClick(nil);
end;

procedure TForm1.mnuWireClick(Sender: TObject);
begin
  scene.wireFrame:=(scene.wireFrame+1) mod 3;
end;

procedure TForm1.selModeChange(Sender: TObject);
var newmode, oldmode: TSceneMode; n: integer;
    //selArray2: array of boolean;
begin
  oldmode:=mode;
  newmode:=TSceneMode(selMode.ItemIndex);
  if newmode=mode then exit;
  if (newmode<>smObject) and (scene.selObject=nil) then begin
    if length(scene.o)=1 then begin
      scene.SelectObj(0);
    end else begin
      newmode:=mode; selMode.ItemIndex:=0;
      showmessage('Select 1 object first...');
    end;
  end;
  if newmode<>mode then begin
    mode:=newmode;
    case mode of
      smObject: begin
        if scene.selIndex>=0 then begin
          n:=scene.selIndex; ClearSelection; AddSelect(n);
        end else ClearSelection;
      end;
      smVertex: begin
        if oldmode=smFace then begin
          // Select all vertices from selected faces

        end else ClearSelection;
      end;
      smFace: begin
        if oldmode=smVertex then begin
          // Select all faces that have selected vertices

        end else ClearSelection;
      end;
      else ClearSelection;
    end;
  end;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
var i: integer;
begin
  for i:=0 to high(FileNames) do AddFile(FileNames[i]);
end;

procedure TForm1.btnAddObjectClick(Sender: TObject);
begin
  if objlist.Items.Count>0 then begin
    if objlist.ItemIndex<0 then objlist.ItemIndex:=0;
    scene.Add(obj[objlist.ItemIndex]);
    modified:=true;
  end;
end;

procedure TForm1.mnuExitClick(Sender: TObject);
begin
  close;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var dr, i: integer;
begin
  Timer1.Enabled:=false;
  if modified then begin
    dr:=application.MessageBox('Save before exit?',
      'Workspace was modified', MB_ICONQUESTION + MB_YESNOCANCEL);
    if dr=IDYES then begin
      mnuSaveWorkspaceClick(nil);
      if wsFilename='' then begin
        CloseAction:=caNone;
        Timer1.Enabled:=true; exit;
      end;
    end else if dr=IDCANCEL then begin
      CloseAction:=caNone;
      Timer1.Enabled:=true; exit;
    end;
  end;
  FreeObjects;
  for i:=0 to high(eobj) do FreeAndNil(eobj[i]);
  FreeAndNil(scene);
  nx.KillGLWindow;
end;

procedure TForm1.FreeObjects;
var i: integer;
begin
  scene.Clear;
  for i:=0 to high(obj) do FreeAndNil(obj[i]);
  setlength(obj, 0);
end;

end.

