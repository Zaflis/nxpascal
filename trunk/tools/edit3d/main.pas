unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, LCLType,
  dglOpenGL, nxGL, nxTypes, ModelUnit, nxMath3D, types;

type

  { TForm1 }

  TForm1 = class(TForm)
    AppProperties: TApplicationProperties;
    btnAddObject: TButton;
    mnuSaveObjAs: TMenuItem;
    MenuItem11: TMenuItem;
    mnuAddToScene: TMenuItem;
    mnuDeleteObj: TMenuItem;
    objlistPopup: TPopupMenu;
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
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Timer1: TTimer;
    procedure btnAddObjectClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormPaint(Sender: TObject);
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
    procedure Timer1Timer(Sender: TObject);
  private
    obj: array of TEditModel;
    modified: boolean;
    scene: T3DScene;
    mb: byte;
    wsFilename: string;
    procedure AddFile(filename: string);
    procedure AddSelect(index: integer; value: byte = 255);
    procedure ClearSelection;
    procedure FreeObjects;
    procedure LoadGLData;
    procedure ResetMode;
    procedure SetStatus(statusText: string);
  public
  end;

var
  Form1: TForm1;
  DEBUGMODE: boolean = false;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  if paramstr(1)='-debug' then DEBUGMODE:=true;

  width:=800; height:=600;
  if not nx.CreateGlWindow(self) then begin
    showmessage('Cannot initialize OpenGL'); exit;
  end;
  scene:=T3DScene.Create;
  nx.DefaultLights;
  nx.rs.DepthTest:=true;
  mode:=smObject;
  if nx.LastError<>'' then begin
    showmessage(nx.LastError); nx.ClearError;
  end;
end;

procedure TForm1.LoadGLData;
begin
  nx.CreateBasicFont;
  eObj[0]:=TGLModel.Create('data\Arrow.w3d');
  eObj[1]:=TGLModel.Create('data\Select.w3d');
  eObj[2]:=TGLModel.Create('data\Bone.w3d');
  SetStatus('Done');
end;

procedure TForm1.ResetMode;
begin
  mode:=smObject; modified:=false;
  ClearSelection; focus:=-1;
  mnuCamTopClick(nil);
end;

procedure TForm1.SetStatus(statusText: string);
begin
  sbar.Caption:=statusText;
  application.ProcessMessages;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if nx.LastError<>'' then begin
    showmessage(nx.LastError); nx.ClearError; exit;
  end;

  nx.Clear(true, true);
  scene.Render;
  nx.Flip;

  if DEBUGMODE then begin
    if timer1.Tag=0 then begin
      timer1.Tag:=1;
      AddFile('objects\ship.w3d');
      //AddFile('objects\extrude.w3d');
      //AddFile('objects\test.w3d');
      btnAddObjectClick(nil);
      modified:=false;
    end;
  end;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  if (tag=0) and nx.AllOK then begin
    OnPaint:=nil; tag:=1;
    LoadGLData;
    timer1.Enabled:=true;
  end else if (tag=0) and (nx.LastError<>'') then begin
    showmessage(nx.LastError); nx.ClearError;
  end;
end;

procedure TForm1.mnuUndoClick(Sender: TObject);
begin


  modified:=true;
end;

procedure TForm1.AddFile(filename: string);
var n: integer;
begin
  if not fileexists(filename) then begin
    showmessage(filename+' doesn''t exist.'); exit;
  end;
  SetStatus('Loading '+filename+'...');
  n:=length(obj);
  setlength(obj, n+1);
  obj[n]:=TEditModel.Create;
  obj[n].LoadFromFile(filename);
  SetStatus('Loading textures...');
  nxLoadModelTextures(obj[n], 'textures');

  objList.Items.Add(extractfilename(filename));
  objList.ItemIndex:=objList.Items.Count-1;
  modified:=true;
  SetStatus('Done');
end;

procedure TForm1.AddSelect(index: integer; value: byte);
begin
  if selArray[index]=0 then inc(selCount);
  selArray[index]:=value;
end;

procedure TForm1.ClearSelection;
var i, n: integer;
begin
  n:=selCount;
  if n>0 then
    for i:=0 to high(selArray) do
      if selArray[i]>0 then begin
        selArray[i]:=0; dec(n);
        if n<1 then exit;
      end;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
var i: integer;
begin
  for i:=0 to high(FileNames) do AddFile(FileNames[i]);
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Timer1.Enabled then exit;
  if button=mbLeft then begin
    mb:=1;
    if focus<0 then begin
      if not ((ssShift in Shift) or (ssCtrl in Shift)) then
        ClearSelection;
    end else begin
      if not ((ssShift in Shift) or (ssCtrl in Shift)) then ClearSelection;
      AddSelect(focus);
    end;
  end else mb:=2;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var dx, dy: single; i: integer;
begin
  if not Timer1.Enabled then exit;
  dx:=x-mp.x; dy:=y-mp.y;
  scene.cam.SetCamera;
  nx.GetMouseRay(x, y, @ray.start, @ray.dir);
  if mb=2 then begin
    // Move camera

  end else if (ssCtrl in Shift) and (mb>0) then begin
    // Rotate camera
    with scene do begin
      ax:=ax+dx;
      ay:=ay+dy;
      if ax<0 then ax:=ax+360
      else if ax>=360 then ax:=ax-360;
      if ay>90 then ay:=90
      else if ay<-90 then ay:=-90;
    end;
  end else if mb=1 then begin
    if focus<0 then begin
      // Select area

    end else begin
      // Move ...

    end;
  end else if mb=0 then begin
    // Focus
    focus:=-1;
    case mode of
      smObject: begin
        for i:=0 to high(scene.o) do
          if scene.o[i].obj.rayIntersect(ray.start, ray.dir,
             false, nil, nil)>=0 then begin
            focus:=i; break;
          end;
      end;
      smFace: begin

      end;
      smVertex: begin

      end;
      smBone: begin

      end;
      smAnim: begin

      end;
    end;
  end;
  mp:=pointf(x, y);
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Timer1.Enabled then exit;

  mb:=0;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  scene.zoom:=scene.zoom-(WheelDelta div abs(WheelDelta))*0.5;
  if scene.zoom<0 then scene.zoom:=0
  else if scene.zoom>20 then scene.zoom:=20
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

procedure TForm1.mnuExitClick(Sender: TObject);
begin
  close;
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
  objlist.Items.Clear;
  FreeObjects;
  wsFilename:='';
  ResetMode;
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

procedure TForm1.FreeObjects;
var i: integer;
begin
  scene.Clear;
  for i:=0 to high(obj) do FreeAndNil(obj[i]);
  for i:=0 to high(eobj) do FreeAndNil(eObj[i]);
  setlength(obj, 0);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var dr: integer;
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
  FreeAndNil(scene);
  nx.KillGLWindow;
end;

procedure TForm1.btnAddObjectClick(Sender: TObject);
begin
  if objlist.Items.Count>0 then begin
    if objlist.ItemIndex<0 then objlist.ItemIndex:=0;
    scene.Add(obj[objlist.ItemIndex]);
    modified:=true;
  end;
end;

end.

