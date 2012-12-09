unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, LCLType,
  dglOpenGL, nxGL, nxTypes, ModelUnit, nxMath3D;

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
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    mnuAddObject: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    mnuCopy: TMenuItem;
    mnuPaste: TMenuItem;
    mnuDelete: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem6: TMenuItem;
    mnuSaveWorkspace: TMenuItem;
    MenuItem7: TMenuItem;
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
    procedure FormPaint(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem29Click(Sender: TObject);
    procedure MenuItem30Click(Sender: TObject);
    procedure mnuSaveObjAsClick(Sender: TObject);
    procedure mnuDeleteObjClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure mnuAddObjectClick(Sender: TObject);
    procedure mnuNewWorkspaceClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    obj: array of TEditModel;
    modified: boolean;
    selarray: array[0..high(word)] of byte;
    scene: T3DScene;
    mb: byte;
    mp: TPoint;
    procedure AddFile(filename: string);
    procedure FreeObjects;
    procedure LoadGLData;
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
end;

procedure TForm1.LoadGLData;
begin
  nx.CreateBasicFont;
  if DEBUGMODE then begin
    //AddFile('objects\ship.w3d');
    //AddFile('objects\extrude.w3d');
    AddFile('objects\test.w3d');
    btnAddObjectClick(nil);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  nx.Clear(true, true);
  scene.Render;
  nx.Flip;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  if (tag=0) and nx.AllOK then begin
    tag:=1;
    LoadGLData;
    timer1.Enabled:=true;
  end;
end;

procedure TForm1.AddFile(filename: string);
var n: integer;
begin
  if not fileexists(filename) then begin
    showmessage(filename+' doesn''t exist.'); exit;
  end;
  n:=length(obj);
  setlength(obj, n+1);
  obj[n]:=TEditModel.Create;
  obj[n].LoadFromFile(filename);
  nxLoadModelTextures(obj[n], 'textures');

  objList.Items.Add(extractfilename(filename));
  objList.ItemIndex:=objList.Items.Count-1;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
var i: integer;
begin
  for i:=0 to high(FileNames) do AddFile(FileNames[i]);
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if button=mbLeft then mb:=1
  else mb:=2;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var dx, dy: integer;
begin
  dx:=x-mp.x; dy:=y-mp.y;
  if mb=1 then begin
    with scene do begin
      ax:=ax+dx;
      ay:=ay+dy;
      if ax<0 then ax:=ax+360
      else if ax>=360 then ax:=ax-360;
      if ay>90 then ay:=90
      else if ay<-90 then ay:=-90;
    end;
  end;
  mp:=point(x, y);
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  mb:=0;
end;

procedure TForm1.MenuItem28Click(Sender: TObject);
begin
  scene.cam.Load(0);
end;

procedure TForm1.MenuItem29Click(Sender: TObject);
begin
  scene.cam.Load(1);
end;

procedure TForm1.MenuItem30Click(Sender: TObject);
begin
  scene.cam.Load(2);
end;

procedure TForm1.mnuSaveObjAsClick(Sender: TObject);
begin
  if (objlist.Items.Count>0) and (objlist.ItemIndex>=0) then begin
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

procedure TForm1.MenuItem20Click(Sender: TObject);
begin
  showmessage(nx.GetEXT);
end;

procedure TForm1.MenuItem21Click(Sender: TObject);
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
  if openD.Execute then begin
    AddFile(openD.FileName);
  end;
end;

procedure TForm1.mnuNewWorkspaceClick(Sender: TObject);
var dr: integer;
begin
  if modified then begin
    dr:=application.MessageBox('Close file without saving?',
      'File was modified', MB_ICONQUESTION + MB_YESNOCANCEL);
    if dr<>IDYES then exit;
  end;
  modified:=false;
  objlist.Items.Clear;
  FreeObjects;
end;

procedure TForm1.FreeObjects;
var i: integer;
begin
  scene.Clear;
  for i:=0 to high(obj) do obj[i].Free;
  setlength(obj, 0);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var dr: integer;
begin
  Timer1.Enabled:=false;
  if modified then begin
    dr:=application.MessageBox('Quit without saving?',
      'File was modified', MB_ICONQUESTION + MB_YESNOCANCEL);
    if dr<>IDYES then begin
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
  end;
end;

end.

