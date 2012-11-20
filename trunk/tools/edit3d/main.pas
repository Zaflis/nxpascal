unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, LCLType,
  dglOpenGL, nxGL, nxTypes, ModelUnit;

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
    procedure FormPaint(Sender: TObject);
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
    procedure FreeObjects;
    procedure LoadGLData;
  public
  end;

var
  Form1: TForm1;

const
  EPSILON: Single = 1e-40;
  EPSILON2: Single = 1e-30;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  width:=800; height:=600;
  if not nx.CreateGlWindow(self) then begin
    showmessage('Cannot initialize OpenGL'); exit;
  end;
  scene:=T3DScene.Create;
end;

procedure TForm1.LoadGLData;
begin

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

procedure TForm1.mnuSaveObjAsClick(Sender: TObject);
begin
  if (objlist.Items.Count>0) and (objlist.ItemIndex>=0) then begin
    if saveD.Execute then begin
      //obj[objlist.ItemIndex].SaveToOBJ();
    end;
  end;
end;

procedure TForm1.mnuDeleteObjClick(Sender: TObject);
begin
  if (objlist.Items.Count>0) and (objlist.ItemIndex>=0) then begin

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
    'Objects = ?'+#13+
    'Total = %d',[tm, m]));
end;

procedure TForm1.mnuAddObjectClick(Sender: TObject);
var n: integer;
begin
  if openD.Execute then begin
    n:=length(obj);
    setlength(obj, n+1);
    obj[n]:=TEditModel.Create;
    obj[n].LoadFromFile(openD.FileName);
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
  for i:=0 to high(obj) do obj[i].Free;
  setlength(obj, 0);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var dr: integer;
begin
  if modified then begin
    dr:=application.MessageBox('Quit without saving?',
      'File was modified', MB_ICONQUESTION + MB_YESNOCANCEL);
    if dr<>IDYES then begin
      CloseAction:=caNone; exit;
    end;
  end;
  FreeObjects;
  FreeAndNil(scene);
  nx.KillGLWindow;
end;

procedure TForm1.btnAddObjectClick(Sender: TObject);
begin
  if (objlist.Items.Count>0) and (objlist.ItemIndex>=0) then begin
    scene.Add(obj[objlist.ItemIndex]);
  end;
end;

end.

