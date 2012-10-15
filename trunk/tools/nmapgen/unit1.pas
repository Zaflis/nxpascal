unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtDlgs,
  dglOpenGL, nxGL;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    mnuMenu: TMenuItem;
    mnuSelect: TMenuItem;
    mnuSave: TMenuItem;
    MenuItem4: TMenuItem;
    mnuExit: TMenuItem;
    openD: TOpenDialog;
    savePic: TSavePictureDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure mnuSelectClick(Sender: TObject);
  private
    model: TGLModel;
  public
  end;

var Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  width:=512; height:=512+30;

  {model:=TGLModel.CreateSphere(40, 40);
  model.SaveToFile('sphere.w3d');
  model.Free;}

  model:=TGLModel.Create;
  nx.CreateGlWindow(self);
  with nx.window do begin
    Align:=alNone;
    left:=0; top:=0;
    width:=512; height:=512;
  end;
  nx.SetClearColor(0.5, 0.5, 0.5);
end;

procedure TForm1.mnuSelectClick(Sender: TObject);
var i, j: integer;
begin
  if openD.Execute then begin
    model.LoadFromFile(openD.FileName);
    nx.Clear(true, false);
    nx.Enable2D;
    glBegin(GL_TRIANGLES);
    for i:=0 to model.fCount-1 do
      for j:=0 to 2 do begin
        with model.na[model.fa[i, j]] do
          glColor4f(0.5+x/2, 0.5+y/2, 0.5+z/2, 0.5);
        with model.ta[model.fa[i, j]] do
          glVertex2f(x*nx.Width, y*nx.Height);
      end;
    glEnd;
    nx.Disable2D;
    nx.Flip;
  end;
end;

procedure TForm1.mnuSaveClick(Sender: TObject);
begin
  {if savePic.Execute then begin

  end;}
end;

procedure TForm1.mnuExitClick(Sender: TObject);
begin
  close;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(model);
end;

end.

