unit main;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, ExtDlgs, LCLType, StdCtrls, TileUnit, Unit2;

const Title = 'Tileset Generator';

type

  TEditorMode = (eTile, eWorld);

  TBlend = record

  end;

  { TForm1 }

  TForm1 = class(TForm)
    btnConfigBaseImg: TButton;
    Image1: TImage;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuWorld: TMenuItem;
    mnuTileSet: TMenuItem;
    mnuChooseImage: TMenuItem;
    mnuNewTileset: TMenuItem;
    mnuNewWorld: TMenuItem;
    mnu0: TMenuItem;
    mnuOpen: TMenuItem;
    mnu1: TMenuItem;
    mnuSaveAs: TMenuItem;
    mnuSave: TMenuItem;
    mnuExit: TMenuItem;
    openD: TOpenDialog;
    openPic: TOpenPictureDialog;
    pnlWorld: TPanel;
    pnlTileSet: TPanel;
    saveD: TSaveDialog;
    savePic: TSavePictureDialog;
    ScrollBox1: TScrollBox;
    procedure btnConfigBaseImgClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuNewTilesetClick(Sender: TObject);
    procedure mnuNewWorldClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuSaveAsClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
  private
    mode: TEditorMode;
    modified: boolean;
    wsFilename: string;
    base: TBitmap;
    world: array of word;
    sizeX, sizeY: cardinal;
    tileset: TTileSet;
  public
    procedure DrawTileset;
    procedure DrawWorld;
  end;

var Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  width:=800; height:=600;
  //pnlTileSet.Left:=0; pnlTileSet.Top:=0;
  //pnlWorld.Left:=0; pnlWorld.Top:=0;
  pnlTileSet.Align:=alClient; pnlWorld.Align:=alClient;
  tileset:=TTileSet.Create;
  base:=TBitmap.Create;

  mnuNewTilesetClick(nil);
end;

procedure TForm1.DrawTileset;
begin
  //
end;

procedure TForm1.DrawWorld;
begin
  //
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  //
end;

procedure TForm1.mnuNewTilesetClick(Sender: TObject);
var dr: integer;
begin
  if modified then begin
    dr:=application.MessageBox('Discard changes without saving?',
      'Workspace was modified', MB_ICONQUESTION + MB_YESNOCANCEL);
    if dr<>IDYES then exit;
  end;
  mode:=eTile;
  mnuWorld.Visible:=false;
  mnuTileSet.Visible:=true;
  pnlWorld.Visible:=false;
  pnlTileSet.Visible:=true;
end;

procedure TForm1.mnuNewWorldClick(Sender: TObject);
begin
  mnuTileSet.Visible:=true;
  mnuWorld.Visible:=true;
  pnlTileSet.Visible:=false;
  pnlWorld.Visible:=true;
end;

procedure TForm1.mnuOpenClick(Sender: TObject);
var ext: string;
begin
  if openD.Execute then begin
    ext:=lowercase(extractfileext(openD.FileName));
    caption:=title+' - '+ext;
  end;
end;

procedure TForm1.mnuSaveAsClick(Sender: TObject);
begin
  case mode of
    eTile: begin
      saveD.DefaultExt:='.nxw';
      saveD.Filter:='NXW|*.nxw|All files|*.*';
    end;
    eWorld: begin
      saveD.DefaultExt:='.nxw';
      saveD.Filter:='NXW|*.nxw|All files|*.*';
    end;
  end;
  if saveD.Execute then begin
    wsFilename:=saveD.FileName;
    mnuSaveClick(nil);
  end;
end;

procedure TForm1.mnuSaveClick(Sender: TObject);
begin
  if wsFilename<>'' then begin
    // Save workspace
    case mode of
    eTile: begin

    end;
    eWorld: begin

    end;
  end;

    modified:=false;
  end else // Save as...
    mnuSaveAsClick(nil);
end;

procedure TForm1.mnuExitClick(Sender: TObject);
begin
  close;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var dr: integer;
begin
  if modified then begin
    dr:=application.MessageBox('Save before exit?',
      'Workspace was modified', MB_ICONQUESTION + MB_YESNOCANCEL);
    if dr=IDYES then begin
      mnuSaveClick(nil);
      if wsFilename='' then begin
        CloseAction:=caNone; exit;
      end;
    end else if dr=IDCANCEL then begin
      CloseAction:=caNone; exit;
    end;
  end;
end;

procedure TForm1.btnConfigBaseImgClick(Sender: TObject);
begin
  if frmBaseConfig.ShowModal=mrOK then begin
    tileset.Assign(frmBaseConfig.tileSet);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  tileset.Free;
  base.Free;
end;

end.

