unit main;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, Menus, ExtCtrls, ExtDlgs, LCLType, StdCtrls,
  TileUnit, uBaseConfig, uTileStyles;

const Title = 'Tileset Generator';

type
  TEditorMode = (eNone, eTile, eWorld);

  { TForm1 }

  TForm1 = class(TForm)
    btnConfigBaseImg: TButton;
    Button1: TButton;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    imgTiles: TImage;
    MainMenu1: TMainMenu;
    mnuTileStyles: TMenuItem;
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
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlTiles: TPanel;
    pnlWorld: TPanel;
    pnlTileSet: TPanel;
    saveD: TSaveDialog;
    savePic: TSavePictureDialog;
    ScrollBox1: TScrollBox;
    procedure btnConfigBaseImgClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgTilesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgTilesResize(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuNewTilesetClick(Sender: TObject);
    procedure mnuNewWorldClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuSaveAsClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure mnuTileStylesClick(Sender: TObject);
  private
    FMode: TEditorMode;
    modified: boolean;
    wsFilename: string;
    base: TBitmap;
    world: TWorld;
    tileset: TTileSet;
    procedure SetMode(newmode: TEditorMode);
  public
    property mode: TEditorMode read FMode write SetMode;
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
  pnlTileSet.Align:=alClient; pnlWorld.Align:=alClient;
  ScrollBox1.Align:=alClient;
  Image2.Align:=alClient;
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

procedure TForm1.mnuNewTilesetClick(Sender: TObject);
var dr: integer;
begin
  if modified then begin
    dr:=application.MessageBox('Discard changes without saving?',
      'Workspace was modified', MB_ICONQUESTION + MB_YESNOCANCEL);
    if dr<>IDYES then exit;
  end;
  modified:=false; wsFilename:=''; mode:=eTile;
end;

procedure TForm1.mnuNewWorldClick(Sender: TObject);
var dr: integer;
begin
  if modified then begin
    dr:=application.MessageBox('Discard changes without saving?',
      'Workspace was modified', MB_ICONQUESTION + MB_YESNOCANCEL);
    if dr<>IDYES then exit;
  end;
  modified:=false; wsFilename:=''; mode:=eWorld;

  FreeAndNil(tileset);
  world.Free; world:=TWorld.Create;
  DrawWorld;
end;

procedure TForm1.mnuOpenClick(Sender: TObject);
var ext: string;
begin
  if openD.Execute then begin
    ext:=lowercase(extractfileext(openD.FileName));
    wsFilename:=openD.FileName;
    caption:=title+' - '+extractfilename(openD.FileName);
    if ext='.set' then begin
      // Open tileset

    end else if ext='map' then begin
      // Open world

    end;
  end;
end;

procedure TForm1.mnuSaveAsClick(Sender: TObject);
begin
  case mode of
    eTile: begin
      saveD.DefaultExt:='.set';
      saveD.Filter:='Tileset|*.set|All files|*.*';
    end;
    eWorld: begin
      saveD.DefaultExt:='.map';
      saveD.Filter:='Worldmap|*.map|All files|*.*';
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
        //tileset.
      end;
      eWorld: begin
        //world.
      end;
    end;

    modified:=false;
  end else // Save as...
    mnuSaveAsClick(nil);
end;

procedure TForm1.mnuTileStylesClick(Sender: TObject);
begin
  frmTileStyles.styles.Assign(tileset.styles);
  frmTileStyles.ShowModal;
  tileset.styles.Assign(frmTileStyles.styles);
end;

procedure TForm1.SetMode(newmode: TEditorMode);
begin
  case mode of
    eTile: begin
      FreeAndNil(tileset);
    end;
    eWorld: begin
      FreeAndNil(world);
      pnlTiles.Visible:=false;
    end;
  end;
  FMode:=newmode;
  mnuTileSet.Visible:=newmode=eTile;
  pnlTileSet.Visible:=mnuTileSet.Visible;
  mnuWorld.Visible:=newmode=eWorld;
  pnlWorld.Visible:=mnuWorld.Visible;
  case mode of
    eTile: begin
      tileset:=TTileSet.Create;
      DrawTileset;
    end;
    eWorld: begin
      world:=TWorld.Create;
      DrawWorld;
    end;
  end;
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
  frmBaseConfig.tileSet.Assign(tileset);
  frmBaseConfig.UpdateControls;
  if frmBaseConfig.ShowModal=mrOK then begin
    tileset.Assign(frmBaseConfig.tileSet);
    DrawTileset;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  tileset.Free; world.Free; base.Free;
end;

procedure TForm1.Image3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  pnlTiles.Visible:=not pnlTiles.Visible;
end;

procedure TForm1.imgTilesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  pnlTiles.Hide;
end;

procedure TForm1.imgTilesResize(Sender: TObject);
begin
  pnlTiles.Width:=imgTiles.Width+4;
  pnlTiles.Height:=imgTiles.Height+4;
end;

end.

