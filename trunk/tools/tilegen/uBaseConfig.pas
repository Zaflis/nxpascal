unit uBaseConfig;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ExtDlgs, Buttons,
  TileUnit, uTileStyles, types;

type

  { TfrmBaseConfig }

  TfrmBaseConfig = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Image1: TImage;
    Panel1: TPanel;
    Panel2: TPanel;
    openPic: TOpenPictureDialog;
    Panel3: TPanel;
    ScrollBox1: TScrollBox;
    tHeight: TLabeledEdit;
    tSkipHeight: TLabeledEdit;
    tSkipWidth: TLabeledEdit;
    tWidth: TLabeledEdit;
    procedure btnOKClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Panel1Resize(Sender: TObject);
    procedure tWidthChange(Sender: TObject);
    procedure tWidthExit(Sender: TObject);
  private
  public
    tileSet: TTileSet;
    zoom, rx, ry, mx, my: integer;
    procedure Draw;
    procedure LoadImage(filename: string);
    procedure UpdateControls;
  end;

var frmBaseConfig: TfrmBaseConfig;

implementation

{$R *.lfm}

{ TfrmBaseConfig }

procedure TfrmBaseConfig.FormCreate(Sender: TObject);
begin
  tileSet:=TTileSet.Create;
  ScrollBox1.Align:=alClient;
  Panel1Resize(nil);
  zoom:=2;
end;

procedure TfrmBaseConfig.btnOKClick(Sender: TObject);
begin
  tileSet.patternwidth:=strtointdef(tWidth.Text, 16);
  tileSet.patternheight:=strtointdef(tHeight.Text, 16);
  tileSet.skipwidth:=strtointdef(tSkipWidth.Text, 1);
  tileSet.skipheight:=strtointdef(tSkipHeight.Text, 1);
end;

procedure TfrmBaseConfig.Button1Click(Sender: TObject);
begin
  if openPic.Execute then begin
    tileSet.texturefile:=extractfilename(openPic.FileName);
    LoadImage(openPic.FileName);
  end;
end;

procedure TfrmBaseConfig.Button2Click(Sender: TObject);
begin
  frmTileStyles.styles.Assign(tileset.styles);
  frmTileStyles.ShowModal;
  tileset.styles.Assign(frmTileStyles.styles);
end;

procedure TfrmBaseConfig.tWidthChange(Sender: TObject);
var n: integer;
begin
  with sender as TLabeledEdit do begin
    n:=strtointdef(text, 1);
    if n<Tag then n:=Tag;
  end;
  if sender=tWidth then tileset.patternwidth:=n
  else if sender=tHeight then tileset.patternheight:=n
  else if sender=tSkipWidth then tileset.skipwidth:=n
  else if sender=tSkipHeight then tileset.skipheight:=n;
end;

procedure TfrmBaseConfig.tWidthExit(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmBaseConfig.Draw;
begin
  image1.Width:=zoom*image1.Picture.Width;
  image1.Height:=zoom*image1.Picture.Height;
end;

procedure TfrmBaseConfig.LoadImage(filename: string);
begin
  image1.Picture.LoadFromFile(filename);
  tileset.baseWidth:=image1.Picture.Width;
  tileset.baseHeight:=image1.Picture.Height;
end;

procedure TfrmBaseConfig.UpdateControls;
begin
  tWidth.Text:=inttostr(tileset.patternwidth);
  tHeight.Text:=inttostr(tileset.patternheight);
  tSkipWidth.Text:=inttostr(tileset.Skipwidth);
  tSkipHeight.Text:=inttostr(tileset.Skipheight);
end;

procedure TfrmBaseConfig.FormDestroy(Sender: TObject);
begin
  FreeAndNil(tileSet);
end;

procedure TfrmBaseConfig.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  mx:=(x div zoom) {%H-}div (tileset.patternwidth+tileset.skipwidth);
  mx:=(y div zoom) {%H-}div (tileset.patternheight+tileset.skipheight);
end;

procedure TfrmBaseConfig.Image1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  WheelDelta:=WheelDelta div abs(WheelDelta);
  zoom:=zoom+WheelDelta;
  if zoom<1 then zoom:=1
  else if zoom>8 then zoom:=8;
  Draw;
end;

procedure TfrmBaseConfig.Panel1Resize(Sender: TObject);
begin
  btnCancel.Left:=Panel1.Width div 2-100;
  btnOk.Left:=Panel1.Width div 2+20;
end;

end.

