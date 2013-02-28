unit uBaseConfig;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ExtDlgs, Buttons, LCLType,
  TileUnit, uTileStyles, uBlends, types;

type

  { TfrmBaseConfig }

  TfrmBaseConfig = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnChooseImage: TButton;
    btnConfigStyles: TButton;
    btnDelAll: TButton;
    btnConfigBlends: TButton;
    btnDelete: TButton;
    grpDetails: TGroupBox;
    groupList: TComboBox;
    GroupBox1: TGroupBox;
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
    typeList: TComboBox;
    procedure btnOKClick(Sender: TObject);
    procedure btnChooseImageClick(Sender: TObject);
    procedure btnConfigStylesClick(Sender: TObject);
    procedure btnDelAllClick(Sender: TObject);
    procedure btnConfigBlendsClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure groupListChange(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseWheel(Sender: TObject; {%H-}Shift: TShiftState; WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure Panel1Resize(Sender: TObject);
    procedure tWidthChange(Sender: TObject);
    procedure tWidthExit(Sender: TObject);
  private
  public
    tileSet: TTileSetConfig;
    zoom, rx, ry, mx, my: integer;
    procedure Draw;
    procedure LoadImage(filename: string);
    procedure UpdateControls(sender: TObject = nil);
  end;

var frmBaseConfig: TfrmBaseConfig;

implementation

{$R *.lfm}

{ TfrmBaseConfig }

procedure TfrmBaseConfig.FormCreate(Sender: TObject);
begin
  tileSet:=TTileSetConfig.Create;
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

procedure TfrmBaseConfig.btnChooseImageClick(Sender: TObject);
begin
  if openPic.Execute then begin
    tileSet.basefile:=extractfilename(openPic.FileName);
    LoadImage(openPic.FileName);
  end;
end;

procedure TfrmBaseConfig.btnConfigStylesClick(Sender: TObject);
begin
  frmTileStyles.styles.Assign(tileset.styles);
  frmTileStyles.ShowModal;
  tileset.styles.Assign(frmTileStyles.styles);
  UpdateControls; Draw;
end;

procedure TfrmBaseConfig.btnDelAllClick(Sender: TObject);
var dr: integer;
begin
  dr:=application.MessageBox('Confirmation',
    'Delete all groups?', MB_ICONQUESTION + MB_YESNOCANCEL);
  if dr<>IDYES then exit;
  tileset.ClearGroups;
  UpdateControls; Draw;
end;

procedure TfrmBaseConfig.btnConfigBlendsClick(Sender: TObject);
begin
  frmBlends.tileset.Assign(tileSet);
  frmBlends.ShowModal;
  tileset.Assign(frmBlends.tileset);
  UpdateControls; Draw;
end;

procedure TfrmBaseConfig.Button3Click(Sender: TObject);
begin
  tileset.DeleteGroup(groupList.ItemIndex);
  UpdateControls; Draw;
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
  Draw;
end;

procedure TfrmBaseConfig.UpdateControls(sender: TObject);
var i, n: integer;
begin
  tWidth.Text:=inttostr(tileset.patternwidth);
  tHeight.Text:=inttostr(tileset.patternheight);
  tSkipWidth.Text:=inttostr(tileset.Skipwidth);
  tSkipHeight.Text:=inttostr(tileset.Skipheight);

  if sender=nil then begin
    n:=typeList.ItemIndex;
    typeList.Items.Clear;
    for i:=0 to tileset.styles.Count-1 do
      with tileset.styles.style[i] do
        typeList.Items.Add(name);
    if n>=tileset.styles.Count then n:=tileset.styles.Count-1
    else if n<0 then n:=0;
    typeList.ItemIndex:=n;
  end;
end;

procedure TfrmBaseConfig.FormDestroy(Sender: TObject);
begin
  FreeAndNil(tileSet);
end;

procedure TfrmBaseConfig.FormShow(Sender: TObject);
begin
  Draw;
end;

procedure TfrmBaseConfig.groupListChange(Sender: TObject);
begin
  grpDetails.Enabled:=groupList.Items.Count>0;
  if grpDetails.Enabled and (groupList.ItemIndex<0) then
    groupList.ItemIndex:=0;
  UpdateControls;
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
  btnCancel.Left:=Panel1.Width div 2-btnCancel.Width-15;
  btnOk.Left:=Panel1.Width div 2+15;
end;

end.

