unit unit2;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ExtDlgs, Buttons, TileUnit;

type

  { TfrmBaseConfig }

  TfrmBaseConfig = class(TForm)
    BitBtn1: TBitBtn;
    btnOK: TBitBtn;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    tWidth: TLabeledEdit;
    tHeight: TLabeledEdit;
    tSkipWidth: TLabeledEdit;
    tSkipHeight: TLabeledEdit;
    openPic: TOpenPictureDialog;
    Panel1: TPanel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
  public
    tileSet: TTileSet;
    procedure LoadImage(filename: string);
  end;

var frmBaseConfig: TfrmBaseConfig;

implementation

{$R *.lfm}

{ TfrmBaseConfig }

procedure TfrmBaseConfig.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if openPic.Execute then begin
    tileSet.texturefile:=extractfilename(openPic.FileName);
    LoadImage(openPic.FileName);
  end;
end;

procedure TfrmBaseConfig.btnOKClick(Sender: TObject);
begin
  tileSet.patternwidth:=strtointdef(tWidth.Text, 16);
  tileSet.patternheight:=strtointdef(tHeight.Text, 16);
  tileSet.skipwidth:=strtointdef(tSkipWidth.Text, 1);
  tileSet.skipheight:=strtointdef(tSkipHeight.Text, 1);
end;

procedure TfrmBaseConfig.FormCreate(Sender: TObject);
begin
  tileSet:=TTileSet.Create;
end;

procedure TfrmBaseConfig.FormDestroy(Sender: TObject);
begin
  tileSet.Free;
end;

procedure TfrmBaseConfig.LoadImage(filename: string);
var w, h: integer; ratio: single;
begin
  image1.Picture.LoadFromFile(filename);
  w:=image1.Picture.Width; h:=image1.Picture.Height;
  ratio:=h/w;
  if w>h then begin
    h:=round(w*ratio); w:=panel1.Width-4;
    image1.Left:=2;
    image1.Top:=(panel1.Height-h) div 2;
  end else begin
    w:=round(h/ratio); h:=panel1.Height-4;
    image1.Top:=2;
    image1.Left:=(panel1.Width-w) div 2;
  end;
  image1.Width:=w; image1.Height:=h;
end;

end.

