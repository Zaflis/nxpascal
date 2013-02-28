unit uBlends;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  LCLType, Buttons, ExtCtrls, StdCtrls, uTileStyles, TileUnit;

type

  { TfrmBlends }

  TfrmBlends = class(TForm)
    btnConfigStyles: TButton;
    btnOk: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    blendList: TComboBox;
    Image1: TImage;
    Panel1: TPanel;
    Panel2: TPanel;
    ScrollBox1: TScrollBox;
    procedure btnConfigStylesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    tileset: TTileSetConfig;
    procedure Draw;
  end;

var
  frmBlends: TfrmBlends;

implementation

{$R *.lfm}

{ TfrmBlends }

procedure TfrmBlends.FormCreate(Sender: TObject);
begin
  tileset:=TTileSetConfig.Create;
  btnOk.Left:=(ClientWidth-btnOk.Width) div 2;
  ScrollBox1.Align:=alClient;
end;

procedure TfrmBlends.btnConfigStylesClick(Sender: TObject);
begin
  frmTileStyles.styles.Assign(tileset.styles);
  frmTileStyles.ShowModal;
  tileset.styles.Assign(frmTileStyles.styles);
  //UpdateControls; Draw;
end;

procedure TfrmBlends.FormDestroy(Sender: TObject);
begin
  FreeAndNil(tileset);
end;

procedure TfrmBlends.Draw;
begin
  //
end;

end.

