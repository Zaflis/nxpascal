unit uTileStyles;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, Buttons, StdCtrls, ExtCtrls, TileUnit, LCLType;

type

  { TfrmTileStyles }

  TfrmTileStyles = class(TForm)
    btnOk: TBitBtn;
    btn0: TButton;
    btn6: TButton;
    btn7: TButton;
    btn8: TButton;
    btnAdd: TButton;
    btnDelete: TButton;
    btnLoadFromSet: TButton;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    btn5: TButton;
    btnRename: TButton;
    eStyleName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    openD: TOpenDialog;
    pnlStyle: TPanel;
    styleList: TComboBox;
    GroupBox1: TGroupBox;
    procedure btn4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure btnLoadFromSetClick(Sender: TObject);
    procedure eStyleNameChange(Sender: TObject);
    procedure eStyleNameExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure styleListChange(Sender: TObject);
    procedure styleListDrawItem({%H-}Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  private
    function CHtoStr(const ch: shortint): string;
  public
    styles: TTileStyles;
    procedure UpdateControls;
  end;

var frmTileStyles: TfrmTileStyles;

implementation

{$R *.lfm}

{ TfrmTileStyles }

procedure TfrmTileStyles.FormCreate(Sender: TObject);
begin
  styles:=TTileStyles.Create;
  btnOk.Left:=(ClientWidth-btnOk.Width) div 2;
end;

procedure TfrmTileStyles.eStyleNameChange(Sender: TObject);
begin
  btnRename.Enabled:=eStyleName.Text<>styleList.Text;
end;

procedure TfrmTileStyles.eStyleNameExit(Sender: TObject);
begin
  eStyleName.Text:=trim(eStyleName.Text);
end;

procedure TfrmTileStyles.btnAddClick(Sender: TObject);
var n: integer;
begin
  n:=styles.IndexOf(eStyleName.Text);
  if n<0 then begin
    styleList.Items.Add(eStyleName.Text);
    styleList.ItemIndex:=styles.Add(eStyleName.Text);
    styleListChange(nil);
    btnRename.Enabled:=false;
    eStyleName.SetFocus;
  end else
    showmessage('Choose different name, this already exists.');
end;

procedure TfrmTileStyles.btn4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i, j, n: integer;
begin
  with styles.style[styleList.ItemIndex] do begin
    i:=(TButton(sender).Tag mod 3)-1;
    j:=(TButton(sender).Tag div 3)-1;
    n:=ch[i, j];
    if button=mbLeft then begin
      inc(n);
      if n>3 then n:=0;
    end else begin
      dec(n);
      if n<0 then n:=3;
    end;
    ch[i, j]:=n;
    TButton(sender).Caption:=CHtoStr(ch[i, j]);
  end;
end;

procedure TfrmTileStyles.btnDeleteClick(Sender: TObject);
begin
  if styleList.ItemIndex<2 then exit;
  styles.Delete(styleList.ItemIndex);
  UpdateControls;
  styleList.SetFocus;
end;

procedure TfrmTileStyles.btnRenameClick(Sender: TObject);
var n: integer;
begin
  n:=styles.IndexOf(eStyleName.Text);
  if n<0 then begin
    btnRename.Enabled:=false;
    with styles.style[styleList.ItemIndex] do
      name:=eStyleName.Text;
    styleList.Items[styleList.ItemIndex]:=eStyleName.Text;
  end else
    showmessage('Choose different name, this already exists.');
end;

procedure TfrmTileStyles.btnLoadFromSetClick(Sender: TObject);
var ts: TTileSet;
begin
  if openD.Execute then begin
    ts:=TTileSet.Create;
    ts.LoadFromFile(openD.FileName);
    styles.Assign(ts.styles);
    ts.Free;
    styleList.ItemIndex:=0; self.styleListChange(nil);
  end;
end;

procedure TfrmTileStyles.styleListChange(Sender: TObject);
begin
  pnlStyle.Enabled:=styleList.ItemIndex>1;
  btnRename.Enabled:=false;
  eStyleName.Text:=styles.style[styleList.ItemIndex].name;
  if styleList.ItemIndex>1 then
    with styles.style[styleList.ItemIndex] do begin
      btn0.Caption:=CHtoStr(ch[-1, -1]);
      btn1.Caption:=CHtoStr(ch[ 0, -1]);
      btn2.Caption:=CHtoStr(ch[ 1, -1]);
      btn3.Caption:=CHtoStr(ch[-1,  0]);
      btn4.Caption:=CHtoStr(ch[ 0,  0]);
      btn5.Caption:=CHtoStr(ch[ 1,  0]);
      btn6.Caption:=CHtoStr(ch[-1,  1]);
      btn7.Caption:=CHtoStr(ch[ 0,  1]);
      btn8.Caption:=CHtoStr(ch[ 1,  1]);
    end;
end;

procedure TfrmTileStyles.styleListDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with styleList.Canvas do begin
    if index<2 then font.Color:=clSilver
    else font.Color:=clBlack;
    TextOut(ARect.Left+2, ARect.Top, styleList.Items[index]);
  end;
end;

function TfrmTileStyles.CHtoStr(const ch: shortint): string;
begin
  case ch of
    0: result:='*';
    1: result:='T';
    2: result:='X';
    else result:='B';
  end;
end;

procedure TfrmTileStyles.UpdateControls;
var i, index: integer;
begin
  index:=styleList.ItemIndex;
  styleList.Items.Clear;
  for i:=0 to styles.Count-1 do
    styleList.Items.Add(styles.style[i].name);
  if index<0 then index:=0;
  if index>styles.Count-1 then index:=styles.Count-1;
  styleList.ItemIndex:=index;
  styleListChange(nil);
end;

procedure TfrmTileStyles.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var n: integer;
begin
  if key=VK_ESCAPE then close
  else if key=VK_PRIOR then begin // PgUp key
    eStyleName.SetFocus;
    n:=styleList.ItemIndex-1;
    if ((styleList.Items.Count>3) and (n<2)) or (n<0) then
      n:=styleList.Items.Count-1;
    styleList.ItemIndex:=n; styleListChange(nil);
  end else if key=VK_NEXT then begin // PgDn key
    eStyleName.SetFocus;
    n:=styleList.ItemIndex+1;
    if n>=styleList.Items.Count then
      if styleList.Items.Count>3 then n:=2
      else n:=0;
    styleList.ItemIndex:=n; styleListChange(nil);
  end;
end;

procedure TfrmTileStyles.FormShow(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmTileStyles.FormDestroy(Sender: TObject);
begin
  FreeAndNil(styles);
end;

end.
