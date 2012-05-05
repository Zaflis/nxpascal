unit Main;

{$mode delphi}{$H+}

{$Message 'TODO'}
{
- Window dragging
- Element focusing for TabControl tabs subitems
- Texture sets saving and loading
- Texture property
- Font property

- Multiselect
  - Corner resizing
  - Drag moving

- UI component grid

- Particles
}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, StdCtrls, Buttons, ComCtrls, nxGL, nxGLUI, nxUI, nxTypes,
  dglOpenGL, LCLType, CheckLst, ExtDlgs, math, nxStrings;

type
  TEditorState = (esNone, esMove, esNew, esResize, esSelecting);

  { TForm1 }

  TForm1 = class(TForm)
    alphaScroll: TScrollBar;
    btnAcceptStrings: TButton;
    btnCloseStrings: TButton;
    btnParticles: TButton;
    btnPreviewTex: TButton;
    Button1: TButton;
    btnTextures: TButton;
    btnFonts: TButton;
    btnLayout: TButton;
    btnOpenStrings: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    btnAddTex: TButton;
    btnRemoveTex: TButton;
    btnReloadTex: TButton;
    chkEditable: TCheckBox;
    eTexName: TEdit;
    ePatW: TEdit;
    ePatH: TEdit;
    eSkipW: TEdit;
    eSkipH: TEdit;
    imgPreview: TImage;
    Label3: TLabel;
    Label5: TLabel;
    lblTexFile: TLabel;
    lblTexSize: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenPic: TOpenPictureDialog;
    tabTexPreview: TTabSheet;
    texList: TListBox;
    memoStrings: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    pnlTexInfo: TPanel;
    pnlEditable: TPanel;
    chkCentered: TCheckBox;
    chkEnabled: TCheckBox;
    chkVisible: TCheckBox;
    chkChecked: TCheckBox;
    chkParAlpha: TCheckBox;
    chkWrap: TCheckBox;
    chkPos: TCheckBox;
    chkSize: TCheckBox;
    ColorD: TColorDialog;
    cmbDrawStyle: TComboBox;
    eGroup: TEdit;
    eMax: TEdit;
    ePosition: TEdit;
    eHeight: TEdit;
    eLeft: TEdit;
    eName: TEdit;
    eText: TEdit;
    eTop: TEdit;
    eWidth: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblText: TLabel;
    lblText1: TLabel;
    lblText2: TLabel;
    lblText3: TLabel;
    lblText4: TLabel;
    lblText5: TLabel;
    lblText6: TLabel;
    lblText7: TLabel;
    MenuItem2: TMenuItem;
    menuDelete: TMenuItem;
    MenuItem4: TMenuItem;
    menuToFront: TMenuItem;
    menuToBack: TMenuItem;
    menuSnap: TMenuItem;
    OpenD: TOpenDialog;
    tabs: TPageControl;
    pnlWrap: TPanel;
    Panel4: TPanel;
    pnlScrollbar: TPanel;
    pnlSize: TPanel;
    pnlText: TPanel;
    pnlCentered: TPanel;
    pnlGroup: TPanel;
    pnlChecked: TPanel;
    pnlAlpha: TPanel;
    pnlStrings: TPanel;
    pnlDrawStyle: TPanel;
    SaveD: TSaveDialog;
    sColor: TShape;
    barSections: TScrollBar;
    selItemList: TComboBox;
    Menu1: TMainMenu;
    MenuItem1: TMenuItem;
    menuNew: TMenuItem;
    menuOpen: TMenuItem;
    menuSave: TMenuItem;
    MenuItem5: TMenuItem;
    menuExit: TMenuItem;
    MenuItem7: TMenuItem;
    pnlProperties: TPanel;
    pnlToolbar: TPanel;
    btnAdd: TSpeedButton;
    btnSel: TSpeedButton;
    tabTextures: TTabSheet;
    tabFonts: TTabSheet;
    tabParticles: TTabSheet;
    tabStrings: TTabSheet;
    tabObjects: TTabSheet;
    tabVisibility: TTabSheet;
    Timer1: TTimer;
    TreeView1: TTreeView;
    visItems: TCheckListBox;
    procedure alphaScrollChange(Sender: TObject);
    procedure alphaScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure barSectionsChange(Sender: TObject);
    procedure btnCloseStringsClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnAcceptStringsClick(Sender: TObject);
    procedure btnFontsClick(Sender: TObject);
    procedure btnLayoutClick(Sender: TObject);
    procedure btnParticlesClick(Sender: TObject);
    procedure btnPreviewTexClick(Sender: TObject);
    procedure btnTexturesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnOpenStringsClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure btnAddTexClick(Sender: TObject);
    procedure btnRemoveTexClick(Sender: TObject);
    procedure btnReloadTexClick(Sender: TObject);
    procedure chkCheckedChange(Sender: TObject);
    procedure chkCenteredChange(Sender: TObject);
    procedure chkEditableChange(Sender: TObject);
    procedure chkEnabledChange(Sender: TObject);
    procedure chkParAlphaChange(Sender: TObject);
    procedure chkPosChange(Sender: TObject);
    procedure chkSizeChange(Sender: TObject);
    procedure chkVisibleChange(Sender: TObject);
    procedure chkWrapChange(Sender: TObject);
    procedure cmbDrawStyleChange(Sender: TObject);
    procedure eGroupChange(Sender: TObject);
    procedure eLeftChange(Sender: TObject);
    procedure eMaxChange(Sender: TObject);
    procedure eNameChange(Sender: TObject);
    procedure ePatHChange(Sender: TObject);
    procedure ePatWChange(Sender: TObject);
    procedure ePositionChange(Sender: TObject);
    procedure eSkipHChange(Sender: TObject);
    procedure eSkipWChange(Sender: TObject);
    procedure eTexNameChange(Sender: TObject);
    procedure eTexNameExit(Sender: TObject);
    procedure eTextChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure imgPreviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure menuDeleteClick(Sender: TObject);
    procedure menuToFrontClick(Sender: TObject);
    procedure menuToBackClick(Sender: TObject);
    procedure menuNewClick(Sender: TObject);
    procedure menuExitClick(Sender: TObject);
    procedure menuOpenClick(Sender: TObject);
    procedure menuSaveClick(Sender: TObject);
    procedure pnlPropertiesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlPropertiesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure selItemListChange(Sender: TObject);
    procedure selItemListDropDown(Sender: TObject);
    procedure texListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure TreeView1SelectionChanged(Sender: TObject);
    procedure visItemsClick(Sender: TObject);
  private
    ui: TGLUI;
  public
    redraw,focMove: boolean;
    mx,my,cx,cy,mdx,mdy: integer;
    mb, selCount: integer;
    es: TEditorState;
    sel: array of TUIElement;
    fCon: TUIContainer;
    cRect,fRect, selArea: TBoundsRect;
    lasttick: cardinal;
    InfoUpdate, Modified: boolean;
    ProgDir: string;
    function IsSelected(e: TUIElement): boolean;
    procedure RefreshObjectList;
    procedure RefreshTexList;
    procedure ShowSelInfo;
  end; 

var
  Form1: TForm1;
  MaxSelCount: word = 16;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ShowSelInfo;
begin
  InfoUpdate:=true;
  // Begin updates
  pnlAlpha.Hide;
  if pnlProperties.Enabled then pnlProperties.Color:=clBtnFace
  else pnlProperties.Color:=cl3DDkShadow;
  eName.Enabled:=selCount=1;
  if selCount=1 then eName.Text:=sel[0].name
  else eName.Text:='';
  pnlProperties.Enabled:=selCount>0;
  if selCount=0 then exit;
  sColor.Brush.Color:=RGBToColor(
    round(sel[0].color.r*255),
    round(sel[0].color.g*255),
    round(sel[0].color.b*255));
  alphaScroll.Position:=round(100*(1-sel[0].color.a));
  chkPos.Checked:=sel[0].AbsPos; chkSize.Checked:=sel[0].AbsSize;
  pnlSize.Visible:=sel[0]<>ui;
  eLeft.Text:=format('%.3f',[sel[0].x]);
  eTop.Text:=format('%.3f',[sel[0].y]);
  eWidth.Text:=format('%.3f',[sel[0].width]);
  eHeight.Text:=format('%.3f',[sel[0].height]);
  chkEnabled.Checked:=sel[0].Enabled;
  chkVisible.Checked:=sel[0].Visible;
  chkParAlpha.Checked:=sel[0].ParentAlpha;
  pnlText.Visible:=(sel[0].eType=uiButton) or (sel[0].eType=uiLabel) or
    (sel[0].eType=uiCheckbox) or (sel[0].eType=uiEdit) or (sel[0].eType=uiWindow);
  if sel[0] is TUIButton then eText.Text:=TUIButton(sel[0]).caption
  else if sel[0] is TUILabel then eText.Text:=TUILabel(sel[0]).caption
  else if sel[0] is TUICheckBox then eText.Text:=TUICheckBox(sel[0]).caption
  else if sel[0] is TUIEdit then eText.Text:=TUIEdit(sel[0]).text
  else if sel[0] is TUIWindow then eText.Text:=TUIWindow(sel[0]).caption;
  pnlCentered.Visible:=(sel[0].eType=uiLabel) or (sel[0].eType=uiEdit)
    or (sel[0].eType=uiList);// or (sel[0].eType=uiMemo);
  if sel[0] is TUILabel then chkCentered.Checked:=TUILabel(sel[0]).centered
  else if sel[0] is TUIEdit then chkCentered.Checked:=TUIEdit(sel[0]).centered
  else if sel[0] is TUIList then chkCentered.Checked:=TUIList(sel[0]).centered;
  //else if sel is TUIMemo then chkCentered.Checked:=TUIMemo(sel[0]).centered;
  pnlChecked.Visible:=(sel[0].eType=uiCheckBox);
  if sel[0] is TUICheckBox then chkChecked.Checked:=TUICheckBox(sel[0]).checked;
  pnlGroup.Visible:=(sel[0].eType=uiCheckBox);
  if sel[0] is TUICheckBox then
    eGroup.Text:=inttostr(TUICheckBox(sel[0]).group);
  pnlScrollbar.Visible:=(sel[0].eType=uiScrollbar);
  if sel[0] is TUIScrollBar then begin
    eMax.Text:=inttostr(TUIScrollBar(sel[0]).max);
    ePosition.Text:=inttostr(TUIScrollBar(sel[0]).position);
  end;
  pnlWrap.Visible:=(sel[0].eType=uiMemo);
  if sel[0] is TUIMemo then chkWrap.Checked:=TUIMemo(sel[0]).wrap;
  pnlStrings.Visible:=(sel[0].eType=uiMemo) or (sel[0].eType=uiList) or
    (sel[0].eType=uiDropDown);
  if pnlStrings.Visible then begin
    if sel[0] is TUIMemo then
      memoStrings.Lines.Text:=TUIMemo(sel[0]).strings.Text
    else if sel[0] is TUIList then
      memoStrings.Lines.Text:=TUIList(sel[0]).items.Text
    else if sel[0] is TUIDropDown then
      memoStrings.Lines.Text:=TUIDropDown(sel[0]).list.items.Text;
  end else if tabStrings.Enabled then begin
    tabStrings.Enabled:=false; tabs.Hide;
  end;
  cmbDrawStyle.ItemIndex:=integer(sel[0].drawStyle);
  barSections.Visible:=cmbDrawStyle.ItemIndex=1;
  barSections.Position:=sel[0].bdSize;
  pnlEditable.Visible:=(sel[0].eType=uiDropDown);
  if sel[0] is TUIDropDown then chkEditable.Checked:=TUIDropDown(sel[0]).editable;
  // End of updates
  InfoUpdate:=false;
end;

procedure TForm1.alphaScrollChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  sel[0].color.a:=1-alphaScroll.Position/100;
  Modified:=true;
end;

procedure TForm1.alphaScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  alphaScrollChange(nil);
end;

procedure TForm1.barSectionsChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  sel[0].bdSize:=barSections.Position;
  redraw:=true; Modified:=true;
end;

procedure TForm1.btnCloseStringsClick(Sender: TObject);
begin
  tabStrings.Enabled:=false; tabs.Hide;
end;

procedure TForm1.btnAddClick(Sender: TObject);
begin
  if not selItemList.Focused then selItemList.SetFocus;
  es:=esNone; mb:=0;
  tabStrings.Enabled:=false; tabs.Hide;
  ShowSelInfo; redraw:=true;
end;

procedure TForm1.btnAcceptStringsClick(Sender: TObject);
begin
  if sel[0] is TUIMemo then begin
    TUIMemo(sel[0]).strings.Text:=memoStrings.Lines.Text;
    TUIMemo(sel[0]).UpdateSize;
  end else if sel[0] is TUIList then begin
    TUIList(sel[0]).items.Text:=memoStrings.Lines.Text;
    TUIList(sel[0]).UpdateSize;
  end else if sel[0] is TUIDropDown then begin
    TUIDropDown(sel[0]).list.items.Text:=memoStrings.Lines.Text;
    TUIDropDown(sel[0]).list.UpdateSize;
  end;
  tabStrings.Enabled:=false; tabs.Hide; Modified:=true;
end;

procedure TForm1.btnFontsClick(Sender: TObject);
begin
  if tabs.Visible and (not tabFonts.Enabled) then exit;
  tabFonts.Enabled:=not tabFonts.Enabled;
  tabs.Visible:=tabFonts.Enabled;
  if tabs.Visible then begin
    tabs.ActivePageIndex:=4;

  end;
end;

procedure TForm1.btnLayoutClick(Sender: TObject);
var i,n: integer;
begin
  if tabs.Visible and (not tabVisibility.Enabled) then exit;
  tabVisibility.Enabled:=not tabVisibility.Enabled;
  tabs.Visible:=tabVisibility.Enabled;
  if tabs.Visible then begin
    tabs.ActivePageIndex:=2;
    visItems.Enabled:=false;
    visItems.Items.Clear;
    for i:=0 to ui.count-1 do begin
      n:=visItems.Items.Add(ui.element[i].name);
      visItems.Checked[n]:=ui.element[i].Visible;
    end;
    visItems.Enabled:=true;
  end;
end;

procedure TForm1.btnParticlesClick(Sender: TObject);
begin
  if tabs.Visible and (not tabParticles.Enabled) then exit;
  tabParticles.Enabled:=not tabParticles.Enabled;
  tabs.Visible:=tabParticles.Enabled;
  if tabs.Visible then begin
    tabs.ActivePageIndex:=5;

  end;
end;

procedure TForm1.btnPreviewTexClick(Sender: TObject);
begin
  if tex.texture[texList.ItemIndex].filename<>'' then begin
    imgPreview.Picture.LoadFromFile(tex.texture[texList.ItemIndex].filename);
    tabs.ActivePageIndex:=6;
  end;
end;

procedure TForm1.btnTexturesClick(Sender: TObject);
begin
  if tabs.Visible and (not tabTextures.Enabled) then exit;
  tabTextures.Enabled:=not tabTextures.Enabled;
  tabs.Visible:=tabTextures.Enabled;
  if tabs.Visible then begin
    tabs.ActivePageIndex:=3;
    RefreshTexList;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if tabs.Visible and (not tabObjects.Enabled) then exit;
  if not selItemList.Focused then selItemList.SetFocus;
  tabObjects.Enabled:=not tabObjects.Enabled;
  tabs.Visible:=tabObjects.Enabled;
  if tabs.Visible then begin
    tabs.ActivePageIndex:=1;
    btnSel.Down:=true;
    RefreshObjectList;
  end;
  reDraw:=true;
end;

procedure TForm1.btnOpenStringsClick(Sender: TObject);
begin
  if tabs.Visible and (not tabStrings.Enabled) then exit;
  if sel[0] is TUIMemo then
    memoStrings.Lines.Text:=TUIMemo(sel[0]).strings.Text
  else if sel[0] is TUIList then
    memoStrings.Lines.Text:=TUIList(sel[0]).items.Text
  else if sel[0] is TUIDropDown then
    memoStrings.Lines.Text:=TUIDropDown(sel[0]).list.items.Text;
  tabStrings.Enabled:=true; tabs.Show;
  tabs.ActivePageIndex:=0; memoStrings.SetFocus;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  tabObjects.Enabled:=false; tabs.Hide;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  tabVisibility.enabled:=false; tabs.Hide;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  tabTextures.enabled:=false; tabs.Hide;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  tabFonts.enabled:=false; tabs.Hide;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  tabParticles.enabled:=false; tabs.Hide;
end;

procedure TForm1.btnAddTexClick(Sender: TObject);
var fn,ext: string; tr: boolean;
begin
  if OpenPic.Execute then begin
    fn:=extractfilename(OpenPic.FileName);
    ext:=uppercase(extractfileExt(fn));
    tr:=(ext='.PNG');
    if not fileexistsUTF8('data\'+fn) then
      copyfile(OpenPic.FileName,ProgDir+'data\'+fn);
    tex.AddTexture(fn,'data\'+fn,tr);
    RefreshTexList; Modified:=true;
  end;
end;

procedure TForm1.btnRemoveTexClick(Sender: TObject);
begin
  if texList.Items[texList.ItemIndex][1]='*' then exit;
  tex.RemoveTexture(texList.ItemIndex);
  RefreshTexList;
  pnlTexInfo.Enabled:=false; Modified:=true;
end;

procedure TForm1.btnReloadTexClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to tex.count-1 do
    with tex.texture[i] do
      tex.ReloadTexture(i,filename,values>3);
  ui.LoadTextures('data\');
end;

procedure TForm1.chkCheckedChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  if sel[0] is TUICheckBox then
    TUICheckBox(sel[0]).checked:=chkChecked.Checked;
  Modified:=true;
end;

procedure TForm1.chkCenteredChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  if sel[0] is TUILabel then TUILabel(sel).centered:=chkCentered.Checked
  else if sel[0] is TUIEdit then TUIEdit(sel).centered:=chkCentered.Checked
  else if sel[0] is TUIList then TUIList(sel).centered:=chkCentered.Checked;
  //else if sel[0] is TUIMemo then TUIMemo(sel).centered:=chkCentered.Checked;
  Modified:=true;
end;

procedure TForm1.chkEditableChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  if sel[0] is TUIDropDown then begin
    TUIDropDown(sel[0]).editable:=chkEditable.Checked;
  end;
  Modified:=true;
end;

procedure TForm1.chkEnabledChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  sel[0].enabled:=chkEnabled.Checked; Modified:=true;
end;

procedure TForm1.chkParAlphaChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  sel[0].ParentAlpha:=chkParAlpha.Checked; Modified:=true;
end;

procedure TForm1.chkPosChange(Sender: TObject);
//var r,cr: TBoundsRect;
begin
  if InfoUpdate then exit;
  redraw:=true;
  //r:=sel.GetDrawRect; cr:=sel.parent.GetDrawRect;
  sel[0].AbsPos:=chkPos.Checked;
  {if sel.AbsPos then begin
    sel.x:=r.x-cr.x; sel.y:=r.y-cr.y;
  end else begin
    sel.x:=sel.x/cr.w;
    sel.y:=sel.y/cr.h;
  end;  }
  ShowSelInfo; Modified:=true;
end;

procedure TForm1.chkSizeChange(Sender: TObject);
//var r,cr: TBoundsRect;
begin
  if InfoUpdate then exit;
  redraw:=true;
  //r:=sel.GetDrawRect; cr:=sel.parent.GetDrawRect;
  sel[0].AbsSize:=chkSize.Checked;
  {if sel.AbsSize then begin
    sel.width:=r.w;
    sel.height:=r.h;
  end else begin
    sel.width:=sel.width/cr.w;
    sel.height:=sel.height/cr.h;
  end; }
  ShowSelInfo; Modified:=true;
end;

procedure TForm1.chkVisibleChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  sel[0].Visible:=chkVisible.Checked; Modified:=true;
end;

procedure TForm1.chkWrapChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  if sel[0] is TUIMemo then begin
    TUIMemo(sel[0]).wrap:=chkWrap.Checked;
    TUIMemo(sel[0]).UpdateSize;
  end;
  Modified:=true;
end;

procedure TForm1.cmbDrawStyleChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  sel[0].drawStyle:=TUIDrawStyle(cmbDrawStyle.ItemIndex);
  barSections.Visible:=cmbDrawStyle.ItemIndex=1;
  redraw:=true; Modified:=true;
end;

procedure TForm1.eGroupChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  if sel[0] is TUICheckbox then
    TUICheckbox(sel[0]).group:=strtoint2(eGroup.Text);
  redraw:=true; Modified:=true;
end;

procedure TForm1.eLeftChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  sel[0].x:=StrToFloat2(eLeft.Text);
  sel[0].y:=StrToFloat2(eTop.Text);
  sel[0].width:=StrToFloat2(eWidth.Text);
  sel[0].height:=StrToFloat2(eHeight.Text);
  redraw:=true; Modified:=true;
end;

procedure TForm1.eMaxChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  if sel[0] is TUIScrollbar then
    TUIScrollbar(sel[0]).max:=strtoint2(eMax.Text);
  redraw:=true; Modified:=true;
end;

procedure TForm1.eNameChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  if trim(eName.Text)<>'' then
    sel[0].name:=eName.Text;
  if tabObjects.Enabled then RefreshObjectList;
  redraw:=true; Modified:=true;
end;

procedure TForm1.ePatHChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  tex.texture[texList.ItemIndex].PatternHeight:=strtoint2(ePatH.Text);
end;

procedure TForm1.ePatWChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  tex.texture[texList.ItemIndex].PatternWidth:=strtoint2(ePatW.Text);
end;

procedure TForm1.ePositionChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  if sel[0] is TUIScrollbar then
    TUIScrollbar(sel[0]).position:=strtoint2(ePosition.Text);
  redraw:=true; Modified:=true;
end;

procedure TForm1.eSkipHChange(Sender: TObject);
begin
  tex.texture[texList.ItemIndex].SkipHeight:=strtoint2(eSkipH.Text);
end;

procedure TForm1.eSkipWChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  tex.texture[texList.ItemIndex].SkipWidth:=strtoint2(eSkipW.Text);
end;

procedure TForm1.eTexNameChange(Sender: TObject);
var s: string;
begin
  if InfoUpdate then exit;
  s:=trim(eTexName.Text);
  if (copy(s,1,1)<>'*') and (copy(s,1,1)<>'$') and (copy(s,1,3)<>'ui_') then
    tex.texture[texList.ItemIndex].name:=s;
end;

procedure TForm1.eTexNameExit(Sender: TObject);
begin
  RefreshTexList;
end;

procedure TForm1.eTextChange(Sender: TObject);
begin
  if InfoUpdate then exit;
  if sel[0] is TUIButton then
    TUIButton(sel[0]).caption:=eText.Text
  else if sel[0] is TUILabel then
    TUILabel(sel[0]).caption:=eText.Text
  else if sel[0] is TUICheckBox then
    TUICheckBox(sel[0]).caption:=eText.Text
  else if sel[0] is TUIEdit then
    TUIEdit(sel[0]).text:=eText.Text
  else if sel[0] is TUIWindow then
    TUIWindow(sel[0]).caption:=eText.Text;
  redraw:=true; Modified:=true;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  nx.KillGLWindow;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var res: integer;
begin
  if Modified then begin
    CanClose:=false;
    res:=MessageDlg('Modified',
      'There are some unsaved changes. Do you want to save them?',
      mtConfirmation, mbYesNoCancel, 0);
    if res=IDNO then CanClose:=true
    else if res=IDYES then
      if saveD.Execute then begin
        ui.SaveToFile(saveD.FileName);
        Modified:=false; CanClose:=true;
      end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  nxUIEditorMode:=true;
  ProgDir:=extractfilepath(application.ExeName);
  openPic.InitialDir:=progdir+'data\';
  Width:=900; Height:=650;
  tabs.ActivePageIndex:=0; tabs.ShowTabs:=false;
  tex.Options:=tex.Options+[toFileName];
  nx.CreateGlWindow(self);
  //pnlObjects.BringToFront;
  nx.CreateBasicFont;
  nx.Font[0].SetColor(0,0,0);
  //nx.CreateFont('Arial',14,512);
  ui:=TGLUI.Create(ProgDir+'data\');
  tex.AddTexture('$ editor_focus',ProgDir+'editor_data\editor_buttons.png',true);
  tex.SetPattern(tex.IndexOf('$ editor_focus'),32,32,0,0);
  pnlAlpha.Width:=alphaScroll.Width; pnlAlpha.Height:=alphaScroll.Height;

  visItems.Align:=alClient;
  memoStrings.Align:=alClient;
  TreeView1.Align:=alClient;
  texList.Align:=alClient;
  imgPreview.Align:=alClient;

  setlength(sel, MaxSelCount);
  selCount:=0; fCon:=ui;
  redraw:=true; es:=esNone;
  ShowSelInfo;
  timer1.Enabled:=true;
  if not nx.AllOK then ShowMessage(nx.LastError);
  //caption:=inttostr(length('test'+TextCutChar))+'/5';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ui.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if memoStrings.Focused then exit;
  if key=VK_RETURN then begin
    button1.SetFocus; exit;
  end;
  if key=VK_F1 then begin
    btnSel.Down:=true; btnAddClick(btnSel);
  end else if key=VK_F2 then begin
    btnAdd.Down:=true; btnAddClick(btnAdd);
  end;
  if eName.Focused or eText.Focused or eLeft.Focused or eTop.Focused
    or eWidth.Focused or eHeight.Focused or eGroup.Focused
    or eMax.Focused or ePosition.Focused then exit;

  if (key=VK_DELETE) and btnSel.Down then begin
    if eGroup.Focused or eMax.Focused or ePosition.Focused then
      exit;
    if sel[0]<>ui then begin
      if sel[0]=ui.focus then ui.focus:=nil;
      TUIContainer(sel[0].parent).Delete(
        TUIContainer(sel[0].parent).IndexOf(sel[0].name));
      selCount:=0; fCon:=ui;
      if tabObjects.Enabled then begin
        RefreshObjectList;
        if ui.count=0 then begin
          btnAdd.Down:=true; btnAddClick(nil);
        end;
      end else ShowSelInfo;
      redraw:=true;
      Modified:=true;
    end;
  end;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var mp: TPoint; x1,y1: single;
begin
  if (mb>0) or (sender<>nx.window) then exit;
  if tabs.Visible then exit;
  if not selItemList.Focused then selItemList.SetFocus;
  x1:=nx.window.Width/100;
  y1:=nx.window.Height/80;
  if menuSnap.Checked then begin
    mx:=round(round(mx/x1)*x1);
    my:=round(round(my/y1)*y1);
  end;
  selArea.x:=mx; selArea.y:=my;
  if button=mbLeft then begin
    cx:=mx; cy:=my; mb:=1;
    if btnAdd.Down then begin
      es:=esNew;
    end else if btnSel.Down then begin
      if (ui.focus<>nil) and (fCon<>nil) then begin
        sel[0]:=ui.focus; ShowSelInfo;
        if ui.focus<>ui then begin
          if ui.focus=fCon then begin
            fCon:=TUIContainer(fCon.parent); cRect:=fCon.GetDrawRect;
          end;
          if focMove then begin
            es:=esMove;
            mdx:=mx-fRect.x;
            mdy:=my-fRect.y;
            if menuSnap.Checked then begin
              mdx:=round(round(mdx/x1)*x1);
              mdy:=round(round(mdy/y1)*y1);
            end;
          end else begin
            es:=esResize;
            cx:=fRect.x; cy:=fRect.y;
            mp.x:=ClientOrigin.x+fRect.x+fRect.w-1+nx.window.Left;
            mp.y:=ClientOrigin.y+fRect.y+fRect.h-1+nx.window.Top;
            mouse.CursorPos:=mp;
          end;
        end;
      end;
    end;
  end else
    mb:=2;
  reDraw:=true;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);

  procedure ResizeItems;
  begin
    if sel[0] is TUIScrollBar then begin
      with TUIScrollBar(sel[0]) do begin
        vertical:=DrawH>DrawW; UpdateSize;
      end;
    end else
      if sel[0] is TUIList then TUIList(sel[0]).UpdateSize
      else if sel[0] is TUIMemo then TUIMemo(sel[0]).UpdateSize
      else if sel[0] is TUIDropDown then TUIDropDown(sel[0]).UpdateSize
      else if sel[0] is TUITabControl then TUITabControl(sel[0]).UpdateSize
      else if sel[0] is TUIContainer then TUIContainer(sel[0]).UpdateSize;
  end;

var d1,d2: single; x1,y1: single;
begin
  if ui=nil then exit;
  mx:=x; my:=y;
  if menuSnap.Checked and (mb<>0) then begin
    x1:=nx.window.Width/100;
    y1:=nx.window.Height/80;
    mx:=trunc(round(mx/x1)*x1);
    my:=trunc(round(my/y1)*y1);
    //mx:=x div 5*5; my:=y div 5*5;
  end;
  if selItemList.Tag=1 then begin
    selItemList.Tag:=0; reDraw:=true;
  end else if mb>0 then reDraw:=true;

  if mb=1 then begin
    // Move element
    if es=esMove then begin
      Modified:=true;
      sel[0].x:=mx-mdx-cRect.x;
      sel[0].y:=my-mdy-cRect.y;
      if sel[0].x+fRect.w>cRect.w then sel[0].x:=cRect.w-fRect.w;
      if sel[0].y+fRect.h>cRect.h then sel[0].y:=cRect.h-fRect.h;
      if not sel[0].AbsPos then begin
        sel[0].x:=sel[0].x/cRect.w;
        sel[0].y:=sel[0].y/cRect.h;
      end;
      if sel[0].x<0 then sel[0].x:=0;
      if sel[0].y<0 then sel[0].y:=0;
      ResizeItems;

    // Resize or create new element
    end else if (es=esResize) or (es=esNew) then begin
      Modified:=true;
      if mx<cx+4 then mx:=cx+4
      else if mx>=cRect.x+cRect.w then mx:=cRect.x+cRect.w-1;
      if my<cy+4 then my:=cy+4
      else if my>=cRect.y+cRect.h then my:=cRect.y+cRect.h-1;
      if es=esResize then begin
        sel[0].width:=(mx+1-cRect.x)-(fRect.x-cRect.x);
        sel[0].height:=(my+1-cRect.y)-(fRect.y-cRect.y);
        if not sel[0].AbsSize then begin
          sel[0].width:=sel[0].width/cRect.w;
          sel[0].height:=sel[0].height/cRect.h;
        end;
        ResizeItems;
      end;
    end;
    reDraw:=true;

  // Get cursor focus when no mouse pressed
  end else if mb=0 then begin       {$Message 'TabControl focusing'}
    ui.focus:=ui.GetFocus(mx,my);
    if ui.focus<>nil then begin
      if ui.focus is TUIContainer then fCon:=TUIContainer(ui.focus)
      else fCon:=TUIContainer(ui.focus.parent);
      cRect:=fCon.GetDrawRect;
      fRect:=ui.focus.GetDrawRect;
      if ui.focus<>ui then begin
        d1:=hypot(fRect.x-2-mx,fRect.y-2-my);
        d2:=hypot(fRect.x+fRect.w+2-mx,fRect.y+fRect.h+2-my);
        focMove:=d1<d2;
      end;
    end;
    reDraw:=true;
  end;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var x1,y1,w,h: single; e: TUIElement;
begin
  if tabs.Visible then exit;
  selArea.w:=mx-selArea.x+1; selArea.y:=my-selArea.y+1;
  if (mb=1) and (es=esNew) then begin
    Modified:=true;
    x1:=(cx-cRect.x)/cRect.w;
    y1:=(cy-cRect.y)/cRect.h;
    w:=((mx+1-cRect.x)-(cx-cRect.x))/cRect.w;
    h:=((my+1-cRect.y)-(cy-cRect.y))/cRect.h;
    e:=nil;
    case selItemList.ItemIndex of
      0: begin // Button
           e:=TUIButton.Create(fCon.NewName('Button')); fCon.Add(e);
           with TUIButton(e) do begin
             x:=x1; y:=y1; width:=w; height:=h;
           end;
         end;
      1: begin // Checkbox
           e:=TUICheckBox.Create(fCon.NewName('CheckBox')); fCon.Add(e);
           with TUICheckBox(e) do begin
             x:=x1; y:=y1; width:=w; height:=h;
           end;
         end;
      2: begin // Dropdown list
           e:=TUIDropDown.Create(fCon.NewName('DropDown')); fCon.Add(e);
           with TUIDropDown(e) do begin
             x:=x1; y:=y1; width:=w; height:=h;
             UpdateSize;
           end;
         end;
      3: begin // Edit
           e:=TUIEdit.Create(fCon.NewName('Edit')); fCon.Add(e);
           with TUIEdit(e) do begin
             x:=x1; y:=y1; width:=w; height:=h;
           end;
         end;
      4: begin // Image
           e:=TUIImage.Create(fCon.NewName('Image')); fCon.Add(e);
           with TUIImage(e) do begin
             x:=x1; y:=y1; width:=w; height:=h;
           end;
         end;
      5: begin // Label
           e:=TUILabel.Create(fCon.NewName('Label')); fCon.Add(e);
           with TUILabel(e) do begin
             x:=x1; y:=y1; width:=w; height:=h;
           end;
         end;
      6: begin // List
           e:=TUIList.Create(fCon.NewName('List')); fCon.Add(e);
           with TUIList(e) do begin
             x:=x1; y:=y1; width:=w; height:=h;
             UpdateSize;
           end;
         end;
      7: begin // Memo
           e:=TUIMemo.Create(fCon.NewName('Memo')); fCon.Add(e);
           with TUIMemo(e) do begin
             x:=x1; y:=y1; width:=w; height:=h;
             UpdateSize;
           end;
         end;
      8: begin // Panel
           e:=TUIPanel.Create(fCon.NewName('Panel')); fCon.Add(e);
           with TUIPanel(e) do begin
             x:=x1; y:=y1; width:=w; height:=h;
           end;
           fCon:=TUIContainer(e);
         end;
      9: begin // ScrollBar
           e:=TUIScrollBar.Create(fCon.NewName('ScrollBar'));
           fCon.Add(e);
           with TUIScrollBar(e) do begin
             x:=x1; y:=y1; width:=w; height:=h;
             vertical:=DrawH>DrawW;
             UpdateSize;
           end;
         end;
      10: begin // TabControl
            e:=TUITabControl.Create(fCon.NewName('TabControl')); fCon.Add(e);
            with TUITabControl(e) do begin
              x:=x1; y:=y1; width:=w; height:=h;
              UpdateSize;
            end;
          end;
      11: begin // Window
            e:=TUIWindow.Create(fCon.NewName('Window')); fCon.Add(e);
            with TUIWindow(e) do begin
              x:=x1; y:=y1; width:=w; height:=h;
            end;
      end;
    end;
    {if e<>nil then begin
      sel[0]:=e; selCount:=1;
    end;
    btnSel.Down:=true; btnAddClick(nil);}
  end;
  mb:=0; es:=esNone; redraw:=true;
  ShowSelInfo;

  // Update mouse focus
  FormMouseMove(sender,Shift,x,y);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  ui.UpdateSize;
  redraw:=true;
end;

procedure TForm1.imgPreviewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  tabs.ActivePageIndex:=3;
end;

procedure TForm1.menuDeleteClick(Sender: TObject);
var key: word;
begin
  key:=VK_DELETE; FormKeyDown(sender,key,[]);
end;

procedure TForm1.menuExitClick(Sender: TObject);
begin
  close;
end;

procedure TForm1.menuToFrontClick(Sender: TObject);
begin
  if (sel[0]<>nil) and (sel[0]<>ui) then begin
    TUIContainer(sel[0].parent).MoveToFront(sel[0].Index);
    if tabObjects.Enabled then RefreshObjectList;
  end;
end;

procedure TForm1.menuToBackClick(Sender: TObject);
begin
  if (sel[0]<>nil) and (sel[0]<>ui) then begin
    TUIContainer(sel[0].parent).MoveToBack(sel[0].Index);
    if tabObjects.Enabled then RefreshObjectList;
  end;
end;

procedure TForm1.menuNewClick(Sender: TObject);
var res: integer;
begin
  if Modified then begin
    res:=MessageDlg('Modified',
      'There are some unsaved changes. Do you want to save them?',
      mtConfirmation, mbYesNoCancel, 0);
    if res=IDCANCEL then exit
    else if res=IDYES then
      if saveD.Execute then begin
        ui.SaveToFile(saveD.FileName);
        Modified:=false;
      end else
        exit;
  end;
  ui.Free;
  ui:=TGLUI.CreateByName;
  selCount:=0; fCon:=ui;
  btnAdd.Down:=true; btnAddClick(nil);
  Modified:=false;
end;

procedure TForm1.menuOpenClick(Sender: TObject);
var res: integer;
begin
  if Modified then begin
    res:=MessageDlg('Modified',
      'There are some unsaved changes. Do you want to save them?',
      mtConfirmation, mbYesNoCancel, 0);
    if res=IDCANCEL then exit
    else if res=IDYES then
      if saveD.Execute then begin
        ui.SaveToFile(saveD.FileName);
        Modified:=false;
      end else
        exit;
  end;
  if (sender=nil) or openD.Execute then begin
    ui.LoadFromFile(openD.FileName);
    selCount:=0; fCon:=ui;
    btnSel.Down:=true; btnAddClick(nil);
    saveD.FileName:=openD.FileName;
    if nx.LastError<>'' then begin
      showmessage(nx.LastError);
      nx.ClearError;
    end;
    Modified:=false;
  end;
end;

procedure TForm1.menuSaveClick(Sender: TObject);
begin
  if saveD.Execute then begin
    ui.SaveToFile(saveD.FileName);
    Modified:=false;
  end;
end;

procedure TForm1.pnlPropertiesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  pnlAlpha.Hide;
  button1.SetFocus;
end;

procedure TForm1.pnlPropertiesMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ui=nil then exit;
  ui.focus:=nil; fCon:=nil; mb:=0;
end;

procedure TForm1.RefreshObjectList;
  procedure AddContainer(c: TUIContainer; child: TTreeNode);
  var i: integer; n: TTreeNode;
  begin
    for i:=0 to c.count-1 do begin
      n:=TreeView1.Items.AddChild(child,c.element[i].name);
      if c.element[i] is TUIContainer then
        AddContainer(TUIContainer(c.element[i]),n);
      if (selCount>0) and (c.element[i]=sel[0]) then begin
        TreeView1.Selected:=n;
      end;
    end;
  end;
begin
  TreeView1.Items.Clear;
  AddContainer(ui,TreeView1.Items.Add(nil,ui.name));
  if selCount=0 then begin
    treeview1.Selected:=treeview1.Items.Item[0];
  end;
end;

procedure TForm1.RefreshTexList;
var i: integer; s: string;
begin
  texList.Items.Clear;
  for i:=0 to tex.count-1 do begin
    s:=tex.texture[i].name;
    if tex.texture[i].filename<>'' then
      s:='['+UpperCase(ExtractFileExt(tex.texture[i].filename))+'] '+s;
    if copy(tex.texture[i].name,1,3)='ui_' then s:='* '+s
    else if copy(tex.texture[i].name,1,1)='$' then s:='* '+s;
    texList.Items.Add(s);
  end;
end;

procedure TForm1.sColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if pnlAlpha.Visible then
    pnlAlpha.Hide
  else if button=mbLeft then begin
    ColorD.Color:=sColor.Brush.Color;
    if ColorD.Execute then begin
      sColor.Brush.Color:=ColorD.Color;
      sel[0].color.r:=Red(sColor.Brush.Color)/255;
      sel[0].color.g:=Green(sColor.Brush.Color)/255;
      sel[0].color.b:=Blue(sColor.Brush.Color)/255;
      Modified:=true;
    end;
  end else begin
    pnlAlpha.Show;
  end;
  redraw:=true;
end;

procedure TForm1.selItemListChange(Sender: TObject);
begin
  btnAdd.Down:=true;
  btnAddClick(nil);
end;

procedure TForm1.selItemListDropDown(Sender: TObject);
begin
  selItemList.Tag:=1;
  // To force redraw behind list, after list hides (in MouseMove)
end;

procedure TForm1.texListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if InfoUpdate then exit;
  InfoUpdate:=true;
  pnlTexInfo.Enabled:=texList.Items[texList.ItemIndex][1]<>'*';
  with tex.texture[texList.ItemIndex] do begin
    lblTexFile.Caption:=filename;
    lblTexFile.Hint:=filename;
    eTexName.Text:=name;
    lblTexSize.Caption:=sysutils.format('%d:%d [%d:%d]',[width,height,sizex,sizey]);
    ePatW.Text:=inttostr(patternWidth);
    ePatH.Text:=inttostr(patternHeight);
    eSkipW.Text:=inttostr(skipWidth);
    eSkipH.Text:=inttostr(skipHeight);
  end;
  InfoUpdate:=false;
end;

procedure TForm1.Timer1Timer(Sender: TObject);

  procedure DrawCorners(r: TBoundsRect);
  begin
    glPushMatrix;
    glTranslatef(0.5,0.5,0);
    glBegin(GL_LINE_STRIP);
      glVertex2i(r.x+r.w div 8, r.y);
      glVertex2i(r.x, r.y);
      glVertex2i(r.x, r.y+r.h div 8);
    glEnd;
    glBegin(GL_LINE_STRIP);
      glVertex2i(r.x+r.w*7 div 8, r.y+r.h-1);
      glVertex2i(r.x+r.w-1, r.y+r.h-1);
      glVertex2i(r.x+r.w-1, r.y+r.h*7 div 8);
    glEnd;
    glPopMatrix;
  end;

var t: cardinal; r: TBoundsRect; c: single; i: integer;
begin
  if not nx.Initialized then exit;

  if tag=0 then begin
    tag:=1;
    if paramCount>0 then begin
      openD.FileName:=paramStr(1);
      menuOpenClick(nil);
    end;
  end;

  t:=nx.GetTick;
  if t>lasttick+1000 then begin
    lasttick:=t; redraw:=true;
  end;
  //if not redraw then exit;
  //redraw:=false;

  nx.Clear(true,false);
  nx.Enable2D;
  ui.Draw;

  {glColor3f(0,0,0); tex.SetTex(tex.IndexOf('ui_button'));
  nx.DrawScaled(20,100,130,30,0);
  nx.Draw(10,10,1);  }

  //glColor3f(0,0,0);
  //nx.Font[0].SetTexture; nx.Font[0].DrawC(100,100,'test');

  tex.SetTex(-1);

  c:=sin(t*toRad*0.5)*0.5+0.5;

  if fCon<>nil then begin
    glColor3f(c,c,c);
    DrawCorners(cRect);
  end;

  for i:=0 to selCount-1 do begin
    r:=sel[i].GetDrawRect;
    glColor3f(0,0.5+0.5*c,0);
    DrawCorners(r);
  end;

  if mb=1 then
    if es=esNew then begin
      glColor3f(0,1,0);
      glBegin(GL_LINE_LOOP);
        glVertex2f(cx,cy);
        glVertex2f(mx,cy);
        glVertex2f(mx,my);
        glVertex2f(cx,my);
      glEnd;
    end;

  if (ui.focus<>nil) and (ui.focus<>ui) then begin
    glColor4f(1,1,0,c);
    DrawCorners(fRect);

    if btnSel.Down then begin
      // Move/Resize icon
      glColor4f(1,1,1,0.7+0.3*c);
      tex.SetByName('$ editor_focus');
      if focMove then
        nx.Draw(fRect.x,fRect.y,0)
      else
        nx.Draw(fRect.x+fRect.w-32,fRect.y+fRect.h-32,1);
    end;
  end;

  // Cursor test
  {glColor3f(0,1,0);
  glBegin(GL_LINES);
    glVertex2f(mx-10,my);
    glVertex2f(mx+10,my);
    glVertex2f(mx,my-10);
    glVertex2f(mx,my+10);
  glEnd;}

  nx.Disable2D;
  nx.Flip;
end;

procedure TForm1.TreeView1SelectionChanged(Sender: TObject);
var indices: array[0..30] of integer; pCount: integer;
  procedure GetIndices(n: TTreeNode);
  begin
    indices[pCount]:=n.Index; inc(pCount);
    if n.Parent<>nil then GetIndices(n.Parent);
  end;
var i: integer; c: TUIContainer;
begin
  if (treeview1.Selected=nil) or InfoUpdate then exit;
  pCount:=0;
  GetIndices(treeview1.Selected);
  if pCount<=1 then selCount:=0
  else begin
    c:=ui;
    for i:=pcount-2 downto 0 do
      c:=TUIContainer(c.element[indices[i]]);
    selCount:=1; sel[0]:=c;
    if sel[0] is TUIContainer then fCon:=TUIContainer(sel[0]);
  end;
  ShowSelInfo;
end;

procedure TForm1.visItemsClick(Sender: TObject);
var i: integer;
begin
  if not visItems.Enabled then exit;
  i:=visItems.ItemIndex;
  if (i>=0) and (i<visItems.Items.Count) then begin
    ui.element[i].Visible:=visItems.Checked[i];
  end;
end;

function TForm1.IsSelected(e: TUIElement): boolean;
var i: integer;
begin
  result:=false;
  for i:=0 to selCount-1 do
    if sel[i]=e then begin
      result:=true; exit;
    end;
end;

end.

