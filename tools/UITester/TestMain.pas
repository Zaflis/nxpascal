unit TestMain;

{$mode objfpc}{$H+}

interface

{ TODO:

- dropdown opening up-side
- memo cursor blink
- memo typing
- window: dragging,closing
- better cursor focus for edit
}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  nxGL, types, nxGraph, nxUI, nxGLUI;

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    sEdit: TUIEdit;
    lblFPS: TUILabel;
  public
    ui: TGLUI;
    procedure UIClick(sender: TObject; button: TMouseButton);
    procedure UIScroll(sender: TObject);
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var e: TUIElement; bar: TUIScrollBar;
begin
  clientwidth:=800; clientheight:=600;
  nx.CreateGlWindow(self);
  ui:=TGLUI.Create('data\');
  ui.LoadFromFile('test.ui');
  timer1.Enabled:=true;
  caption:=caption+format(' - GFXmem: %d bytes in %d textures',
    [tex.GetMemoryUsage,tex.count]);
  sEdit:=TUIEdit(ui.Find('sEdit'));
  bar:=TUIScrollBar(ui.Find('bar'));
  bar.onScroll:=@UIScroll;
  UIScroll(bar);
  lblFPS:=TUILabel(ui.Find('lblFPS'));
  e:=ui.Find('Panel1');
  if e<>nil then e.onMouseDown:=@UIClick;
  ui.onMouseDown:=@UIClick;
  ui.Visible:=false;
  ui.FadeIn(ui,1000);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ui.FadeFinished and ui.Visible then begin
    ui.FadeOut(ui,1000); CanClose:=false;
  end;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  ui.KeyDown(key,shift);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  ui.KeyPress(key);
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ui.MouseDown(button);
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  ui.MouseMove(x,y);
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ui.MouseUp(button);
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ui.MouseWheel(WheelDelta);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  ui.UpdateSize;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if not nx.Initialized then exit;
  if lblFPS<>nil then
    lblFPS.caption:=format('FPS: %d',[nx.FPS]);
  nx.Clear(true,true);
  nx.Enable2D;
  ui.Draw;
  //nx.SetFont(0);
  {with nx.Font[0] do begin
    nx.SetColor(0,0,0);
    Draw(8,8,format('FPS: %d',[nx.FPS]));
  end;}
  nx.Disable2D;
  nx.Flip;
  if ui.Visible=false then close;
end;

procedure TForm1.UIClick(sender: TObject; button: TMouseButton);
begin
  ui.FadeOut(TUIElement(sender),500);
end;

procedure TForm1.UIScroll(sender: TObject);
begin
  if sEdit<>nil then
    sEdit.text:=inttostr(TUIScrollBar(sender).position);
end;

end.

