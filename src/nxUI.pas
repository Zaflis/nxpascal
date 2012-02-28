unit nxUI;

{$H+}

interface

uses Classes, Controls, SysUtils, {$IFDEF fpc}FileUtil, LCLtype
  {$ELSE}Windows{$ENDIF}, math, nxStrings, nxGraph, nxTypes;

{$Message 'TODO'}
{
- save/load TextureSet
- window and tabcontrol mouse events
- window moving
- TMemo writing and text selecting
- TEdit selecting
- add message dialogs (message, question)
- (particles?)
}

type
  TUIElementType = (uiUndefined, uiButton, uiCheckBox, uiDropDown, uiEdit,
    uiImage, uiLabel, uiList, uiMainPanel, uiMemo, uiPanel, uiScrollBar,
    uiTab, uiTabControl, uiWindow, uiGrid, uiTreeView);

  TUIMouseMove = procedure(sender: TObject; mx, my: integer) of object;
  TUIMouseClick = procedure(sender: TObject; button: TMouseButton) of object;
  TUIEvent = procedure(sender: TObject) of object;

  TUIDrawStyle = (uiScaled, uiSections, uiRepeat);
  // Section are read as 3x3 grid, divided by default 25%/50%/25% texture size.
  // Middle parts are scaled and borders kept pixel-perfect

  { TUIElement }

  TUIElement = class
  private
    FFocused, FAbsPos, FAbsSize: boolean;
    procedure SetAbsPos(enable: boolean);
    procedure SetAbsSize(enable: boolean);
    procedure SetFocus(enable: boolean);
  public
    eType: TUIElementType;
    name: string;
    x,y,width,height: single;
    color: TfRGBA;
    Visible, Enabled, ParentAlpha: boolean;
    parent: TUIElement;
    drawStyle: TUIDrawStyle;
    bdSize: byte; // Used by section drawing
    onMouseMove: TUIMouseMove;
    onMouseDown, onMouseUp: TUIMouseClick;
    onBeforeDraw, onAfterDraw: TUIEvent;
    property AbsPos: boolean read FAbsPos write SetAbsPos;
    property AbsSize: boolean read FAbsSize write SetAbsSize;
    property Focused: boolean read FFocused write SetFocus;
    constructor Create(const _name: string; const _eType: TUIElementType);
    procedure BringToFront;
    function DrawAlpha: single;
    function DrawX: integer;
    function DrawY: integer;
    function DrawW: integer;
    function DrawH: integer;
    function GetDrawRect: TBoundsRect;
    function GetRoot: TUIElement;
    function Index: integer;
    function IsGeneric: boolean;
    function IsGenericContainer: boolean;
    function PointInside(_x,_y: integer): boolean;
    procedure SendToBack;
    procedure SetColor(r,g,b: single; a: single = 1);
    procedure SetPosition(_x,_y: single);
    procedure SetSize(w,h: single);
    function IsVisible: boolean;
  end;

  { TUIContainer }

  TUIContainer = class(TUIElement)
  private
    FCount: integer;
  public
    element: array of TUIElement;
    property count: integer read FCount;
    constructor Create(const _name: string; const _eType: TUIElementType);
    destructor Destroy; override;
    procedure Add(const e: TUIElement);
    procedure Clear;
    procedure Delete(_index: integer; noFree: boolean = false); overload;
    function Delete(e: TUIElement; noFree: boolean = false): boolean; overload;
    function Find(_name: string): TUIElement;
    function GetFocus(_x, _y: integer): TUIElement;
    function IndexOf(_name: string): integer;
    procedure MoveTo(_index,toIndex: integer);
    procedure MoveToBack(_index: integer);
    procedure MoveToFront(_index: integer);
    function NewName(base: string): string;
    procedure ReadFromFile(var f: TextFile);
    function TotalCount: integer;
    procedure UpdateSize;
    procedure WriteToFile(f: TStringList);
  end;

  { TUIButton }

  TUIButton = class(TUIElement)
  private
    procedure MouseDown;
    procedure MouseUp;
  public
    caption: string;
    texture,font: integer;
    down: boolean;
    constructor Create(_name: string);
  end;

  { TUIEdit }

  TUIEdit = class(TUIElement)
  private
    FText: string;
    FCaretPos: integer;
    procedure KeyDown(key: word; shift: TShiftState);
    procedure KeyPress(c: char);
    procedure MouseDown(mx, my: integer);
    procedure MouseMove(mx, my: integer);
    procedure MouseUp(mx, my: integer);
    procedure PointCaret(mx: integer);
    procedure SetCaretPos(p: integer);
    procedure SetText(s: string);
  public
    texture, font, maxLength, selStart, selLength: integer;
    centered: boolean;
    property caretPos: integer read FCaretPos write SetCaretPos;
    property text: string read FText write SetText;
    constructor Create(_name: string);
  end;

  { TUILabel }

  TUILabel = class(TUIElement)
  public
    caption: string;
    font: integer;
    centered: boolean;
    constructor Create(_name: string);
  end;

  { TUIImage }

  TUIImage = class(TUIElement)
  public
    texture,pattern: integer;
    constructor Create(_name: string);
  end;

  { TUIScrollBar }

  TUIScrollBar = class(TUIElement)
  private
    FPosition: integer;
    FMax: integer;
    procedure KeyDown(key: word);
    procedure MouseDown(mx,my: integer);
    procedure MouseMove(mx,my: integer);
    procedure MouseUp;
    procedure MouseWheel(scroll: integer);
    procedure SetMax(p: integer);
    procedure SetPosition(p: integer);
  public
    texture: integer;
    prev,next,middle: TUIButton;
    vertical: boolean;
    focus: byte;
    scrollDelay: cardinal;
    onScroll: TUIEvent;
    property max: integer read FMax write SetMax;
    property position: integer read FPosition write SetPosition;
    constructor Create(_name: string);
    destructor Destroy; override;
    procedure LostFocus;
    procedure UpdateSize;
  end;

  { TUICheckBox }

  TUICheckBox = class(TUIElement)
  private
    FChecked: boolean;
    procedure MouseDown;
    procedure SetChecked(enable: boolean);
  public
    caption: string;
    texture,font,group: integer;
    property checked: boolean read FChecked write SetChecked;
    constructor Create(_name: string);
  end;

  { TUIList }

  TUIList = class(TUIElement)
  private
    focus: byte;
    FItemIndex: integer;
    procedure KeyDown(key: word);
    procedure MouseDown(mx,my: integer);
    procedure MouseMove(mx,my: integer);
    procedure MouseUp;
    procedure MouseWheel(scroll: integer);
    procedure SetItemIndex(n: integer);
  public
    onSelect: TUIEvent;
    bar: TUIScrollBar;
    texture,font,visibleItems: integer;
    items: TStringList;
    centered: boolean;
    SelColor: TfRGBA;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    constructor Create(_name: string);
    destructor Destroy; override;
    procedure LostFocus;
    procedure UpdateSize;
  end;

  { TUIDropDown }

  TUIDropDown = class(TUIElement)
  private
    FDropped: boolean;
    focus: byte;
    oldHeight, oldY: single;
    oldAbsSize, oldAbsPos: boolean;
    function GetIIndex: integer;
    procedure doListClick({%H-}sender: TObject; {%H-}button: TMouseButton);
    procedure KeyDown(key: word; shift: TShiftState);
    procedure KeyPress(c: char);
    procedure MouseDown(mx,my: integer);
    procedure MouseMove(mx,my: integer);
    procedure MouseUp(mx,my: integer);
    procedure MouseWheel(scroll: integer);
    procedure SetDropped(enable: boolean);
    procedure SetIIndex(i: integer);
  public
    texture,font: integer;
    text: string;
    edit: TUIEdit;
    button: TUIButton;
    list: TUIList;
    editable: boolean;
    property dropped: boolean read FDropped write SetDropped;
    property ItemIndex: integer read GetIIndex write SetIIndex;
    constructor Create(_name: string);
    destructor Destroy; override;
    procedure LostFocus;
    procedure UpdateSize;
  end;

  { TUIMemo }

  TUIMemo = class(TUIElement)
  private
    focus: byte;
    _mx,_my: integer;
    function CutText(var s: string; var newLine: string): boolean;
    procedure KeyDown(key: word; shift: TShiftState);
    procedure KeyPress(key: char);
    procedure MouseDown(mx,my: integer);
    procedure MouseMove(mx,my: integer);
    procedure MouseUp(mx, my: integer);
    procedure MouseWheel(scroll: integer);
    procedure UpdateWrap;
  public
    strings: TStringList;
    texture,font: integer;
    barVertical,barHorizontal: TUIScrollBar;
    wrap: boolean;
    SelColor: TfRGBA;
    constructor Create(_name: string);
    destructor Destroy; override;
    procedure AddText(s: string);
    procedure LostFocus;
    procedure RemoveWrap;
    procedure UpdateSize;
  end;

  { TUIPanel }

  TUIPanel = class(TUIContainer)
  public
    texture: integer;
    constructor Create(_name: string);
  end;

  { TUITab }

  TUITab = class(TUIContainer)
  public
    caption: string;
    texture,posX: integer;
    constructor Create;
  end;

  { TUITabControl }

  TUITabControl = class(TUIElement)
  private
    FTabIndex: integer;
    procedure MouseDown(mx,my: integer);
    procedure SetTabIndex(n: integer);
  public
    tab: array of TUITab;
    count, font, texture, tabHeight, tabWidth, overLap: integer;
    property tabIndex: integer read FTabIndex write SetTabIndex;
    constructor Create(_name: string; AddTab: boolean = true);
    destructor Destroy; override;
    function Add: TUITab;
    procedure Clear;
    function Current: TUITab;
    function Delete(_index: integer): boolean;
    procedure UpdateSize;
  end;

  { TUIWindow }

  TUIWindow = class(TUIContainer)
  private
    dragging: boolean;
    dragX, dragY: single;
    procedure MouseDown(mx,my: integer);
    procedure MouseMove(mx,my: integer);
    procedure MouseUp;
  public
    caption: string;
    draggable: boolean;
    texture, titleTex, titleHeight, font: integer;
    constructor Create(_name: string);
  end;

  TUIFadeItem = record
    e: TUIElement;
    fadeIn: boolean;
    startTime, fadeTime: cardinal;
  end;

  { TCustomUI }

  TCustomUI = class(TUIPanel)
  private
    Fmx, Fmy, faderCount: integer;
    Fmb: TMouseButton;
    Ffocus,FmouseFocus: TUIElement;
    FisMouseDown: boolean;
    fader: array of TUIFadeItem;
    procedure _initialize;
    procedure SetFocus(e: TUIElement);
  public
    property isMouseDown: boolean read FisMousedown;
    property focus: TUIElement read Ffocus write SetFocus;
    property mouseButton: TMousebutton read Fmb;
    property mouseFocus: TUIElement read FmouseFocus;
    property mouseX: integer read Fmx;
    property mouseY: integer read Fmy;
    procedure Clear;
    constructor CreateByName(const _name: string = 'Main');
    destructor Destroy; override;
    function FadeFinished: boolean;
    procedure FadeIn(_e: TUIElement; _fadeTime: cardinal; _waitTime: cardinal = 0);
    procedure FadeOut(_e: TUIElement; _fadeTime: cardinal; _waitTime: cardinal = 0);
    function FindFader(e: TUIElement): integer;
    function GetFocus(_x,_y: integer): TUIElement;
    function LoadFromFile(filename: string): boolean;
    procedure MouseDown(button: TMouseButton);
    procedure MouseMove(_x,_y: integer);
    procedure MouseUp(button: TMouseButton);
    procedure MouseWheel(scroll: integer);
    procedure KeyDown(key: word; shift: TShiftState);
    procedure KeyPress(key: char);
    procedure ProcessFaders;
    procedure SaveToFile(filename: string);
    //procedure ShowMsg(text: string);
  end;

var
  nxUIEditorMode: boolean = false;

implementation

{ TCustomUI }

procedure TCustomUI._initialize;
begin
  eType:=uiMainPanel; AbsPos:=true; AbsSize:=true;
  x:=0; y:=0; Width:=nxEngine.Width; Height:=nxEngine.Height;
  parent:=nil; color.a:=0;
end;

function TCustomUI.FindFader(e: TUIElement): integer;
var i: integer;
begin
  for i:=0 to faderCount-1 do
    if fader[i].e=e then begin
      result:=i; exit;
    end;
  result:=-1;
end;

procedure TCustomUI.ProcessFaders;
var i: integer; t: cardinal;
begin
  for i:=faderCount-1 downto 0 do
    with fader[i] do
      if nxEngine.FrameTick>startTime then begin
        t:=nxEngine.FrameTick-startTime;
        if t>=fadeTime then begin
          if fadeIn then begin
            e.color.a:=1;
          end else begin
            e.Visible:=false; e.color.a:=0;
            if focus=e then begin
              focus:=nil;
            end;
          end;
          dec(faderCount); fader[i]:=fader[faderCount];
        end else begin
          if fadeIn then e.color.a:=t/fadeTime
          else e.color.a:=1-t/fadeTime;
        end;
      end;
end;

procedure TCustomUI.SetFocus(e: TUIElement);
begin
  if FFocus<>nil then FFocus.Focused:=false;
  FFocus:=e;
  if e<>nil then e.Focused:=true;
end;

procedure TCustomUI.Clear;
begin
  inherited Clear;
  Ffocus:=nil;
end;

constructor TCustomUI.CreateByName(const _name: string);
begin
  inherited Create(_name);
  _initialize;
end;

destructor TCustomUI.Destroy;
begin
  if (focus<>nil) and (focus.eType=uiDropDown) then
    TUIDropDown(focus).LostFocus;
  inherited Destroy;
end;

function TCustomUI.FadeFinished: boolean;
begin
  result:=faderCount=0;
end;

procedure TCustomUI.FadeIn(_e: TUIElement; _fadeTime: cardinal; _waitTime: cardinal);
var i,n: integer;
begin
  if _e=nil then exit;
  n:=faderCount;
  for i:=0 to faderCount-1 do
    if fader[i].e=_e then begin
      n:=i; break;
    end;
  if n=faderCount then begin
    inc(faderCount); setlength(fader,faderCount);
  end;
  with fader[n] do begin
    e:=_e;
    startTime:=nxEngine.FrameTick+_waitTime;
    fadeTime:=max(1,_fadeTime);
    fadeIn:=true; e.Visible:=true; e.color.a:=0;
  end;
end;

procedure TCustomUI.FadeOut(_e: TUIElement; _fadeTime: cardinal; _waitTime: cardinal);
var i,n: integer;
begin
  if _e=nil then exit;
  n:=faderCount;
  for i:=0 to faderCount-1 do
    if fader[i].e=_e then begin
      n:=i; break;
    end;
  if n=faderCount then begin
    inc(faderCount); setlength(fader,faderCount);
  end;
  with fader[n] do begin
    e:=_e;
    startTime:=nxEngine.FrameTick+_waitTime;
    fadeTime:=max(1,_fadeTime);
    fadeIn:=false; e.Visible:=true; e.color.a:=1;
  end;
end;

function TCustomUI.GetFocus(_x, _y: integer): TUIElement;
begin
  result:=inherited GetFocus(_x,_y);
end;

function TCustomUI.LoadFromFile(filename: string): boolean;
var f: TextFile; c,s: string;
begin
  result:=false;
  Clear;
  filename:=UTF8ToSys(filename); FixPath(filename);
  if (filename<>'') and (not FileExistsUTF8(filename)) then begin
    nxSetError(format('[GLUI] File "%s" not found.',[filename]));
    exit;
  end;
  assignfile(f,filename); reset(f);
  repeat
    readln(f,c);
    c:=UTF8ToSys(c);
    if (c<>'') and (c[1]='?') then c:=copy(c,2,length(c)-1);
    if c='textures' then begin
      //tex.readFromFile(f);
    end else if c='fonts' then begin
      //ReadFonts(f);
    end else if c='objects' then begin
      readln(f,s);
      name:=strafter(s,':'); // Skip command "add(uiMainPanel)"
      ReadFromFile(f);
    end else if c='particles' then begin
      //pt.readFromFile(f);
    end;
  until eof(f) or (c='');
  closefile(f);
  UpdateSize;
  result:=true;
end;

procedure TCustomUI.MouseDown(button: TMouseButton);
var newFocus: TUIElement;
begin
  if isMouseDown then exit;
  newFocus:=GetFocus(Fmx,Fmy);
  if (newFocus=nil) or (not newFocus.Enabled) then exit;
  if (focus<>nil) and (newFocus<>focus) then begin
    case focus.eType of
      uiMemo: TUIMemo(focus).LostFocus;
      uiList: TUIList(focus).LostFocus;
      uiDropDown: TUIDropDown(focus).LostFocus;
      uiScrollBar: TUIScrollBar(focus).LostFocus;
    end;
    focus.Focused:=false;
  end;
  FisMouseDown:=true; Fmb:=button;
  Ffocus:=newFocus; focus.Focused:=true;
  case focus.eType of
    uiButton: TUIButton(focus).MouseDown;
    uiCheckBox: TUICheckBox(focus).MouseDown;
    uiEdit: TUIEdit(focus).MouseDown(Fmx,Fmy);
    uiList: TUIList(focus).MouseDown(Fmx,Fmy);
    uiMemo: TUIMemo(focus).MouseDown(Fmx,Fmy);
    uiDropDown: TUIDropDown(focus).MouseDown(Fmx,Fmy);
    uiScrollBar: TUIScrollBar(focus).MouseDown(Fmx,Fmy);
    uiTabControl: TUITabControl(focus).MouseDown(Fmx,Fmy);
    uiWindow: TUIWindow(focus).MouseDown(Fmx,Fmy);
    else
      if assigned(focus.onMouseDown) then
        focus.onMouseDown(focus,button);
  end;
  if not focus.Focused then Ffocus:=nil;
end;

procedure TCustomUI.MouseMove(_x, _y: integer);
procedure HandleMouseMove(e: TUIElement);
  begin
    case e.eType of
      uiScrollBar: TUIScrollBar(e).MouseMove(Fmx,Fmy);
      uiList: TUIList(e).MouseMove(Fmx,Fmy);
      uiDropDown: TUIDropDown(e).MouseMove(Fmx,Fmy);
      uiMemo: TUIMemo(e).MouseMove(Fmx,Fmy);
      uiWindow: TUIWindow(e).MouseMove(Fmx,Fmy);
      else
        if assigned(e.onMouseMove) then e.onMouseMove(e,Fmx,Fmy);
    end;
  end;

begin
  Fmx:=_x; Fmy:=_y;
  FmouseFocus:=GetFocus(_x,_y);
  if (focus<>nil) and (not focus.IsVisible) then focus:=nil;
  if (FmouseFocus<>nil) and (not isMouseDown) then
    HandleMouseMove(FMouseFocus)
  else if focus<>nil then
    HandleMouseMove(focus);
  if (FmouseFocus=self) and (DrawAlpha<0.01) then FmouseFocus:=nil;
  if (focus=self) and (DrawAlpha<0.01) then focus:=nil;
end;

procedure TCustomUI.MouseUp(button: TMouseButton);
begin
  if not isMouseDown then exit;
  FisMouseDown:=false;
  if (focus=nil) or (not focus.Enabled) then exit;
  case focus.eType of
    uiButton: TUIButton(focus).MouseUp;
    uiScrollBar: TUIScrollBar(focus).MouseUp;
    uiEdit: TUIEdit(focus).MouseUp(Fmx,Fmy);
    uiList: TUIList(focus).MouseUp;
    uiDropDown: TUIDropDown(focus).MouseUp(Fmx,Fmy);
    uiMemo: TUIMemo(focus).MouseUp(Fmx,Fmy);
    uiWindow: TUIWindow(focus).MouseUp;
    else
      if assigned(focus.onMouseUp) then focus.onMouseUp(focus,button);
  end;
  if not focus.Focused then Ffocus:=nil;
end;

procedure TCustomUI.MouseWheel(scroll: integer);
procedure HandleWheel(e: TUIElement);
  begin
    case e.eType of
      uiScrollBar: TUIScrollBar(e).MouseWheel(scroll);
      uiList: TUIList(e).MouseWheel(scroll);
      uiDropDown: TUIDropDown(e).MouseWheel(scroll);
      uiMemo: TUIMemo(e).MouseWheel(scroll);
    end;
  end;

begin
  scroll:=scroll div abs(scroll);
  if (not isMouseDown) and (mouseFocus<>nil) then HandleWheel(mouseFocus)
  else if (focus<>nil) and (not focus.Enabled) then HandleWheel(focus);
end;

procedure TCustomUI.KeyDown(key: word; shift: TShiftState);
begin
  if (focus=nil) or (not focus.Enabled) then exit;
  case focus.eType of
    uiEdit: TUIEdit(focus).KeyDown(key,shift);
    uiMemo: TUIMemo(focus).KeyDown(key,shift);
    uiScrollBar: TUIScrollBar(focus).KeyDown(key);
    uiList: TUIList(focus).KeyDown(key);
    uiDropDown: TUIDropDown(focus).KeyDown(key,shift);
  end;
  if not focus.Focused then Ffocus:=nil;
end;

procedure TCustomUI.KeyPress(key: char);
begin
  if (focus=nil) or (not focus.Enabled) then exit;
  case focus.eType of
    uiEdit: TUIEdit(focus).KeyPress(key);
    uiDropDown: TUIDropDown(focus).KeyPress(key);
    uiMemo: TUIMemo(focus).KeyPress(key);
  end;
end;

procedure TCustomUI.SaveToFile(filename: string);
var f: TStringList;
begin
  f:=TStringList.Create;
  //f.Add('textures');
  //tex.WriteToFile(f);
  //f.Add('fonts');
  //WriteFonts(f);
  f.Add('objects');
  WriteToFile(f);
  //f.Add('particles');
  //pt.WriteToFile(f);
  f.SaveToFile(filename); f.Free;
end;

{ TUIElement }

procedure TUIElement.SetAbsPos(enable: boolean);
var r,cr: TBoundsRect;
begin
  if enable=FAbsPos then exit;
  r:=GetDrawRect;
  if parent<>nil then cr:=parent.GetDrawRect
  else cr:=bRect(0,0,1,1);
  FAbsPos:=enable;
  if FAbsPos then begin
    x:=r.x-cr.x; y:=r.y-cr.y;
  end else begin
    x:=x/cr.w; y:=y/cr.h;
  end;
end;

procedure TUIElement.SetAbsSize(enable: boolean);
var r,cr: TBoundsRect;
begin
  if enable=FAbsSize then exit;
  r:=GetDrawRect;
  if parent<>nil then cr:=parent.GetDrawRect
  else cr:=bRect(0,0,1,1);
  FAbsSize:=enable;
  if FAbsSize then begin
    width:=r.w; height:=r.h;
  end else begin
    width:=width/cr.w; height:=height/cr.h;
  end;
end;

procedure TUIElement.SetFocus(enable: boolean);
begin
  FFocused:=enable and Enabled and Visible;
  case eType of
    uiUndefined, uiImage, uiPanel, uiMainPanel, uiLabel,
    uiCheckBox, uiTab:
      FFocused:=false;
  end;
end;

constructor TUIElement.Create(const _name: string; const _eType: TUIElementType);
begin
  SetColor(1,1,1,1); bdSize:=64;
  Enabled:=true; Visible:=true; eType:=_eType;
  name:=_name; drawStyle:=uiScaled;
  ParentAlpha:=true;
end;

procedure TUIElement.BringToFront;
begin
  if (parent<>nil) and (parent is TUIContainer) then
    TUIContainer(parent).MoveToFront(index);
end;

function TUIElement.DrawAlpha: single;
begin
  if color.a>0 then begin
    if ParentAlpha and (parent<>nil) then result:=parent.DrawAlpha*color.a
    else result:=color.a;
  end else
    result:=0;
end;

function TUIElement.DrawH: integer;
begin
  if (parent<>nil) and (not AbsSize) then
    result:=round(parent.DrawH*height)
  else result:=round(height);
end;

function TUIElement.DrawW: integer;
begin
  if (parent<>nil) and (not AbsSize) then
    result:=round(parent.DrawW*width)
  else result:=round(width);
end;

function TUIElement.DrawX: integer;
begin
  if parent=nil then result:=0
  else if AbsPos then result:=parent.DrawX+round(x)
  else result:=parent.DrawX+round(parent.DrawW*x);
end;

function TUIElement.DrawY: integer;
begin
  if parent=nil then result:=0
  else if AbsPos then result:=parent.DrawY+round(y)
  else result:=parent.DrawY+round(parent.DrawH*y);
end;

function TUIElement.GetDrawRect: TBoundsRect;
var p: TBoundsRect;
begin
  if parent=nil then begin
    result:=bRect(0,0,round(width),round(height))
  end else begin
    p:=parent.GetDrawRect;
    if AbsPos then begin
      result.x:=p.x+round(x);
      result.y:=p.y+round(y);
    end else begin
      result.x:=p.x+round(p.w*x);
      result.y:=p.y+round(p.h*y);
    end;
    if AbsSize then begin
      result.w:=round(width);
      result.h:=round(height);
    end else begin
      result.w:=round(p.w*width);
      result.h:=round(p.h*height);
    end;
  end;
end;

function TUIElement.GetRoot: TUIElement;
begin
  if (parent<>nil) and (parent<>self) then result:=parent.GetRoot
  else result:=self;
end;

function TUIElement.Index: integer;
var i: integer;
begin
  if (parent<>nil) and (parent is TUIContainer) then
    with TUIContainer(parent) do
      for i:=0 to count-1 do
        if element[i]=self then begin
          result:=i; exit;
        end;
  result:=-1;
end;

function TUIElement.IsGeneric: boolean;
begin
  if parent<>nil then result:=parent.IsGenericContainer
  else result:=false;
end;

function TUIElement.IsGenericContainer: boolean;
begin
  case eType of
    uiDropDown, uiList, uiMemo, uiScrollBar, uiTabControl,
      uiTreeView: result:=true;
    else result:=false;
  end;
end;

function TUIElement.PointInside(_x, _y: integer): boolean;
var r: TBoundsRect;
begin
  r:=GetDrawRect;
  result:=(_x>=r.x) and (_y>=r.y) and (_x<r.x+r.w) and (_y<r.y+r.h);
end;

procedure TUIElement.SendToBack;
begin
  if (parent<>nil) and (parent is TUIContainer) then
    TUIContainer(parent).MoveToBack(index);
end;

procedure TUIElement.SetColor(r, g, b: single; a: single);
begin
  color.r:=r; color.g:=g; color.b:=b; color.a:=a;
end;

procedure TUIElement.SetPosition(_x, _y: single);
begin
  x:=_x; y:=_y;
end;

procedure TUIElement.SetSize(w, h: single);
begin
  width:=w; height:=h;
end;

function TUIElement.IsVisible: boolean;
begin
  if not visible then result:=false
  else if parent<>nil then result:=visible and parent.IsVisible
  else result:=true;
end;

{ TUIContainer }

procedure TUIContainer.Add(const e: TUIElement);
begin
  if e=nil then exit;
  if e.name='' then e.name:=NewName('Object');
  inc(FCount); setlength(element,FCount);
  element[FCount-1]:=e; element[FCount-1].parent:=self;
  if eType=uiMainPanel then e.ParentAlpha:=false;
end;

procedure TUIContainer.Clear;
var i: integer;
begin
  for i:=0 to FCount-1 do element[i].Free;
  setlength(element,0);
  FCount:=0;
end;

constructor TUIContainer.Create(const _name: string; const _eType: TUIElementType);
begin
  inherited Create(_name,_eType);
  FCount:=0;
end;

procedure TUIContainer.Delete(_index: integer; noFree: boolean);
var temp: TUIElement;
begin
  temp:=element[_index]; MoveTo(_index,count-1);
  dec(FCount); setlength(element,FCount);
  if (temp<>nil) and (not noFree) then temp.Free;
end;

function TUIContainer.Delete(e: TUIElement; noFree: boolean): boolean;
var i: integer;
begin
  for i:=0 to count-1 do
    if element[i]=e then begin
      Delete(i,noFree); result:=true; exit;
    end;
  result:=false;
end;

// Case sensitive object find
function TUIContainer.Find(_name: string): TUIElement;
var i: integer;
begin
  for i:=0 to count-1 do
    if element[i].name=_name then begin
      result:=element[i]; exit;
    end;
  for i:=0 to count-1 do
    if element[i] is TUIContainer then begin
      result:=TUIContainer(element[i]).Find(_name);
      if result<>nil then exit;
    end;
  result:=nil;
end;

function TUIContainer.GetFocus(_x, _y: integer): TUIElement;
var i: integer;
begin
  result:=nil;
  if (not Visible) and
     ( (not nxUIEditorMode) or
       ((parent<>nil) and (parent.eType=uiMainPanel)) ) then exit;
  if PointInside(_x,_y) then begin
    for i:=count-1 downto 0 do
      if element[i] is TUIContainer then begin
        result:=TUIContainer(element[i]).GetFocus(_x,_y);
        if result<>nil then exit;
      end else if element[i].PointInside(_x,_y) then
        if element[i].IsVisible or (nxUIEditorMode and (eType<>uiMainPanel)) then begin
          if element[i].eType=uiTabControl then begin
            if TUITabControl(element[i]).current.PointInside(_x,_y) then
              result:=TUITabControl(element[i]).current.GetFocus(_x,_y);
            if result=nil then begin
              result:=element[i]; exit;
            end;
          end else begin
            result:=element[i]; exit;
          end;
        end;
    if isVisible or nxUIEditorMode then result:=self;
  end;
end;

destructor TUIContainer.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TUIContainer.IndexOf(_name: string): integer;
var i: integer;
begin
  for i:=0 to count-1 do
    if element[i].name=_name then begin
      result:=i; exit;
    end;
  result:=-1;
end;

procedure TUIContainer.MoveTo(_index, toIndex: integer);
var i: integer; temp: TUIElement;
begin
  if _index=toIndex then exit;
  temp:=element[_index];
  if toIndex<_index then begin
    for i:=_index downto toIndex+1 do
      element[i]:=element[i-1];
  end else if toIndex>_index then begin
    for i:=_index to toIndex-1 do
      element[i]:=element[i+1];
  end;
  element[toIndex]:=temp;
end;

// Move element in the list to be index (0)
procedure TUIContainer.MoveToBack(_index: integer);
begin
  MoveTo(_index,0);
end;

// Move element in the list to be index (Count-1)
procedure TUIContainer.MoveToFront(_index: integer);
begin
  MoveTo(_index,count-1);
end;

function TUIContainer.NewName(base: string): string;
var i,n: integer; h: boolean;
begin
  result:=base+'1'; n:=1;
  repeat
    h:=true;
    for i:=0 to count-1 do
      if result=element[i].name then begin
        h:=false; inc(n); result:=base+inttostr(n);
      end;
  until h;
end;

procedure TUIContainer.ReadFromFile(var f: TextFile);
procedure _ReadString(s: TStrings);
  var s2: string;
  begin
    readln(f,s2);
    while s2<>'¤end¤' do begin
      s.Add(s2); readln(f,s2);
    end;
  end;

var s: string;
  procedure ReadElement(e: TUIElement);
  var s,c,s2: string; sa: array[0..9] of string;
  begin
    with e do begin
      repeat
        readln(f,s); s:=trimleft(s);
        readstrings(s,' ',sa{%H-});
        c:=sa[0];
        s2:=copy(s,length(c)+2,length(s)-1-length(c));
        // ** General properties **
        if c='rgba' then begin
          color.r:=strtofloat(sa[1]);
          color.g:=strtofloat(sa[2]);
          color.b:=strtofloat(sa[3]);
          color.a:=strtofloat(sa[4]);
        end else if c='bool' then begin
          Visible:=sa[1]<>'0';
          Enabled:=sa[2]<>'0';
          AbsPos:=sa[3]<>'0';
          AbsSize:=sa[4]<>'0';
          ParentAlpha:=sa[5]<>'0';
        end else if c='size' then begin
          x:=strtofloat(sa[1]);
          y:=strtofloat(sa[2]);
          width:=strtofloat(sa[3]);
          height:=strtofloat(sa[4]);
        // ** Other properties **
        end else if c='centered' then begin
          case eType of
            uiLabel: TUILabel(e).centered:=s2<>'0';
            uiEdit: TUIEdit(e).centered:=s2<>'0';
            uiList: TUIList(e).centered:=s2<>'0';
          end;
        end else if c='checked' then begin
          case eType of
            uiCheckBox: TUICheckBox(e).checked:=s2<>'0';
          end;
        end else if c='draggable' then begin
          case eType of
            uiWindow: TUIWindow(e).draggable:=s2<>'0';
          end;
        end else if c='editable' then begin
          case eType of
            uiDropDown: TUIDropDown(e).editable:=s2<>'0';
          end;
        end else if c='font' then begin
          case eType of
            uiButton: TUIButton(e).font:=nxEngine.IndexOfFont(s2);
            uiLabel: TUILabel(e).font:=nxEngine.IndexOfFont(s2);
            uiEdit: TUIEdit(e).font:=nxEngine.IndexOfFont(s2);
            uiCheckBox: TUICheckBox(e).font:=nxEngine.IndexOfFont(s2);
            uiList: TUIList(e).font:=nxEngine.IndexOfFont(s2);
            uiMemo: TUIMemo(e).font:=nxEngine.IndexOfFont(s2);
            uiDropDown: TUIDropDown(e).edit.font:=nxEngine.IndexOfFont(s2);
            uiWindow: TUIWindow(e).font:=nxEngine.IndexOfFont(s2);
          end;
        end else if c='group' then begin
          case eType of
            uiCheckBox: TUICheckBox(e).group:=strtoint(s2);
          end;
        end else if c='itemindex' then begin
          case eType of
            uiList: TUIList(e).itemIndex:=strtoint(s2);
            uiDropDown: TUIDropDown(e).list.itemIndex:=strtoint(s2);
          end;
        end else if c='max' then begin
          case eType of
            uiScrollBar: TUIScrollBar(e).max:=strtoint(s2);
          end;
        end else if c='maxlength' then begin
          case eType of
            uiEdit: TUIEdit(e).maxLength:=strtoint(s2);
          end;
        end else if c='position' then begin
          case eType of
            uiScrollBar: TUIScrollBar(e).position:=strtoint(s2);
          end;
        end else if c='strings' then begin
          case eType of
            uiMemo: _ReadString(TUIMemo(e).strings);
            uiList: _ReadString(TUIList(e).items);
            uiDropDown: _ReadString(TUIDropDown(e).list.items);
          end;
        end else if c='text' then begin
          case eType of
            uiButton: TUIButton(e).caption:=s2;
            uiLabel: TUILabel(e).caption:=s2;
            uiEdit: TUIEdit(e).text:=s2;
            uiCheckBox: TUICheckBox(e).caption:=s2;
            uiWindow: TUIWindow(e).caption:=s2;
          end;
        end else if c='texture' then begin
          case eType of
            uiImage: TUIImage(e).texture:=nxTex.IndexOf(s2);
            uiPanel,uiMainPanel: TUIPanel(e).texture:=nxTex.IndexOf(s2);
            uiButton: TUIButton(e).texture:=nxTex.IndexOf(s2);
            uiEdit: TUIEdit(e).texture:=nxTex.IndexOf(s2);
            uiCheckBox: TUICheckBox(e).texture:=nxTex.IndexOf(s2);
            uiList: TUIList(e).texture:=nxTex.IndexOf(s2);
            uiMemo: TUIMemo(e).texture:=nxTex.IndexOf(s2);
            uiDropDown: TUIDropDown(e).texture:=nxTex.IndexOf(s2);
            uiTabControl: TUITabControl(e).texture:=nxTex.IndexOf(s2);
            uiWindow: TUIWindow(e).texture:=nxTex.IndexOf(s2);
          end;
        end else if c='titleheight' then begin
          case eType of
            uiWindow: TUIWindow(e).titleHeight:=strtoint(s2);
          end;
        end else if c='titletex' then begin
          case eType of
            uiWindow: TUIWindow(e).titleTex:=nxTex.IndexOf(s2);
          end;
        end else if c='vertical' then begin
          case eType of
            uiScrollBar: TUIScrollBar(e).vertical:=s2<>'0';
          end;
        end else if c='wrap' then begin
          case eType of
            uiMemo: TUIMemo(e).wrap:=s2<>'0';
          end;
        end;
      until eof(f) or (c='end');
    end; // with e do
  end;

  function StringToType(s: string): TUIElementType;
  begin
    if s='uiButton' then result:=uiButton
    else if s='uiCheckBox' then result:=uiCheckBox
    else if s='uiDropDown' then result:=uiDropDown
    else if s='uiEdit' then result:=uiEdit
    else if s='uiImage' then result:=uiImage
    else if s='uiLabel' then result:=uiLabel
    else if s='uiList' then result:=uiList
    else if s='uiMainPanel' then result:=uiMainPanel
    else if s='uiMemo' then result:=uiMemo
    else if s='uiPanel' then result:=uiPanel
    else if s='uiScrollBar' then result:=uiScrollBar
    else if s='uiTab' then result:=uiTab
    else if s='uiTabControl' then result:=uiTabControl
    else if s='uiTreeView' then result:=uiTreeView
    else if s='uiWindow' then result:=uiWindow
    else result:=uiUndefined;
  end;

var i,_count: integer; e: TUIElement; et: TUIElementType;
begin
  e:=nil;
  ReadElement(self);
  readln(f,_count);
  for i:=0 to _count-1 do begin
    readln(f,s);
    if copy(s,1,3)<>'add' then begin
      nxSetError('UI read error - "add" expected'); exit;
    end;
    et:=StringToType(strbetween(s,'(',')'));
    s:=strafter(s,':');
    case et of
      uiPanel,uiMainPanel: e:=TUIPanel.Create(s);
      uiButton: e:=TUIButton.Create(s);
      uiLabel: e:=TUILabel.Create(s);
      uiEdit: e:=TUIEdit.Create(s);
      uiMemo: e:=TUIMemo.Create(s);
      uiScrollBar: e:=TUIScrollBar.Create(s);
      uiList: e:=TUIList.Create(s);
      uiCheckBox: e:=TUICheckBox.Create(s);
      uiDropDown: e:=TUIDropDown.Create(s);
      uiImage: e:=TUIImage.Create(s);
      uiTabControl: e:=TUITabControl.Create(s,false);
      uiWindow: e:=TUIWindow.Create(s);
      //uiTreeView
      else begin
        nxSetError('UI read error - unknown element'); exit;
      end;
    end;
    readln(f,s); // begin
    if s<>'begin' then begin
      nxSetError('UI read error - "begin" expected'); exit;
    end;
    Add(e);
    if e is TUIContainer then
      TUIContainer(e).ReadFromFile(f)
    else
      ReadElement(e);
    e:=nil;
  end;
  UpdateSize;
end;

function TUIContainer.TotalCount: integer;
var i: integer;
begin
  result:=count;
  for i:=0 to Count-1 do begin
    if element[i] is TUIContainer then
      result:=result+TUIContainer(element[i]).TotalCount
    else
      inc(result);
  end;
end;

// Call this when window is resized to instantly update all UI controls
procedure TUIContainer.UpdateSize;
var i: integer;
begin
  if eType=uiMainPanel then begin
    width:=nxEngine.Width; height:=nxEngine.Height;
  end;
  for i:=0 to count-1 do begin
    case element[i].eType of
      uiScrollBar: TUIScrollBar(element[i]).UpdateSize;
      uiMemo: TUIMemo(element[i]).UpdateSize;
      uiList: TUIList(element[i]).UpdateSize;
      uiDropDown: TUIDropDown(element[i]).UpdateSize;
      uiTabControl: TUITabControl(element[i]).UpdateSize;
    end;
    if element[i] is TUIContainer then
      TUIContainer(element[i]).UpdateSize;
  end;
end;

procedure TUIContainer.WriteToFile(f: TStringList);
function BoolToStr(b: boolean): string;
  begin
    if b then result:='1' else result:='0';
  end;

  procedure WriteString(s: TStrings);
  var i: integer;
  begin
    f.Add(' strings ');
    for i:=0 to s.Count-1 do f.Add(s[i]);
    f.Add('¤end¤');
  end;

  function TypeToString(t: TUIElementType): string;
  begin
    case t of
      uiButton: result:='uiButton';
      uiCheckBox: result:='uiCheckBox';
      uiDropDown: result:='uiDropDown';
      uiEdit: result:='uiEdit';
      uiImage: result:='uiImage';
      uiLabel: result:='uiLabel';
      uiList: result:='uiList';
      uiMainPanel: result:='uiMainPanel';
      uiMemo: result:='uiMemo';
      uiPanel: result:='uiPanel';
      uiScrollBar: result:='uiScrollBar';
      uiTab: result:='uiTab';
      uiTabControl: result:='uiTabControl';
      uiTreeView: result:='uiTreeView';
      uiWindow: result:='uiWindow';
      else result:='Unknown';
    end;
  end;

  procedure WriteElement(e: TUIElement);
  begin
    with e do begin
      f.Add(format('add(%s):%s',[TypeToString(eType),name]));
      f.Add('begin');
      with color do
        f.Add(format(' rgba %.3f %.3f %.3f %.3f',[r,g,b,a]));
      f.Add(format(' bool %s %s %s %s %s',[
        BoolToStr(Visible)
       ,BoolToStr(Enabled)
       ,BoolToStr(AbsPos)
       ,BoolToStr(AbsSize)
       ,BoolToStr(ParentAlpha)
          ]));
      if eType<>uiMainPanel then
        f.Add(format(' size %.3f %.3f %.3f %.3f',[x,y,width,height]));
      case e.eType of
        uiButton:
          begin
            f.Add(' texture '+nxTex.texture[TUIButton(e).texture].name);
            f.Add(' text '+TUIButton(e).caption);
            f.Add(' font '+nxEngine.Font[TUIButton(e).font].name);
          end;
        uiCheckBox:
          begin
            f.Add(' texture '+nxTex.texture[TUICheckBox(e).texture].name);
            f.Add(' text '+TUICheckBox(e).caption);
            f.Add(' font '+nxEngine.Font[TUICheckBox(e).font].name);
            f.Add(' group '+inttostr(TUICheckBox(e).group));
            f.Add(' checked '+booltostr(TUICheckBox(e).checked));
          end;
        uiDropDown:
          begin
            f.Add(' texture '+nxTex.texture[TUIDropDown(e).edit.texture].name);
            f.Add(' font '+nxEngine.Font[TUIDropDown(e).edit.font].name);
            f.Add(' editable '+booltostr(TUIDropDown(e).editable));
            WriteString(TUIDropDown(e).list.items);
            f.Add(' itemindex '+inttostr(TUIDropDown(e).itemIndex));
          end;
        uiEdit:
          begin
            f.Add(' texture '+nxTex.texture[TUIEdit(e).texture].name);
            f.Add(' maxlength '+inttostr(TUIEdit(e).maxLength));
            f.Add(' text '+TUIEdit(e).text);
            f.Add(' font '+nxEngine.Font[TUIEdit(e).font].name);
            f.Add(' centered '+booltostr(TUIEdit(e).centered));
          end;
        uiImage:
          begin
            f.Add(' texture '+nxTex.texture[TUIImage(e).texture].name);
          end;
        uiList:
          begin
            f.Add(' texture '+nxTex.texture[TUIList(e).texture].name);
            f.Add(' font '+nxEngine.Font[TUIList(e).font].name);
            f.Add(' centered '+booltostr(TUIList(e).centered));
            WriteString(TUIList(e).items);
            f.Add(' itemindex '+inttostr(TUIList(e).itemIndex));
          end;
        uiMemo:
          begin
            f.Add(' texture '+nxTex.texture[TUIMemo(e).texture].name);
            f.Add(' font '+nxEngine.Font[TUIMemo(e).font].name);
            f.Add(' wrap '+booltostr(TUIMemo(e).wrap));
            WriteString(TUIMemo(e).strings);
          end;
        uiPanel, uiMainPanel:
          begin
            f.Add(' texture '+nxTex.texture[TUIPanel(e).texture].name);
          end;
        uiScrollBar:
          begin
            f.Add(' max '+inttostr(TUIScrollBar(e).max));
            f.Add(' vertical '+booltostr(TUIScrollBar(e).vertical));
            f.Add(' position '+inttostr(TUIScrollBar(e).position));
          end;
        uiTabControl:
          begin
            f.Add(' texture '+nxTex.texture[TUITabControl(e).texture].name);
            f.Add(' tabw '+inttostr(TUITabControl(e).tabWidth));
            f.Add(' tabh '+inttostr(TUITabControl(e).tabHeight));
            f.Add(' tabindex '+inttostr(TUITabControl(e).tabIndex));
          end;
        uiLabel:
          begin
            f.Add(' text '+TUILabel(e).caption);
            f.Add(' font '+nxEngine.Font[TUILabel(e).font].name);
            f.Add(' centered '+booltostr(TUILabel(e).centered));
          end;
        uiWindow:
          begin
            f.Add(' texture '+nxTex.texture[TUIwindow(e).texture].name);
            f.Add(' titletex '+nxTex.texture[TUIwindow(e).titleTex].name);
            f.Add(' text '+TUIwindow(e).caption);
            f.Add(' font '+nxEngine.Font[TUIwindow(e).font].name);
            f.Add(' titleheight '+inttostr(TUIwindow(e).titleHeight));
            f.Add(' draggable '+booltostr(TUIwindow(e).draggable ));
          end;
      end;
      f.Add('end');
    end; // with e do
  end;
var i: integer;
begin
  WriteElement(self);
  f.Add(inttostr(count));
  for i:=0 to count-1 do
    if element[i] is TUIContainer then
      TUIContainer(element[i]).WriteToFile(f)
    else
      WriteElement(element[i]);
end;

{ TUIButton }

procedure TUIButton.MouseDown;
begin
  down:=true;
  if assigned(onMouseDown) then onMouseDown(self,mbLeft);
end;

procedure TUIButton.MouseUp;
begin
  down:=false;
  if assigned(onMouseUp) then onMouseUp(self,mbLeft);
end;

constructor TUIButton.Create(_name: string);
begin
  inherited Create(_name,uiButton);
  name:=_name; caption:=_name;
  texture:=nxTex.IndexOf('ui_button');
  drawStyle:=uiSections;
end;

{ TUICheckBox }

procedure TUICheckBox.MouseDown;
begin
  checked:=not checked;
  if assigned(onMouseDown) then onMouseDown(self,mbLeft);
end;

procedure TUICheckBox.SetChecked(enable: boolean);
begin
  if FChecked=enable then exit;
  FChecked:=enable;
end;

constructor TUICheckBox.Create(_name: string);
begin
  inherited Create(_name,uiCheckBox);
  name:=_name; caption:=_name;
  texture:=nxTex.IndexOf('ui_checkbox');
end;

{ TUIDropDown }

function TUIDropDown.GetIIndex: integer;
begin
  result:=list.itemIndex;
end;

procedure TUIDropDown.doListClick(sender: TObject; button: TMouseButton);
begin
  dropped:=false;
end;

procedure TUIDropDown.KeyDown(key: word; shift: TShiftState);
begin
  if dropped then begin
    case key of
      VK_RETURN,VK_ESCAPE: dropped:=false;
      else list.KeyDown(key);
    end;
  end else if focus=1 then edit.KeyDown(key,shift);
  if (list.itemIndex>=0) and (list.items.Count>0) then
    edit.text:=list.items[list.itemIndex];
end;

procedure TUIDropDown.KeyPress(c: char);
begin
  if focus=1 then begin
    edit.KeyPress(c);
    if editable and (ItemIndex>=0) and (list.items.Count>0) then
      list.items[itemindex]:=edit.text;
  end;
end;

procedure TUIDropDown.SetIIndex(i: integer);
begin
  list.itemIndex:=i;
  if (list.itemIndex>=0) and (list.items.Count>0) then
    edit.text:=list.items[list.itemIndex];
end;

constructor TUIDropDown.Create(_name: string);
begin
  inherited Create(_name,uiDropDown);
  name:=_name;
  texture:=nxTex.IndexOf('ui_edit');
  drawStyle:=uiSections;
  edit:=TUIEdit.Create('edit'); edit.parent:=self;
  edit.AbsPos:=true; edit.AbsSize:=true;
  edit.text:='';
  edit.ParentAlpha:=true;
  list:=TUIList.Create('list'); list.parent:=self;
  list.AbsPos:=true; list.AbsSize:=true;
  list.Visible:=false;
  list.onMouseDown:={$IFDEF fpc}@{$ENDIF}doListClick;
  list.ParentAlpha:=true;
  button:=TUIButton.Create('button'); button.parent:=self;
  button.caption:=''; button.texture:=nxTex.IndexOf('ui_downbutton');
  button.AbsPos:=true; button.AbsSize:=true;
  button.ParentAlpha:=true;
  UpdateSize;
end;

destructor TUIDropDown.Destroy;
begin
  edit.Free; list.Free; button.Free;
  inherited Destroy;
end;

procedure TUIDropDown.LostFocus;
begin
  dropped:=false; list.Visible:=false;
  edit.Focused:=false; list.Focused:=false;
  list.LostFocus;
end;

procedure TUIDropDown.MouseDown(mx, my: integer);
begin
  focus:=0; edit.Focused:=false;
  if button.PointInside(mx,my) then begin
    focus:=2; button.MouseDown; dropped:=not dropped;
  end else if dropped and list.PointInside(mx,my) then begin
    focus:=3; list.MouseDown(mx,my);
  end else begin
    if editable then begin
      dropped:=false;
      edit.Focused:=true; edit.MouseDown(mx,my);
      focus:=1;
    end else begin
      focus:=2; button.MouseDown; dropped:=not dropped;
    end;
  end;
end;

procedure TUIDropDown.MouseMove(mx, my: integer);
begin
  if focus=1 then edit.MouseMove(mx,my)
  else if focus=3 then list.MouseMove(mx,my);
  if assigned(onMouseMove) then onMouseMove(self,mx,my);
end;

procedure TUIDropDown.MouseUp(mx,my: integer);
begin
  if focus=1 then edit.MouseUp(mx,my)
  else if focus=2 then button.MouseUp
  else if focus=3 then list.MouseUp;
  if assigned(onMouseUp) then onMouseUp(self,mbLeft);
end;

procedure TUIDropDown.MouseWheel(scroll: integer);
begin
  if dropped then list.MouseWheel(scroll);
end;

procedure TUIDropDown.SetDropped(enable: boolean);
begin
  if enable<>FDropped then begin
    FDropped:=enable; list.Visible:=dropped;
    if enable then begin
      oldAbsSize:=AbsSize; oldHeight:=Height;
      oldAbsPos:=AbsPos; oldY:=Y;
      AbsSize:=true; AbsPos:=true;
      height:=height+list.DrawH;
      if (parent<>nil) and (parent is TUIContainer) then
        TUIContainer(parent).MoveToFront(index);
    end else begin
      AbsSize:=oldAbsSize; Height:=oldHeight;
      AbsPos:=oldAbsPos; Y:=oldY;
      edit.y:=0; button.y:=0;
      if (list.itemIndex>=0) and (list.items.Count>0) then
        edit.text:=list.items[list.itemIndex];
    end;
    list.Focused:=dropped;
    if dropped then list.LostFocus;
  end;
end;

procedure TUIDropDown.UpdateSize;
var r: TBoundsRect;
begin
  r:=GetDrawRect;
  if dropped then r.h:=edit.DrawH;
  button.height:=r.h;
  if edit.Visible then button.width:=min(r.h*0.75,r.w*0.2)
  else button.width:=r.w;
  button.x:=r.w-button.width;
  edit.height:=r.h;
  if button.Visible then edit.width:=r.w-button.width
  else edit.width:=r.w;
  list.y:=r.h;
  list.width:=r.w; list.height:=80;
  list.UpdateSize;
  if editable and (ItemIndex>=0) and (list.items.Count>0) then
    list.items[itemindex]:=edit.text;
end;

{ TUIEdit }

constructor TUIEdit.Create(_name: string);
begin
  inherited Create(_name,uiEdit);
  name:=_name;
  MaxLength:=64;
  text:=_name;
  texture:=nxTex.IndexOf('ui_edit');
  drawStyle:=uiSections;
end;

procedure TUIEdit.MouseDown(mx, my: integer);
begin
  PointCaret(mx);
  if assigned(onMouseDown) then onMouseDown(self,mbLeft);
end;

procedure TUIEdit.MouseMove(mx, my: integer);
begin
  PointCaret(mx);
  if assigned(onMouseMove) then onMouseMove(self,mx,my);
end;

procedure TUIEdit.MouseUp(mx, my: integer);
begin
  PointCaret(mx);
  if assigned(onMouseUp) then onMouseUp(self,mbLeft);
end;

procedure TUIEdit.PointCaret(mx: integer);
var i,x1: integer; s: string;
begin
  s:=UTF8toSys(text);
  caretPos:=0;
  if centered then x1:=DrawX+1+(DrawW-nxEngine.Font[font].TextW(text)) div 2
  else x1:=DrawX+6;
  for i:=1 to length(s) do begin
    if x1>mx then break;
    CaretPos:=i;
    x1:=x1+nxEngine.Font[font].TextW(s[i]);
  end;
end;

procedure TUIEdit.SetCaretPos(p: integer);
var l: integer;
begin
  l:=length(UTF8toSys(text));
  if p<0 then FCaretPos:=0
  else if p>l then FCaretPos:=l
  else FCaretPos:=p;
end;

procedure TUIEdit.SetText(s: string);
var l: integer;
begin
  l:=length(UTF8toSys(s));
  FText:=copy(s,1,MaxLength);
  if caretPos>l then caretPos:=l;
end;

procedure TUIEdit.KeyDown(key: word; shift: TShiftState);
var l,p: integer;
begin
  l:=length(text);
  case byte(key) of
    VK_LEFT: caretPos:=caretPos-1;
    VK_RIGHT: caretPos:=caretPos+1;
    VK_HOME: caretPos:=0;
    VK_END: caretPos:=l;
    VK_DELETE:
      if (caretPos<l) and (l>0) then begin
        FText:=UTF8toSys(FText);
        SetText(SysToUTF8(copy(text,1,caretPos)+copy(text,caretPos+2,l-caretPos)));
      end;
    VK_BACK:
      if caretPos>0 then begin
        p:=caretPos; caretPos:=caretPos-1;
        FText:=UTF8toSys(FText);
        SetText(SysToUTF8(copy(text,1,p-1)+copy(text,p+1,l-p)));
      end;
    VK_RETURN: Focused:=false;
  end;
end;

procedure TUIEdit.KeyPress(c: char);
var l: integer;
begin
  if byte(c)>31 then begin
    FText:=UTF8toSys(FText);
    l:=length(FText);
    SetText(SysToUTF8(copy(FText,1,caretPos)+c+copy(FText,caretPos+1,l-caretPos)));
    caretPos:=caretPos+1;
  end;
end;

{ TUIList }

constructor TUIList.Create(_name: string);
begin
  inherited Create(_name,uiList);
  name:=_name;
  texture:=nxTex.IndexOf('ui_list');
  items:=TStringList.Create;
  drawStyle:=uiSections;
  bar:=TUIScrollBar.Create('ui_scrollbar'); bar.parent:=self;
  bar.AbsPos:=true; bar.AbsSize:=true;
  bar.width:=14; bar.vertical:=true;
  bar.ParentAlpha:=true;
  SelColor:=fRGBA(0.2,0.4,1,0.6);
end;

destructor TUIList.Destroy;
begin
  items.Free;
  if bar<>nil then bar.Free;
  inherited Destroy;
end;

procedure TUIList.LostFocus;
begin
  bar.Focused:=false; bar.LostFocus;
end;

procedure TUIList.KeyDown(key: word);
begin
  if focus=2 then bar.KeyDown(key);
  case key of
    VK_UP: ItemIndex:=ItemIndex-1;
    VK_DOWN: ItemIndex:=ItemIndex+1;
  end;
end;

procedure TUIList.MouseDown(mx, my: integer);
begin
  if bar.Visible and bar.PointInside(mx,my) then begin
    focus:=2; bar.MouseDown(mx,my);
  end else begin
    focus:=1;
    ItemIndex:=bar.position+(my-DrawY-2) div nxEngine.Font[font].height;
    if assigned(onMouseDown) then onMouseDown(self,mbLeft);
  end;
end;

procedure TUIList.MouseMove(mx, my: integer);
begin
  if focus=2 then bar.MouseMove(mx,my);
  if assigned(onMouseMove) then onMouseMove(self,mx,my);
end;

procedure TUIList.MouseUp;
begin
  if focus=2 then bar.MouseUp;
  if assigned(onMouseUp) then onMouseUp(self,mbLeft);
end;

procedure TUIList.MouseWheel(scroll: integer);
begin
  bar.MouseWheel(scroll);
end;

procedure TUIList.SetItemIndex(n: integer);
begin
  if n>items.count-1 then n:=items.count-1;
  if n<0 then n:=0;
  if n<>FItemIndex then begin
    FItemIndex:=n;
    if FItemIndex<bar.position then bar.position:=FItemIndex
    else begin
      if FItemIndex>bar.position+VisibleItems-1 then
        bar.position:=FItemIndex-VisibleItems+1;
    end;
    if assigned(onSelect) then onSelect(self);
  end;
end;

procedure TUIList.UpdateSize;
var r: TBoundsRect;
begin
  r:=GetDrawRect;
  bar.x:=r.w-bar.DrawW; bar.height:=r.h;
  visibleItems:=(DrawH-3) div nxEngine.Font[font].height;
  bar.max:=max(0,items.Count-visibleItems);
  bar.Visible:=bar.max>0;
  bar.UpdateSize;
end;

{ TUIMemo }

constructor TUIMemo.Create(_name: string);
begin
  inherited Create(_name,uiMemo);
  name:=_name;
  texture:=nxTex.IndexOf('ui_list');
  strings:=TStringList.Create;
  drawStyle:=uiSections; wrap:=true;
  barHorizontal:=TUIScrollBar.Create('ui_scroll_h');
  barHorizontal.parent:=self;
  barVertical:=TUIScrollBar.Create('ui_scroll_v');
  barVertical.parent:=self;
  barVertical.vertical:=true;
  barHorizontal.AbsSize:=true; barHorizontal.AbsPos:=true;
  barVertical.AbsSize:=true; barVertical.AbsPos:=true;
  barVertical.width:=14;
  barHorizontal.ParentAlpha:=true;
  barVertical.ParentAlpha:=true;
end;

destructor TUIMemo.Destroy;
begin
  strings.Free;
  barHorizontal.Free; barVertical.Free;
  inherited Destroy;
end;

function TUIMemo.CutText(var s: string; var newLine: string): boolean;
var i,x1,w,sl: integer; f: TNXFont;
begin
  result:=false;
  if wrap then begin
    sl:=length(s);
    if sl>1 then begin
      f:=nxEngine.Font[font];
      x1:=f.charW[byte(s[1])]; w:=DrawW-4-barVertical.DrawW;
      for i:=2 to sl do begin
        x1:=x1+f.charW[byte(s[i])];
        if x1>w then begin // Cut text
          newLine:=copy(s,i,sl-i+1);
          s:=copy(s,1,i-1); result:=true; exit;
        end;
      end;
    end;
  end;
end;

procedure TUIMemo.KeyDown(key: word; shift: TShiftState);
begin
  if focus=2 then barVertical.KeyDown(key)
  else if focus=3 then barHorizontal.KeyDown(key);
end;

procedure TUIMemo.UpdateWrap;
var i: integer; s,newLine: string;
begin
  if strings.Count=0 then exit;
  i:=0;
  repeat
    s:=strings[i];
    if CutText(s,newLine{%H-}) then begin
      strings[i]:=s+TextCutChar; strings.Insert(i+1,newLine);
    end;
    inc(i);
  until i>=strings.Count;
end;

procedure TUIMemo.RemoveWrap;
var i,l: integer;
begin
  for i:=strings.Count-2 downto 0 do begin
    l:=length(strings[i]);
    if (l>1) and (strings[i][l]=TextCutChar) then begin
      strings[i]:=copy(strings[i],1,l-1)+strings[i+1];
      strings.Delete(i+1);
    end;
  end;
end;

procedure TUIMemo.AddText(s: string);
var h: boolean; newLine: string;
begin
  repeat
    if CutText(s,newLine{%H-}) then begin
      h:=false; strings.Add(s);
      s:=newLine;
    end else begin
      h:=true; strings.Add(s);
    end;
  until h;
end;

procedure TUIMemo.LostFocus;
begin
  barVertical.Focused:=false; barHorizontal.Focused:=false;
  barVertical.LostFocus; barHorizontal.LostFocus;
end;

procedure TUIMemo.MouseDown(mx, my: integer);
begin
  if barVertical.Visible and barVertical.PointInside(mx,my) then begin
    focus:=2; barVertical.MouseDown(mx,my);
  end else if barHorizontal.Visible and barHorizontal.PointInside(mx,my) then begin
    focus:=3; barHorizontal.MouseDown(mx,my);
  end else begin
    focus:=1;
    // Set caret position...

    if assigned(onMouseDown) then onMouseDown(self,mbLeft);
  end;
end;

procedure TUIMemo.MouseMove(mx, my: integer);
begin
  _mx:=mx; _my:=my;
  if focus=2 then barVertical.MouseMove(mx,my)
  else if focus=3 then barHorizontal.MouseMove(mx,my);
  if assigned(onMouseMove) then onMouseMove(self,mx,my);
end;

procedure TUIMemo.MouseUp(mx, my: integer);
begin
  if focus=2 then barVertical.MouseUp
  else if focus=3 then barHorizontal.MouseUp
  else if focus=1 then begin

  end;
  if assigned(onMouseUp) then onMouseUp(self,mbLeft);
end;

procedure TUIMemo.MouseWheel(scroll: integer);
begin
  if barHorizontal.PointInside(_mx,_my) then
    barHorizontal.MouseWheel(scroll)
  else barVertical.MouseWheel(scroll);
end;

procedure TUIMemo.KeyPress(key: char);
begin
  if focus=1 then begin

  end;
end;

procedure TUIMemo.UpdateSize;
var r: TBoundsRect; i,w: integer;
begin
  r:=GetDrawRect;
  if wrap then barHorizontal.height:=0
  else barHorizontal.height:=14;
  barHorizontal.y:=r.h-barHorizontal.height;
  barHorizontal.width:=r.w-barVertical.width;
  barVertical.x:=r.w-barVertical.width;
  barVertical.height:=r.h-barHorizontal.height;
  barHorizontal.UpdateSize;
  barVertical.UpdateSize;
  RemoveWrap; UpdateWrap;
  barVertical.max:=max(0,strings.Count+1-r.h div (nxEngine.Font[font].height));
  barHorizontal.max:=0;
  for i:=0 to strings.Count-1 do begin
    w:=nxEngine.Font[font].TextW(strings[i])-r.w+integer(round(barVertical.width))*2;
    if w>barHorizontal.max then barHorizontal.max:=w;
  end;
end;

{ TUIPanel }

constructor TUIPanel.Create(_name: string);
begin
  inherited Create(_name,uiPanel);
  texture:=nxTex.IndexOf('ui_panel');
  drawStyle:=uiSections;
end;

{ TUIScrollBar }

constructor TUIScrollBar.Create(_name: string);
begin
  inherited Create(_name,uiScrollBar);
  name:=_name; texture:=nxTex.IndexOf('ui_scrollbar');
  max:=100;
  prev:=TUIButton.Create('ui_btn_prev'); prev.parent:=self;
  prev.caption:=''; prev.AbsSize:=true; prev.AbsPos:=true;
  next:=TUIButton.Create('ui_btn_next'); next.parent:=self;
  next.caption:=''; next.AbsSize:=true; next.AbsPos:=true;
  middle:=TUIButton.Create('ui_btn_mid'); middle.parent:=self;
  middle.caption:=''; middle.AbsSize:=true; middle.AbsPos:=true;
end;

destructor TUIScrollBar.Destroy;
begin
  prev.Free; next.Free; middle.Free;
  inherited Destroy;
end;

procedure TUIScrollBar.LostFocus;
begin
  prev.Focused:=false; next.Focused:=false; middle.Focused:=false;
end;

procedure TUIScrollBar.KeyDown(key: word);
begin
  case key of
    VK_UP,VK_LEFT: position:=position-1;
    VK_DOWN,VK_RIGHT: position:=position+1;
  end;
end;

procedure TUIScrollBar.MouseDown(mx, my: integer);
begin
  focus:=3; scrollDelay:=nxEngine.FrameTick+500;
  LostFocus;
  if prev.PointInside(mx,my) then begin
    prev.down:=true; focus:=1; prev.Focused:=true;
    position:=position-1;
  end else if next.PointInside(mx,my) then begin
    next.down:=true; focus:=2; next.Focused:=true;
    position:=position+1;
  end else begin
    MouseMove(mx,my); middle.Focused:=true;
  end;
  if assigned(onMouseDown) then onMouseDown(self,mbLeft);
end;

procedure TUIScrollBar.MouseMove(mx, my: integer);
var r: TBoundsRect;
begin
  if focus=3 then begin
    r:=GetDrawRect;
    if Vertical then begin
      position:=round(max*(my-r.y-r.w*1.5)/(r.h-r.w*3));
    end else begin // Horizontal
      position:=round(max*(mx-r.x-r.h*1.5)/(r.w-r.h*3));
    end;
    {if Vertical then begin
      position:=(max)*(my-r.y-r.w*3 div 2) div (r.h-r.w*3);
    end else begin // Horizontal
      position:=(max)*(mx-r.x-r.h*3 div 2) div (r.w-r.h*3);
    end;}
  end;
  if assigned(onMouseMove) then onMouseMove(self,mx,my);
end;

procedure TUIScrollBar.MouseUp;
begin
  if focus=1 then prev.down:=false
  else if focus=2 then next.down:=false;
  focus:=0;
  if assigned(onMouseUp) then onMouseUp(self,mbLeft);
end;

procedure TUIScrollBar.MouseWheel(scroll: integer);
begin
  position:=position-scroll*(1+max div 20);
end;

procedure TUIScrollBar.SetMax(p: integer);
begin
  FMax:=p;
  if FMax<0 then FMax:=0;
  if position>FMax then FPosition:=FMax;
end;

procedure TUIScrollBar.SetPosition(p: integer);
begin
  if p<>FPosition then begin
    FPosition:=p;
    if FPosition>max then FPosition:=max
    else if FPosition<0 then FPosition:=0;
    if assigned(onScroll) then onScroll(self);
  end;
end;

procedure TUIScrollBar.UpdateSize;
var s: integer;
begin
  if vertical then s:=DrawW
  else s:=DrawH;
  prev.width:=s; prev.height:=s;
  next.width:=s; next.height:=s;
  if vertical then begin
    next.x:=prev.x; next.y:=prev.y+DrawH-s;
  end else begin
    next.x:=prev.x+DrawW-s; next.y:=prev.y;
  end;
end;

{ TUIText }

constructor TUILabel.Create(_name: string);
begin
  inherited Create(_name,uiLabel);
  caption:=_name;
end;

{ TUIImage }

constructor TUIImage.Create(_name: string);
begin
  inherited Create(_name,uiImage);
  texture:=nxTex.IndexOf('ui_list');
end;

{ TUITabControl }

constructor TUITabControl.Create(_name: string; AddTab: boolean);
begin
  inherited Create(_name,uiTabControl);
  texture:=nxTex.IndexOf('ui_tab'); drawStyle:=uiSections;
  TabWidth:=80; TabHeight:=30; overlap:=20;
  if AddTab then Add.caption:='Tab0';
  Add.caption:='Tab1';
  Add.caption:='Tab2';
  Add.caption:='Tab3';
  Add.caption:='Tab4';
  TabIndex:=random(count);
end;

destructor TUITabControl.Destroy;
var i: integer;
begin
  for i:=0 to count-1 do tab[i].Free;
  setlength(tab,0); count:=0;
  inherited Destroy;
end;

procedure TUITabControl.MouseDown(mx, my: integer);
begin

end;

procedure TUITabControl.SetTabIndex(n: integer);
begin
  if n<0 then n:=0;
  if n>count-1 then n:=count-1;
  if n<>FTabIndex then begin
    FTabIndex:=n; UpdateSize;
  end;
end;

function TUITabControl.Add: TUITab;
begin
  inc(count); setlength(tab,count);
  tab[count-1]:=TUITab.Create;
  result:=tab[count-1]; result.parent:=self;
  result.ParentAlpha:=true;
end;

procedure TUITabControl.Clear;
var i: integer;
begin
  for i:=1 to count-1 do tab[i].Free;
  setlength(tab,0); count:=min(1,count);
end;

function TUITabControl.Current: TUITab;
begin
  result:=tab[tabIndex];
end;

function TUITabControl.Delete(_index: integer): boolean;
var i: integer;
begin
  result:=false;
  if count<=1 then exit; // Always leave at least 1 tab
  tab[_index].Free;
  for i:=_index to count-2 do tab[i]:=tab[i+1];
  dec(count); setlength(tab,count);
  result:=true;
end;

procedure TUITabControl.UpdateSize;
var i,w,h,tabW,cx,tw: integer;
begin
  if count<1 then exit;
  w:=DrawW; h:=DrawH; tw:=min(tabWidth,w div min(count,3));
  if count<=1 then tabW:=1
  else tabW:=min(tabWidth-overlap,(w-tw) div (count-1));
  if tabW<1 then tabW:=1;
  cx:=DrawX;
  for i:=0 to count-1 do begin
    tab[i].y:=tabHeight; tab[i].height:=h-tabHeight; tab[i].width:=w;
    tab[i].posX:=cx; inc(cx,tabw);
  end;
end;

{ TUITab }

constructor TUITab.Create;
begin
  inherited Create('Tab',uiTab);
  texture:=nxTex.IndexOf('ui_window'); drawStyle:=uiSections;
  caption:='Tab'; AbsPos:=true; AbsSize:=true;
end;

{ TUIWindow }

constructor TUIWindow.Create(_name: string);
begin
  inherited Create(_name,uiWindow);
  caption:=_name; drawStyle:=uiSections;
  texture:=nxTex.IndexOf('ui_window');
  titleTex:=nxTex.IndexOf('ui_titlebar');
  titleHeight:=25; draggable:=true;
  ParentAlpha:=false;
end;

procedure TUIWindow.MouseDown(mx, my: integer);
begin
  if my<drawY+titleHeight then begin
    dragging:=true; dragX:=DrawX-mx; dragY:=DrawY-my;
  end;
  if assigned(onMouseDown) then onMouseDown(self,mbLeft);
end;

procedure TUIWindow.MouseMove(mx, my: integer);
var oldAP: boolean; r,cr: TBoundsRect;
begin
  if dragging then begin
    oldAP:=AbsPos; AbsPos:=true;
    x:=mx+dragX; y:=my+dragY;

    // Limit window movement to parent bounds
    r:=GetDrawRect;
    if parent<>nil then cr:=parent.GetDrawRect
    else cr:=bRect(0,0,nxEngine.Width,nxEngine.Height);
    if x<0 then x:=0
    else if x+r.w>=cr.w then x:=cr.w-r.w-1;
    if y<0 then y:=0
    else if y+r.h>=cr.h then y:=cr.h-r.h-1;

    AbsPos:=oldAP;
  end;
  if assigned(onMouseMove) then onMouseMove(self,mx,my);
end;

procedure TUIWindow.MouseUp;
begin
  dragging:=false;
  if assigned(onMouseUp) then onMouseUp(self,mbLeft);
end;

end.
