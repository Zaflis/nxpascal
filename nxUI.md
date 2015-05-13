#nxUI documentation

# nxUI #

---

uses [nxGraph](nxGraph.md), [nxStrings](nxStrings.md), [nxTypes](nxTypes.md)

## Classes ##
  * TUIElement
  * TUIContainer (TUIElement)
  * TUIButton (TUIElement)
  * TUICheckBox (TUIElement)
  * TUIDropDown (TUIElement)
  * TUIEdit (TUIElement)
  * TUIImage (TUIElement)
  * TUILabel (TUIElement)
  * TUIList (TUIElement)
  * TUIMemo (TUIElement)
  * TUIPanel (TUIContainer)
  * TUIScrollBar (TUIElement)
  * TUITab (TUIContainer)
  * TUITabControl (TUIElement)
  * TUIWindow (TUIContainer)
  * TCustomUI (TUIPanel)

## Other ##
```
var nxUIEditorMode: boolean = false;
```

## TUIElement class ##

```
eType: TUIElementType;
name: string;
x,y, width,height: single;
color: TfRGBA;
Visible, Enabled, ParentAlpha: boolean;
parent: TUIElement;
drawStyle: TUIDrawStyle;
bdSize: byte; // Used by section drawing
onMouseMove: TUIMouseMove;
onMouseDown, onMouseUp: TUIMouseClick;
onBeforeDraw, onAfterDraw: TUIEvent;
property AbsPos: boolean;
property AbsSize: boolean;
property Focused: boolean;
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
function IsVisible: boolean;
function PointInside(_x,_y: integer): boolean;
procedure SendToBack;
procedure SetColor(r,g,b: single; a: single = 1);
procedure SetPosition(_x,_y: single);
procedure SetSize(w,h: single);
```

## TUIContainer class ##

```
element: array of TUIElement;
property count: integer read FCount;
constructor Create(const _name: string; const _eType: TUIElementType);
destructor Destroy;
procedure Add(const e: TUIElement);
procedure Clear;
procedure Delete(_index: integer; noFree: boolean = false);
function Delete(e: TUIElement; noFree: boolean = false): boolean;
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
```

## TUIButton class ##

## TUICheckBox class ##

## TUIDropDown class ##

## TUIEdit class ##

## TUIImage class ##

## TUILabel class ##

## TUIList class ##

## TUIMemo class ##

## TUIPanel class ##

## TUIScrollBar class ##

## TUITab class ##

## TUITabControl class ##

## TUIWindow class ##

## TCustomUI class ##