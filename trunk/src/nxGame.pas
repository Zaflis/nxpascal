unit nxGame;

{
  In Lazarus you may have to do this for each project:
   "Project" menu -> "Project inspector"
   -> Press "+" button -> New requirement
     -> Add "LazOpenGLContext" (if using nxGL unit)
     -> Add "Lazmouseandkeyinput"
}

interface

uses {$IFDEF fpc}mouseandkeyinput,{$ELSE}windows,{$ENDIF}
  SysUtils, Classes, Forms, Controls, nxGraph, nxTypes;

type

  { TGameHandler }

  TGameHandler = class
  private
    FrameInterval, nextTick: cardinal;
    FNoFrameSkipping: boolean;
  protected
    procedure ResetTick;
  public
    FrameSkips: integer;
    Initialized, isMouseCentered, FMouseInitialized: boolean;
    keys: array[0..255] of boolean;
    mb: array[1..5] of boolean;
    modPath, progDir: string;
    mpt, mouseXSpeed, mouseYSpeed: single;
    mp, mDelta: TVector2f;
    ShiftState: TShiftState;
    t: cardinal;
    constructor Create;
    destructor Destroy; override;
    procedure CenterMouse(enable: boolean = true);
    function GetCursorPos: TPoint;
    function GetPath(filename: string): string;
    procedure Idle;
    procedure KeyDown(key: word; Shift: TShiftState);
    procedure KeyUp(key: word; Shift: TShiftState);
    procedure MouseDown(button: TMouseButton; Shift: TShiftState);
    procedure MouseMove(x, y: integer; Shift: TShiftState);
    procedure MouseUp(button: TMouseButton; Shift: TShiftState);
    procedure SetCursorPos(x, y: integer);
    procedure SetFrameInterval(interval: cardinal);
    procedure SetFrameSkipping(enable: boolean);

    // Override these
    procedure Draw; virtual; abstract;
    procedure GameLoop; virtual; abstract;
  end;

implementation

{ TGameHandler }

constructor TGameHandler.Create;
begin
  Initialized:=false;
  Randomize;
  modPath:='mods\';
  mouseXSpeed:=1; mouseYSpeed:=1;
  FMouseInitialized:=true;
  SetFrameInterval(16);
  progDir:=ExtractFilePath(Application.ExeName);
  ResetTick;
end;

destructor TGameHandler.Destroy;
begin
  inherited Destroy;
end;

procedure TGameHandler.CenterMouse(enable: boolean);
var x, y: integer;
begin
  isMouseCentered:=enable;
  mDelta.x:=0; mDelta.y:=0;
  x:=nxEngine.nxHWND.ClientOrigin.x+nxEngine.Width div 2;
  y:=nxEngine.nxHWND.ClientOrigin.y+nxEngine.Height div 2;
  SetCursorPos(x, y);
  if enable then mp:=vector2f(x, y)
  else mp:=vector2f(nxEngine.Width div 2, nxEngine.Height div 2);
  FMouseInitialized:=false;
end;

function TGameHandler.GetCursorPos: TPoint;
begin
  result:=mouse.CursorPos;
end;

function TGameHandler.GetPath(filename: string): string;
begin
  if modPath<>'' then begin
    if fileexists(modPath+filename) then result:=modPath+filename
    else if fileexists(modPath+PathChar+filename) then
      result:=modPath+PathChar+filename
    else result:=filename;
  end else result:=filename;
end;

procedure TGameHandler.Idle;
var _mp, center: TPoint;
begin
  if not initialized then exit;
  t:=nxEngine.GetTick;
  if t>=nextTick then begin
    // Center mouse
    if isMouseCentered and application.Active then begin
      center.x:=nxEngine.nxHWND.ClientOrigin.x+nxEngine.Width div 2;
      center.y:=nxEngine.nxHWND.ClientOrigin.y+nxEngine.Height div 2;
      _mp:=GetCursorPos;
      if (_mp.x<>center.x) or (_mp.y<>center.y) then begin
        mDelta.x:=(_mp.x-center.x)*mouseXSpeed;
        mDelta.y:=(_mp.y-center.y)*mouseYSpeed;
        mp.x:=mp.x+mDelta.x; mp.y:=mp.y+mDelta.y;
        SetCursorPos(center.x, center.y);
      end;
      if mp.x<0 then mp.x:=0
      else if mp.x>=nxEngine.Width then mp.x:=nxEngine.Width-1;
      if mp.y<0 then mp.y:=0
      else if mp.y>=nxEngine.Height then mp.y:=nxEngine.Height-1;
    end;

    GameLoop;
    mDelta.x:=0; mDelta.y:=0;
    if FrameInterval=0 then nextTick:=t
    else begin
      nextTick:=nextTick+FrameInterval;
      FrameSkips:=0;
      if not FNoFrameSkipping then
        while (nextTick<=t) and (FrameSkips<10) do begin
          inc(FrameSkips);
          nextTick:=nextTick+FrameInterval;
          GameLoop;
        end;
    end;

    Draw;
  end else if FrameInterval>0 then begin
    Application.ProcessMessages; Sleep(1);
  end;
end;

procedure TGameHandler.KeyDown(key: word; Shift: TShiftState);
begin
  ShiftState:=Shift; keys[byte(key)]:=true;
end;

procedure TGameHandler.KeyUp(key: word; Shift: TShiftState);
begin
  ShiftState:=Shift; keys[byte(key)]:=false;
end;

procedure TGameHandler.MouseDown(button: TMouseButton; Shift: TShiftState);
begin
  ShiftState:=Shift;
  case button of
    mbLeft: mb[1]:=true;
    mbRight: mb[2]:=true;
    mbMiddle: mb[3]:=true;
    {$IFDEF fpc}
    mbExtra1: mb[4]:=true;
    mbExtra2: mb[5]:=true;
    {$ENDIF}
  end;
end;

procedure TGameHandler.MouseMove(x, y: integer; Shift: TShiftState);
var old: TVector2f;
begin
  ShiftState:=Shift;
  if mb[2] and (not (ssRight in Shift)) then mb[2]:=false;
  if not isMouseCentered then begin
    old:=mp;
    mp.x:=x; mp.y:=y;
    if FMouseInitialized then begin
      mDelta.x:=(mp.x-old.x)*mouseXSpeed;
      mDelta.y:=(mp.y-old.y)*mouseYSpeed;
    end else
      FMouseInitialized:=true;
  end;
end;

procedure TGameHandler.MouseUp(button: TMouseButton; Shift: TShiftState);
begin
  ShiftState:=Shift;
  case button of
    mbLeft: mb[1]:=false;
    mbRight: mb[2]:=false;
    mbMiddle: mb[3]:=false;
    {$IFDEF fpc}
    mbExtra1: mb[4]:=false;
    mbExtra2: mb[5]:=false;
    {$ENDIF}
  end;
end;

procedure TGameHandler.SetCursorPos(x, y: integer);
begin
  {$IFDEF fpc}mouseinput.Move([], x, y);
  {$ELSE}windows.setcursorpos(x, y);
  {$ENDIF}
end;

procedure TGameHandler.ResetTick;
begin
  t:=nxEngine.GetTick; nextTick:=t;
end;

// Default interval 16, for smooth 60 fps
procedure TGameHandler.SetFrameInterval(interval: cardinal);
begin
  FrameInterval:=interval;
  mpt:=FrameInterval/1000.0;
end;

procedure TGameHandler.SetFrameSkipping(enable: boolean);
begin
  FNoFrameSkipping:=not enable;
end;

end.
