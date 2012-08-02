unit nxGame;

interface

uses SysUtils, Classes, Forms, Controls, nxGraph, nxTypes
  {$IFDEF windows}, windows{$ENDIF};

type

  { TGameHandler }

  TGameHandler = class
  private
    FCenterMouse: boolean;
    FrameInterval, nextTick: cardinal;
  protected
    procedure ResetTick;
  public
    Initialized: boolean;
    keys: array[0..255] of boolean;
    mb: array[1..5] of boolean;
    modPath, progDir: string;
    mpt, mouseXSpeed, mouseYSpeed: single;
    mp, mDelta: TVector2f;
    Shift: TShiftState;
    t: cardinal;
    constructor Create;
    destructor Destroy; override;
    procedure CenterMouse(enable: boolean);
    function GetPath(filename: string): string;
    procedure Idle;
    procedure KeyDown(key: word; _Shift: TShiftState);
    procedure KeyUp(key: word; _Shift: TShiftState);
    procedure MouseDown(button: TMouseButton; _Shift: TShiftState);
    procedure MouseMove(x, y: integer; _Shift: TShiftState);
    procedure MouseUp(button: TMouseButton; _Shift: TShiftState);
    procedure SetFrameInterval(interval: cardinal);

    // Override these
    procedure Draw; virtual; abstract;
    procedure GameLoop; virtual; abstract;
  end;

implementation

{ TGameHandler }

constructor TGameHandler.Create;
begin
  Initialized:=false;
  modPath:='mods\';
  mouseXSpeed:=1; mouseYSpeed:=1;
  mp:=vector2f(-9999, -9999);
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
  FCenterMouse:=enable;
  mDelta.x:=0; mDelta.y:=0;
  if enable then begin
    x:=nxEngine.nxHWND.ClientOrigin.x+nxEngine.Width div 2;
    y:=nxEngine.nxHWND.ClientOrigin.y+nxEngine.Height div 2;
    mp:=vector2f(x, y);
    {$IFDEF windows}
    SetCursorPos(x, y);
    {$ELSE}

    {$ENDIF}
  end else
    mp:=vector2f(-9999, -9999);
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
var frameSkips: integer; {$IFDEF windows}_mp, center: TPoint;{$ENDIF}
begin
  if not initialized then exit;
  t:=nxEngine.GetTick;
  if t<nextTick then begin
    Application.ProcessMessages; Sleep(1);
  end else begin
    if FCenterMouse and application.Active then begin
      {$IFDEF windows}
      center.x:=nxEngine.nxHWND.ClientOrigin.x+nxEngine.Width div 2;
      center.y:=nxEngine.nxHWND.ClientOrigin.y+nxEngine.Height div 2;
      GetCursorPos(_mp);
      if (_mp.x<>center.x) or (_mp.y<>center.y) then begin
        mDelta.x:=(_mp.x-center.x)*mouseXSpeed;
        mDelta.y:=(_mp.y-center.y)*mouseYSpeed;
        mp.x:=mp.x+mDelta.x; mp.y:=mp.y+mDelta.y;
        SetCursorPos(center.x, center.y);
      end else begin

      end;
      {$ELSE}

      {$ENDIF}
    end;

    frameSkips:=0;
    repeat
      nextTick:=nextTick+FrameInterval;
      GameLoop;
      inc(frameSkips);
    until (nextTick>t) or (frameSkips=10);
    Draw;
    mDelta.x:=0; mDelta.y:=0;
  end;
end;

procedure TGameHandler.KeyDown(key: word; _Shift: TShiftState);
begin
  Shift:=_Shift; keys[byte(key)]:=true;
end;

procedure TGameHandler.KeyUp(key: word; _Shift: TShiftState);
begin
  Shift:=_Shift; keys[byte(key)]:=false;
end;

procedure TGameHandler.MouseDown(button: TMouseButton; _Shift: TShiftState);
begin
  Shift:=_Shift;
  case button of
    mbLeft: mb[1]:=true;
    mbRight: mb[2]:=true;
    mbMiddle: mb[3]:=true;
    mbExtra1: mb[4]:=true;
    mbExtra2: mb[5]:=true;
  end;
end;

procedure TGameHandler.MouseMove(x, y: integer; _Shift: TShiftState);
var old: TVector2f;
begin
  Shift:=_Shift;
  if not FCenterMouse then begin
    old:=mp;
    mp.x:=x; mp.y:=y;
    if old.x>-9998 then begin
      mDelta.x:=(mp.x-old.x)*mouseXSpeed;
      mDelta.y:=(mp.y-old.y)*mouseYSpeed;
    end;
  end;
end;

procedure TGameHandler.MouseUp(button: TMouseButton; _Shift: TShiftState);
begin
  Shift:=_Shift;
  case button of
    mbLeft: mb[1]:=false;
    mbRight: mb[2]:=false;
    mbMiddle: mb[3]:=false;
    mbExtra1: mb[4]:=false;
    mbExtra2: mb[5]:=false;
  end;
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

end.
