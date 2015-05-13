#nxGame documentation

# nxGame #

---

Uses [nxGraph](nxGraph.md), [nxTypes](nxTypes.md), (mouseandkeyinput with Lazarus)

## Classes ##
  * TGameHandler

## TGameHandler class ##

```
constructor Create;
```
Constructor for TGameHandler. When inheriting from the class, there are 2 final operations you should do in overridden constructor:
  * Set Initialized:=true; (This will tell class that constructor has successfully finished. Idle-method will silently quit before even starting if not initialized.)
  * ResetTick; (It may take time to load textures and your game data. Reset framerate counter with this.)

```
destructor Destroy; override;
```
Class destructor.

```
procedure CenterMouse(enable: boolean);
```
Set mouse centering on/off. This will center mouse in the middle of game window. Use mDelta.x, mDelta.y to retrieve change per tick, otherwise mp.x, mp.y.
While mouse-centering is on, mp vector is also updated within window boundaries.
Centering is also affected by mouseXSpeed, mouseYSpeed, which are by default 1.0.

```
function GetCursorPos: TPoint;
```
Get mouse-cursor position on screen. Crossplatform.

```
function GetPath(filename: string): string;
```
Returns automatic path for file, checking modPath first. You can define modPath:='mods', and it would look in '\AppPath\mods\filename'.

```
procedure Idle;
```
Called in TApplicationProperties.onIdle event.
Calls GameLoop- and Draw-methods at frequency defined by interval. Will Sleep(1) to rest hardware at other times.

```
procedure KeyDown(key: word; _Shift: TShiftState);
```
Called in TForm.onKeyDown event.

```
procedure KeyUp(key: word; _Shift: TShiftState);
```
Called in TForm.onKeyUp event.

```
procedure MouseDown(button: TMouseButton; _Shift: TShiftState);
```
Called in TForm.onMouseDown event.

```
procedure MouseMove(x, y: integer; _Shift: TShiftState);
```
Called in TForm.onMouseMove event.

```
procedure MouseUp(button: TMouseButton; _Shift: TShiftState);
```
Called in TForm.onMouseUp event.

```
procedure SetCursorPos(x, y: integer);
```
Move mouse-cursor on screen. Crossplatform.

```
procedure SetFrameInterval(interval: cardinal);
```
Sets frame-interval. Default 16, for commonly steady 60 fps. 1000 / interval = FPS. Minimum interval 1, resulting maximum of 1000 FPS.

```
procedure SetFrameSkipping(enable: boolean);
```
Sets frame-skipping on/off.

```
procedure Draw; virtual; abstract;
```
Override Draw method to define your own drawing event. Notice, frameskipping may be ignoring Drawing if last frame took long time. Always update your changing variables in GameLoop instead.
Maximum frameskips is set to 10.

```
procedure GameLoop; virtual; abstract;
```
Override GameLoop to define what happens in game each tick.