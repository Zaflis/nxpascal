#nxBass documentation

# nxBass #

---

Uses [BASS](http://www.un4seen.com/), [nxTypes](nxTypes.md)
(Notice: bass.dll is required.)

## Classes ##
  * TBassEngine
  * TBassSound
  * TNXSoundListener

## TBassEngine class ##

```
sound: array of TBassSound;
count: integer;
listener: TNXSoundListener;
Use3D: boolean;
constructor Create(winHandle: HWND; _3D: boolean; freq: DWORD = 44100);
destructor Destroy;
function Add(const name, filename: string; _3D: boolean; channels: integer=1): TBassSound;
function CheckErrors: boolean;
procedure Clear;
function CPU: single;
procedure DeleteSound(index: integer);
function Find(const name: string): TBassSound;
function IndexOf(const name: string): integer;
procedure Resume;
procedure Set3DFactors(distf, rollf, doppf: single);
procedure Stop;
procedure Update;
```

## TBassSound class ##

```
property Use3D: boolean;
property Channel: QWORD;
property Position: QWORD;
property TimePos: TTime;
constructor Create(const filename: string; _3D: boolean; channels: integer);
destructor Destroy;
procedure FadeFreq(freq: single; ms: cardinal);
procedure FadePan(pan: single; ms: cardinal);
procedure FadeVolume(vol: single; ms: cardinal);
function GetVolume: single;
function Length: QWORD;
procedure Load(const filename: string; _3d: boolean = false);
procedure Pause;
function Play: boolean;
function Playing: boolean;
procedure Set3DAttributes(min, max: FLOAT; iangle, oangle, outvol: LongInt);
procedure SetLocation(const _loc: TVector);
procedure SetOrientation(const _front: TVector);
procedure SetPanning(pan: single);
procedure SetVelocity(const vel: TVector; fps: single);
procedure SetVolume(vol: single);
procedure Stop;
function TimeLength: TTime;
procedure Unload;
procedure Update;
```

## TNXSoundListener class ##

```
procedure SetLocation(const _loc: TVector);
procedure SetOrientation(const _front, _up: TVector);
procedure SetVelocity(const vel: TVector; fps: single);
```