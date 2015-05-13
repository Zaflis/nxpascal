#nxSound documentation

# nxSound #
Unit is focused on [OpenAL](http://www.noeska.com/doal/). Check [nxBass](nxBass.md) for more finished BASS-implementation.

---

uses [nxTypes](nxTypes.md), [OpenAL](http://www.noeska.com/doal/)

## Classes ##
  * TNXSoundEngine
  * TNXSoundListener
  * TNXSoundSource

## Other ##
```
var sound: TNXSoundEngine;
```
Instantiates itself in sound-variable when unit is used.

## TNXSoundEngine class ##

```
buffer: array of TNXSoundBuffer;
source: array of TNXSoundSource;
bufferCount, count: integer;
listener: TNXSoundListener;
constructor Create;
destructor Destroy;
function AddWAV(const filename: string): TNXSoundBuffer;
function AddSource(const _name: string; buf: TNXSoundBuffer): TNXSoundSource;
procedure ClearSources;
procedure Clear;
procedure DeleteSource(index: integer);
procedure DeleteBuffer(index: integer);
function Find(const name: string): TNXSoundSource;
function IndexOf(const name: string): integer;
```

## TNXSoundListener class ##

```
position, velocity: TVector;
orientation: TListenerOrientation;
```

## TNXSoundSource class ##

```
position, velocity: TVector;
constructor Create(const _name: string; buf: TNXSoundBuffer);
destructor Destroy; override;
procedure Pause;
procedure Play;
procedure SetBuffer(buf: TNXSoundBuffer);
procedure SetLooping(enable: boolean = true);
procedure Stop;
procedure Update;
```