#nxData documentation

# nxData #

---

uses [nxGraph](nxGraph.md), [nxTypes](nxTypes.md)

## Classes ##
  * TDataStore

## Other ##
```
var nxDataThreadCount: integer;
```
Tells how many threads are alive. Not recommended to write to it manually, but you can hold program exit until threads are all freed.

## TDataStore class ##

```
MaxTime, lastCheck: cardinal;
property count: integer;
constructor Create(_blockSize: integer; _level: TCompressionlevel = clDefault);
destructor Destroy;
procedure Clear;
procedure Compress(n: integer; leaveData, forced: boolean);
procedure DeCompress(n: integer; leaveCompressed, forced: boolean);
function GetBlock(n: integer; forced: boolean): PointerArrayType;
procedure Initialize(newBlockSize: integer);
function IsLoaded(n: integer): boolean;
procedure LoadFromFile(filename: string; CanCreate: boolean = false);
procedure Save;
procedure SetCompressionlevel(_level: TCompressionlevel);
procedure SetFile(filename: string);
procedure SetSize(n: integer);
procedure Update;
```