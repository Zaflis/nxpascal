#nxTypes documentation

# nxTypes #

---


## Functions and procedures ##

```
function pointf(const x, y: single): TVector2f;
function rectf(const x1, y1, x2, y2: single): TRectf;
function recti(const x1, y1, x2, y2: integer): TRecti;
function bRect(const x, y, w, h: integer): TBoundsRect;
function vector(const x, y: single; const z: single = 0): TVector;
function vector2f(const x, y: single): TVector2f;
function vector3i(const x, y, z: integer): TVector3i;
function vector4f(const x, y, z: single; const w: single = 1): TVector4f;
function RGB(const r, g, b: byte): TRGB;
function RGBA(const r, g, b: byte; const a: byte = 255): TRGBA;
function fRGBA(const r, g, b, a: single): TfRGBA;
```
Creator-functions for multiple types.

```
procedure switchVar(const p1, p2: Pointer; const Size: integer);
```
Switches value of 2 variables of same memory size, using pointers.

```
procedure FixPath(var path: string);
function getFixPath(const path: string): string;
```
Makes given path crossplatform. FixPath('/path1/path2') would do nothing with linux based systems, but change it to '\path1\path2' with Windows.

```
function UTFToChr(const s: string): char;
function ChrToUTF(const c: char): string;
```
Converts single character between UTF and compact character table used by nxPascal fonts. Includes all ascii and EU-wide latin characters, everything that can be found on keyboards. Fit in 1 byte of 0..255.