#nxGraph documentation

# nxGraph #

---

Uses [nxMath](nxMath.md), [nxStrings](nxStrings.md), [nxTypes](nxTypes.md)

## Classes ##
  * TNXCustomEngine
  * TCustomVertexArray
  * TNXFont
  * TTextureSet

## Other ##

```
NX_SaveFontBitmap: boolean = false;
```
If set NX\_SaveFontBitmap:=true, font creation will cause it to write 'FontBitmap.bmp' in application folder.

## TNXCustomEngine class ##

```
function CheckFile(const filename: string): boolean;
procedure ClearError;
function GetTick: cardinal;
function GetTick(range: integer; scale: single = 1): single;
function LastError: string;
function IndexOfFont(name: string): integer;
```

## TCustomVertexArray class ##

```
constructor Create(Faces, Vertices: integer; _rendermode: cardinal;	textures, normals, colors: boolean);
constructor Create(Faces: integer; _rendermode: cardinal;	textures, normals, colors: boolean);
destructor Destroy;
procedure FreeArrays;
function GetIndexCount: integer;
procedure MakeArrays;
procedure MakeLinearIndices;
procedure Set3DTextures;
procedure SetAlphaColors;
```

## TNXFont class ##

```
constructor Create(_TexSize: integer);
procedure CreateBMP(fontName: string; fontSize, _TexSize: integer);
procedure SetColor(const r,g,b: single; a: single = 1);
function TextW(s: string): integer;
function WrapLines(w,h: integer; s: TStrings; first: integer = 0): integer;
```

## TTextureSet class ##

```
constructor Create;
destructor Destroy;
function AddTexture2(name,filename: string; transparency: boolean = false): integer;
function Add3DTexture2(name,filename: string; cols,rows: integer; transparency: boolean = false): integer;
procedure Clear;
procedure CopyArea(source, dest: PTexture; x,y: integer);
function FindTex(tex: PTexture): integer;
function GetMemoryUsage: cardinal;
procedure GetPatternCoords(var x,y,w,h: single; pattern: integer);
function IndexOf(name: string): integer;
function LoadBMPDataFile(tex: PTexture; filename: string): boolean;
function LoadBMPData(tex: PTexture; bmp: TBitmap): boolean;
function LoadBMPData(tex: integer; bmp: TBitmap): boolean;
function LoadPNGData(tex: PTexture; filename: string): boolean;
function LoadJPGData(tex: PTexture; filename: string): boolean;
function LoadTextureData(tex: PTexture; filename: string): boolean;
procedure LoadRaw(tex: PTexture; RawImage: TRawImage);
function NewName(const base: string): string;
procedure ReadFromFile(var f: TextFile);
procedure RemoveTexture(n: integer);
procedure Resize(_texture: PTexture; newWidth, newHeight: integer);
procedure SetColorKey(r,g,b: byte);
procedure SetCount(n: integer);
procedure SetPattern(tex: integer; PatternWidth, PatternHeight, SkipWidth, SkipHeight: word);
procedure SetSize(tex: integer; Width, Height: word; DefaultPattern: boolean = true);
procedure Swap(tex1,tex2: integer);
procedure WriteToFile(f: TStringList);
```