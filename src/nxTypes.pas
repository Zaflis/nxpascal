unit nxTypes;

{$H+}
{$I nxInc.inc}

interface

{$IFnDEF windows}uses SysUtils;{$ENDIF}

const ToRad = 0.0174532925199433;  // (PI / 180) = Deg To Rad
      ToDeg = 57.295779513082321;  // (180 / PI) = Rad To Deg
      TextCutChar = #30;
      {$IFDEF windows}PathChar = '\';
      {$ELSE}PathChar = PathDelim;{$ENDIF}
      EPSILON: Single = 1e-40;

type
  {$IFDEF fpc}PointerArrayType = PByte;
  {$ELSE}PointerArrayType = PByteArray;
  {$ENDIF}
  DynamicByteArray = array of byte;
  TRGB = packed record r,g,b: byte; end;
  TRGBA = packed record r,g,b,a: byte; end;
  TfRGB = record r,g,b: single; end;
  TfRGBA = record r,g,b,a: single; end;
  TVector2f = packed record x,y: single; end;
  TVector3f = packed record x,y,z: single; end;
  TVector3d = packed record x,y,z: double; end;
  TVector3i = packed record x,y,z: longint; end;
  TVector4f = packed record x,y,z,w: single; end;
  TQuaternion = TVector4f;
  PVector2f = ^TVector2f;
  PVector3f = ^TVector3f;
  PVector4f = ^TVector4f;
  fPoint = TVector2f;
  TVector = TVector3f;
  PVector = PVector3f;
  TRecti = packed record x1,y1,x2,y2: longint; end;
  TBoundsRect = record x,y,w,h: longint; end;
  TRectf = packed record x1,y1,x2,y2: single; end;
  pRGB = ^TRGB;
  pRGBA = ^TRGBA;

  TByteArray = array[0..65535] of byte;
  PByteArray = ^TByteArray;
  TWordArray = array[0..65535] of word;
  PWordArray = ^TWordArray;

  TVector2fArray = array of TVector2f;
  TVector3fArray = array of TVector3f;
  PVector2fArray = ^TVector2fArray;
  PVector3fArray = ^TVector3fArray;

  TMatrix3f = array[0..2,0..2] of single;
  TMatrix4f = array[0..3,0..3] of single;
  TMatrix4d = array[0..3,0..3] of double;
  TMatrix = TMatrix4f;
  PMatrix = ^TMatrix;
  PMatrix3f = ^TMatrix3f;

  TTextureLoadOptions = set of (toMipMap, toAlphaColor, toKeepData,
    toScale, toFitScale, toCustomScale, toColorKey, toFileName, toNoLoad);

  TPathingMode = (pmInterpolate, pmSmooth, pmCatmull);

  TMouseRay = record
    start, dir: TVector;
  end;
  PMouseRay = ^TMouseRay;

  T2DVertex = packed record // size 8 float
    v: TVector2f;
    uv: TVector2f;
    color: TfRGBA;
  end;
  T3DVertex = packed record // size 12 float
    v, n: TVector;
    uv: TVector2f;
    color: TfRGBA;
  end;

  T3DLight = packed record // size 7 float
    position: TVector;
    radius: single;
    color: TfRGB;
  end;

  {TTexturePart = record
    r: TRecti; center: TVector2f; name: string;
  end;}

  function pointf(const x, y: single): TVector2f;
  function rectf(const x1, y1, x2, y2: single): TRectf;
  function recti(const x1, y1, x2, y2: longint): TRecti;
  function boundsf(const x1, y1, w, h: single): TRectf;
  function boundsi(const x1, y1, w, h: longint): TRecti;
  function bRect(const x, y, w, h: longint): TBoundsRect;
  procedure switchVar(const p1, p2: Pointer; const Size: longint);
  function vector(const x, y: single; const z: single = 0): TVector;{$IFDEF CanInline}inline;{$ENDIF}
  function vector2f(const x, y: single): TVector2f;
  function vector3i(const x, y, z: longint): TVector3i;
  function vector4f(const x, y, z: single; const w: single = 1): TVector4f;{$IFDEF CanInline}inline;{$ENDIF}
  function RGB(const r, g, b: byte): TRGB;
  function RGBA(const r, g, b: byte; const a: byte = 255): TRGBA;
  function fRGB(const r, g, b: single): TfRGB;
  function fRGBA(const r, g, b, a: single): TfRGBA;
  function MultiplyColor(const a, b: TfRGB): TfRGB; overload;
  function MultiplyColor(const a, b: TfRGBA): TfRGBA; overload;
  procedure FixPath(var path: string);
  function getFixPath(const path: string): string;
  function UTFToChr(const s: string): char;
  function ChrToUTF(const c: char): string;

var
  nxError: string = '';

  procedure nxSetError(err: string);
  procedure nxClearError;

{$IFnDEF fpc}
  // Workaround for missing functions with Delphi
  function FileExistsUTF8(filename: string): boolean;
  function SysToUTF8(const s: string): string;
  function UTF8ToSys(const s: string): string;
  function UTF8Length(const s: string): longint;
  function UTF8Copy(const s: string; StartCharIndex, CharCount: longint): string;
{$ENDIF}

implementation

{$IFnDEF fpc}
//uses SysUtils;

function FileExistsUTF8(filename: string): boolean;
begin
  result:=FileExists(filename);
end;

function SysToUTF8(const s: string): string;
begin
  result:=s;
end;

function UTF8ToSys(const s: string): string;
begin
  result:=s;
end;

function UTF8Length(const s: string): longint;
begin
  result:=length(s);
end;

function UTF8Copy(const s: string; StartCharIndex, CharCount: longint): string;
begin
  result:=copy(s, StartCharIndex, CharCount);
end;
{$ENDIF}

function pointf(const x, y: single): TVector2f;
begin
  result.x:=x; result.y:=y;
end;

function rectf(const x1, y1, x2, y2: single): TRectf;
begin
  result.x1:=x1; result.y1:=y1;
  result.x2:=x2; result.y2:=y2;
end;

function recti(const x1, y1, x2, y2: longint): TRecti;
begin
  result.x1:=x1; result.y1:=y1;
  result.x2:=x2; result.y2:=y2;
end;

function boundsf(const x1, y1, w, h: single): TRectf;
begin
  result.x1:=x1; result.y1:=y1;
  result.x2:=x1+w; result.y2:=y1+h;
end;

function boundsi(const x1, y1, w, h: longint): TRecti;
begin
  result.x1:=x1; result.y1:=y1;
  result.x2:=x1+w-1; result.y2:=y1+h-1;
end;

function bRect(const x, y, w, h: longint): TBoundsRect;
begin
  result.x:=x; result.y:=y;
  result.w:=w; result.h:=h;
end;

procedure switchVar(const p1, p2: Pointer; const Size: longint);
var temp: pointer;
begin
  getmem(temp, size);
  move(p1^, temp^, Size);
  move(p2^, p1^, Size);
  move(temp^, p2^, Size);
  freemem(temp);
end;

function vector(const x, y: single; const z: single): TVector;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result.x:=x; result.y:=y; result.z:=z;
end;

function vector2f(const x, y: single): TVector2f;
begin
  result.x:=x; result.y:=y;
end;

function vector3i(const x, y, z: longint): TVector3i;
begin
  result.x:=x; result.y:=y; result.z:=z;
end;

function vector4f(const x, y, z, w: single): TVector4f;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result.x:=x; result.y:=y; result.z:=z; result.w:=w;
end;

function RGB(const r, g, b: byte): TRGB;
begin
  result.r:=r; result.g:=g; result.b:=b;
end;

function RGBA(const r, g, b: byte; const a: byte): TRGBA;
begin
  result.r:=r; result.g:=g; result.b:=b; result.a:=a;
end;

function fRGB(const r, g, b: single): TfRGB;
begin
  result.r:=r; result.g:=g; result.b:=b;
end;

function fRGBA(const r, g, b, a: single): TfRGBA;
begin
  result.r:=r; result.g:=g; result.b:=b; result.a:=a;
end;

function MultiplyColor(const a, b: TfRGB): TfRGB;
begin
  result.r:=a.r*b.r;
  result.g:=a.g*b.g;
  result.b:=a.b*b.b;
end;

function MultiplyColor(const a, b: TfRGBA): TfRGBA;
begin
  result.r:=a.r*b.r;
  result.g:=a.g*b.g;
  result.b:=a.b*b.b;
  result.a:=a.a*b.a;
end;

procedure FixPath(var path: string);
var i: longint; c: char;
begin
  for i:=1 to length(path) do begin
    c:=path[i];
    if ((c='\') or (c='/')) and (c<>PathChar) then path[i]:=PathChar;
  end;
end;

function getFixPath(const path: string): string;
begin
  result:=path; FixPath(result);
end;

function UTFToChr(const s: string): char;
begin
  if s='' then result:=char(0)
  else begin
    {$IFnDEF fpc}result:=s[1];{$ELSE}
    case byte(s[1]) of
      194: result:=char((byte(s[2])-163)+128);
      195: result:=char((byte(s[2])-130)+160);
      226: result:=char(255);
      else result:=s[1];
    end;{$ENDIF}
  end;
end;

function ChrToUTF(const c: char): string;
begin
  {$IFnDEF fpc}result:=c;{$ELSE}
  case byte(c) of
    0..127: result:=c;
    255: result:='€';
    128..159: begin
        result:='£';
        result[2]:=char((byte(c)-128)+163);
      end;
    else begin
      result:='Â';
      result[2]:=char((byte(c)-160)+130);
    end;
  end;{$ENDIF}
end;

procedure nxSetError(err: string);
begin
  if nxError='' then nxError:=err;
end;

procedure nxClearError;
begin
  nxError:='';
end;

end.
