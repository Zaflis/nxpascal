unit nxGraph;

{$IFDEF fpc}
{$mode objfpc}{$H+}
{$ENDIF}

{ TODO:
- TTextureSet.ReadFromFile, WriteToFile
- Move TCustomVertexArray to nxModel
}

interface

{ If using Delphi, add png-folder in library path aswell,
  to avoid compile error here.
  (Or move png folder contents to nx source folder.)}

uses SysUtils,{$IFDEF fpc}GraphType, LazUTF8, LCLIntf, FPimage
  {$ELSE}Windows, pngimage, JPEG{$ENDIF}, Graphics,
  {$IFDEF fpc}zstream{$ELSE}zlib{$ENDIF},
  Controls, Classes, math, nxStrings, nxMath, nxTypes;

type
  TTexture = record
    name, filename: string;
    sizeX, sizeY, sizeZ, RefCount: word;
    Width, Height, PatternWidth, PatternHeight, SkipWidth, SkipHeight: word;
    values: byte;  // values 3 or 4
    Enabled, tex3D: boolean;
    index, format, intFormat: cardinal; // GL_RGB, GL_RGBA
    Data: DynamicByteArray; // size = sizeX*sizeY*values
  end;
  PTexture = ^TTexture;

  { TTextureSet }

  TTextureSet = class
  protected
    count2: longint;
    enable3Dtex: boolean;
    function Pow2Near(var w,h: word; out sx,sy: single): boolean;
    function Pow2Fit(var w,h: word; out sx,sy: single): boolean;
  public
    count, LastTexIndex, TextureQuality, cols3D, rows3D: longint;
    Options: TTextureLoadOptions;
    scaleX, scaleY: single;
    texture: array of TTexture;
    TransparentColor: TRGB;
    constructor Create;
    destructor Destroy; override;
    function AddTexture2(name,filename: string; transparency: boolean = false): longint;
    function Add3DTexture2(name,filename: string; cols,rows: integer; transparency: boolean = false): longint;
    procedure Clear;
  	procedure CopyArea(source, dest: PTexture; x,y: integer);
    function FindTex(tex: PTexture): longint;
    function GetMemoryUsage: cardinal;
    procedure GetImageSize(out w, h: single);
    procedure GetPatternCoords(out x, y, w, h: single; pattern: longint);
    function IndexOf(name: string): longint;
    function LoadBMPDataFile(tex: PTexture; filename: string): boolean;
    function LoadBMPData(tex: PTexture; bmp: TBitmap): boolean; overload;
    function LoadBMPData(tex: longint; bmp: TBitmap): boolean; overload;
    function LoadPNGData(tex: PTexture; filename: string): boolean;
    function LoadJPGData(tex: PTexture; filename: string): boolean;
    function LoadTextureData(tex: PTexture; filename: string): boolean;
    {$IFDEF fpc}
    procedure LoadRaw(tex: PTexture; RawImage: TRawImage);
    {$ENDIF}
    function NewName(const base: string): string;
    procedure ReadFromFile(var f: TextFile);
    procedure RemoveTexture(n: longint);
 	  procedure Resize(_texture: PTexture; newWidth, newHeight: integer);
    procedure SetColorKey(r,g,b: byte);
    procedure SetCount(n: longint);
    procedure SetPattern(tex: longint; PatternWidth, PatternHeight,
      SkipWidth, SkipHeight: word);
    procedure SetSize(tex: longint; Width, Height: word; DefaultPattern: boolean = true);
    procedure Swap(tex1,tex2: longint);
    procedure WriteToFile(f: TStringList);
  end;

  { TNXFont }

  TNXFont = class
  private
  public
    name: string;
    textureI: longint;
    charW: array[32..255] of shortint;
    CenterH, sx, sy: shortint;
    TexSize, height: word;
    color: TfRGBA;
    constructor Create(_TexSize: integer);
    constructor Load(filename: string);
    constructor CreateBMP(fontName: string; fontSize, _TexSize: integer);
    constructor CreateBMPAndSave(fontName: string; fontSize, _TexSize: integer; filename: string);
    procedure SetColor(const r,g,b: single; a: single = 1);
    function TextW(s: string): longint;
    function WrapLines(w,h: integer; s: TStrings; first: integer = 0): longint;
    procedure Draw(x,y: single; s: string; maxW: integer = 0); virtual; abstract;
    procedure DrawC(x,y: single; s: string; maxW: integer = 0);
    procedure DrawCScaled(x,y, scaleX,scaleY: single; s: string; maxW: integer = 0); virtual; abstract;
    procedure DrawRotate(x,y, scaleX,scaleY, _angle: single; s: string; maxW: integer = 0); virtual; abstract;
    procedure DrawScaled(x,y, scaleX,scaleY: single; s: string; maxW: integer = 0); virtual; abstract;
    procedure DrawTextArea(r: TBoundsRect; s: TStrings; x_scroll,y_scroll: integer); virtual; abstract;
    procedure DrawWrap(r: TBoundsRect; s: TStrings; y_scroll: longint = 0); virtual; abstract;
    procedure SetTexture; virtual; abstract;
  end;

  { TCustomVertexArray }

  TCustomVertexArray = class
  protected
    function ColorComponents: integer;
    function TextureComponents: integer;
  public
    _textures,_normals,_colors,_AlphaColors,_3Dtextures: boolean;
    Count, fCount, vCount: longint;
    rendermode: Cardinal;
    fa: array of word; // Faces
    va, na: array of TVector; // Vertices, Normals
    ta: array of single; // Textures
    ca: array of byte; // Colors
    constructor Create(Faces, Vertices: longint; _rendermode: cardinal;
      textures, normals, colors: boolean); overload;
    constructor Create(Faces: longint; _rendermode: cardinal;
      textures, normals, colors: boolean); overload;
    destructor Destroy; override;
    procedure FreeArrays;
    function GetIndexCount: longint; overload;
    function GetIndexCount(const faceCount: longint): longint; overload; virtual;abstract;
    procedure MakeArrays;
    procedure MakeLinearIndices;
    procedure Set3DTextures;
    procedure SetAlphaColors;
  end;

  { TNXCustomEngine }

  TNXCustomEngine = class
  private
  public
    {$IFnDEF fpc}
    nxDC: HDC;
    nxRC: HGLRC;
    {$ENDIF}
    nxHWND: TWinControl;
    Left, Top, Width, Height, frames, FFPS: longint;
    secTick, FrameTick: cardinal;
    Enabled2D, RenderThreadReserved, PerspectiveStretch: boolean;
    Font: array of TNXFont;
    FontCount: integer;
    function CheckFile(const filename: string): boolean;
    procedure ClearError;
    function GetTick: cardinal; overload;
    function GetTick(range: longint; scale: single = 1): single; overload;
    function LastError: string;
    function IndexOfFont(name: string): integer;
  end;

var nxTex: TTextureSet;
    nxEngine: TNXCustomEngine;
    NX_SaveFontBitmap: boolean = false;

implementation

{$IFDEF fpc}uses FileUtil;{$ENDIF}

{function CompareBitmap(src,dest: TCanvas): longint;
var x,y,r: integer;
begin
  r:=0;
  for y:=0 to src.Height-1 do
    for x:=0 to src.Width-1 do begin
      r:=r+255-abs(src.Colors[x,y].red-dest.Colors[x,y].red);
      r:=r+255-abs(src.Colors[x,y].green-dest.Colors[x,y].green);
      r:=r+255-abs(src.Colors[x,y].blue-dest.Colors[x,y].blue);
    end;
  result:=r;
end; }

{ TTextureSet }

function TTextureSet.Add3DTexture2(name, filename: string; cols, rows: integer; transparency: boolean): longint;
var i: longint;
begin
  result:=-1;
  if (filename<>'') and (not FileExistsUTF8(filename)) then begin
    nxSetError(format('[Add3DTexture2] File "%s" not found.',[filename]));
    exit;
  end;
  if name='' then begin
    name:=filename;
    repeat
      i:=pos(PathChar,name);
      if i>0 then name:=strafter(name,PathChar);
    until i=0;
    if pos('.',name)>0 then name:=strbefore(name,'.');
  end;
  for i:=0 to count-1 do
    if texture[i].Enabled and (texture[i].name=name) then begin
      result:=i; exit;
    end else if not texture[i].Enabled then
      result:=i;
  if result=-1 then begin
    SetCount(count+1); result:=count-1;
    texture[result].index:=0;
    texture[result].Data:=nil;
    texture[result].RefCount:=0;
  end;
  with texture[result] do
    if transparency then values:=4
    else values:=3;
  cols3D:=cols; rows3D:=rows;
  texture[result].Enabled:=true;
  texture[result].name:=name;
  texture[result].tex3D:=true;
end;

function TTextureSet.AddTexture2(name, filename: string; transparency: boolean): longint;
var i: longint;
begin
  result:=-1;
  // File not found
  if (filename<>'') and (not FileExistsUTF8(filename)) then begin
    nxSetError(format('[AddTexture2] File "%s" not found.',[filename]));
    exit;
  end;
  // Trim name from filename if name isn't given
  if name='' then begin
    name:=filename;
    repeat
      i:=pos(PathChar, name);
      if i>0 then name:=strafter(name,PathChar);
    until i=0;
    if pos('.',name)>0 then name:=strbefore(name,'.');
  end;
  // Find existing same texture
  for i:=0 to count-1 do
    if texture[i].Enabled and (texture[i].name=name) then begin
      result:=i; exit;
    end else if not texture[i].Enabled then
      result:=i;
  // Add new texture ID if matching was not found
  if result=-1 then begin
    SetCount(count+1); result:=count-1;
    texture[result].index:=0;
    texture[result].Data:=nil;
    texture[result].sizeZ:=1;
    texture[result].RefCount:=0;
  end;
  with texture[result] do
    if transparency then values:=4
    else values:=3;
  texture[result].Enabled:=true;
  texture[result].name:=name;
  texture[result].tex3D:=false;
end;

procedure TTextureSet.Clear;
var i: longint;
begin
  for i:=0 to Count-1 do
    if texture[i].Data<>nil then begin
      setlength(texture[i].Data, 0);
    end;
  SetCount(0);
end;

procedure TTextureSet.CopyArea(source, dest: PTexture; x,y: integer);
var i,j,k,x2,y2: longint;
begin
  with dest^ do begin
    setlength(Data, SizeX*SizeY*values);
    for j:=0 to SizeY-1 do begin
      if y+j<source^.Height then y2:=y+j
      else if j=SizeY-1 then y2:=y
      else y2:=source^.Height-1;
      for i:=0 to SizeX-1 do begin
        if x+i<source^.Width then x2:=x+i
        else if i=SizeX-1 then x2:=x
        else x2:=source^.Width-1;
	      for k:=0 to 2 do
	        Data[values*(i+sizeX*j)+k]:=
            source^.Data[source^.values*(x2+source^.sizeX*y2)+k];
	 	    if values>3 then
	 	      if source^.values>3 then Data[values*(i+sizeX*j)+3]:=
            source^.Data[source^.values*(x2+source^.sizeX*y2)+3]
		      else Data[values*(i+sizeX*j)+3]:=255;
      end;
    end;
  end;
end;

constructor TTextureSet.Create;
begin
  Options:=[];
end;

destructor TTextureSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TTextureSet.FindTex(tex: PTexture): longint;
var i: longint;
begin
  for i:=count-1 downto 0 do
    if tex=@texture[i] then begin
      result:=i; exit;
    end;
  result:=-1;
end;

function TTextureSet.GetMemoryUsage: cardinal;
var i: longint;
begin
  result:=0;
  for i:=0 to count-1 do
    with texture[i] do result:=result+sizeX*sizeY*sizeZ*values;
end;

procedure TTextureSet.GetImageSize(out w, h: single);
var pTex: PTexture;
begin
  if LastTexIndex>=0 then begin
    pTex:=@texture[LastTexIndex];
    if pTex^.PatternWidth>0 then begin
      w:=pTex^.patternwidth; h:=pTex^.patternheight;
    end else begin
      w:=pTex^.Width; h:=pTex^.Height;
    end;
  end else begin
    w:=1; h:=1;
  end;
end;

// Returns texture coordinates for pattern
procedure TTextureSet.GetPatternCoords(out x, y, w, h: single; pattern: longint);
var cols, rows, mult: longint; sx, sy: single;
    pTex: PTexture;
begin
  if LastTexIndex>=0 then begin
    pTex:=@texture[LastTexIndex];
    if pTex^.PatternWidth>0 then begin
      w:=pTex^.PatternWidth/pTex^.SizeX;
      h:=pTex^.PatternHeight/pTex^.SizeY;
      sx:=pTex^.SkipWidth/pTex^.SizeX;
      sy:=pTex^.SkipHeight/pTex^.SizeY;
      cols:=(pTex^.Width+pTex^.SkipWidth) div
        (pTex^.PatternWidth+pTex^.SkipWidth);
      rows:=(pTex^.Height+pTex^.SkipHeight) div
        (pTex^.PatternHeight+pTex^.SkipHeight);
      mult:=cols*rows;
      if mult>0 then pattern:=pattern mod mult
      else pattern:=0;
      x:=(pattern mod cols)*(w+sx);
      y:=(pattern div cols)*(h+sy);
      exit;
    end;
  end;
  x:=0; y:=0; w:=1; h:=1;
end;

function TTextureSet.IndexOf(name: string): longint;
var i: longint;
begin
  for i:=0 to count-1 do
    if AnsiCompareStr(name, texture[i].name)=0 then begin
      IndexOf:=i; exit;
    end;
  IndexOf:=-1;
end;

function TTextureSet.LoadBMPData(tex: longint; bmp: TBitmap): boolean;
begin
  if tex>=0 then LoadBMPData:=LoadBMPData(@texture[tex], bmp)
  else LoadBMPData:=false;
end;

function TTextureSet.LoadBMPData(tex: PTexture; bmp: TBitmap): boolean;
var sx, sy: single; n, x1, y1: longint;
    x, y: word; UseScale: boolean;
    {$IFDEF fpc}fpColor: TFPColor;{$ENDIF}
begin
  result:=false;
  if (tex=nil) or (bmp=nil) then exit;
  tex^.Width:=bmp.Width; tex^.Height:=bmp.Height;
  tex^.sizeX:=bmp.Width; tex^.sizeY:=bmp.Height;

  //LoadRaw(tex,bmp.RawImage); // Doesn't work for BMP

  if toFitScale in Options then begin
    UseScale:=false;
    Pow2Fit(tex^.sizeX, tex^.sizeY, sx, sy);
    sx:=1; sy:=1;
  end else if toScale in Options then begin
    UseScale:=Pow2Near(tex^.sizeX, tex^.sizeY, sx, sy);
    tex^.Width:=tex^.SizeX; tex^.Height:=tex^.SizeY;
  end else if (toCustomScale in Options) and (scaleX*scaleY>0) then begin
    UseScale:=true;
    sx:=1/scaleX; sy:=1/scaleY;
    tex^.sizeX:=trunc(tex^.sizeX*scaleX);
    tex^.sizeY:=trunc(tex^.sizeY*scaleY);
    tex^.Width:=tex^.SizeX; tex^.Height:=tex^.SizeY;
  end else begin
    UseScale:=false; sx:=1; sy:=1;
  end;
  tex^.PatternWidth:=tex^.Width;
  tex^.PatternHeight:=tex^.Height;
  setlength(tex^.Data, tex^.sizeX*tex^.sizeY*tex^.values);
  n:=0;
  for y:=0 to tex^.Height-1 do begin
    if UseScale then y1:=trunc(y*sy) else y1:=y;
    for x:=0 to tex^.Width-1 do begin
      if UseScale then x1:=trunc(x*sx) else x1:=x;
      {$IFDEF fpc}
      fpColor:=bmp.Canvas.Colors[x1, y1];
      tex^.Data[n]:=fpColor.red div 256;
      tex^.Data[n+1]:=fpColor.green div 256;
      tex^.Data[n+2]:=fpColor.blue div 256;
      {$ELSE}
      tex^.Data[n]:=GetRValue(bmp.Canvas.Pixels[x1, y1]);
      tex^.Data[n+1]:=GetGValue(bmp.Canvas.Pixels[x1, y1]);
      tex^.Data[n+2]:=GetBValue(bmp.Canvas.Pixels[x1, y1]);
      {$ENDIF}
      if tex^.values=4 then
        if toAlphaColor in Options then
          tex^.Data[n+3]:=(tex^.Data[n]+tex^.Data[n+1]+tex^.Data[n+2]) div 3
        else begin // toColorKey
          if (tex^.Data[n]=TransparentColor.r) and
             (tex^.Data[n+1]=TransparentColor.g) and
             (tex^.Data[n+2]=TransparentColor.b) then
            tex^.Data[n+3]:=0
          else
            tex^.Data[n+3]:=255;
        end;
      inc(n,tex^.values);
    end;
    if not UseScale then
      inc(n, tex^.values*(tex^.sizeX-tex^.Width));
  end;
  LoadBMPData:=true;
end;

function TTextureSet.LoadBMPDataFile(tex: PTexture; filename: string): boolean;
var temp: TBitmap;
begin
  FixPath(filename);
  result:=false;
  if tex=nil then exit;
  temp:=TBitmap.Create;
  try
    temp.LoadFromFile(filename);
  except
    nxSetError(format('[LoadBMPDataFile] Error "%s"',[filename]));
    freeandnil(temp); exit;
  end;
  LoadBMPData(tex, temp);
  temp.Free; result:=true;
end;

function TTextureSet.LoadJPGData(tex: PTexture; filename: string): boolean;
var jpg: TJpegImage; {$IFnDEF fpc}bmp: TBitmap;{$ENDIF}
begin
  FixPath(filename);
  result:=false;
  if tex=nil then exit;
  try
    jpg:=TJpegImage.Create; jpg.LoadFromFile(filename);
  except
    nxSetError(format('[LoadJPGData] Error "%s"',[filename]));
    freeandnil(jpg); exit;
  end;
  tex^.Width:=jpg.Width; tex^.Height:=jpg.Height;
  {$IFDEF fpc}
  LoadRaw(tex, jpg.RawImage);
  {$ELSE}
  bmp:=TBitmap.Create; bmp.Assign(jpg);
  self.LoadBMPData(tex, bmp);
  bmp.Free;
  {$ENDIF}
  jpg.Free;
  result:=true;
end;

{$IFDEF fpc}
function TTextureSet.LoadPNGData(tex: PTexture; filename: string): boolean;
var png: TPortableNetworkGraphic;
begin
  FixPath(filename);
  LoadPNGData:=false;
  if tex=nil then exit;
  png:=TPortableNetworkGraphic.Create;
  try
    png.LoadFromFile(filename);
  except
    nxSetError(format('[LoadPNGData] Error "%s"',[filename]));
    freeandnil(png); exit;
  end;
  tex^.Width:=png.Width; tex^.Height:=png.Height;
  LoadRaw(tex, png.RawImage);
  png.Free; LoadPNGData:=true;
end;
{$ELSE}
function TTextureSet.LoadPNGData(tex: PTexture; filename: string): boolean;
var png: TPNGObject; sx,sy: single;
    x,y: word; n,x1,y1: longint;
    a: pngimage.pByteArray; col: pRGBLine; UseScale: boolean;
begin
  result:=false;
  FixPath(filename);
  if tex=nil then exit;
  png:=TPNGObject.Create;
  try
    png.LoadFromFile(filename);
  except
    nxSetError(format('[LoadPNGData] Error "%s"',[filename]));
    freeandnil(png); exit;
  end;
  tex^.Width:=png.Width; tex^.Height:=png.Height;
  tex^.sizeX:=tex^.Width; tex^.sizeY:=tex^.Height;
  if toFitScale in Options then begin
    UseScale:=false;
    Pow2Fit(tex^.sizeX, tex^.sizeY, sx, sy);
    sx:=1; sy:=1;
  end else if toScale in Options then begin
    UseScale:=Pow2Near(tex^.sizeX, tex^.sizeY, sx, sy);
    tex^.Width:=tex^.SizeX; tex^.Height:=tex^.SizeY;
  end else if (toCustomScale in Options) and (scaleX*scaleY>0) then begin
    UseScale:=true;
    sx:=1/scaleX; sy:=1/scaleY;
    tex^.sizeX:=trunc(tex^.sizeX*scaleX);
    tex^.sizeY:=trunc(tex^.sizeY*scaleY);
    tex^.Width:=tex^.SizeX; tex^.Height:=tex^.SizeY;
  end else begin
    UseScale:=false; sx:=1; sy:=1;
  end;
  tex^.PatternWidth:=tex^.Width;
  tex^.PatternHeight:=tex^.Height;
  setlength(tex^.Data, tex^.sizeX*tex^.sizeY*tex^.values);
  n:=0; a:=nil;
  {$R-}
  for y:=0 to tex.sizeY-1 do begin
    if UseScale then y1:=trunc(y*sy) else y1:=y;
    col:=png.Scanline[y1];
    if tex.values=4 then a:=png.AlphaScanline[y1];
    for x:=0 to tex.sizeX-1 do begin
      if UseScale then x1:=trunc(x*sx) else x1:=x;
      tex.Data[n]:=col[x1].rgbtRed;
      tex.Data[n+1]:=col[x1].rgbtGreen;
      tex.Data[n+2]:=col[x1].rgbtBlue;
      if tex.values=4 then begin
        if toAlphaColor in Options then
          tex.Data[n+3]:=(tex.Data[n]+tex.Data[n+1]+tex.Data[n+2]) div 3
        else if toColorKey in Options then begin
          if (tex.Data[n]=TransparentColor.r) and
             (tex.Data[n+1]=TransparentColor.g) and
             (tex.Data[n+2]=TransparentColor.b) then
            tex.Data[n+3]:=0
          else
            tex.Data[n+3]:=255;
        end else if a<>nil then
          tex.Data[n+3]:=a[x1]
        else tex.Data[n+3]:=255;
        if tex.Data[n+3]=0 then begin
          tex.Data[n]:=0; tex.Data[n+1]:=0; tex.Data[n+2]:=0;
        end;
      end;
      inc(n, tex.values);
    end;
  end;
  {$R+}
  png.Free; result:=true;
end;
{$ENDIF}

{$IFDEF fpc}
procedure TTextureSet.LoadRaw(tex: PTexture; RawImage: TRawImage);
var sx,sy: single; n, x1,y1,ny,n2: longint;
    x,y,r,g,b,a: word; UseScale: boolean;
    rp: TRawImagePosition;
begin
  tex^.sizeX:=tex^.Width; tex^.sizeY:=tex^.Height;
  if toFitScale in Options then begin
    UseScale:=false;
    Pow2Fit(tex^.sizeX,tex^.sizeY,sx,sy);
    sx:=1; sy:=1;
  end else if toScale in Options then begin
    UseScale:=Pow2Near(tex^.sizeX,tex^.sizeY,sx,sy);
    tex^.Width:=tex^.SizeX; tex^.Height:=tex^.SizeY;
  end else if (toCustomScale in Options) and (scaleX*scaleY>0) then begin
    UseScale:=true;
    sx:=1/scaleX; sy:=1/scaleY;
    tex^.sizeX:=trunc(tex^.sizeX*scaleX);
    tex^.sizeY:=trunc(tex^.sizeY*scaleY);
    tex^.Width:=tex^.SizeX; tex^.Height:=tex^.SizeY;
  end else begin
    UseScale:=false; sx:=1; sy:=1;
  end;
  tex^.PatternWidth:=tex^.Width;
  tex^.PatternHeight:=tex^.Height;
  setlength(tex^.Data, tex^.sizeX*tex^.sizeY*tex^.values);
  n:=0; a:=high(word);
  for y:=0 to tex^.Height-1 do begin
    if UseScale then y1:=trunc(y*sy) else y1:=y;
    ny:=y1*longint(RawImage.Description.BitsPerLine);
    for x:=0 to tex^.Width-1 do begin
      if UseScale then x1:=trunc(x*sx) else x1:=x;
      n2:=ny+x1*longint(RawImage.Description.BitsPerPixel);
      rp.Byte:=n2 div 8; rp.Bit:=n2 mod 8;
      RawImage.ReadChannels(rp,r,g,b,a);
      r:=r div 256; g:=g div 256; b:=b div 256;
      tex^.Data[n]:=r; tex^.Data[n+1]:=g; tex^.Data[n+2]:=b;
      if tex^.values=4 then begin
        if toColorKey in Options then begin
          if (tex^.Data[n]=TransparentColor.r) and
             (tex^.Data[n+1]=TransparentColor.g) and
             (tex^.Data[n+2]=TransparentColor.b) then tex^.Data[n+3]:=0
          else tex^.Data[n+3]:=255;
        end else if (not (toAlphaColor in Options)) and (rawimage.Description.AlphaPrec>0) then
          tex^.Data[n+3]:=a div 256
        else if toAlphaColor in Options then tex^.Data[n+3]:=(r+g+b) div 3
        else tex^.Data[n+3]:=255;
        if tex^.Data[n+3]=0 then begin
          tex^.Data[n]:=0; tex^.Data[n+1]:=0; tex^.Data[n+2]:=0;
        end;
      end;
      inc(n,tex^.values);
    end;
    if not UseScale then
      inc(n,tex^.values*(tex^.sizeX-tex^.Width));
  end;

  // Fill extra areas with border pixels
  // Right side and bottom corner
  y1:=tex^.values*tex^.sizeX;
  n2:=tex^.values*(tex^.Width-1);
  for y:=0 to tex^.sizeY-1 do begin
    n:=n2+tex^.values;
    if y>=tex^.Height then
      n2:=tex^.values*((tex^.Height-1)*tex^.sizeX+tex^.Width-1);
    for x:=tex^.Width to tex^.sizeX-1 do begin
      move(tex^.Data[n2], tex^.Data[n], tex^.values);
      inc(n,tex^.values);
    end;
    inc(n2,y1);
  end;
  // Bottom
  n2:=tex^.values*((tex^.Height-1)*tex^.sizeX);
  y1:=tex^.values*tex^.sizeX;
  for x:=0 to tex^.Width-1 do begin
    n:=n2+y1;
    for y:=tex^.Height to tex^.sizeY-1 do begin
      move(tex^.Data[n2], tex^.Data[n], tex^.values);
      inc(n,y1);
    end;
    inc(n2,tex^.values);
  end;
end;
{$ENDIF}

function TTextureSet.NewName(const base: string): string;
var i: longint; n: cardinal; ok: boolean;
begin
  n:=0; result:=base+inttostr(n);
  repeat
    ok:=true;
    for i:=0 to count-1 do
      if texture[i].name=result then begin
        ok:=false; inc(n); result:=base+inttostr(n);
      end;
  until ok;
end;

procedure TTextureSet.ReadFromFile(var f: TextFile);
begin
  {$Message 'texSet ReadFromFile'}

end;

function TTextureSet.LoadTextureData(tex: PTexture; filename: string): boolean;
var ext: string;
begin
  result:=false;
  FixPath(filename);
  ext:=lowercase(copy(filename,length(filename)-3,4));
  if not FileExistsUTF8(filename) or false then begin
    nxSetError(format('[LoadTextureData] File "%s" not found.',[filename]));
  end else if length(filename)>=5 then begin
    try
      if toFileName in Options then tex^.filename:=filename;
      if ext='.bmp' then
        result:=LoadBMPDataFile(tex,filename)
      else if ext='.png' then
        result:=LoadPNGData(tex,filename)
      else if (ext='.jpg') or (ext='jpeg') then
        result:=LoadJPGData(tex,filename);
    except
      nxSetError(format('[LoadTextureData] Error loading texture "%s"',[filename]));
      exit;
    end;
  end;
end;

function TTextureSet.Pow2Fit(var w, h: word; out sx, sy: single): boolean;
var w1,h1: word;
begin
  w1:=w; h1:=h;
  w:=min(max(nxMath.Pow2fit(w),1),4096);
  h:=min(max(nxMath.Pow2fit(h),1),4096);
  sx:=(w1-1)/(w-1); sy:=(h1-1)/(h-1);
  result:=(w<>w1) or (h<>h1);
end;

function TTextureSet.Pow2Near(var w, h: word; out sx, sy: single): boolean;
var w1,h1: word;
begin
  w1:=w; h1:=h;
  w:=min(max(nxMath.Pow2near(w),1),4096); h:=min(max(nxMath.Pow2near(h),1),4096);
  sx:=(w1-1)/(w-1); sy:=(h1-1)/(h-1);
  result:=(w<>w1) or (h<>h1);
end;

procedure TTextureSet.RemoveTexture(n: longint);
begin
  texture[n].Enabled:=false; texture[n].RefCount:=0;
  if LastTexIndex=n then LastTexIndex:=-1;
  if texture[n].Data<>nil then begin
    setlength(texture[n].Data, 0);
  end;
  while (count>0) and (not texture[count-1].Enabled) do
    SetCount(count-1);
end;

procedure TTextureSet.Resize(_texture: PTexture; newWidth, newHeight: integer);
var tmp: TTexture; i,j,k: longint;
begin
  setlength(tmp.Data, newWidth*newHeight*_texture^.values);
  {$R-}
  for j:=0 to newHeight-1 do
    if j<_texture^.sizeY then begin
      for i:=0 to newWidth-1 do
	      if i<_texture^.sizeX then begin
	        for k:=0 to _texture^.values-1 do begin
	          tmp.Data[_texture^.values*(i+newWidth*j)+k]:=
              _texture^.Data[_texture^.values*(i+_texture^.sizeX*j)+k];
          end;
	      end else
  		    for k:=0 to _texture^.values-1 do
            tmp.Data[_texture^.values*(i+newWidth*j)+k]:=0;
    end else
 	    for i:=0 to newWidth-1 do
	      for k:=0 to _texture^.values-1 do
	        tmp.Data[_texture^.values*(i+newWidth*j)+k]:=0;
  {$R+}

  _texture^.Data:=tmp.Data; tmp.Data:=nil;
  _texture^.sizeX:=newWidth;
  _texture^.sizeY:=newHeight;
  _texture^.width:=min(_texture^.width, newWidth);
  _texture^.height:=min(_texture^.height, newHeight);
end;

procedure TTextureSet.SetColorKey(r, g, b: byte);
begin
  if not (toColorKey in Options) then
    Options:=Options+[toColorKey];
  TransparentColor.r:=r; TransparentColor.g:=g;
  TransparentColor.b:=b;
end;

procedure TTextureSet.SetCount(n: longint);
begin
  count:=n; n:=nxMath.pow2fit(n);
  if n<>count2 then begin
    count2:=n; setlength(texture, count2);
  end;
end;

procedure TTextureSet.SetPattern(tex: longint; PatternWidth, PatternHeight, SkipWidth, SkipHeight: word);
var t: pTexture;
begin
  t:=@texture[tex];
  if PatternWidth>t^.Width then PatternWidth:=t^.Width;
  if PatternHeight>t^.Height then PatternHeight:=t^.Height;
  t^.PatternWidth:=PatternWidth;
  t^.PatternHeight:=PatternHeight;
  SkipWidth:=min(SkipWidth, t^.Width-PatternWidth);
  SkipHeight:=min(SkipHeight, t^.Height-PatternHeight);
  t^.SkipWidth:=max(0, SkipWidth);
  t^.SkipHeight:=max(0, SkipHeight);
end;

procedure TTextureSet.SetSize(tex: longint; Width, Height: word; DefaultPattern: boolean);
var t: pTexture;
begin
  t:=@texture[tex];
  if Width>=t^.sizeX then t^.Width:=t^.sizeX
  else t^.Width:=Width;
  if Height>=t^.sizeY then t^.Height:=t^.sizeY
  else t^.Height:=Height;
  if DefaultPattern then begin
    t^.PatternWidth:=t^.Width; t^.PatternHeight:=t^.Height;
    t^.SkipWidth:=0; t^.SkipHeight:=0;
  end else with t^ do begin
    if PatternWidth>Width then PatternWidth:=Width;
    if PatternHeight>Height then PatternHeight:=Height;
    SkipWidth:=min(SkipWidth, Width-PatternWidth);
    SkipHeight:=min(SkipHeight, Height-PatternHeight);
    SkipWidth:=max(0, SkipWidth);
    SkipHeight:=max(0, SkipHeight);
  end;
end;

procedure TTextureSet.Swap(tex1, tex2: longint);
var temp: TTexture;
begin
  temp:=texture[tex1];
  texture[tex1]:=texture[tex2];
  texture[tex2]:=temp;
end;

procedure TTextureSet.WriteToFile(f: TStringList);
begin
  {$Message 'texSet WriteToFile'}

end;

{ TNXFont }

constructor TNXFont.Create(_TexSize: integer);
var i: integer;
begin
  height:=_TexSize div 14;
  CenterH:=_TexSize div 28;
  sx:=texSize div 16; sy:=texSize div 14;
  for i:=32 to 255 do charW[i]:=_TexSize div 16-1;
  SetColor(1, 1, 1, 1);
  name:='unnamed';
end;

constructor TNXFont.Load(filename: string);
var fs: TFileStream; ds: TDeCompressionStream;
    tex: PTexture; c: cardinal;
begin
  textureI:=nxTex.AddTexture2(filename, '', true);
  tex:=@nxTex.texture[textureI];
  fs:=TFileStream.Create(filename, fmOpenRead);
  fs.ReadBuffer(TexSize, 2);
  tex^.sizeX:=texSize; tex^.sizeY:=texSize;
  Create(TexSize);
  nxTex.SetSize(textureI, TexSize, TexSize);
  fs.ReadBuffer(Height, 2);
  fs.ReadBuffer(CenterH, 1);
  fs.ReadBuffer(charW, 256-32);
  c:=0;
  fs.ReadBuffer(c, 4);
  setlength(tex^.Data, c);
  ds:=TDeCompressionStream.create(fs);
  ds.read(tex^.Data[0], c);
  ds.Free; fs.Free;
end;

constructor TNXFont.CreateBMP(fontName: string; fontSize, _TexSize: integer);
var i,x,y: integer; b: TBitmap; c: char; s: string;
    temp: TTextureLoadOptions;
begin
  temp:=nxTex.Options; nxTex.Options:=[toAlphaColor];
  TexSize:=_TexSize;
  TextureI:=nxTex.AddTexture2('*F '+fontname, '', true);
  b:=TBitmap.Create;
  b.Width:=TexSize; b.Height:=TexSize;
  sx:=texSize div 16; sy:=texSize div 14;
  with b.Canvas do begin
    pen.Color:=clBlack; brush.Color:=clBlack;
    rectangle(0,0,texSize,texSize);
    font.Name:=fontName; font.Size:=fontSize; font.Color:=clWhite;
    brush.Style:=bsClear;
    pen.Color:=clRed;
    for i:=0 to 255-32 do begin
      c:=char(i+32); s:=ChrToUTF(c);
      charW[i+32]:=TextWidth(s);
      if charW[i+32]=0 then charW[i+32]:=7;
      if charW[i+32]>sx-1 then charW[i+32]:=sx-1;
      x:=sx*(i mod 16); y:=sy*(i div 16);
      TextRect(bounds(x,y,sx,sy), x+1, y, s);
    end;
    self.height:=TextHeight('Åjq@');
    CenterH:=self.height div 2;
  end;
  nxTex.LoadBMPData(@nxTex.texture[textureI], b);
  if NX_SaveFontBitmap then b.SaveToFile('FontBitmap.bmp');
  b.Free;
  nxTex.Options:=temp;
end;

constructor TNXFont.CreateBMPAndSave(fontName: string; fontSize, _TexSize: integer; filename: string);
var fs: TFileStream; cs: TCompressionStream;
    tex: PTexture; c: cardinal;
begin
  CreateBMP(fontName, fontSize, _TexSize);
  fs:=TFileStream.Create(filename, fmCreate);
  fs.WriteBuffer(TexSize, 2);
  fs.WriteBuffer(Height, 2);
  fs.WriteBuffer(CenterH, 1);
  fs.WriteBuffer(charW, 256-32);
  tex:=@nxTex.texture[textureI];
  c:=tex^.sizeX*tex^.sizeY*tex^.values;
  fs.WriteBuffer(c, 4);
  cs:=TCompressionStream.create(cldefault, fs);
  cs.write(tex^.Data[0], c);
  cs.Free; fs.Free;
end;

procedure TNXFont.SetColor(const r, g, b: single; a: single);
begin
  color.r:=r; color.g:=g; color.b:=b; color.a:=a;
end;

function TNXFont.TextW(s: string): longint;
var i: longint; b: byte;
begin
  result:=0;
  for i:=1 to length(s) do begin
    b:=byte(UTFToChr(UTF8Copy(s, i, 1)));
    if b>31 then inc(result, charW[b]);
  end;
end;

function TNXFont.WrapLines(w, h: integer; s: TStrings; first: integer): longint;
var i,l,x,y,c: longint;
begin
  result:=0; y:=0;
  for l:=first to s.Count-1 do begin
    inc(result); x:=0;
    for i:=1 to length(s[l]) do begin
      c:=byte(s[l][i]);
      if c>31 then begin
        c:=charW[c];
        if x+c>w then begin
          inc(y,height);
          if y+height>h then exit;
          inc(result); x:=0;
        end;
        inc(x,c);
      end;
    end;
  end;
end;

procedure TNXFont.DrawC(x, y: single; s: string; maxW: integer);
var w: single;
begin
  w:=TextW(s)*0.5;
  if (maxW>0) and (w>maxW/2) then w:=maxW/2;
  Draw(x-w, y-CenterH, s, maxW);
end;

function TNXCustomEngine.CheckFile(const filename: string): boolean;
begin
  result:=FileExists(filename);
  if not result then nxSetError('File ['+filename+'] doesn''t exist!');
end;

procedure TNXCustomEngine.ClearError;
begin
  nxClearError;
end;

function TNXCustomEngine.GetTick: cardinal;
begin
  result:=GetTickCount mod cardinal(high(longint));
end;

// Return tick value 0..range-1
// Call with range >= 1 and scale >0
function TNXCustomEngine.GetTick(range: longint; scale: single): single;
begin
  result:=(GetTickCount mod round(range/scale))*scale;
end;

{ TNXCustomEngine }

function TNXCustomEngine.LastError: string;
begin
  result:=nxError;
end;

function TNXCustomEngine.IndexOfFont(name: string): integer;
var i: integer;
begin
  for i:=0 to fontCount-1 do
    if name=font[i].name then begin
      result:=i; exit;
    end;
  result:=-1;
end;

{ TCustomVertexArray }

function TCustomVertexArray.ColorComponents: integer;
begin
  if _AlphaColors then result:=4
  else result:=3;
end;

function TCustomVertexArray.TextureComponents: integer;
begin
  if _3Dtextures then result:=3
  else result:=2;
end;

constructor TCustomVertexArray.Create(Faces, Vertices: longint; _rendermode: cardinal; textures, normals, colors: boolean);
begin
  fCount:=Faces; vCount:=Vertices;
  rendermode:=_rendermode; Count:=GetIndexCount;
  _textures:=textures; _normals:=normals; _colors:=colors;
  MakeArrays;
end;

constructor TCustomVertexArray.Create(Faces: longint; _rendermode: cardinal; textures, normals, colors: boolean);
begin
  rendermode:=_rendermode;
  Create(Faces, GetIndexCount(Faces), _rendermode, textures, normals, colors);
end;

destructor TCustomVertexArray.Destroy;
begin
  FreeArrays;
  inherited Destroy;
end;

procedure TCustomVertexArray.FreeArrays;
begin
  setlength(fa, 0); setlength(va, 0);
  setlength(ta, 0); setlength(na, 0);
  setlength(ca, 0);
end;

function TCustomVertexArray.GetIndexCount: longint;
begin
  result:=GetIndexCount(fCount);
end;

procedure TCustomVertexArray.MakeArrays;
begin
  setlength(fa, Count);
  setlength(va, vCount);
  if _3Dtextures then setlength(ta, vCount*3)
  else if _textures then setlength(ta, vCount*2);
  if _normals then setlength(na, vCount);
  if _AlphaColors then setlength(ca, vCount*4)
  else if _colors then setlength(ca, vCount*3);
end;

procedure TCustomVertexArray.MakeLinearIndices;
var i: longint;
begin
  for i:=0 to GetIndexCount-1 do fa[i]:=i;
end;

procedure TCustomVertexArray.Set3DTextures;
begin
  _3Dtextures:=true; setlength(ta, vCount*3);
end;

procedure TCustomVertexArray.SetAlphaColors;
begin
  _AlphaColors:=true; setlength(ca, vCount*4);
end;

end.
