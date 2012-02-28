unit nxDX;

interface

uses Windows, nxModel, Direct3D8, nxTypes;

type
  TTexture = record
    name: string[11];
    sizeX, sizeY, values: word; // values 3 or 4
    index,format: cardinal; // GL_RGB or GL_RGBA
    Data: PByteArray; // size = sizeX*sizeY*values
  end;
  PTexture = ^TTexture;

  TTextureLoadOptions = set of (toMipMap, toAlphaColor,
    toKeepData, toAbsoluteSize, toColorKey);

  TTextureSet = class
    texture: array of TTexture;
    count, TextureQuality: integer;
    Options: TTextureLoadOptions;
    TransparentColor: TRGBTriple;
  private
    count2: integer;
    function Pow2Limit(var w,h: word; var sx,sy: single): boolean;
    procedure SetCount(n: integer);
    function LoadBMPData(tex: PTexture; filename: string): boolean;
    function LoadPNGData(tex: PTexture; filename: string): boolean;
    function LoadJPGData(tex: PTexture; filename: string): boolean;
    function LoadTextureData(tex: PTexture; filename: string): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function AddTexture(name,filename: string; transparency: boolean = false): integer;
    procedure Clear;
    procedure Disable;
    procedure Enable;
    function IndexOf(name: string): integer;
    procedure RemoveTexture(n: integer);
    procedure ReloadTexture(n: integer; filename: string; transparency: boolean);
    procedure SetTex(n: integer);
  end;

  TDXModel = class
  public
    //procedure EnableStates;
    //procedure DisableStates;
    //procedure MakeDisplayList(var list: cardinal);
    procedure Render;
    //procedure SetPointers;
  end;

var
  nxHWND: HWND;
  nxDC: HDC;
  d3ddev: IDirect3DDevice8;
  d3dpp: TD3DPresentParameters;
  hr: HResult;
  {nxFARZ: single = 1000;
  nxNEARZ: single = 0.1;
  nxFov: single = 45;
  nxAspectRatio: single = 1.33;}
  tex: TTextureSet;

  function nxCreateDXWindow(hwindow: HWND): boolean;
  procedure nxKillDXWindow;
  //procedure nxAddBlend(Enable: boolean);
  procedure nxClear(color,depth: boolean);
  //procedure nxDefaultLights(Enable: boolean = true);
  procedure nxFlip;
  function nxInitialized: boolean;
  //procedure nxSetView(X, Y, Width, Height: integer);
  //procedure nxPerspective(Width, Height: integer; Stretch: boolean = false);

implementation

uses PNGImage, JPEG, AltUtils, math, Graphics, Classes,
  nxMath, messages;

//var lastTex: cardinal;

function GetPresentParameters(hdc: THandle): TD3DPresentParameters;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Windowed:= True;
  Result.hDeviceWindow:= nxHWND;
  Result.BackBufferWidth:= 320;
  Result.BackBufferHeight:= 240;
  if Windows.GetDeviceCaps(hdc, BITSPIXEL) = 16
    then Result.BackBufferFormat:= D3DFMT_R5G6B5
    else Result.BackBufferFormat:= D3DFMT_X8R8G8B8;
  Result.BackBufferCount:= 1;
  Result.EnableAutoDepthStencil:= True;
  Result.AutoDepthStencilFormat:= D3DFMT_D16;
  Result.SwapEffect:= D3DSWAPEFFECT_FLIP;
end;

procedure nxInitDX;
begin

  tex:=TTextureSet.Create;
end;

function nxCreateDXWindow(hWindow: HWND): boolean;
var pEnum: IDirect3D8;
begin
  result:=false;
  if hWindow=0 then Exit;
  nxHWND:=hWindow; pEnum:=nil;
  pEnum:= Direct3DCreate8(D3D_SDK_VERSION);
  if (pEnum = nil) then Halt(1);
  nxDC:=GetDC(nxHWND);
  if nxDC = 0 then Halt(1);

  d3dpp:=GetPresentParameters(nxDC);
  hr:=pEnum.CreateDevice(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, nxHWND,
    D3DCREATE_SOFTWARE_VERTEXPROCESSING, d3dpp, d3ddev);
  pEnum:=nil;

  ReleaseDC(nxHWND, nxDC);

  if (FAILED(hr)) then Halt(1);

  nxInitDX; result:=true;
end;

procedure nxKillDXWindow;
begin
  d3ddev:=nil; nxDC:=0;
end;

{procedure nxAddBlend(Enable: boolean);
begin
  if Enable then glBlendFunc(GL_SRC_ALPHA,GL_ONE)
  else glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
end; }

procedure nxClear(color,depth: boolean);
var mask: cardinal;
begin
  if color then mask:=D3DCLEAR_TARGET
  else mask:=0;
  if depth then mask:=mask or D3DCLEAR_ZBUFFER;
  d3ddev.Clear(0, nil, mask, D3DCOLOR_XRGB(0,0,0), 1, 0);
end;

{procedure nxDefaultLights(Enable: boolean);
var f4: TVector4f;
begin
  if Enable then begin
    glEnable(GL_LIGHTING); glEnable(GL_LIGHT0);
    glEnable(GL_COLOR_MATERIAL);
    glColorMaterial(GL_FRONT_AND_BACK,GL_DIFFUSE);
    f4.x:=0; f4.y:=0; f4.z:=0; f4.w:=0;
    glMaterialfv(GL_FRONT,GL_SPECULAR,PGLFloat(@f4));
  end else
    glDisable(GL_LIGHTING);
end;}

procedure nxFlip;
begin
  //SwapBuffers(nxDC);
  UpdateWindow(nxHWND);
end;

function nxInitialized: boolean;
begin
  result:=d3ddev<>nil;
end;

procedure nxPerspective(Width, Height: integer; Stretch: boolean = false);
begin
  {if nxInitialized then begin
    if Height<1 then Height:=1;
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    if Stretch then
      gluPerspective(nxFov, nxAspectRatio, nxNEARZ, nxFARZ)
    else
      gluPerspective(nxFov, Width/Height, nxNEARZ, nxFARZ);
    glMatrixMode(GL_MODELVIEW);
  end; }
end;

procedure nxSetView(X, Y, Width, Height: integer);
begin
  //if nxInitialized then glViewport(X,Y,Width,Height);
  //if nxInitialized then glViewport(X,Y,Width-1,Height-1);
end;

procedure nxSetTex(t: cardinal; force: boolean);
begin
  {if (t<>lastTex) or force then begin
    glBindTexture(GL_TEXTURE_2D,t); lastTex:=t;
  end;}
end;

{ TGLModel }

procedure TDXModel.Render;
begin
  //glDrawElements(GL_TRIANGLES,fCount*3,GL_UNSIGNED_SHORT,@(fa[0]));
end;

function BitmapToDIBHandle(Bitmap: TBitmap): THandle;
var MS: TMemoryStream; DIBHandle: THandle; p: Pointer;
begin
  result:=0; MS:=TMemoryStream.Create;
  try
    Bitmap.SaveToStream(MS);
    DIBHandle := GlobalAlloc(GHND, MS.Size-SizeOf(TBitmapFileHeader));
    //if DIBHandle = 0 then Raise Exception.Create('Cannot Get Memory');
    if DIBHandle>0 then begin
      try
        p:=GlobalLock(DIBHandle);
        try
          System.Move((PChar(MS.Memory)+SizeOf(TBitmapFileHeader))^,
                      p^,
                      MS.Size-SizeOf(TBitmapFileHeader));
        finally
          GlobalUnlock(DIBHandle);
        end;
        Result:=DIBHandle;
      except
        GlobalFree(DIBHandle);
        raise;
      end;
    end;
  finally
    MS.Free;
  end;
end;

{ TTextureSet }

function TTextureSet.AddTexture(name,filename: string;
  transparency: boolean): integer;
begin
  result:=-1;
  {SetCount(count+1); result:=count-1;
  texture[result].name:=name;
  glGenTextures(1,@texture[result].index);
  ReloadTexture(result,filename,transparency);}
end;

procedure TTextureSet.Clear;
var i: integer;
begin
  for i:=0 to Count-1 do
    if texture[i].Data<>nil then begin
      FreeMem(texture[i].Data); texture[i].Data:=nil;
    end;
  SetCount(0);
end;

constructor TTextureSet.Create;
begin
  inherited;
  Options:=[]; //TextureQuality:=GL_LINEAR;
end;

destructor TTextureSet.Destroy;
begin
  Clear;
  inherited;
end;

procedure TTextureSet.Disable;
begin
  //glDisable(GL_TEXTURE_2D);
end;

procedure TTextureSet.Enable;
begin
  //glEnable(GL_TEXTURE_2D);
end;

function TTextureSet.IndexOf(name: string): integer;
var i: integer;
begin
  for i:=0 to count-1 do
    if name=texture[i].name then begin
      result:=i; exit;
    end;
  result:=-1;
end;

function TTextureSet.LoadBMPData(tex: PTexture; filename: string): boolean;
var x,y,n,x1,y1: integer; temp: TBitmap;
    line: PByteArray; sx,sy: single; UseScale: boolean;
begin
  result:=false;
  if not fileexists(filename) then begin
    //LogError(format('Not found[%s]',[Extractfilename(filename)]),true);
    Exit;
  end;
  temp:=TBitmap.Create; temp.LoadFromFile(filename);
  if temp.PixelFormat=pfDevice then BitmapToDIBHandle(temp);
  temp.PixelFormat:=pf24bit;
  tex.sizeX:=temp.Width; tex.sizeY:=temp.Height;
  if toAbsoluteSize in Options then begin
    UseScale:=false; sx:=1; sy:=1;
  end else UseScale:=Pow2Limit(tex.sizeX,tex.sizeY,sx,sy);
  ReAllocMem(tex.Data,tex.sizeX*tex.sizeY*tex.values);
  {$R-}
  n:=0;
  for y:=0 to tex.sizeY-1 do begin
    if UseScale then y1:=trunc(y*sy) else y1:=y;
    line:=temp.ScanLine[y1];
    for x:=0 to tex.sizeX-1 do begin
      if UseScale then x1:=trunc(x*sx) else x1:=x;
      tex.Data[n]:=line[x1*3+2];
      tex.Data[n+1]:=line[x1*3+1];
      tex.Data[n+2]:=line[x1*3];
      if tex.values=4 then
        if toAlphaColor in Options then
          tex.Data[n+3]:=(tex.Data[n]+tex.Data[n+1]+tex.Data[n+2]) div 3
        else if (tex.Data[n]=TransparentColor.rgbtRed) and
             (tex.Data[n+1]=TransparentColor.rgbtGreen) and
             (tex.Data[n+2]=TransparentColor.rgbtBlue) then
          tex.Data[n+3]:=0
        else
          tex.Data[n+3]:=255;
      inc(n,tex.values);
    end;
  end;
  {$R-}
  temp.Free; result:=true;
end;

function TTextureSet.LoadJPGData(tex: PTexture; filename: string): boolean;
var sx,sy: single; x,y: word; n,x1,y1: integer;
    jpg: TJpegImage; temp: TBitmap;
    col: pRGBLine; UseScale: boolean;
begin
  result:=false;
  if not fileexists(filename) then begin
    //LogError(format('Not found[%s]',[Extractfilename(filename)]),true);
    Exit;
  end;

  jpg:=TJpegImage.Create; jpg.LoadFromFile(filename);
  temp:=TBitmap.Create; temp.Assign(jpg);
  jpg.Free;

  tex.sizeX:=temp.Width; tex.sizeY:=temp.Height;
  if toAbsoluteSize in Options then begin
    UseScale:=false; sx:=1; sy:=1;
  end else UseScale:=Pow2Limit(tex.sizeX,tex.sizeY,sx,sy);
  ReAllocMem(tex.Data,tex.sizeX*tex.sizeY*tex.values);

  {$R-}
  n:=0;
  for y:=0 to tex.sizeY-1 do begin
    if UseScale then y1:=trunc(y*sy) else y1:=y;
    col:=temp.Scanline[y1];
    for x:=0 to tex.sizeX-1 do begin
      if UseScale then x1:=trunc(x*sx) else x1:=x;
      tex.Data[n]:=col[x1].rgbtRed;
      tex.Data[n+1]:=col[x1].rgbtGreen;
      tex.Data[n+2]:=col[x1].rgbtBlue;
      if tex.values=4 then
        tex.Data[n+3]:=(tex.Data[n]+tex.Data[n+1]+tex.Data[n+2]) div 3;
      inc(n,tex.values);
    end;
  end;
  {$R+}
  temp.Free; result:=true;
end;

function TTextureSet.LoadPNGData(tex: PTexture; filename: string): boolean;
var sx,sy: single;
    x,y: word; temp: TPngObject; n,x1,y1: integer;
    a: pByteArray; col: pRGBLine; UseScale: boolean;
begin
  result:=false;
  if not fileexists(filename) then begin
    //LogError(format('Not found[%s]',[Extractfilename(filename)]),true);
    Exit;
  end;

  temp:=TPngObject.Create; temp.LoadFromFile(filename);
  //if temp.TransparencyMode=ptmNone then transparent:=false;
  tex.sizeX:=temp.Width; tex.sizeY:=temp.Height;
  if toAbsoluteSize in Options then begin
    UseScale:=false; sx:=1; sy:=1;
  end else UseScale:=Pow2Limit(tex.sizeX,tex.sizeY,sx,sy);
  ReAllocMem(tex.Data,tex.sizeX*tex.sizeY*tex.values);
  a:=nil;
  {$R-}
  n:=0;
  for y:=0 to tex.sizeY-1 do begin
    if UseScale then y1:=trunc(y*sy) else y1:=y;
    col:=temp.Scanline[y1];
    if tex.values=4 then a:=temp.AlphaScanline[y1];
    for x:=0 to tex.sizeX-1 do begin
      if UseScale then x1:=trunc(x*sx) else x1:=x;
      tex.Data[n]:=col[x1].rgbtRed;
      tex.Data[n+1]:=col[x1].rgbtGreen;
      tex.Data[n+2]:=col[x1].rgbtBlue;
      if tex.values=4 then begin
        if toAlphaColor in Options then
          tex.Data[n+3]:=(tex.Data[n]+tex.Data[n+1]+tex.Data[n+2]) div 3
        else if toColorKey in Options then begin
          if (tex.Data[n]=TransparentColor.rgbtRed) and
             (tex.Data[n+1]=TransparentColor.rgbtGreen) and
             (tex.Data[n+2]=TransparentColor.rgbtBlue) then
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
      inc(n,tex.values);
    end;
  end;
  {$R+}
  temp.Free; result:=true;
end;

function TTextureSet.LoadTextureData(tex: PTexture;
  filename: string): boolean;
var ext: string;
begin
  result:=false;
  try
    ext:=lowercase(copy(filename,length(filename)-3,4));
    if ext='.bmp' then result:=LoadBMPData(tex,filename)
    else if ext='.png' then result:=LoadPNGData(tex,filename)
    else if (ext='.jpg') or (ext='jpeg') then result:=LoadJPGData(tex,filename);
  except exit; end;
end;

function TTextureSet.Pow2Limit(var w, h: word; var sx,
  sy: single): boolean;
var w1,h1: word;
begin
  w1:=w; h1:=h;
  w:=min(max(Pow2near(w),1),2048); h:=min(max(Pow2near(h),1),2048);
  sx:=(w1-1)/(w-1); sy:=(h1-1)/(h-1);
  result:=(w<>w1) or (h<>h1);
end;

procedure TTextureSet.ReloadTexture(n: integer; filename: string;
  transparency: boolean);
begin
  {if n>=Count then exit;
  with texture[n] do begin
    nxSetTex(index,false);
    if transparency then begin
      values:=4; Format:=GL_RGBA;
    end else begin
      values:=3; Format:=GL_RGB;
    end;

    if not LoadTextureData(@texture[n],filename) then begin
      if texture[n].Data<>nil then begin
        FreeMem(texture[n].Data); texture[n].Data:=nil;
      end;
      exit;
    end;

    if data<>nil then begin
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,TextureQuality);
      if not (toMipMap in Options) then begin
        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER, TextureQuality);
        glTexImage2D(GL_TEXTURE_2D, 0, values, sizeX, sizeY, 0,
          Format, GL_UNSIGNED_BYTE, Data);
      end else begin
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
        gluBuild2DMipmaps(GL_TEXTURE_2D, values, sizeX, sizeY,
          Format, GL_UNSIGNED_BYTE, Data);
      end;
      if not (toKeepData in Options) then begin
        freemem(data); data:=nil;
      end;
    end;
  end;  }
end;

procedure TTextureSet.RemoveTexture(n: integer);
begin
  {glDeleteTextures(1,@texture[n].index);
  if texture[n].Data<>nil then begin
    FreeMem(texture[n].Data); texture[n].Data:=nil;
  end;
  SetCount(count-1);
  if count>0 then texture[n]:=texture[count-1]; }
end;

procedure TTextureSet.SetCount(n: integer);
begin
  count:=n; n:=pow2fit(n);
  if n<>count2 then begin
    count2:=n; setlength(texture,count2);
  end;
end;

procedure TTextureSet.SetTex(n: integer);
begin
  nxSetTex(texture[n].index,false);
end;

end.
