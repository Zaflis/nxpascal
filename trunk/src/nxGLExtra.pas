unit nxGLExtra;

{$H+}

interface

uses dglOpenGL, nxGraph, nxGL, nxTypes;

type
  TParticleType = record
    a,ia, irad: single;
    texture: integer;
  end;
  pParticleType = ^TParticleType;

  TParticle = record
    x,y,z, r,g,b,a, rad: single;
    ptype: word;
    velocity: TVector;
  end;
  pParticle = ^TParticle;

  TTrail = record
    x,y,z, r,g,b,a, rad, totalLen: single;
    count, maxCount, texture: integer;
    fading: boolean;
    node: array of TVector;
    len: array of single;
  end;
  pTrail = ^TTrail;

  TParticleLauncher = record
    x,y,z, minR,maxR, minG,maxG, minB,maxB, minRad,maxRad: single;
    speedMin,speedMax, spreadAngle, interval,progress: single;
    direction: TVector;
    ptype: word;
    decay: integer;
  end;
  pParticleLauncher = ^TParticleLauncher;

  { TParticleEngine }

  TParticleEngine = class
    pCount, tCount, ptCount, lCount: integer;
    pt: array of TParticle;
    trail: array of TTrail;
    pTypes: array of TParticleType;
    launcher: array of TParticleLauncher;
  private
    mat: TMatrix;
    pCount2: integer;
  public
    destructor Destroy; override;
    function AddLauncher(ptype: word; interval, minRad,maxRad, speedMin,speedMax,
      spreadAngle: single; dir: TVector; decay: integer): pParticleLauncher;
    function AddParticle(ptype: word; r,g,b, rad, x, y: single;
      z: single = 0): pParticle;
    function AddPType(texture: integer; a,ia, irad: single): integer;
    function AddTrail(texture: integer; maxCount: integer; rad, x, y: single;
      z: single = 0): pTrail;
    procedure ApplyForce(ptype: word; fx, fy: single; fz: single = 0);
    procedure Clear;
    procedure ClearLaunchers;
    procedure ClearParticles;
    procedure ClearPTypes;
    procedure ClearTrails;
    function CloneLauncher(l: pParticleLauncher; dir: TVector;
      x,y: single; z: single = 0): pParticleLauncher;
    procedure Draw2D;
    procedure Draw2DParticles(ptype: integer);
    procedure Draw2DTrails(iStart, iCount: integer);
    procedure Draw3D;
    procedure Draw3DParticles(ptype: integer);
    procedure Draw3DTrails(iStart, iCount: integer);
    function IndexOf(p: pParticle): integer; overload;
    function IndexOf(t: pTrail): integer; overload;
    procedure Launch2D(l: pParticleLauncher);
    procedure Launch3D(l: pParticleLauncher);
    procedure Move2D(mpt: single=1);
    procedure Move3D(mpt: single=1);
    procedure ReadMatrix;
    procedure RemoveParticle(const index: integer);
    procedure SetLauncherColor(l: pParticleLauncher;
      minR,maxR, minG,maxG, minB,maxB: single);
    procedure SetLauncherPos(l: pParticleLauncher;
      x,y: single; z: single = 0);
    procedure SetParticleColor(p: pParticle; r,g,b: single);
    procedure SetTrailColor(t: pTrail; r,g,b: single; a: single = 1);
    procedure SetTrailPos(t: pTrail; x, y: single); overload;
    procedure SetTrailPos(t: pTrail; x, y, z: single); overload;
  end;

  { TQuadTexture }

  TQuadTexture = class
    Width, Height: word;
    textureI: array[0..3] of integer;
	  addX, addY: boolean;
  private
    procedure DrawTdx(const x,y,tdx,tdy: single);
    procedure DrawPart(const tex: PTexture; x,y,tdx,tdy: single);
    function GetTexture(index: integer): PTexture;
  public
    UseTDX: boolean;
    property Texture[index: integer]: PTexture read GetTexture;
    constructor Create; overload;
    constructor Create(filename: string; transparent: boolean = false); overload;
    destructor Destroy; override;
    procedure Draw(const x, y: single);
    procedure DrawRotated(const x, y, _angle, _width, _height, cx, cy: single);
    procedure DrawScaled(const x, y, _width, _height: single);
    procedure FreeTextures;
    procedure LoadTexture(filename: string; transparent: boolean = false);
  end;

  { TGLTextureParter }

  {TGLTextureParter = class
    FPicFile: string;
    picIndex, pCount: integer;
    p: array of TRecti;
  private
    procedure LoadFromFile(filename: string);
    procedure LoadTexture;
  public
    constructor Create(picFile: string);
    constructor CreateFromFile(filename: string);
    destructor Destroy; override;
    procedure Draw(index: integer; x, y: single);
    procedure DrawMirror(index: integer; x, y: single);
    procedure SaveToFile(filename: string);
    procedure SetTexture;
    procedure TexCoord(part,n: integer);
  end;}

implementation

uses math, nxMath, nxMath3D, SysUtils;

{ TGLTextureParter }

{constructor TGLTextureParter.Create(picFile: string);
begin
  FPicFile:=picfile;
  LoadTexture;
end;

constructor TGLTextureParter.CreateFromFile(filename: string);
begin
  LoadFromFile(filename);
end;

destructor TGLTextureParter.Destroy;
begin
  tex.RemoveTexture(PicIndex);
  inherited Destroy;
end;

procedure TGLTextureParter.Draw(index: integer; x, y: single);
//var dw,dh,cx,cy: single;
begin
  SetTexture;
  with p[index] do begin
    //dw:=TexW; dh:=TexH; cx:=center.x; cy:=center.y;
    glBegin(GL_QUADS);
    TexCoord(index,0); glVertex2f(x+(r.x1-cx)*dw, y+(r.y1-cy)*dh);
    TexCoord(index,1); glVertex2f(x+(r.x1-cx)*dw, y+(r.y2-cy)*dh);
    TexCoord(index,2); glVertex2f(x+(r.x2-cx)*dw, y+(r.y2-cy)*dh);
    TexCoord(index,3); glVertex2f(x+(r.x2-cx)*dw, y+(r.y1-cy)*dh);
    glEnd;
  end;
end;

procedure TGLTextureParter.DrawMirror(index: integer; x, y: single);
//var dw,dh,cx,cy: single;
begin
  SetTexture;
  with p[index] do begin
    //dw:=TexW; dh:=TexH; cy:=center.y; cx:=r.x2-(center.x-r.x1);
    glBegin(GL_QUADS);
    TexCoord(index,3); glVertex2f(x+(r.x1-cx)*dw, y+(r.y1-cy)*dh);
    TexCoord(index,2); glVertex2f(x+(r.x1-cx)*dw, y+(r.y2-cy)*dh);
    TexCoord(index,1); glVertex2f(x+(r.x2-cx)*dw, y+(r.y2-cy)*dh);
    TexCoord(index,0); glVertex2f(x+(r.x2-cx)*dw, y+(r.y1-cy)*dh);
    glEnd;
  end;
end;

procedure TGLTextureParter.LoadFromFile(filename: string);
var F: TextFile; i: integer;
begin
  assignfile(F, filename); reset(F);
  readln(F, FPicFile);
  readln(F, pCount);
  setlength(p, pCount);
  for i:=0 to pCount-1 do
    with p[i] do
      readln(F, x1, y1, x2, y2);
  closefile(F);
  LoadTexture;
end;

procedure TGLTextureParter.LoadTexture;
var ext: string; tmp: TTextureLoadOptions;
begin
  tmp:=tex.Options; tex.Options:=tex.Options+[toFitNoScale];
  ext:=lowercase(extractfileext(FPicFile));
  picIndex:=tex.AddTexture(FPicFile, FPicFile, ext='.png');
  tex.Options:=tmp;
end;

procedure TGLTextureParter.SaveToFile(filename: string);
var F: TextFile; i: integer;
begin
  assignfile(F, filename); rewrite(F);
  writeln(F, FPicFile);
  writeln(F, pCount);
  for i:=0 to pCount-1 do
    with p[i] do
      writeln(F, x1, ' ', y1, ' ', x2, ' ', y2);
  closefile(F);
end;

procedure TGLTextureParter.SetTexture;
begin
  tex.SetTex(picIndex);
end;

procedure TGLTextureParter.TexCoord(part, n: integer);
begin
  with p[part] do
    case n of
      0: glTexCoord2f(x1, y1);
      1: glTexCoord2f(x1, y2);
      2: glTexCoord2f(x2, y2);
      3: glTexCoord2f(x2, y1);
    end;
end; }

{ TParticleEngine }

destructor TParticleEngine.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TParticleEngine.AddLauncher(ptype: word; interval, minRad, maxRad,
  speedMin, speedMax, spreadAngle: single; dir: TVector; decay: integer): pParticleLauncher;
begin
  inc(lCount); setlength(launcher, lCount); result:=@launcher[lCount-1];
  result^.ptype:=ptype;
  result^.interval:=interval;
  result^.minRad:=minRad; result^.maxRad:=maxRad;
  result^.speedMin:=speedMin; result^.speedMax:=speedMax;
  result^.spreadAngle:=spreadAngle;
  result^.direction:=dir;
  result^.decay:=decay;
end;

function TParticleEngine.AddParticle(ptype: word; r,g,b, rad, x, y: single;
  z: single): pParticle;
begin
  inc(pCount);
  if pCount>pCount2 then begin
    inc(pCount2, 1000); setlength(pt, pCount2);
  end;
  result:=@pt[pCount-1];
  result^.x:=x; result^.y:=y; result^.z:=z;
  SetParticleColor(result, r, g, b);
  result^.ptype:=ptype;
  result^.a:=pTypes[ptype].a;
  result^.rad:=rad;
end;

function TParticleEngine.AddPType(texture: integer; a,ia, irad: single): integer;
begin
  inc(ptCount); setlength(pTypes, ptCount); result:=ptCount-1;
  pTypes[result].a:=a; pTypes[result].ia:=ia;
  pTypes[result].irad:=irad;
  pTypes[result].texture:=texture;
end;

function TParticleEngine.AddTrail(texture: integer; maxCount: integer;
  rad, x, y: single; z: single): pTrail;
begin
  inc(tCount); setlength(trail, tCount); result:=@trail[tCount-1];
  result^.maxCount:=maxCount; setlength(result^.node, maxCount);
  setlength(result^.len, maxCount);
  result^.x:=x; result^.y:=y; result^.z:=z;
  result^.texture:=texture; result^.rad:=rad;
  SetTrailColor(result, 1, 1, 1, 1);
  result^.count:=1;
  result^.node[0]:=vector(x, y, z); result^.len[0]:=0;
  result^.fading:=true;
end;

procedure TParticleEngine.ApplyForce(ptype: word; fx, fy: single; fz: single);
var i: integer;
begin
  for i:=pCount-1 downto 0 do
    if pt[i].ptype=ptype then
      with pt[i] do begin
        velocity.x:=velocity.x+fx; velocity.y:=velocity.y+fy; velocity.z:=velocity.z+fz;
      end;
end;

procedure TParticleEngine.Clear;
begin
  ClearParticles; ClearTrails;
end;

procedure TParticleEngine.ClearLaunchers;
begin
  lCount:=0; setlength(launcher, 0);
end;

procedure TParticleEngine.ClearParticles;
begin
  pCount:=0; setlength(pt, 0);
end;

procedure TParticleEngine.ClearPTypes;
begin
  ptCount:=0; setlength(pTypes, 0);
end;

procedure TParticleEngine.ClearTrails;
begin
  tCount:=0; setlength(trail, 0);
end;

function TParticleEngine.CloneLauncher(l: pParticleLauncher; dir: TVector; x,
  y: single; z: single): pParticleLauncher;
begin
  result:=AddLauncher(l^.ptype, l^.interval, l^.minRad,l^.maxRad,
    l^.speedMin,l^.speedMax, l^.spreadAngle, dir, l^.decay);
  SetLauncherPos(result, x,y,z);
  with l^ do SetLauncherColor(result, minR,maxR, minG,maxG, minB,maxB);
end;

procedure TParticleEngine.Draw2D;
begin
  glDepthMask(false);
  nx.rs.Push;
  nx.rs.Lighting:=false;
  Draw2DParticles(-1);
  Draw2DTrails(0, tCount);
  nx.rs.Pop;
  glDepthMask(true);
end;

procedure TParticleEngine.Draw2DParticles(ptype: integer);
var i: integer; curTex: integer;
begin
  if pCount>0 then begin
    curTex:=pTypes[pt[0].ptype].texture; tex.SetTex(curTex);
    glBegin(GL_QUADS);
    for i:=0 to pCount-1 do
      if (ptype=-1) or (pt[i].ptype=ptype) then
        with pt[i] do begin
          if curTex<>pTypes[pt[i].ptype].texture then begin
            glEnd;
            curTex:=pTypes[pt[i].ptype].texture; tex.SetTex(curTex);
            glBegin(GL_QUADS);
          end;
          glColor4fv(@r);
          glTexCoord2f(0,0); glVertex2f(x-rad, y-rad);
          glTexCoord2f(0,1); glVertex2f(x-rad, y+rad);
          glTexCoord2f(1,1); glVertex2f(x+rad, y+rad);
          glTexCoord2f(1,0); glVertex2f(x+rad, y-rad);
        end;
    glEnd;
  end;
end;

procedure TParticleEngine.Draw2DTrails(iStart, iCount: integer);
var i,j: integer; d,dx,dy, tp,lpos: single;
    color: TfRGBA;
begin
  for i:=iStart to iStart+iCount-1 do
    with trail[i] do
      if (count>1) and (TotalLen>0) then begin
        tex.SetTex(texture);
        color:=fRGBA(r,g,b,a);
        glColor4fv(@color);
        glBegin(GL_QUAD_STRIP);
        with node[0] do begin
          dx:=node[1].x-x; dy:=node[1].y-y; d:=rad/len[0];
          dx:=dx*d; dy:=dy*d;
          glTexCoord2f(1, 1); glVertex2f(x+dy, y-dx);
          glTexCoord2f(1, 0); glVertex2f(x-dy, y+dx);
        end;
        lpos:=len[0]; 
        for j:=1 to count-2 do
          with node[j] do begin
            dx:=node[j+1].x-x; dy:=node[j+1].y-y; d:=rad/len[j];
            dx:=dx*d; dy:=dy*d;
            if lpos<rad then tp:=max(0.5, 1-0.5*lpos/rad)
            else if lpos>totalLen-rad then tp:=min(0.5,0.5*(totalLen-lpos)/rad)
            else tp:=0.5;
            if fading then begin
              color.a:=1-lpos/totalLen;
              glColor4fv(@color);
            end;
            glTexCoord2f(tp, 1); glVertex2f(x+dy, y-dx);
            glTexCoord2f(tp, 0); glVertex2f(x-dy, y+dx);
            lpos:=lpos+len[j];
          end;
        with node[count-1] do begin
          dx:=x-node[count-2].x; dy:=y-node[count-2].y; d:=rad/len[count-2];
          dx:=dx*d; dy:=dy*d;
          if fading then glColor4f(0,0,0,0);
          glTexCoord2f(0, 1); glVertex2f(x+dy, y-dx);
          glTexCoord2f(0, 0); glVertex2f(x-dy, y+dx);
        end;
        glEnd;
      end;
end;

procedure TParticleEngine.Draw3D;
begin
  ReadMatrix;
  glDepthMask(false);
  nx.rs.Push;
  nx.rs.Lighting:=false;
  Draw3DParticles(-1);
  nx.rs.CullBack:=false;
  Draw3DTrails(0, tCount);
  nx.rs.Pop;
  glDepthMask(true);
end;

procedure TParticleEngine.Draw3DParticles(ptype: integer);
var i: integer; curTex: integer; xv, yv: TVector;
begin
  if pCount>0 then begin
    xv:=nxMath3D.GetVector(mat, 0);
    yv:=nxMath3D.GetVector(mat, 1);
    curTex:=pTypes[pt[0].ptype].texture; tex.SetTex(curTex);
    glBegin(GL_QUADS);
    for i:=0 to pCount-1 do
      if (ptype=-1) or (pt[i].ptype=ptype) then
        with pt[i] do begin
          if curTex<>pTypes[pt[i].ptype].texture then begin
            glEnd;
            curTex:=pTypes[pt[i].ptype].texture; tex.SetTex(curTex);
            glBegin(GL_QUADS);
          end;
          glColor4fv(@r);
          glTexCoord2f(0,1);
           glVertex3f(x+(-xv.x-yv.x)*rad, y+(-xv.y-yv.y)*rad, z+(-xv.z-yv.z)*rad);
          glTexCoord2f(1,1);
           glVertex3f(x+(+xv.x-yv.x)*rad, y+(+xv.y-yv.y)*rad, z+(+xv.z-yv.z)*rad);
          glTexCoord2f(1,0);
           glVertex3f(x+(+xv.x+yv.x)*rad, y+(+xv.y+yv.y)*rad, z+(+xv.z+yv.z)*rad);
          glTexCoord2f(0,0);
           glVertex3f(x+(-xv.x+yv.x)*rad, y+(-xv.y+yv.y)*rad, z+(-xv.z+yv.z)*rad);
        end;
    glEnd;
  end;
end;

procedure TParticleEngine.Draw3DTrails(iStart, iCount: integer);
var i,j: integer; tp,lpos: single;
    color: TfRGBA;
begin
  for i:=iStart to iStart+iCount-1 do
    with trail[i] do
      if (count>1) and (TotalLen>0) then begin
        tex.SetTex(texture); color:=fRGBA(r,g,b,a);
        if not fading then glColor4fv(@color);

        glBegin(GL_TRIANGLE_STRIP);
        glTexCoord2f(1, 0.5);
        if fading then glColor4f(0,0,0,0);
        with node[0] do glVertex3f(x, y, z);
        lpos:=len[0];
        for j:=1 to count-1 do
          with node[j] do begin
            if lpos<rad then tp:=max(0.5, 1-0.5*lpos/rad)
            else if lpos>totalLen-rad then tp:=min(0.5,0.5*(totalLen-lpos)/rad)
            else tp:=0.5;
            if fading then begin
              color.a:=1-lpos/totalLen;
              glColor4fv(@color);
            end;
            glTexCoord2f(tp, 1); glVertex3f(x-rad, y, z);
            glTexCoord2f(tp, 0); glVertex3f(x+rad, y, z);
            lpos:=lpos+len[j];
          end;
        glEnd;

        glBegin(GL_TRIANGLE_STRIP);
        glTexCoord2f(1, 0.5);
        if fading then glColor4f(0,0,0,0);
        with node[0] do glVertex3f(x, y, z);
        lpos:=len[0];
        for j:=1 to count-1 do
          with node[j] do begin
            if lpos<rad then tp:=max(0.5, 1-0.5*lpos/rad)
            else if lpos>totalLen-rad then tp:=min(0.5,0.5*(totalLen-lpos)/rad)
            else tp:=0.5;
            if fading then begin
              color.a:=1-lpos/totalLen;
              glColor4fv(@color);
            end;
            glTexCoord2f(tp, 1); glVertex3f(x, y-rad, z);
            glTexCoord2f(tp, 0); glVertex3f(x, y+rad, z);
            lpos:=lpos+len[j];
          end;
        glEnd;

        glBegin(GL_TRIANGLE_STRIP);
        glTexCoord2f(1, 0.5);
        if fading then glColor4f(0,0,0,0);
        with node[0] do glVertex3f(x, y, z);
        lpos:=len[0];
        for j:=1 to count-1 do
          with node[j] do begin
            if lpos<rad then tp:=max(0.5, 1-0.5*lpos/rad)
            else if lpos>totalLen-rad then tp:=min(0.5,0.5*(totalLen-lpos)/rad)
            else tp:=0.5;
            if fading then begin
              color.a:=1-lpos/totalLen;
              glColor4fv(@color);
            end;
            glTexCoord2f(tp, 1); glVertex3f(x, y, z-rad);
            glTexCoord2f(tp, 0); glVertex3f(x, y, z+rad);
            lpos:=lpos+len[j];
          end;
        glEnd;
      end;
end;

function TParticleEngine.IndexOf(p: pParticle): integer;
var i: integer;
begin
  for i:=pCount-1 downto 0 do
    if p=@pt[i] then begin
      result:=i; exit;
    end;
  result:=-1;
end;

function TParticleEngine.IndexOf(t: pTrail): integer;
var i: integer;
begin
  for i:=tCount-1 downto 0 do
    if t=@trail[i] then begin
      result:=i; exit;
    end;
  result:=-1;
end;

procedure TParticleEngine.Launch2D(l: pParticleLauncher);
var _pt: pParticle;
begin
  with l^ do
    repeat
      progress:=progress-1;
      _pt:=AddParticle(ptype,
        minR+random*(maxR-minR),
        minG+random*(maxG-minG),
        minB+random*(maxB-minB),
        minRad+random*(maxRad-minRad), x, y);
      _pt^.velocity:=scale(direction, speedMin+random*(speedMax-speedMin));
      Rotate(_pt^.velocity.x, _pt^.velocity.y, (random*2-1)*spreadAngle, 0, 0);
    until progress<1;
end;

procedure TParticleEngine.Launch3D(l: pParticleLauncher);
var _pt: pParticle; vec, cp: TVector;
begin
  with l^ do
    repeat
      progress:=progress-1;
      _pt:=AddParticle(ptype,
        minR+random*(maxR-minR),
        minG+random*(maxG-minG),
        minB+random*(maxB-minB),
        minRad+random*(maxRad-minRad), x, y, z);
      cp:=CrossProduct(direction, vector(direction.z,direction.x,direction.y));
      norm(cp);
      vec:=Rotate(direction, (random-0.5)*spreadAngle*toRad, cp);
      vec:=Rotate(vec, random*2*pi, direction);
      _pt^.velocity:=nxMath3D.Scale(vec, speedMin+random*(speedMax-speedMin));
    until progress<1;
end;

procedure TParticleEngine.Move2D(mpt: single);
var i, j: integer;
begin
  for i:=pCount-1 downto 0 do
    with pt[i] do begin
      x:=x+velocity.x*mpt;
      y:=y+velocity.y*mpt;
      a:=a+pTypes[ptype].ia*mpt;
      rad:=rad+pTypes[ptype].irad*mpt;
      if a<=0 then RemoveParticle(i);
    end;
  for i:=0 to tCount-1 do
    with trail[i] do begin
      if count<maxCount then inc(count)
      else totalLen:=totalLen-len[count-1];
      for j:=count-1 downto 1 do begin
        node[j]:=node[j-1]; len[j]:=len[j-1];
      end;
      node[0].x:=x; node[0].y:=y;
      len[0]:=hypot(node[0].x-node[1].x, node[0].y-node[1].y);
      totalLen:=totalLen+len[0];
    end;
  for i:=lCount-1 downto 0 do
    with launcher[i] do begin
      progress:=progress+interval;
      if progress>=1 then Launch2D(@launcher[i]);
      if decay>0 then dec(decay);
      if decay=0 then begin
        dec(lCount); launcher[i]:=launcher[lCount];
      end;
    end;                                        
end;

procedure TParticleEngine.Move3D(mpt: single);
var i, j: integer;
begin
  for i:=pCount-1 downto 0 do
    with pt[i] do begin
      x:=x+velocity.x*mpt;
      y:=y+velocity.y*mpt;
      z:=z+velocity.z*mpt;
      a:=a+pTypes[pType].ia*mpt;
      rad:=rad+pTypes[pType].irad*mpt;
      if a<=0 then RemoveParticle(i);
    end;
  for i:=0 to tCount-1 do
    with trail[i] do begin
      if count<maxCount then inc(count)
      else totalLen:=totalLen-len[count-1];
      for j:=count-1 downto 1 do begin
        node[j]:=node[j-1]; len[j]:=len[j-1];
      end;
      node[0].x:=x; node[0].y:=y; node[0].z:=z;
      len[0]:=hypot3d(node[0].x-node[1].x, node[0].y-node[1].y, node[0].z-node[1].z);
      totalLen:=totalLen+len[0];
    end;
  for i:=lCount-1 downto 0 do
    with launcher[i] do begin
      progress:=progress+interval;
      if progress>=1 then Launch3D(@launcher[i]);
      if decay>0 then dec(decay);
      if decay=0 then begin
        dec(lCount); launcher[i]:=launcher[lCount];
      end;
    end;
end;

procedure TParticleEngine.ReadMatrix;
var modelM: TMatrix;
begin
  glGetFloatv(GL_MODELVIEW_MATRIX,@modelM);
  mat:=GetRotation(modelM);
  Invert(mat);
end;

procedure TParticleEngine.RemoveParticle(const index: integer);
begin
  dec(pCount); pt[index]:=pt[pCount];
  if pCount<pCount2-2000 then begin
    dec(pCount2, 1000); setlength(pt, pCount2);
  end;
end;

procedure TParticleEngine.SetLauncherColor(l: pParticleLauncher; minR, maxR,
  minG, maxG, minB, maxB: single);
begin
  l^.minR:=minR; l^.maxR:=maxR;
  l^.minG:=minG; l^.maxG:=maxG;
  l^.minB:=minB; l^.maxB:=maxB;
end;

procedure TParticleEngine.SetLauncherPos(l: pParticleLauncher; x, y: single;
  z: single);
begin
  l^.x:=x; l^.y:=y; l^.z:=z;
end;

procedure TParticleEngine.SetParticleColor(p: pParticle; r, g, b: single);
begin
  p^.r:=r; p^.g:=g; p^.b:=b;
end;

procedure TParticleEngine.SetTrailColor(t: pTrail; r, g, b: single; a: single);
begin
  t^.r:=r; t^.g:=g; t^.b:=b; t^.a:=a;
end;

procedure TParticleEngine.SetTrailPos(t: pTrail; x, y: single);
begin
  t^.x:=x; t^.y:=y;
end;

procedure TParticleEngine.SetTrailPos(t: pTrail; x, y, z: single);
begin
  t^.x:=x; t^.y:=y; t^.z:=z;
end;

{ TQuadTexture }

procedure TQuadTexture.DrawTdx(const x, y, tdx, tdy: single);
begin
  if textureI[0]<0 then exit;
  tex.SetTex(textureI[0]);
  DrawPart(texture[0], x, y, tdx, tdy);
  if addX then begin
    tex.SetTex(textureI[1]);
    DrawPart(texture[1], x+texture[0]^.sizeX, y, tdx, tdy);
  end;
  if addY then begin
    tex.SetTex(textureI[2]);
    DrawPart(texture[2], x, y+texture[0]^.sizeY, tdx, tdy);
  end;
  if addX and addY then begin
    tex.SetTex(textureI[3]);
    DrawPart(texture[3], x+texture[0]^.sizeX, y+texture[0]^.sizeY, tdx, tdy);
  end;
end;

procedure TQuadTexture.DrawPart(const tex: PTexture; x, y, tdx, tdy: single);
var tw,th: single;
begin
  with tex^ do begin
    tdx:=tdx/SizeX; tdy:=tdy/SizeY;
    tw:=Width/SizeX; th:=Height/SizeY;
    glBegin(GL_QUADS);
      glTexCoord2f(tdx, tdy);       glVertex2f(x, y);
      glTexCoord2f(tdx, th-tdy);    glVertex2f(x, y+Height);
      glTexCoord2f(tw-tdx, th-tdy); glVertex2f(x+Width, y+Height);
      glTexCoord2f(tw-tdx, tdy);    glVertex2f(x+Width, y);
    glEnd;
  end;
end;

function TQuadTexture.GetTexture(index: integer): PTexture;
begin
  if textureI[index]>=0 then result:=@tex.texture[textureI[index]]
  else result:=nil;
end;

constructor TQuadTexture.Create;
var i: integer;
begin
  UseTDX:=true;
  for i:=0 to 3 do TextureI[i]:=-1;
end;

constructor TQuadTexture.Create(filename: string; transparent: boolean);
begin
  Create;
  LoadTexture(filename, transparent);
end;

destructor TQuadTexture.Destroy;
begin
  FreeTextures;
  inherited Destroy;
end;

procedure TQuadTexture.Draw(const x, y: single);
begin
  DrawTdx(x, y, 0, 0);
end;

procedure TQuadTexture.DrawRotated(const x, y, _angle, _width, _height, cx, cy: single);
begin
  glPushMatrix;
  glTranslatef(x,y,0);
  glRotatef(-_angle,0,0,1);
  glScalef(_width/Width, _height/Height, 1);
  glTranslatef(-cx*Width,-cy*Height,0);
  if UseTDX then DrawTdx(0, 0, 0.5, 0.5)
  else DrawTdx(0, 0, 0, 0);
  glPopMatrix;
end;

procedure TQuadTexture.DrawScaled(const x, y, _width, _height: single);
begin
  glPushMatrix;
  glTranslatef(x,y,0);
  glScalef(_width/Width, _height/Height, 1);
  if UseTDX then DrawTdx(0, 0, 0.5, 0.5)
  else DrawTdx(0, 0, 0, 0);
  glPopMatrix;
end;

procedure TQuadTexture.FreeTextures;
var i: integer;
begin
  for i:=3 downto 0 do
    if textureI[i]>=0 then begin
      tex.RemoveTexture(textureI[i]); textureI[i]:=-1;
    end;
end;

procedure TQuadTexture.LoadTexture(filename: string; transparent: boolean);
var i,dx,dy: integer; tlo: TTextureLoadOptions;
begin
  FreeTextures;

  tlo:=Tex.Options; // Save old options
  Tex.Options:=Tex.Options+[toKeepData, toFit, toNoLoad];
  textureI[0]:=Tex.AddTexture('#Q0_'+filename, filename, transparent);
  Width:=texture[0]^.Width;
  Height:=texture[0]^.Height;
  dx:=texture[0]^.sizeX-texture[0]^.width;
  dy:=texture[0]^.sizeY-texture[0]^.height;

  // Add right side texture if needed
  if (dx>0) and (dx<texture[0]^.sizeX div 2) then begin
    addX:=true;
    textureI[1]:=Tex.AddTexture('#Q1_'+filename,'', transparent);
	  dx:=texture[0]^.width-texture[0]^.sizeX div 2;
	  texture[1]^.Width:=dx;
	  texture[1]^.SizeX:=Pow2Fit(dx);
  end else
    addX:=false;
  // Add Bottom texture if needed
  if (dy>0) and (dy<texture[0]^.sizeY div 2) then begin
    addY:=true;
	  dy:=texture[0]^.height-texture[0]^.sizeY div 2;
    textureI[2]:=Tex.AddTexture('#Q2_'+filename,'', transparent);
	  texture[2]^.Height:=dy;
	  texture[2]^.SizeY:=Pow2Fit(dy);
  end else
    addY:=false;

  // Finalize sizes and copy texture data from original
  if addX and addY then begin
    // Add bottom right corner texture
    textureI[3]:=Tex.AddTexture('#Q3_'+filename,'', transparent);
	  texture[3]^.Width:=texture[1]^.Width;
    texture[3]^.SizeX:=texture[1]^.SizeX;
    texture[3]^.Height:=texture[2]^.Height;
	  texture[3]^.SizeY:=texture[2]^.SizeY;

	  texture[1]^.SizeY:=texture[0]^.SizeY div 2;
	  texture[1]^.Height:=texture[0]^.SizeY div 2;
	  texture[2]^.SizeX:=texture[0]^.SizeX div 2;
	  texture[2]^.Width:=texture[0]^.SizeX div 2;

    // Copy texture data
	  tex.CopyArea(texture[0], texture[1], texture[0]^.sizeX div 2, 0);
	  tex.CopyArea(texture[0], texture[2], 0, texture[0]^.sizeY div 2);
	  tex.CopyArea(texture[0], texture[3],
      texture[0]^.sizeX div 2, texture[0]^.sizeY div 2);
    // Clip original texture
    Tex.Resize(texture[0], texture[0]^.sizeX div 2, texture[0]^.sizeY div 2);
  end else if addX then begin
    texture[1]^.SizeY:=texture[0]^.SizeY;
  	texture[1]^.Height:=texture[0]^.Height;
	  tex.CopyArea(texture[0], texture[1], texture[0]^.sizeX div 2, 0);
    Tex.Resize(texture[0], texture[0]^.sizeX div 2, texture[0]^.sizeY);
  end else if addY then begin
    texture[2]^.SizeX:=texture[0]^.SizeX;
  	texture[2]^.Width:=texture[0]^.Width;
	  tex.CopyArea(texture[0], texture[2], 0, texture[0]^.sizeY div 2);
    Tex.Resize(texture[0], texture[0]^.sizeX, texture[0]^.sizeY div 2);
  end;

  // Load textures into OpenGL
  Tex.Options:=tlo;
  for i:=0 to 3 do
    if texture[i]<>nil then begin
      texture[i]^.PatternWidth:=texture[i]^.Width;
      texture[i]^.PatternHeight:=texture[i]^.Height;
      tex.Restore(texture[i]);
      tex.SetClamp(false, false);
    end;
end;

end.
