#nxGLExtra documentation

# nxGLExtra #

---

uses [dglOpenGL](dglOpenGL.md), [nxGL](nxGL.md), [nxGraph](nxGraph.md), [nxMath](nxMath.md), [nxMath3D](nxMath3D.md), [nxTypes](nxTypes.md)

## Classes ##
  * TParticleEngine
  * TQuadTexture

## TParticleEngine class ##

```
destructor Destroy;
function AddLauncher(ptype: word; interval, minRad,maxRad, speedMin,speedMax, spreadAngle: single; dir: TVector; decay: integer): pParticleLauncher;
function AddParticle(ptype: word; r,g,b, rad, x, y: single; z: single = 0): pParticle;
function AddPType(texture: integer; a,ia, irad: single): integer;
function AddTrail(texture: integer; maxCount: integer; rad, x, y: single; z: single = 0): pTrail;
procedure ApplyForce(ptype: word; fx, fy: single; fz: single = 0);
procedure Clear;
procedure ClearLaunchers;
procedure ClearParticles;
procedure ClearPTypes;
procedure ClearTrails;
function CloneLauncher(l: pParticleLauncher; dir: TVector; x,y: single; z: single = 0): pParticleLauncher;
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
procedure SetLauncherColor(l: pParticleLauncher; minR,maxR, minG,maxG, minB,maxB: single);
procedure SetLauncherPos(l: pParticleLauncher; x,y: single; z: single = 0);
procedure SetLauncherPos(l: pParticleLauncher; const v: TVector);
procedure SetParticleColor(p: pParticle; r,g,b: single);
procedure SetTrailColor(t: pTrail; r,g,b: single; a: single = 1);
procedure SetTrailPos(t: pTrail; x, y: single);
procedure SetTrailPos(t: pTrail; x, y, z: single);
procedure SetTrailPos(t: pTrail; const v: TVector);
```

## TQuadTexture class ##

```
property Texture[index: integer]: PTexture;
constructor Create;
constructor Create(filename: string; transparent: boolean = false);
destructor Destroy;
procedure Draw(const x, y: single);
procedure DrawRotated(const x, y, _angle, _width, _height, cx, cy: single);
procedure DrawScaled(const x, y, _width, _height: single);
procedure FreeTextures;
procedure LoadTexture(filename: string; transparent: boolean = false);
```