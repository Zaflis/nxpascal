unit nxMath;

{$I nxInc.inc}

interface

{ TODO:
- Recheck Distance2
}

uses nxTypes, math;

  function Angle(const px1,py1,px2,py2: single): single; overload;
  function Angle(src,dest: single): single; overload;
  function Catmull(const p0,p1,p2,p3,t: single): single; overload;
  function Catmull(const a,b,c,d: TVector2f; const delta: single): TVector2f; overload;
  function Distance(x1,y1,x2,y2, px,py: single): single;
  function Distance2(x1,y1,x2,y2, px,py: single): single;
  function dmod(a, b: double): double;
  function Dot(const a,b: TVector2f): single;
  function fmod(a, b: single): single;
  function Interpolate(const v1,v2,s: single): single; overload;{$IFDEF CanInline}inline;{$ENDIF}
  function Interpolatei(const v1,v2: integer; s: single): integer;
  function Interpolate(const v1,v2: TVector2f; const s: single): TVector2f; overload;{$IFDEF CanInline}inline;{$ENDIF}
  function Invert(const v: TVector2f): TVector2f; overload;
  function LinesCross(const x0, y0, x1, y1, x2, y2, x3, y3: Integer): Boolean;
  function Mod2(a,b: integer): integer;
  function Norm(const v: TVector2f; const h: PSingle = nil): TVector2f; overload;
  procedure Norm(var x,y: single; const h: PSingle = nil); overload;
  procedure Norm(var x,y: double; const h: PDouble); overload;
  function PointInCircle(const p,circle: TVector2f; const radius: single): boolean; overload;
  function PointInCircle(const pX,pY,circleX,circleY,radius: single): boolean; overload;
  function PointInRect(const x, y: integer; rect: TRecti): boolean; overload;
  function PointInRect(const x, y: single; rect: TRectf): boolean; overload;
  function Pow2fit(n: integer): integer;
  function Pow2near(n: integer): integer;
  function Reflect(const rayStart, rayDir, wallPoint: TVector2f): TVector2f; overload;
  function Reflect(const rayDir, wallNormal: TVector2f): TVector2f; overload;
  function Rotate(const pt,center: TVector2f; const _angle: single): TVector2f; overload;
  procedure Rotate(var x, y: single; const _angle, centerX, centerY: single); overload;{$IFDEF CanInline}inline;{$ENDIF}
  function Smoothen(n: single): single;
  function Tangent(const p1, p2: TVector2f): TVector2f; overload;
  function Vector2f(const x, y: single): TVector2f;
  function VectorAdd(const v1, v2: TVector2f): TVector2f; overload;
  function VectorMatch(const a, b: TVector2f; delta: single=0.01): boolean; overload;
  function VectorSub(const v1, v2: TVector2f): TVector2f; overload;

// Operator overloading
{$IFDEF fpc}
  operator +(const a, b: TVector2f): TVector2f;
  operator -(const a, b: TVector2f): TVector2f;
  operator *(const a, b: TVector2f): TVector2f;
  operator *(const a: TVector2f; n: single): TVector2f;
  operator /(const a: TVector2f; n: single): TVector2f;
{$ENDIF}

implementation

// Angle from 1 point to another, given in radians
function Angle(const px1,py1,px2,py2: single): single;
begin
  result:=arctan2(py2-py1,px2-px1);
  if result<0 then result:=result+PI*2;
end;

// Angle between 2 vectors, given in radians
function Angle(src,dest: single): single;
begin
  result:=src-dest;
  while result<-PI do result:=result+PI*2;
  while result>PI do result:=result-PI*2;
end;

function Catmull(const p0,p1,p2,p3,t: single): single;
begin
  result:=0.5*( 2*p1+(p2-p0)*t +
   (2*p0-5*p1+4*p2-p3)*t*t +
   (3*p1-p0-3*p2+p3)*t*t*t );
end;

function Catmull(const a,b,c,d: TVector2f; const delta: single): TVector2f;
begin
  result.x:=Catmull(a.x, b.x, c.x, d.x, delta);
  result.y:=Catmull(a.y, b.y, c.y, d.y, delta);
end;

// Distance from point to line
function Distance(x1,y1,x2,y2, px,py: single): single;
var k: single;
begin
  if x1=x2 then Distance:=abs(px-x1)
  else begin
    k:=(y2-y1)/(x2-x1);
    Distance:=abs(k*px-py+y1-k*x1)/sqrt(k*k+1);
  end;
end;

// Also returns side (positive/negative)
function Distance2(x1,y1,x2,y2, px,py: single): single;
var k: single;
begin
  if x1=x2 then result:=abs(px-x1)
  else begin
    k:=(y2-y1)/(x2-x1);
    result:=abs(k*px-py+y1-k*x1)/sqrt(k*k+1);
  end;
  if Angle(Angle(x1,y1,px,py),Angle(x1,y1,x2,y2))<0 then
    result:=-result;

  {.$NOTE could this use dot product instead of angles?}

end;

function dmod(a, b: double): double;
begin
  result:=a-int(a/b)*b;
end;

function Dot(const a, b: TVector2f): single;
begin
  result:=a.x*b.x + a.y*b.y;
end;

function fmod(a, b: single): single;
begin
  result:=a-int(a/b)*b;
end;

function Interpolate(const v1,v2,s: single): single;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result:=v1+s*(v2-v1);
end;

function Interpolatei(const v1,v2: integer; s: single): integer;
begin
  result:=round(v1+s*(v2-v1));
end;

function Interpolate(const v1,v2: TVector2f; const s: single): TVector2f;{$IFDEF CanInline}inline;{$ENDIF}
begin
  result.x:=v1.x+s*(v2.x-v1.x);
  result.y:=v1.y+s*(v2.y-v1.y);
end;

function Invert(const v: TVector2f): TVector2f;
begin
  result.x:=-v.x;
  result.y:=-v.y;
end;

// Line1(x0,y0,x1,y1), Line2(x2,y2,x3,y3)
function LinesCross(const x0, y0, x1, y1, x2, y2, x3, y3: Integer): Boolean;
var D, AB, CD: Single;
begin
  LinesCross:=false;
  D:=(x1 - x0) * (y3 - y2) - (y1 - y0) * (x3 - x2);
  if (Abs(D)<0.001) then exit;
  AB:=((y0 - y2) * (x3 - x2) - (x0 - x2) * (y3 - y2)) / D;
  if (AB>0.0) and (AB<1.0) then begin
    CD:=((y0 - y2) * (x1 - x0) - (x0 - x2) * (y1 - y0)) / D;
    if (CD>0.0) and (CD<1.0) then begin
      LinesCross:=true; exit;
    end;
  end;
end;

// 11 mod2 10 = 1  |  -11 mod2 10 = -9  |  5 mod2 0 = 0
function Mod2(a,b: integer): integer;
begin
  if b=0 then result:=0
  else begin
    b:=abs(b);
    result:=abs(a) mod b;
    if (a<0) and (result>0) then result:=result-b;
  end;
end;

function Norm(const v: TVector2f; const h: PSingle): TVector2f;
var _h: single;
begin
  _h:=hypot(v.x,v.y);
  if h<>nil then h^:=_h;
  if _h>0 then begin
    _h:=1/_h; result.x:=v.x*_h; result.y:=v.y*_h;
  end else begin
    result.x:=1; result.y:=0;
  end;
end;

procedure Norm(var x, y: single; const h: PSingle);
var _h: single;
begin
  _h:=hypot(x,y);
  if h<>nil then h^:=_h;
  if _h>0 then begin
    _h:=1/_h; x:=x*_h; y:=y*_h;
  end else begin
    x:=1; y:=0;
  end;
end;

procedure Norm(var x, y: double; const h: PDouble);
var _h: double;
begin
  _h:=hypot(x,y);
  if h<>nil then h^:=_h;
  if _h>0 then begin
    _h:=1/_h; x:=x*_h; y:=y*_h;
  end else begin
    x:=1; y:=0;
  end;
end;

function PointInCircle(const p,circle: TVector2f; const radius: single): boolean;
var x,y: single;
begin
  x:=circle.x-p.x; y:=circle.y-p.y;
  PointInCircle:=(x*x+y*y)<(radius*radius);
end;

function PointInCircle(const pX,pY,circleX,circleY,radius: single): boolean;
var x,y: single;
begin
  x:=circleX-pX; y:=circleY-pY;
  PointInCircle:=(x*x+y*y)<(radius*radius);
end;

function PointInRect(const x, y: integer; rect: TRecti): boolean;
var tmp: integer;
begin
  if rect.x1>rect.x2 then begin
    tmp:=rect.x1; rect.x1:=rect.x2; rect.x2:=tmp;
  end;
  if rect.y1>rect.y2 then begin
    tmp:=rect.y1; rect.y1:=rect.y2; rect.y2:=tmp;
  end;
  result:=(x>=rect.x1) and (x<=rect.x2) and (y>=rect.y1) and (y<=rect.y2);
end;

function PointInRect(const x, y: single; rect: TRectf): boolean;
var tmp: single;
begin
  if rect.x1>rect.x2 then begin
    tmp:=rect.x1; rect.x1:=rect.x2; rect.x2:=tmp;
  end;
  if rect.y1>rect.y2 then begin
    tmp:=rect.y1; rect.y1:=rect.y2; rect.y2:=tmp;
  end;
  result:=(x>=rect.x1) and (x<=rect.x2) and (y>=rect.y1) and (y<=rect.y2);
end;

function Pow2fit(n: integer): integer;
var neg: boolean;
begin
  if n<0 then begin
    n:=-n; neg:=true;
  end else neg:=false;
  if n<3 then result:=n
  else result:=round(intpower(2,ceil(log2(n))));
  if neg then result:=-result;
end;

function Pow2near(n: integer): integer;
var h,l: integer; neg: boolean;
begin
  if n<0 then begin
    n:=-n; neg:=true;
  end else neg:=false;
  if n<3 then result:=n
  else begin
    l:=trunc(log2(n)); h:=round(log2(n));
    if l=h then result:=trunc(intpower(2,l))
    else begin
      h:=round(intpower(2,h)); l:=round(intpower(2,l));
      if n<((h+l) div 2) then result:=l else result:=h;
    end;
  end;
  if neg then result:=-result;
end;

function Reflect(const rayStart, rayDir, wallPoint: TVector2f): TVector2f;
var n: TVector2f;
begin
  n.x:=rayStart.x-wallPoint.x;
  n.y:=rayStart.y-wallPoint.y;
  result:=Reflect(rayDir,norm(n));
end;

function Reflect(const rayDir, wallNormal: TVector2f): TVector2f;
var temp: single;
begin
  temp:=2*(rayDir.x*wallNormal.x+rayDir.y*wallNormal.y);
  result.x:=rayDir.x-wallNormal.x*temp;
  result.y:=rayDir.y-wallNormal.y*temp;
end;

function Rotate(const pt,center: TVector2f; const _angle: single): TVector2f;
begin
  result:=pt;
  rotate(result.x, result.y, _angle, center.x, center.y);
end;

procedure Rotate(var x, y: single; const _angle, centerX, centerY: single);{$IFDEF CanInline}inline;{$ENDIF}
var ca, sa, _x, _y: single;
begin
  sa:=sin(_angle*toRad); ca:=cos(_angle*toRad);
  _x:=x-centerX; _y:=y-centerY;
  x:=_x*ca-_y*sa+centerX;
  y:=_x*sa+_y*ca+centerY;
end;

function Smoothen(n: single): single;
begin
  if n<0 then n:=0
  else if n>1 then n:=1;
  Smoothen:=0.5-cos(n*pi)*0.5;
end;

function Tangent(const p1, p2: TVector2f): TVector2f;
begin
  Result.x:=-(p2.y-p1.y);
  Result.y:= (p2.x-p1.x);
end;

function Vector2f(const x,y: single): TVector2f;
begin
  Vector2f.x:=x; Vector2f.y:=y;
end;

function VectorAdd(const v1, v2: TVector2f): TVector2f;
begin
  result.x:=v1.x+v2.x;
  result.y:=v1.y+v2.y;
end;

function VectorMatch(const a, b: TVector2f; delta: single): boolean;
begin
  result:=(abs(a.x-b.x)<delta) and (abs(a.y-b.y)<delta);
end;

function VectorSub(const v1, v2: TVector2f): TVector2f;
begin
  result.x:=v1.x-v2.x;
  result.y:=v1.y-v2.y;
end;

// Operator overloading
{$IFDEF fpc}
operator +(const a, b: TVector2f): TVector2f;
begin
  result.x:=a.x+b.x; result.y:=a.y+b.y;
end;

operator-(const a, b: TVector2f): TVector2f;
begin
  result.x:=a.x-b.x; result.y:=a.y-b.y;
end;

operator*(const a, b: TVector2f): TVector2f;
begin
  result:=tangent(a, b);
end;

operator*(const a: TVector2f; n: single): TVector2f;
begin
  result.x:=a.x*n; result.y:=a.y*n;
end;

operator/(const a: TVector2f; n: single): TVector2f;
begin
  result.x:=a.x/n; result.y:=a.y/n;
end;
{$ENDIF}

end.
