unit GameUnit;

interface

uses Classes, Dialogs, SysUtils, math, nxMath, nxMath3D, nxGL,
  nxGame, nxGLExtra, nxTypes;

const
  radius = 7.0;
  movespeed = 0.3;
  animspeed = 21.0;
  physicsDetail = 0.5;
  physicsTime = 10;

type

  TWalker = record
    position: TVector2f;
    goLeft: boolean;
    anim: single;
    color: TRGB;
  end;

  { TGame }

  TGame = class(TGameHandler)
  public
    walker: array of TWalker;
    physLoops: integer;
    pt: TParticleEngine;
    physEnd: cardinal;
    constructor Create;
    destructor Destroy; override;
    procedure GameLoop; override;
    procedure AddWalker(x, y: single);
    function DoPhysics: boolean;
    procedure Reset;
    procedure Switch(w1, w2: integer);
  end;

implementation

{ TCustomGame }

constructor TGame.Create;
begin
  inherited Create;
  Reset;
  pt:=TParticleEngine.Create;
end;

destructor TGame.Destroy;
begin
  pt.Free;
  inherited Destroy;
end;

procedure TGame.GameLoop;
var i: integer;
begin
  for i:=high(walker) downto 0 do begin
    with walker[i] do begin
      anim:=1.5+1.0*sin(toRad*animSpeed*position.x);

      if goLeft then begin
        position.x:=position.x-movespeed;
        if position.x<-radius then position.x:=400+radius*0.9;
      end else begin
        position.x:=position.x+movespeed;
        if position.x>400+radius then position.x:=-radius*0.9;
      end;
      if position.y<radius then position.y:=position.y+movespeed*0.5
      else if position.y>300-radius then position.y:=position.y-movespeed*0.5;

    end;
  end;

  pt.Move2D(mpt);

  // Repeat physics until no collisions
  physLoops:=0;
  repeat
    inc(physLoops);
    if not DoPhysics then break;
  until nx.GetTick>physEnd;

  // "Lazy" sorting for drawing order
  for i:=0 to high(walker)-1 do
    if walker[i].position.y>walker[i+1].position.y then
      Switch(i, i+1);
end;

procedure TGame.AddWalker(x, y: single);
var i: integer;
begin
  setlength(walker, length(walker)+1);
  i:=high(walker);
  while i>=1 do begin
    if y<walker[i-1].position.y then begin
      walker[i]:=walker[i-1];
      dec(i);
    end else break;
  end;
  walker[i].position:=vector2f(x, y);
  walker[i].goLeft:=random<0.5;
  walker[i].anim:=random*3;
  walker[i].color.r:=180+random(76);
  walker[i].color.g:=180+random(76);
  walker[i].color.b:=180+random(76);
end;

function TGame.DoPhysics: boolean;
var i, j: integer; d: single; v: TVector2f;
begin
  result:=false;
  for i:=0 to high(walker)-1 do
    with walker[i].position do begin
      for j:=i+1 to high(walker) do
        with walker[j] do begin
          if (abs(x-position.x)<radius*2) and (abs(y-position.y)<radius*2) then begin
            d:=hypot(x-position.x, y-position.y);
            if d<radius*2 then begin
              v.x:=position.x-x;
              v.y:=position.y-y;
              d:=movespeed*physicsDetail/d;
              v.x:=v.x*d;
              v.y:=v.y*d;
              x:=x-v.x;
              y:=y-v.y;
              position.x:=position.x+v.x;
              position.y:=position.y+v.y;
              result:=true;
              if random<0.1 then begin
                pt.AddParticle(0, 1-random*0.4, 1-random*0.3, 1, 4,
                  (x+position.x)*0.5, (y+position.y)*0.5-10)^.velocity:=
                  vector(random*8-4, random*8-4, 0);
              end;
            end;
          end;
        end;
    end;
end;

procedure TGame.Reset;
var i: integer;
begin
  setlength(walker, 0);
  for i:=1 to 100 do
    AddWalker(random*400, 21+random*(300-21-8));
end;

procedure TGame.Switch(w1, w2: integer);
var tmp: TWalker;
begin
  tmp:=walker[w1];
  walker[w1]:=walker[w2];
  walker[w2]:=tmp;
end;

end.

