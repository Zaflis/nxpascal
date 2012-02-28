unit nxPath;

interface

uses Math, Types;

const MAX_NODES = 1024;

type
  TWNode = record
    x,y,parent,n: integer; w: single;
    open: boolean;
  end;

  TPath = record
    nodes: word;
    node: array[0..MAX_NODES-1] of TPoint;
  end;

  // 0 = clear , 1 = collide
  {$IFDEF fpc}PathMap = ^shortint;
  {$ELSE}TSA = array[0..0] of shortint; PathMap = ^TSA;
  {$ENDIF}

  { TTilePath }

  TTilePath = class
  private
    sx, sy: integer;
    map: PathMap;
  public
    property Width: integer read sx;
    property Height: integer read sy;
    procedure FreeMap(var _map: PathMap);
    function GetCell(x,y: integer): shortint;
    function GetPath(const sp,tp: TPoint; var path: TPath): boolean;
    function GetBetterPath(const sp,tp: TPoint; var path: TPath): boolean;
    //function RawBestPath(const sp,tp: TPoint; var path: TPath): boolean;
    procedure SetCell(x,y: integer; value: shortint);
    procedure SetMap(var _map: PathMap; _width, _height: integer);
  end;

implementation

{ TTilePath }

procedure TTilePath.FreeMap(var _map: PathMap);
begin
  FreeMem(_map); map:=nil; _map:=nil;
end;

// Tries path from "start to end" and "end to start" and returns shortest
function TTilePath.GetBetterPath(const sp, tp: TPoint; var path: TPath): boolean;
var rTemp: boolean; temp: TPath; i: integer;
begin
  result:=getpath(sp,tp,path);
  rTemp:=getpath(tp,sp,temp{%H-});
  if temp.nodes>1 then
    if rTemp and ((result=false) or (temp.nodes<path.nodes)) then begin
      if getcell(tp.x,tp.y)=0 then path.node[temp.nodes-1]:=tp
      else path.node[temp.nodes-1]:=temp.node[0];
      for i:=0 to temp.nodes-2 do
        path.node[temp.nodes-2-i]:=temp.node[i];
      path.nodes:=temp.nodes; result:=rTemp;

      // Fix error sometimes of last and second last node being the same
      if (path.node[path.nodes-1].x=path.node[path.nodes-2].x) and
         (path.node[path.nodes-1].y=path.node[path.nodes-2].y) then
        dec(path.nodes);
    end;
end;

function TTilePath.GetCell(x, y: integer): shortint;
begin
  if (x>-1) and (y>-1) and (x<sx) and (y<sy) then
    result:=map[y*sx+x]
  else result:=1;
end;

function TTilePath.GetPath(const sp, tp: TPoint; var path: TPath): boolean;
// fn = Finish node   c = Current node
// px,py = parent x,y
var fn,i,j,nodes,c,x,y,n0: integer; nearest: single;
    n: array[0..MAX_NODES*4] of TWNode; CanExit: boolean;

  function GetW(const n: TWNode): single;
  begin
    //result:=abs(n.x-tp.X)+abs(n.y-tp.Y);
    result:=hypot(n.x-tp.X, n.y-tp.Y);
      //+Distance(sp.X,sp.Y,tp.X,tp.Y,n.x,n.y);
  end;

  procedure AddNode(const ax,ay: integer);
  begin
    if nodes-1>=high(n) then exit;
    if GetCell(x+ax,y+ay)=0 then begin
      n[nodes].x:=x+ax; n[nodes].y:=y+ay; n[nodes].parent:=c;
      n[nodes].n:=n[c].n+1; n[nodes].open:=true;
      n[nodes].w:=GetW(n[nodes]); inc(nodes);
      SetCell(x+ax, y+ay, -1);
    end;
  end;

begin
  result:=false; path.nodes:=0; x:=sp.X; y:=sp.Y;

  if (x=tp.X) and (y=tp.Y) then begin
    result:=true; exit;
  end;

  fn:=-1; nodes:=1; c:=0;
  n0:=GetCell(sp.X,sp.Y);
  n[0].parent:=-1; n[0].x:=sp.X; n[0].y:=sp.Y;
  n[0].n:=0; n[0].open:=true; n[0].w:=GetW(n[0]);
  repeat
    CanExit:=true;

    x:=n[c].x; y:=n[c].y; n[c].open:=false;

    if (x=tp.X) and (y=tp.Y) then fn:=c
    else begin

      AddNode(-1,0); AddNode(1,0); AddNode(0,-1); AddNode(0,1);

      // Select nearest node to destination
      c:=-1; nearest:=0;
      for i:=0 to nodes-1 do
        if n[i].open then
          if (n[i].w<nearest) or (c=-1) then begin
            c:=i; nearest:=n[i].w;
          end;
    end;

    if fn=-1 then
      for i:=0 to nodes-1 do
        if n[i].open then begin
          CanExit:=false; break;
        end;
  until CanExit;

  for j:=0 to nodes-1 do
    with n[j] do map[y*sx+x]:=0;
  map[sp.Y*sx+sp.X]:=n0;

  if (fn=-1) and (nodes>0) then begin
    fn:=0; nearest:=n[0].w;
    for i:=1 to nodes-1 do
      if n[i].w<nearest then begin
        fn:=i; nearest:=n[i].w;
      end;
  end;

  if fn>-1 then begin
    if n[fn].n>MAX_NODES then exit;

    path.nodes:=n[fn].n;
    j:=fn;
    for i:=n[fn].n-1 downto 0 do
      if j>-1 then begin
        path.node[i].X:=n[j].x;
        path.node[i].Y:=n[j].y;
        j:=n[j].parent;
      end;
  end;
  result:=true;
end;

procedure TTilePath.SetCell(x, y: integer; value: shortint);
begin
  map[y*sx+x]:=value;
end;

procedure TTilePath.SetMap(var _map: PathMap; _width, _height: integer);
begin
  sx:=_width; sy:=_height;
  if _map=nil then getmem(_map,sx*sy);
  self.map:=_map;
end;

end.
