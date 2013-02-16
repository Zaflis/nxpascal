unit TileUnit;

{$mode objfpc}{$H+}

interface

//uses Classes, SysUtils;

type
  TBaseIndexInfo = record
    style: word;
  end;

  TIndexInfo = record
    style: word;
  end;

  TTileStyle = record
    name: string;
    tt: byte;
    ch: array[-1..1, -1..1] of shortint;
  end;

  { TTileStyles }

  TTileStyles = class
  private
  public
    style: array of TTileStyle;
    constructor Create;
    function Add(name: string): integer;
    procedure Assign(source: TTileStyles);
    function Count: integer;
    procedure Delete(index: integer);
    function IndexOf(name: string): integer;
  end;

  { TTileSet }

  TTileSet = class
  private
  public
    texturefile: string;
    patternwidth, patternheight, skipwidth, skipheight,
      baseWidth, baseHeight, width, height: word;
    baseInfo: array of TBaseIndexInfo;
    indexInfo: array of TIndexInfo;
    styles: TTileStyles;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(source: TTileSet);
    function BaseCols: integer;
    function BaseRows: integer;
    function Cols: integer;
    function Rows: integer;
    procedure LoadFromFile(filename: string);
    procedure SaveToFile(filename: string);
  end;

  TTileArray = array of word;

  { TWorld }

  TWorld = class
  private
  public
    tileset: TTileSet;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(filename: string);
    procedure SaveToFile(filename: string);
  end;

implementation

{ TTileSet }

constructor TTileSet.Create;
begin
  styles:=TTileStyles.Create;
  patternwidth:=16; patternheight:=16;
  skipwidth:=1; skipheight:=1;
end;

destructor TTileSet.Destroy;
begin
  styles.Free;
  inherited Destroy;
end;

procedure TTileSet.Assign(source: TTileSet);
begin
  if source=nil then exit;
  texturefile:=source.texturefile;
  patternwidth:=source.patternwidth;
  patternheight:=source.patternheight;
  skipwidth:=source.skipwidth;
  skipheight:=source.skipheight;
  styles.Assign(source.styles);
end;

function TTileSet.BaseCols: integer;
begin
  result:=(baseWidth-skipWidth) {%H-}div (patternWidth+skipWidth);
end;

function TTileSet.BaseRows: integer;
begin
  result:=(baseHeight-skipHeight) {%H-}div (patternHeight+skipHeight);
end;

function TTileSet.Cols: integer;
begin
  result:=(width-skipWidth) {%H-}div (patternWidth+skipWidth);
end;

function TTileSet.Rows: integer;
begin
  result:=(height-skipHeight) {%H-}div (patternHeight+skipHeight);
end;

procedure TTileSet.LoadFromFile(filename: string);
begin
  //
end;

procedure TTileSet.SaveToFile(filename: string);
begin
  //
end;

{ TWorld }

constructor TWorld.Create;
begin
  tileset:=TTileSet.Create;
end;

destructor TWorld.Destroy;
begin
  tileset.Free;
  inherited Destroy;
end;

procedure TWorld.LoadFromFile(filename: string);
begin
  //
end;

procedure TWorld.SaveToFile(filename: string);
begin
  //
end;

{ TTileStyles }

constructor TTileStyles.Create;
begin
  setlength(style, 2);
  style[0].name:='Single tile';
  style[1].name:='Combination';
end;

function TTileStyles.Add(name: string): integer;
var i, j: integer;
begin
  result:=length(style);
  setlength(style, length(style)+1);
  style[result].name:=name;
  style[result].tt:=1;
  for j:=-1 to 1 do
    for i:=-1 to 1 do style[result].ch[i, j]:=-1;
  style[result].ch[0, 0]:=0;
end;

procedure TTileStyles.Assign(source: TTileStyles);
var i: integer;
begin
  if source=nil then exit;
  setlength(style, length(source.style));
  for i:=0 to high(source.style) do
    style[i]:=source.style[i];
end;

function TTileStyles.Count: integer;
begin
  result:=length(style);
end;

procedure TTileStyles.Delete(index: integer);
var i: integer;
begin
  for i:=index to high(style)-1 do
    style[i]:=style[i+1];
  setlength(style, high(style));
end;

function TTileStyles.IndexOf(name: string): integer;
var i: integer;
begin
  name:=lowercase(name);
  for i:=0 to high(style) do
    if name=lowercase(style[i].name) then begin
      result:=i; exit;
    end;
  result:=-1;
end;

end.

