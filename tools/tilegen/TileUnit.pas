unit TileUnit;

{$mode objfpc}{$H+}

interface

uses SysUtils;

type
  TBaseIndexInfo = record
    style: word;
  end;

  TIndexInfo = record
    group: integer;
  end;

  TTileStyle = record
    name: string;
    ch: array[-1..1, -1..1] of byte;
  end;

  { TBaseGroup }

  TBaseGroup = class
  public
    tType: word;
    procedure Assign(source: TBaseGroup);
  end;

  { TAreaBaseGroup }

  TAreaBaseGroup = class(TBaseGroup)
  public
    tiles: array of word;
    procedure Assign(source: TAreaBaseGroup);
    function Clone: TAreaBaseGroup;
  end;

  { TBlendBaseGroup }

  TBlendBaseGroup = class(TBaseGroup)
  public
    ts: word;
    tile: word;
    procedure Assign(source: TBlendBaseGroup);
    function Clone: TBlendBaseGroup;
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

  TBlendImage = record
    filename: string;
    ts: array of word;
  end;

  { TTileSetConfig }

  TTileSetConfig = class
  private
  public
    texturefile, basefile: string;
    patternwidth, patternheight, skipwidth, skipheight,
      baseWidth, baseHeight: word;
    baseGroup: array of word;
    groups: array of TBaseGroup;
    styles: TTileStyles;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(source: TTileSetConfig);
    function BaseCols: integer;
    function BaseRows: integer;
    procedure ClearGroups;
    procedure DeleteGroup(index: integer);
  end;

  { TTileSet }

  TTileSet = class(TTileSetConfig)
  private
  public
    width, height: word;
    indexInfo: array of TIndexInfo;
    procedure Assign(source: TTileSet); overload;
    procedure Assign(source: TTileSetConfig); overload;
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

var
  TILE_DEBUG: boolean = false;

implementation

{ TTileSetConfig }

constructor TTileSetConfig.Create;
begin
  styles:=TTileStyles.Create;
  patternwidth:=16; patternheight:=16;
  skipwidth:=1; skipheight:=1;
end;

destructor TTileSetConfig.Destroy;
begin
  styles.Free;
  inherited Destroy;
end;

procedure TTileSetConfig.Assign(source: TTileSetConfig);
begin
  styles.Assign(source.styles);
  texturefile:=source.texturefile;
  patternwidth:=source.patternwidth;
  patternheight:=source.patternheight;
  skipwidth:=source.skipwidth;
  skipheight:=source.skipheight;
end;

function TTileSetConfig.BaseCols: integer;
begin
  result:=(baseWidth-skipWidth) {%H-}div (patternWidth+skipWidth);
end;

function TTileSetConfig.BaseRows: integer;
begin
  result:=(baseHeight-skipHeight) {%H-}div (patternHeight+skipHeight);
end;

procedure TTileSetConfig.ClearGroups;
var i: integer;
begin
  for i:=0 to high(groups) do FreeAndNil(groups[i]);
  setlength(groups, 0);
end;

procedure TTileSetConfig.DeleteGroup(index: integer);
begin
  groups[index].Free;
  groups[index]:=groups[high(groups)];
  groups[high(groups)]:=nil;
  setlength(groups, high(groups));
end;

{ TTileSet }

procedure TTileSet.Assign(source: TTileSet);
begin
  if source=nil then exit;
  inherited Assign(source);
  width:=source.width; height:=source.height;
end;

procedure TTileSet.Assign(source: TTileSetConfig);
begin
  inherited Assign(source);
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
var F: TextFile; i, j, n: integer; s: string;
begin
  assignfile(F, filename);
  reset(F);
  readln(F); // TileSet_v 1
  // Styles
  readln(F, n);
  setlength(styles.style, n);
  for n:=0 to styles.Count-1 do
    with styles.style[n] do begin
      readln(F, name);
      readln(F, s);
      for j:=-1 to 1 do
        for i:=-1 to 1 do
          ch[i, j]:=strtoint(s[1+ (j+1)*3 + (i+1)]);
    end;
  // Tileset
  readln(F, basefile);

  closefile(F);
end;

procedure TTileSet.SaveToFile(filename: string);
var F: TextFile; i, j, n: integer; s: string;
begin
  assignfile(F, filename);
  rewrite(F);
  writeln(F, 'Tileset_v 1');
  // Styles
  writeln(F, styles.Count);
  for n:=0 to styles.Count-1 do
    with styles.style[n] do begin
      writeln(F, name);
      s:='';
      for j:=-1 to 1 do
        for i:=-1 to 1 do
          if i+j<2 then s:=s+inttostr(ch[i, j])+' '
          else s:=s+inttostr(ch[i, j]);
      writeln(F, s);
    end;
  // Tileset
  writeln(F, basefile);

  closefile(F);
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
  for j:=-1 to 1 do
    for i:=-1 to 1 do style[result].ch[i, j]:=0;
  style[result].ch[0, 0]:=1;
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

{ TAreaBaseGroup }

procedure TAreaBaseGroup.Assign(source: TAreaBaseGroup);
var i: integer;
begin
  inherited Assign(source);
  setlength(tiles, length(source.tiles));
  for i:=0 to high(tiles) do tiles[i]:=source.tiles[i];
end;

function TAreaBaseGroup.Clone: TAreaBaseGroup;
begin
  result:=TAreaBaseGroup.Create;
  result.Assign(self);
end;

{ TBlendBaseGroup }

procedure TBlendBaseGroup.Assign(source: TBlendBaseGroup);
begin
  inherited Assign(source);
  tile:=source.tile; ts:=source.ts;
end;

function TBlendBaseGroup.Clone: TBlendBaseGroup;
begin
  result:=TBlendBaseGroup.Create;
  result.Assign(self);
end;

{ TBaseGroup }

procedure TBaseGroup.Assign(source: TBaseGroup);
begin
  tType:=source.tType;
end;

end.

