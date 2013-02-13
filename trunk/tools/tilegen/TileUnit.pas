unit TileUnit;

{$mode objfpc}{$H+}

interface

//uses Classes, SysUtils;

type

  {TBaseIndexInfo = record

  end;

  TIndexInfo = record

  end;}

  { TTileSet }

  TTileSet = class
  private
  public
    texturefile, workFolder: string;
    patternwidth, patternheight, skipwidth, skipheight,
      baseWidth, baseHeight, width, height: word;
    constructor Create(_workFolder: string = '');
    procedure Assign(source: TTileSet);
    function Cols: integer;
    function Rows: integer;
  end;

implementation

{ TTileSet }

constructor TTileSet.Create(_workFolder: string);
begin
  workFolder:=_workFolder;
end;

procedure TTileSet.Assign(source: TTileSet);
begin
  texturefile:=source.texturefile;
  patternwidth:=source.patternwidth;
  patternheight:=source.patternheight;
  skipwidth:=source.skipwidth;
  skipheight:=source.skipheight;
end;

function TTileSet.Cols: integer;
begin
  //result:=
end;

function TTileSet.Rows: integer;
begin
  //
end;

end.

