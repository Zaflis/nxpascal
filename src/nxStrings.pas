unit nxStrings;

{$IFDEF fpc}{$H+}{$ENDIF}

interface

uses {$IFDEF fpc}LCLProc,{$ENDIF} IniFiles;

type
  TStrAlign = (saLeft, saRight, saCenter);
  TCustomRead = (crString, crInt, crSingle, crDouble, crByte, crWord,
    crShortInt, crSmallInt, crCardinal, crBool, crInt64);

  { TIniFileExt }

  TIniFileExt = class(IniFiles.TIniFile)
  public
    // Allows boolean values be read as 1/0, true/false with or without caps
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; override;
  end;

  function BoolToStr(const b: Boolean): string;
  function FillStr(s,str: string; count: Word; align: TStrAlign = saCenter): string;
  function ReadCustom(const s,separator: string; const arr: array of pointer;
    const arrt: array of TCustomRead): integer;
  function ReadInts(const s, separator: string; var ints: array of integer): integer;
  function ReadStrings(const s, separator: string; out strings: array of string): integer;
  function StrAfter(const s, s2: string): string;
  function StrBefore(const s, s2: string): string;
  function StrBetween(const s, s1, s2: string): string;
  function StrReplace(s: string; const src, dest: string): string;
  function StrToFloat2(const s: string; default: single = 0): single;
  function StrToInt2(const s: string; default: integer = 0): integer;

{$IFDEF VER2_7}
  //operator+(const s: string; i: integer): string;
  operator+(const i: integer; const s: string): string;
{$ENDIF}

implementation

uses SysUtils;

function BoolToStr(const b: Boolean): string;
begin
  if b then BoolToStr:='True' else BoolToStr:='False';
end;

// Makes certain length string with alignment of old string
// 'abc' could become '--abc---' or '.....abc' with length 8
function FillStr(s,str: string; count: Word; align: TStrAlign): string;
var side: Boolean;
begin
  side:=true;
  while length(s)<count do
    if align=saCenter then begin
      if side then s:=s+str
      else s:=str+s;
      side:=not side;
    end else if align=saRight then
      s:=str+s
    else s:=s+str;  // Align = Left
  FillStr:=s;
end;

function ReadCustom(const s,separator: string; const arr: array of pointer;
  const arrt: array of TCustomRead): integer;
var p,arrtl: integer; cur: string; defaultStr: boolean;
  procedure SetValue;
  begin
    if arr[p]=nil then exit;
    if defaultStr then string(arr[p]^):=cur
    else case arrt[p mod arrtl] of
      crString: string(arr[p]^):=cur;
      crInt: integer(arr[p]^):=strtointdef(cur,0);
      crSingle: single(arr[p]^):=strtofloat(cur);
      crDouble: double(arr[p]^):=strtofloat(cur);
      crByte: byte(arr[p]^):=strtointdef(cur,0);
      crWord: word(arr[p]^):=strtointdef(cur,0);
      crShortInt: shortint(arr[p]^):=strtointdef(cur,0);
      crSmallInt: smallint(arr[p]^):=strtointdef(cur,0);
      crCardinal: cardinal(arr[p]^):=strtointdef(cur,0);
      crBool: boolean(arr[p]^):=(cur<>'0') and (lowercase(cur)<>'false');
      crInt64: int64(arr[p]^):=strtoint(cur);
    end;
  end;
var ha,i,l: integer; c,cs: string;
begin
  ha:=high(arr);
  if ha<1 then begin
    ReadCustom:=0; exit;
  end;
  defaultStr:=high(arrt)<1; arrtl:=length(arrt);
  p:=0; l:=length(separator); i:=1; cur:='';
  while i<=length(s) do begin
    c:=copy(s,i,1); cs:=copy(s,i,l);
    if cs=separator then begin
      if cur<>'' then begin
        SetValue; cur:=''; inc(p);
        if p>ha then break;
      end;
      inc(i,l-1);
    end else cur:=cur+c;
    inc(i);
  end;
  if cur<>'' then begin
    SetValue; inc(p);
  end;
  ReadCustom:=p;
end;

function ReadInts(const s,separator: string; var ints: array of integer): integer;
var i,p,l,c2: integer; c,cs,cur: string;
begin
  c2:=high(ints);
  if c2<1 then begin
    ReadInts:=0; exit;
  end;
  if c2>10 then c2:=10;
  for i:=0 to c2 do ints[i]:=0;
  p:=0; l:=length(separator); i:=1; cur:='';
  while i<=length(s) do begin
    c:=copy(s,i,1); cs:=copy(s,i,l);
    if cs=separator then begin
      if cur<>'' then begin
        ints[p]:=strtointdef(cur,0); cur:='';
        inc(p);
        if p>high(ints) then break;
      end;
      i:=i+l-1;
    end else cur:=cur+c;
    inc(i);
  end;
  if cur<>'' then begin
    ints[p]:=strtointdef(cur,0); inc(p);
  end;
  ReadInts:=p;
end;

function ReadStrings(const s,separator: string; out strings: array of string): integer;
var i,p,l,c2: integer; c,cs,cur: string;
begin
  c2:=high(strings);
  if c2<1 then begin
    ReadStrings:=0; exit;
  end;
  if c2>10 then c2:=10;
  for i:=0 to c2 do strings[i]:='';
  p:=0; l:=length(separator); i:=1; cur:='';
  while i<=length(s) do begin
    c:=copy(s,i,1); cs:=copy(s,i,l);
    if cs=separator then begin
      if cur<>'' then begin
        strings[p]:=cur; cur:='';
        inc(p);
        if p>high(strings) then break;
        strings[p]:='';
      end;
      i:=i+l-1;
    end else cur:=cur+c;
    inc(i);
  end;
  if cur<>'' then begin
    strings[p]:=cur; inc(p);
  end;
  ReadStrings:=p;
end;

// String after s2
function StrAfter(const s,s2: string): string;
var p: integer;
begin
  p:=pos(s2,s);
  if p>0 then StrAfter:=copy(s,p+length(s2),length(s)-p)
  else StrAfter:='';
end;

// String before s2
function StrBefore(const s,s2: string): string;
var p: integer;
begin
  p:=pos(s2,s);
  if p>0 then StrBefore:=copy(s,1,p-1)
  else StrBefore:='';
end;

// Returns string from s that is between s1 and s2
function StrBetween(const s,s1,s2: string): string;
var p1,p2: integer;
begin
  p1:=pos(s1,s); p2:=pos(s2,s);
  if (p1>0) and (p2>0) then
    StrBetween:=copy(s,p1+length(s1),p2-p1-length(s1))
  else StrBetween:='';
end;

// Replace all src strings with dest string
function StrReplace(s: string; const src,dest: string): string;
var l,p: integer;
begin
  if src<>'' then begin
    l:=length(src);
    repeat
      p:=pos(src,s);
      if p>0 then s:=copy(s,1,p-1)+dest+copy(s,p+l,length(s));
    until p=0;
  end;
  StrReplace:=s;
end;

function StrToFloat2(const s: string; default: single): single;
begin
  try
    result:=StrToFloat(s);
  except
    result:=default;
  end;
end;

function StrToInt2(const s: string; default: integer): integer;
begin
  try
    result:=StrToInt(s);
  except
    result:=default;
  end;
end;

{$IFDEF VER2_7}
{operator+(const s: string; i: integer): string;
begin
  // Usage does not compile on latest FPC 2.7.1
  result:=s+inttostr(i);
end;}

// string := int + string
operator+(const i: integer; const s: string): string;
begin
  result:=inttostr(i)+s;
end;
{$ENDIF}

{ TIniFileExt }

function TIniFileExt.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
  function CharToBool(AChar: char): boolean;
  begin
    Result := (AChar<>'0') and (AChar<>'f') and (AChar<>'F');
  end;
var s: string;
begin
  s := ReadString(Section, Ident, '');
  if s > '' then Result := CharToBool(s[1])
  else Result := Default;
end;

initialization

  // Is known to be deprecated, but nxPascal needs consistency
  // where all floating points are represented with a dot in strings.
  DecimalSeparator{%H-}:='.';

end.
