#nxStrings documentation

# nxStrings #

---


## Functions and procedures ##
```
function BoolToStr(const b: Boolean): string;
function FillStr(s,str: string; count: Word; align: TStrAlign = saCenter): string;
function ReadCustom(const s,separator: string; const arr: array of pointer;
  const arrt: array of TCustomRead): integer;
function ReadInts(const s,separator: string; var ints: array of integer): integer;
function ReadStrings(const s,separator: string; out strings: array of string): integer;
function StrAfter(const s,s2: string): string;
function StrBefore(const s,s2: string): string;
function StrBetween(const s,s1,s2: string): string;
function StrReplace(s: string; const src,dest: string): string;
function StrToFloat2(const s: string; default: single = 0): single;
function StrToInt2(const s: string; default: integer = 0): integer;
```