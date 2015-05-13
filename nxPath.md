#nxPath documentation

# nxPath #

---


## Classes ##
  * TTilePath

## TTilePath class ##

```
procedure FreeMap(var _map: PathMap);
function GetCell(x,y: integer): shortint;
function GetPath(const sp,tp: TPoint; var path: TPath): boolean;
function GetBetterPath(const sp,tp: TPoint; var path: TPath): boolean;
procedure SetCell(x,y: integer; value: shortint);
procedure SetMap(var _map: PathMap; _width, _height: integer);
```