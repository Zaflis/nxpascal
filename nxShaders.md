#nxShaders documentation

# nxShaders #

---


## Functions and procedures ##

```
procedure MakeFShader2D(text: TStrings);
```
Writes fragment-shader for 2D to text.

```
procedure MakeFShader3D(text: TStrings; bump: boolean);
```
Writes fragment-shader for 3D to text. Optional bump-mapping.

```
procedure MakeVShader2D(text: TStrings);
```
Writes vertex-shader for 2D to text.

```
procedure MakeVShader3D(text: TStrings);
```
Writes vertex-shader for 3D to text.