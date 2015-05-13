#nxGLUI documentation

# nxGLUI #

---

uses [dglOpenGL](dglOpenGL.md), [nxGL](nxGL.md), [nxGraph](nxGraph.md), [nxTypes](nxTypes.md), [nxUI](nxUI.md)

## Classes ##
  * TGLUI (TCustomUI)

## TGLUI ##
```
constructor Create(texturePath: string);
```
Create OpenGL-based UI, and load textures using given texture-path.

```
procedure Draw;
```
Draw whole UI.

```
procedure LoadTextures(path: string);
```
Load UI textures from this path.

```
procedure DrawElement(e: TUIElement);
procedure DrawSubElements(e: TUIContainer);
procedure DrawButton(e: TUIButton);
procedure DrawCheckBox(e: TUICheckBox);
procedure DrawDropDown(e: TUIDropDown);
procedure DrawEdit(e: TUIEdit);
procedure DrawImage(e: TUIImage);
procedure DrawLabel(e: TUILabel);
procedure DrawList(e: TUIList);
procedure DrawMemo(e: TUIMemo);
procedure DrawPanel(e: TUIPanel);
procedure DrawScrollBar(e: TUIScrollBar);
procedure DrawTab(e: TUITab);
procedure DrawTabControl(e: TUITabControl);
procedure DrawWindow(e: TUIWindow);
```
Draw specific UI elements.