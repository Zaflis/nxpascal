unit nxGLUI;

{$H+}

interface

uses SysUtils, {$IFDEF fpc}FileUtil,{$ENDIF} math, dglOpenGL,
  nxTypes, nxGL, nxUI;

type

  { TGLUI }

  TGLUI = class(TCustomUI)
  private
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
  public
    constructor Create(texturePath: string);
    procedure Draw;
    procedure LoadTextures(path: string);
  end;

implementation

procedure UI_Draw(const r: TBoundsRect; pattern: integer; dt: TUIDrawStyle;
  bdSize: byte);
begin
  if dt=uiSections then nx.DrawSections(r.x,r.y,r.w,r.h,pattern,bdSize/255)
  else if dt=uiRepeat then nx.DrawTexRepeat(r.x,r.y,r.w,r.h)
  else nx.DrawScaled(r.x,r.y,r.w,r.h,pattern);
end;

{ TGLUI }

constructor TGLUI.Create(texturePath: string);
begin
  if nx.FontCount=0 then begin
    nx.CreateBasicFont; nx.Font[0].SetColor(0,0,0);
  end;
  LoadTextures(texturePath);
  inherited CreateByName('Main');
end;

procedure TGLUI.Draw;
begin
  ProcessFaders;
  DrawElement(self);
end;

procedure TGLUI.LoadTextures(path: string);
  procedure TryAdd(_name: string);
  var FN: string; t: integer;
  begin
    FN:=path+_name+'.png';
    if fileexists(FN) then begin
      t:=tex.AddTexture('ui_'+_name,FN,true);
      tex.SetClamp(false,false);
      with tex.texture[t] do begin
        PatternWidth:=Width; PatternHeight:=Height;
        if _name='button' then PatternHeight:=Height div 2
        else if _name='downbutton' then PatternHeight:=Height div 2
        else if _name='checkbox' then PatternWidth:=Width div 2
        else if _name='scrollbar' then begin
          PatternWidth:=Width div 4; PatternHeight:=Height div 2;
        end else if _name='tab' then PatternHeight:=Height div 2;
      end;
    end;
  end;
var l: integer;
begin
  l:=length(path);
  if (path<>'') and ((path[l]<>'\') and (path[l]<>'/')) then
    path:=path+'\';
  FixPath(path);
  TryAdd('button');
  TryAdd('checkbox');
  TryAdd('downbutton');
  TryAdd('edit');
  TryAdd('list');
  TryAdd('panel');
  TryAdd('scrollbar');
  TryAdd('tab');
  TryAdd('titlebar');
  TryAdd('window');
end;

{ TGLUIContainer }

procedure TGLUI.DrawElement(e: TUIElement);
begin
  if not e.Visible then
    if (not nxUIEditorMode) or (eType=uiMainPanel) then exit;
  case e.eType of
    uiImage: DrawImage(TUIImage(e));
    uiPanel,uiMainPanel: DrawPanel(TUIPanel(e));
    uiButton: DrawButton(TUIButton(e));
    uiLabel: DrawLabel(TUILabel(e));
    uiEdit: DrawEdit(TUIEdit(e));
    uiList: DrawList(TUIList(e));
    uiDropDown: DrawDropDown(TUIDropDown(e));
    uiMemo: DrawMemo(TUIMemo(e));
    uiScrollBar: DrawScrollBar(TUIScrollBar(e));
    uiCheckBox: DrawCheckBox(TUICheckBox(e));
    uiTabControl: DrawTabControl(TUITabControl(e));
    uiWindow: DrawWindow(TUIWindow(e));
  end;
  if e is TUIContainer then DrawSubElements(TUIContainer(e));
  if assigned(e.onAfterDraw) then e.onAfterDraw(e);
end;

procedure TGLUI.DrawSubElements(e: TUIContainer);
var i: integer;
begin
  for i:=0 to e.Count-1 do DrawElement(e.element[i]);
end;

procedure TGLUI.DrawButton(e: TUIButton);
var r: TBoundsRect; d: single;
begin
  with e do begin
    r:=GetDrawRect;
    if texture>=0 then begin
      tex.SetTex(texture);
      if focused then begin
        d:=sin(nx.FrameTick*toRad*0.2)*0.1+1.1;
        with Color do glColor4f(r*d,g*d,b*(2-d),DrawAlpha*d);
      end else begin
        with Color do glColor4f(r,g,b,DrawAlpha);
      end;
      if assigned(onBeforeDraw) then onBeforeDraw(e);
      if down then UI_Draw(r,1,drawStyle,bdSize)
      else UI_Draw(r,0,drawStyle,bdSize);
    end;
    if (nx.FontCount>0) and (caption<>'') then begin
      nx.SetFont(font);
      with nx.Font[font].color do glColor4f(r,g,b,a*DrawAlpha);
      nx.Font[font].DrawC(r.x+r.w/2,r.y+r.h/2-1,Caption);
    end;
  end;
end;

procedure TGLUI.DrawCheckBox(e: TUICheckBox);
var r: TBoundsRect;
begin
  with e do begin
    r:=GetDrawRect; r.w:=r.h;
    tex.SetTex(texture);
    with Color do glColor4f(r,g,b,DrawAlpha);
    if assigned(onBeforeDraw) then onBeforeDraw(self);
    // Draw checkbox
    if checked then UI_Draw(r,0,drawStyle,bdSize)
    else UI_Draw(r,1,drawStyle,bdSize);
    if (nx.FontCount>0) and (caption<>'') then begin
      nx.SetFont(font);
      // Draw text
      with nx.Font[font].color do glColor4f(r,g,b,a*DrawAlpha);
      nx.Font[font].Draw(r.x+r.w+4,r.y+r.h/2-nx.Font[font].CenterH,caption);
    end;
  end;
end;

procedure TGLUI.DrawDropDown(e: TUIDropDown);
begin
  with e do begin
    if assigned(onBeforeDraw) then onBeforeDraw(self);
    if edit.Visible then DrawEdit(edit);
    if button.Visible then DrawButton(button);
    if list.Visible then DrawList(list);
  end;
end;

procedure TGLUI.DrawEdit(e: TUIEdit);
var r: TBoundsRect; d: integer; a: single;
begin
  with e do begin
    r:=GetDrawRect;
    tex.SetTex(texture);
    with Color do glColor4f(r,g,b,DrawAlpha);
    if assigned(onBeforeDraw) then onBeforeDraw(self);
    // Draw background
    if texture>=0 then UI_Draw(r,0,drawStyle,bdSize);
    if nx.FontCount>0 then begin
      if text<>'' then begin
        // Set font and color
        nx.SetFont(font);
        with nx.Font[font].color do glColor4f(r,g,b,a*DrawAlpha);
        // Caret pos
        d:=nx.Font[font].TextW(SysToUTF8(copy(UTF8ToSys(text),1,caretPos)));
        // Draw text
        if centered then
          nx.Font[font].DrawC(r.x+r.w/2, r.y+r.h/2-1, text)
        else nx.Font[font].Draw(r.x+5, r.y+r.h/2-nx.Font[font].CenterH, text);
      end else
        d:=0; // Reset caret
      // Draw caret
      if focused and (not nxUIEditorMode) then begin
        tex.Disable;
        glBegin(GL_LINES);
        a:=sin(nx.FrameTick*toRad*0.25)*0.5+0.5;
        if centered then d:=d-5+(r.w-nx.Font[font].TextW(text)) div 2;
        glColor4f(1-a,1-a,1-a, DrawAlpha);
        glVertex2f(r.x+d+5.5, r.y+r.h*0.2);
        glColor4f(a*0.5,a,a, DrawAlpha);
        glVertex2f(r.x+d+5.5, r.y+r.h*0.8);
        glEnd;
        tex.Enable;
      end;
    end;
  end;
end;

procedure TGLUI.DrawImage(e: TUIImage);
begin
  with e do begin
    tex.SetTex(texture);
    with Color do glColor4f(r,g,b,DrawAlpha);
    if assigned(onBeforeDraw) then onBeforeDraw(self);
    // Draw background
    UI_Draw(GetDrawRect,pattern,drawStyle,bdSize);
  end;
end;

procedure TGLUI.DrawLabel(e: TUILabel);
var r: TBoundsRect;
begin
  with e do begin
    r:=GetDrawRect;
    if nx.FontCount>0 then begin
      nx.SetFont(font);
      with nx.Font[font].color do glColor4f(r,g,b,a*DrawAlpha);
      // Draw text
      if centered then
        nx.Font[font].DrawC(r.x+r.w/2,r.y+r.h/2-1,Caption)
      else
        nx.Font[font].Draw(r.x,r.y+r.h/2-nx.Font[font].CenterH,Caption);
    end;
  end;
end;

procedure TGLUI.DrawList(e: TUIList);
var r: TBoundsRect; i,maxW,loopMax: integer; d: single;
begin
  with e do begin
    if (nx.frames=1) or (bar=nil) then UpdateSize;
    tex.SetTex(texture);
    with Color do glColor4f(r,g,b,DrawAlpha);
    r:=GetDrawRect;
    if assigned(onBeforeDraw) then onBeforeDraw(self);
    // Draw background
    if texture>=0 then UI_Draw(r,0,drawStyle,bdSize);
    // Draw items
    if nx.FontCount>0 then begin
      maxW:=r.w-4; loopMax:=min(items.Count-1,bar.position+visibleItems-1);
      if (ItemIndex>=bar.position) and (ItemIndex<=loopMax) then begin
        // Draw selection rectangle
        tex.Disable;
        i:=r.y+2+(ItemIndex-bar.position)*nx.Font[font].height;
        d:=sin(nx.FrameTick*toRad*0.25)*0.5+0.5;
        glBegin(GL_QUADS);
          with SelColor do
            if focused then glColor4f(r,g,b,a*DrawAlpha*d)
            else glColor4f(r,g,b,a*DrawAlpha);
          glVertex2f(r.x+2,i);
          glVertex2f(r.x+2,i+nx.Font[font].height);
          if focused then
            with SelColor do
              glColor4f(r,g,b,a*DrawAlpha*(1-d));
          glVertex2f(r.x+r.w-2,i+nx.Font[font].height);
          glVertex2f(r.x+r.w-2,i);
        glEnd;
        tex.Enable;
      end;
      // Draw texts
      nx.SetFont(font);
      with nx.Font[font].color do glColor4f(r,g,b,a*DrawAlpha);
      if bar.Visible then maxW:=maxW-bar.DrawW;
      for i:=bar.position to loopMax do
        if centered then
          nx.Font[font].DrawC(r.x+r.w/2, r.y+1+nx.Font[font].CenterH +
            nx.Font[font].height*(i-bar.position), items[i], maxW)
        else
          nx.Font[font].Draw(r.x+2, r.y+1 +
            nx.Font[font].height*(i-bar.position), items[i], maxW);
    end;
    if bar.Visible then DrawScrollBar(bar);
  end;
end;

procedure TGLUI.DrawMemo(e: TUIMemo);
var r,r2: TBoundsRect;
begin
  with e do begin
    tex.SetTex(texture);
    with Color do glColor4f(r,g,b,DrawAlpha);
    r:=GetDrawRect;
    if assigned(onBeforeDraw) then onBeforeDraw(self);
    // Draw background
    if texture>=0 then UI_Draw(r,0,drawStyle,bdSize);
    r2.x:=r.x+3; r2.y:=r.y+1;
    r2.w:=r.w-4; r2.h:=r.h-3;
    barHorizontal.Visible:=not wrap;
    if barHorizontal.Visible then r2.h:=r2.h-round(barHorizontal.height);
    if barVertical.Visible then r2.w:=r2.w-round(barVertical.width);
    if nx.FontCount>0 then begin
      nx.SetFont(font);
      with nx.Font[font].color do glColor4f(r,g,b,a*DrawAlpha);
      // Draw text
      nx.Font[font].DrawTextArea(r2,strings,barHorizontal.position,barVertical.position);
    end;
    if Focused and (not nxUIEditorMode) then begin
      // Draw caret

    end;
    if barHorizontal.Visible then DrawScrollBar(barHorizontal);
    if barVertical.Visible then DrawScrollBar(barVertical);
  end;
end;

procedure TGLUI.DrawPanel(e: TUIPanel);
var da: single;
begin
  with e do begin
    if eType=uiMainPanel then begin
      width:=nx.Width; height:=nx.Height;
    end;
    da:=DrawAlpha;
    with Color do glColor4f(r,g,b,da);
    tex.SetTex(texture);
    if assigned(onBeforeDraw) then onBeforeDraw(self);
    // Draw background
    if (texture>-1) and (da>0.01) then UI_Draw(GetDrawRect,0,drawStyle,bdSize);
  end;
end;

procedure TGLUI.DrawScrollBar(e: TUIScrollBar);
var r: TBoundsRect;
begin
  with e do begin
    if focus>0 then begin
      if nx.FrameTick>=ScrollDelay then begin
        ScrollDelay:=nx.FrameTick+50;
        if focus=1 then position:=math.max(0,position-1)
        else if focus=2 then position:=math.min(max,position+1);
      end;
    end;
    if nx.frames=1 then UpdateSize;
    r:=GetDrawRect; tex.SetTex(texture);
    if assigned(onBeforeDraw) then begin
      with Color do glColor4f(r,g,b,DrawAlpha);
      onBeforeDraw(self);
    end;
    // Darker background bar
    with Color do glColor4f(r*0.5,g*0.5,b*0.5,DrawAlpha);
    if vertical then nx.DrawScaled(r.x,r.y, r.w,r.h, 3)
    else nx.DrawScaled(r.x,r.y, r.w,r.h, 2);

    // Prev and next buttons
    with Color do glColor4f(r,g,b,DrawAlpha);
    with prev do
      if not down then UI_Draw(GetDrawRect,0,drawStyle,bdSize)
      else UI_Draw(GetDrawRect,1,drawStyle,bdSize);
    with next do
      if not down then UI_Draw(GetDrawRect,0,drawStyle,bdSize)
      else UI_Draw(GetDrawRect,1,drawStyle,bdSize);

    if vertical then begin // Vertical
      nx.DrawScaled(r.x,r.y, r.w,r.w, 4); // Up arrow
      nx.DrawScaled(r.x,r.y+next.y, r.w,r.w, 5); // Down arrow
      // Slider
      if max>0 then
        if focus<>3 then
          nx.DrawScaled(r.x,r.y+r.w+(r.h-r.w*3)/max*position, r.w,r.w, 0)
        else
          nx.DrawScaled(r.x,r.y+r.w+(r.h-r.w*3)/max*position, r.w,r.w, 1);
    end else begin // Horizontal
      nx.DrawScaled(r.x,r.y, r.h,r.h, 6); // Left arrow
      nx.DrawScaled(r.x+next.x,r.y, r.h,r.h, 7); // Right arrow
      // Slider
      if max>0 then
        if focus<>3 then
          nx.DrawScaled(r.x+r.h+(r.w-r.h*3)/max*position,r.y, r.h,r.h, 0)
        else
          nx.DrawScaled(r.x+r.h+(r.w-r.h*3)/max*position,r.y, r.h,r.h, 1);
    end;
  end;
end;

procedure TGLUI.DrawTab(e: TUITab);
begin
  with e do begin
    tex.SetTex(texture);
    with Color do glColor4f(r,g,b,DrawAlpha);
    if assigned(onBeforeDraw) then onBeforeDraw(self);
    // Draw background
    UI_Draw(GetDrawRect,0,drawStyle,bdSize);
    // Draw tab components
    DrawSubElements(e);
  end;
end;

procedure TGLUI.DrawTabControl(e: TUITabControl);
var tw: integer; r: TBoundsRect; c1,c2: TfRGBA;

  procedure _DrawTab(i: integer; c: pointer);
  begin
    with e do begin
      tex.SetTex(texture); glColor4fv(c);
      UI_Draw(bRect(tab[i].posX,r.y,tw,tabHeight),0,drawStyle,bdSize);
      if nx.FontCount>0 then begin
        nx.SetFont(font);
        with nx.Font[font].color do glColor4f(r,g,b,a*c1.a);
        nx.Font[font].DrawC(tab[i].posX+tw/2,r.y+tabHeight/2-1,tab[i].caption);
      end;
    end;
  end;

var i: integer;
begin
  with e do begin
    tex.SetTex(texture);
    with Color do c1:=fRGBA(r,g,b,DrawAlpha);
    glColor4fv(@c1);
    if assigned(onBeforeDraw) then onBeforeDraw(self);
    if (tabHeight>0) and (count>0) then begin
      r:=GetDrawRect; tw:=min(tabWidth,r.w div min(count,3));
      with Color do c2:=fRGBA(r*0.9,g*0.9,b*0.9,c1.a);
      for i:=0 to tabIndex-1 do
        if tab[i].Visible then _DrawTab(i,@c2);
      for i:=count-1 downto tabIndex+1 do
        if tab[i].Visible then _DrawTab(i,@c2);
      _DrawTab(tabIndex,@c1);
    end;
    if current.Visible then DrawTab(Current);
  end;
end;

procedure TGLUI.DrawWindow(e: TUIWindow);
var r: TBoundsRect; da: single;
begin
  with e do begin
    da:=DrawAlpha;
    with Color do glColor4f(r,g,b,da);
    if assigned(onBeforeDraw) then onBeforeDraw(self);
    r:=GetDrawRect;
    tex.SetTex(texture);
    // Draw background
    UI_Draw(bRect(r.x,r.y+titleHeight,r.w,r.h-titleHeight),0,drawStyle,bdSize);
    if titleHeight>0  then begin
      if TitleTex>-1 then begin
        tex.SetTex(titleTex);
        if not Focused then
          with Color do glColor4f(r*0.8,g*0.8,b*0.8,da);
        // Draw titlebar
        UI_Draw(bRect(r.x,r.y,r.w,titleHeight),0,drawStyle,bdSize);
      end;
      if (nx.FontCount>0) and (Caption<>'') then begin
        nx.SetFont(font);
        with nx.Font[font].color do glColor4f(r,g,b,a*da);
        // Draw caption text
        nx.Font[font].DrawC(r.x+r.w/2,r.y+TitleHeight/2-1,Caption);
      end;
    end;
  end;
end;

end.
