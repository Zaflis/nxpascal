unit Unit1;

interface

uses
  Windows, Controls, Forms, Classes, ExtCtrls, dglOpenGL, nxGL, nxTypes;

const
  USE_CUSTOM_WINDOW = true;
// Should set {$define NX_CUSTOM_WINDOW} in nxGL.pas beginning

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    function CreateWindow: boolean;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

function TForm1.CreateWindow: boolean;
var pfd: TPIXELFORMATDESCRIPTOR; pf: integer;
begin
  result:=false;
  try
    if USE_CUSTOM_WINDOW then begin
      InitOpenGL; // dglOpenGL

      // Custom window
      with nx do begin
        nxHWND:=self;
        nxDC:=GetDC(Handle);
        if nxDC=0 then exit;

        zeromemory(@pfd,sizeof(pfd));
        pfd.nSize:=sizeof(pfd); pfd.nVersion:=1;
        pfd.dwFlags:=PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
        pfd.iPixelType:=PFD_TYPE_RGBA;
        pfd.cColorBits:=24; pfd.cDepthBits:=16;
        pfd.iLayerType:=PFD_MAIN_PLANE;

        pf:=ChoosePixelFormat(nxDC, @pfd);
        if pf=0 then exit;
        SetPixelFormat(nxDC, pf, @pfd);

        nxRC:=wglCreateContext(nxDC);
        if nxRC=0 then exit;
        wglMakeCurrent(nxDC, nxRC);
      end;

      // dglOpenGL
      ReadExtensions;
      ReadImplementationProperties;

      nxInitGL;

      result:=true;

    // Comment these 2 lines if Custom window is used
    {end else begin
      result:=nx.CreateGlWindow(self);}

    end;
  except
    caption:='Error creating window!';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  clientwidth:=800; clientheight:=600;
  if CreateWindow then begin
    tex.Disable; // Disable textures
    nx.rs.CullBack:=false; // Make faces both sides visible

    timer1.Enabled:=true;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  nx.SetView(0, 0, ClientWidth, ClientHeight);
  nx.Perspective(nx.PerspectiveStretch);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  nx.Clear(true,false);
  glLoadIdentity;
  glTranslatef(0, 0, -4);

  // Draw yellow rotating rectangle
  nx.SetColor(1, 1, 0);
  glrotatef(nx.GetTick2(360,0.05), 0, 1, 0);
  nx.RectT(-1, 1, 1, -1);

  //window.SwapBuffers;
  SwapBuffers(nx.nxDC);
end;

end.
 