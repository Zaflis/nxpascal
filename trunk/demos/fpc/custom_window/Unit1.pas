unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, ExtCtrls, OpenGLContext, dglOpenGL, nxGL, nxTypes;

const
  USE_CUSTOM_WINDOW = true;

// Set {$define NX_CUSTOM_WINDOW} in nxGL.pas beginning
// if you don't include LazOpenGLContext library

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    window: TOpenGLControl;
    function CreateWindow: boolean;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

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
  nx.SetView(0, 0, window.Width, window.Height);
  nx.Perspective(nx.PerspectiveStretch);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  nx.Clear(true,false);
  glLoadIdentity;
  glTranslatef(0, 0, -4);

  // Draw yellow rotating rectangle
  nx.SetColor(1, 1, 0);
  glrotatef(nx.GetTick(360,0.05), 0, 1, 0);
  nx.RectT(-1, 1, 1, -1);

  window.SwapBuffers;
end;

function TForm1.CreateWindow: boolean;
begin
  result:=false;
  try
    if USE_CUSTOM_WINDOW then begin
      InitOpenGL; // dglOpenGL

      // Custom window
      window:=TOpenGLControl.Create(self);
      window.Parent:=self;
      window.Align:=alClient;
      window.MakeCurrent(false);

      nx.window:=window; // Comment this only if NX_CUSTOM_WINDOW is set

      // dglOpenGL
      ReadExtensions;
      ReadImplementationProperties;

      nxInitGL;

      result:=true;

    // Comment these 5 lines if Custom window is used
    {end else begin
      result:=nx.CreateGlWindow(self);
      if result then begin
        window:=nx.window;
      end;}

    end;
  except
    caption:='Error creating window!';
  end;
end;

end.

