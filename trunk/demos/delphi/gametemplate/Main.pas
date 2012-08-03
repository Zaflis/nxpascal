unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, nxGL, nxTypes, GraphicsUnit, AppEvnts;

type
  TForm1 = class(TForm)
    AppEvents: TApplicationEvents;
    procedure AppPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    game: TGraphicalGame;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize game window
  clientWidth:=800; clientHeight:=600;

  cursor:=crNone; // Hide cursor
  // (Do it before CreateGlWindow,
  // if afterwards, you need to set it manually to nx.window)

  if not nx.CreateGlWindow(self) then begin
    showmessage('Failed to initialize OpenGL!'); exit;
  end;

  // Create game
  game:=TGraphicalGame.Create;
  if not game.Initialized then begin
    FreeAndNil(game);
    //showmessage('Game failed to initialize!');
    exit;
  end;
end;

procedure TForm1.AppPropertiesIdle(Sender: TObject; var Done: Boolean);
begin
  if game<>nil then begin
    Done:=false;
    game.Idle;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if game<>nil then FreeAndNil(game);
  nx.KillGLWindow;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if game<>nil then begin
    game.KeyDown(key, shift);

    // KeyDown events
    if key=VK_ESCAPE then begin
      Close;
    end;
  end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if game<>nil then begin
    game.KeyUp(key, shift);
    // KeyUp events

  end;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if game<>nil then begin
    game.MouseDown(button, shift);
    // MouseDown events

  end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if game<>nil then begin
    game.MouseMove(x, y, shift);
    // MouseMove events

  end;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if game<>nil then begin
    game.MouseUp(button, shift);
    // MouseUp events

  end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if game<>nil then begin
    // Mousewheel events

  end;
end;

end.
