unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, nxGL, nxTypes, GraphicsUnit, AppEvnts, nxBass;

type
  TForm1 = class(TForm)
    AppEvents: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure AppPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
  Height:=screen.Height*4 div 5;
  Width:=Height*4 div 3;
  cursor:=crNone; // Hide cursor

  if not nx.CreateGlWindow(self) then begin
    showmessage('Failed to initialize OpenGL!'); 
  end;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  // Moved game creation to onPaint event, so we can do more things
  // such as loading screens and getting accurate information of
  // window itself (used for mouse positioning too).

  // Create game
  if (game=nil) and nx.AllOK then begin
    game:=TGraphicalGame.Create;
    game.sound:=TBassEngine.Create(handle, true);
    with game.sound.Add('laser', 'data\laser.wav', true, 1) do begin
      Set3DAttributes(
      0, 100, 360, 360, 0);
      SetVolume(0.4);
    end;
    with game.sound.Add('bounce', 'data\bounce.wav', true, 8) do begin
      Set3DAttributes(0, 100, 360, 360, 0);
      SetVolume(0.3);
    end;
    if not game.Initialized then begin
      // Failed to initialize game
      FreeAndNil(game);
    end;
    onPaint:=nil; // No need to trigger this event again
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
  if game<>nil then
    with game do begin
      KeyDown(key, shift);
      // KeyDown events
      case key of
        VK_ESCAPE: Close;
      end;
    end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if game<>nil then
    with game do begin
      KeyUp(key, shift);
      // KeyUp events

    end;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if game<>nil then
    with game do begin
      MouseDown(button, shift);
      // MouseDown events

    end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if game<>nil then
    with game do begin
      MouseMove(x, y, shift);
      // MouseMove events

    end;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if game<>nil then
    with game do begin
      MouseUp(button, shift);
      // MouseUp events

    end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if game<>nil then
    with game do begin
      // Mousewheel events

    end;
end;

end.
