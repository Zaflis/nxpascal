object Form1: TForm1
  Left = 203
  Top = 136
  Width = 339
  Height = 263
  Caption = 'Walkers demo - with shaders'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnMouseWheel = FormMouseWheel
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object AppEvents: TApplicationEvents
    OnIdle = AppPropertiesIdle
    Left = 32
    Top = 8
  end
end
