object Form1: TForm1
  Left = 215
  Top = 124
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Effects demo 1'
  ClientHeight = 210
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseMove = FormMouseMove
  PixelsPerInch = 96
  TextHeight = 13
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationProperties1Idle
    Left = 88
    Top = 8
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 15
    OnTimer = Timer1Timer
    Left = 16
    Top = 8
  end
  object MusicTimer: TTimer
    Interval = 500
    OnTimer = MusicTimerTimer
    Left = 16
    Top = 72
  end
end
