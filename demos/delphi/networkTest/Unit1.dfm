object Form1: TForm1
  Left = 221
  Top = 136
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  ActiveControl = Button1
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Network Test'
  ClientHeight = 242
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 191
    Width = 39
    Height = 13
    Caption = 'Threads'
    Color = clBtnFace
    ParentColor = False
  end
  object Label2: TLabel
    Left = 232
    Top = 187
    Width = 77
    Height = 13
    Caption = 'Port 5400  Host:'
    Color = clBtnFace
    ParentColor = False
  end
  object Label3: TLabel
    Left = 94
    Top = 211
    Width = 74
    Height = 13
    Caption = 'Now using TCP'
    Color = clBtnFace
    ParentColor = False
  end
  object Label4: TLabel
    Left = 280
    Top = 216
    Width = 30
    Height = 13
    Caption = 'My IP:'
    Color = clBtnFace
    ParentColor = False
  end
  object GroupBox1: TGroupBox
    Left = 6
    Top = 10
    Width = 210
    Height = 166
    Caption = 'Server'
    TabOrder = 0
    object Shape1: TShape
      Left = 112
      Top = 22
      Width = 13
      Height = 13
      Brush.Color = clGray
      Shape = stCircle
    end
    object Memo1: TMemo
      Left = 8
      Top = 46
      Width = 188
      Height = 113
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object Button1: TButton
      Left = 8
      Top = 16
      Width = 48
      Height = 25
      Caption = 'Start'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 56
      Top = 16
      Width = 48
      Height = 25
      Caption = 'Stop'
      TabOrder = 2
      OnClick = Button2Click
    end
    object serverTest1: TButton
      Left = 136
      Top = 16
      Width = 32
      Height = 25
      Caption = 'test'
      TabOrder = 3
      OnClick = serverTest1Click
    end
    object ServerTest2: TButton
      Left = 172
      Top = 16
      Width = 24
      Height = 25
      Caption = 'R'
      TabOrder = 4
      OnClick = ServerTest2Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 224
    Top = 10
    Width = 210
    Height = 166
    Caption = 'Client'
    TabOrder = 1
    object Shape2: TShape
      Left = 110
      Top = 22
      Width = 13
      Height = 13
      Brush.Color = clGray
      Shape = stCircle
    end
    object Memo2: TMemo
      Left = 8
      Top = 46
      Width = 188
      Height = 113
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object Button3: TButton
      Left = 8
      Top = 16
      Width = 48
      Height = 25
      Caption = 'Start'
      TabOrder = 1
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 56
      Top = 16
      Width = 48
      Height = 25
      Caption = 'Stop'
      TabOrder = 2
      OnClick = Button4Click
    end
    object ClientTest1: TButton
      Left = 134
      Top = 16
      Width = 32
      Height = 25
      Caption = 'test'
      TabOrder = 3
      OnClick = ClientTest1Click
    end
    object ClientTest2: TButton
      Left = 174
      Top = 16
      Width = 22
      Height = 25
      Caption = 'R'
      TabOrder = 4
      OnClick = ClientTest2Click
    end
  end
  object Edit1: TEdit
    Left = 318
    Top = 184
    Width = 112
    Height = 21
    TabOrder = 2
    Text = '127.0.0.1'
  end
  object Button5: TButton
    Left = 8
    Top = 207
    Width = 83
    Height = 25
    Caption = 'Change to UDP'
    TabOrder = 3
    OnClick = Button5Click
  end
  object Edit2: TEdit
    Left = 318
    Top = 211
    Width = 112
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 104
    Top = 104
  end
end
