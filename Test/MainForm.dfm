object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'CO2 datalogger test program'
  ClientHeight = 686
  ClientWidth = 999
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 152
    Top = 8
    Width = 54
    Height = 16
    Caption = 'COM port'
  end
  object Memo1: TMemo
    Left = 8
    Top = 280
    Width = 983
    Height = 398
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 40
    Width = 201
    Height = 25
    Caption = 'Get info'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 71
    Width = 201
    Height = 25
    Caption = 'Get memjry statistic'
    TabOrder = 2
  end
  object Button3: TButton
    Left = 8
    Top = 102
    Width = 201
    Height = 25
    Caption = 'Get logged data '
    TabOrder = 3
  end
  object Button4: TButton
    Left = 8
    Top = 249
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Edit1: TEdit
    Left = 416
    Top = 71
    Width = 121
    Height = 24
    TabOrder = 5
    Text = '11111111'
  end
  object Button5: TButton
    Left = 560
    Top = 40
    Width = 193
    Height = 25
    Caption = 'Set date and time'
    TabOrder = 6
  end
  object Button6: TButton
    Left = 560
    Top = 71
    Width = 193
    Height = 25
    Caption = 'Set identifier'
    TabOrder = 7
  end
  object Edit2: TEdit
    Left = 416
    Top = 104
    Width = 121
    Height = 24
    TabOrder = 8
    Text = '10'
  end
  object Button7: TButton
    Left = 560
    Top = 102
    Width = 193
    Height = 25
    Caption = 'Set sampling rate'
    TabOrder = 9
  end
  object Edit3: TEdit
    Left = 224
    Top = 8
    Width = 121
    Height = 24
    TabOrder = 10
    Text = '10'
  end
  object Button8: TButton
    Left = 360
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Create class'
    TabOrder = 11
    OnClick = Button8Click
  end
end
