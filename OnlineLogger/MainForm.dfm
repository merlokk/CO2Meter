object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Online logger'
  ClientHeight = 468
  ClientWidth = 740
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
  object Memo1: TMemo
    Left = 8
    Top = 72
    Width = 689
    Height = 305
    TabOrder = 0
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 16
    Width = 75
    Height = 25
    Caption = 'sender'
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object Button1: TButton
    Left = 104
    Top = 16
    Width = 75
    Height = 25
    Caption = 'exec'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 595
    Top = 8
    Width = 137
    Height = 25
    Caption = 'connector stat'
    TabOrder = 3
    OnClick = Button2Click
  end
end
