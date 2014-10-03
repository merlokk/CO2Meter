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
  PixelsPerInch = 120
  TextHeight = 16
  object Memo1: TMemo
    Left = 8
    Top = 72
    Width = 689
    Height = 305
    Lines.Strings = (
      'Memo1')
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
end
