object MainFrm: TMainFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Online logger'
  ClientHeight = 495
  ClientWidth = 709
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
    Left = 24
    Top = 8
    Width = 53
    Height = 16
    Caption = 'Com port'
  end
  object Label2: TLabel
    Left = 24
    Top = 38
    Width = 89
    Height = 16
    Caption = 'Google client ID'
  end
  object Label3: TLabel
    Left = 24
    Top = 68
    Width = 112
    Height = 16
    Caption = 'Google client secret'
  end
  object Memo1: TMemo
    Left = 8
    Top = 176
    Width = 689
    Height = 305
    TabOrder = 0
  end
  object BitBtn2: TBitBtn
    Left = 38
    Top = 145
    Width = 75
    Height = 25
    Caption = 'sender'
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object Button1: TButton
    Left = 134
    Top = 145
    Width = 75
    Height = 25
    Caption = 'exec'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 512
    Top = 137
    Width = 137
    Height = 25
    Caption = 'connector stat'
    TabOrder = 3
    OnClick = Button2Click
  end
  object edComPort: TEdit
    Left = 104
    Top = 5
    Width = 121
    Height = 24
    NumbersOnly = True
    TabOrder = 4
  end
  object edClientID: TEdit
    Left = 144
    Top = 35
    Width = 557
    Height = 24
    TabOrder = 5
  end
  object edClientSecret: TEdit
    Left = 144
    Top = 65
    Width = 557
    Height = 24
    TabOrder = 6
  end
  object btSave: TButton
    Left = 486
    Top = 95
    Width = 107
    Height = 25
    Caption = 'Save config'
    TabOrder = 7
    OnClick = btSaveClick
  end
  object btReloadServer: TButton
    Left = 24
    Top = 95
    Width = 107
    Height = 25
    Caption = 'Reload server'
    TabOrder = 8
    OnClick = btReloadServerClick
  end
end
