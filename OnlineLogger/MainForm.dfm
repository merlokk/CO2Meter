object MainFrm: TMainFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Online logger'
  ClientHeight = 517
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
    Left = 17
    Top = 8
    Width = 53
    Height = 16
    Caption = 'Com port'
  end
  object Label2: TLabel
    Left = 17
    Top = 38
    Width = 89
    Height = 16
    Caption = 'Google client ID'
  end
  object Label3: TLabel
    Left = 17
    Top = 68
    Width = 112
    Height = 16
    Caption = 'Google client secret'
  end
  object lbStatus: TLabel
    Left = 16
    Top = 128
    Width = 681
    Height = 35
    AutoSize = False
  end
  object Memo1: TMemo
    Left = 8
    Top = 200
    Width = 689
    Height = 305
    TabOrder = 0
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 169
    Width = 75
    Height = 25
    Caption = 'sender'
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object Button1: TButton
    Left = 89
    Top = 169
    Width = 75
    Height = 25
    Caption = 'exec'
    TabOrder = 2
    OnClick = Button1Click
  end
  object edComPort: TEdit
    Left = 97
    Top = 5
    Width = 121
    Height = 24
    NumbersOnly = True
    TabOrder = 3
  end
  object edClientID: TEdit
    Left = 137
    Top = 35
    Width = 564
    Height = 24
    TabOrder = 4
  end
  object edClientSecret: TEdit
    Left = 137
    Top = 65
    Width = 564
    Height = 24
    TabOrder = 5
  end
  object btSave: TButton
    Left = 594
    Top = 95
    Width = 107
    Height = 25
    Caption = 'Save config'
    TabOrder = 6
    OnClick = btSaveClick
  end
  object btReloadServer: TButton
    Left = 8
    Top = 90
    Width = 107
    Height = 25
    Caption = 'Reload server'
    TabOrder = 7
    OnClick = btReloadServerClick
  end
  object cbExecute: TCheckBox
    Left = 594
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Running...'
    TabOrder = 8
  end
  object Timer1: TTimer
    Interval = 10000
    OnTimer = Timer1Timer
    Left = 656
    Top = 8
  end
end
