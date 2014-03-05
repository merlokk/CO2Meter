object fMain: TfMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'AZ 7798 get data tool'
  ClientHeight = 250
  ClientWidth = 474
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
    Left = 8
    Top = 11
    Width = 54
    Height = 16
    Caption = 'COM Port'
  end
  object Label2: TLabel
    Left = 8
    Top = 41
    Width = 56
    Height = 16
    Caption = 'File name'
  end
  object lbInfo: TLabel
    Left = 8
    Top = 88
    Width = 449
    Height = 113
    Alignment = taCenter
    AutoSize = False
  end
  object edCOM: TEdit
    Left = 80
    Top = 8
    Width = 121
    Height = 24
    TabOrder = 0
    Text = '10'
  end
  object btCheckMem: TButton
    Left = 104
    Top = 217
    Width = 115
    Height = 25
    Caption = 'Check memory'
    TabOrder = 1
    OnClick = btCheckMemClick
  end
  object btGetMem: TButton
    Left = 225
    Top = 217
    Width = 115
    Height = 25
    Caption = 'Get memory'
    TabOrder = 2
    OnClick = btGetMemClick
  end
  object btClose: TButton
    Left = 346
    Top = 217
    Width = 115
    Height = 25
    Caption = 'Close'
    TabOrder = 3
    OnClick = btCloseClick
  end
  object edFileName: TEdit
    Left = 80
    Top = 38
    Width = 353
    Height = 24
    TabOrder = 4
    Text = 'data.csv'
  end
  object btFile: TButton
    Left = 439
    Top = 38
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 5
    OnClick = btFileClick
  end
  object dgSave: TSaveDialog
    DefaultExt = 'csv'
    FileName = 'data.csv'
    Filter = 'csv|*.csv|all files|*.*'
    InitialDir = '.'
    Left = 392
    Top = 32
  end
end
