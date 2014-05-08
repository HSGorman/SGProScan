object Form1: TForm1
  Left = 453
  Top = 223
  Width = 731
  Height = 465
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 32
    Top = 88
    Width = 649
    Height = 321
    AutoSize = True
  end
  object btnSelectSource: TButton
    Left = 0
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select Source'
    TabOrder = 0
    OnClick = btnSelectSourceClick
  end
  object btnScan: TButton
    Left = 0
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Single Scan'
    TabOrder = 1
    OnClick = btnScanClick
  end
  object SGProScan1: TSGProScan
    PixelType = ptBlackAndwhite
    RequestPixelType = False
    BitDepth = 0
    OnErrorEvent = SGProScan1ErrorEvent
    Left = 240
    Top = 48
  end
end
