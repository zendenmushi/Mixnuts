object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 172
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 43
    Top = 24
    Width = 152
    Height = 13
    Caption = 'Enter NET Core Runtimes folder'
  end
  object edtRuntimePath: TEdit
    Left = 40
    Top = 40
    Width = 465
    Height = 21
    Enabled = False
    TabOrder = 0
  end
  object Button1: TButton
    Left = 511
    Top = 38
    Width = 75
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 43
    Top = 112
    Width = 543
    Height = 25
    Caption = 'call   netcore_client.dll'
    TabOrder = 2
    OnClick = Button2Click
  end
  object FileOpenDialog1: TFileOpenDialog
    DefaultFolder = 'C:\Program Files\dotnet\shared\Microsoft.NETCore.App'
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders]
    Left = 248
    Top = 8
  end
end
