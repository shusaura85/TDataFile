object frmDFkey: TfrmDFkey
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Access key for DataFile'
  ClientHeight = 240
  ClientWidth = 600
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  Font.Quality = fqClearTypeNatural
  OldCreateOrder = False
  DesignSize = (
    600
    240)
  PixelsPerInch = 96
  TextHeight = 17
  object Label1: TLabel
    Left = 20
    Top = 8
    Width = 51
    Height = 17
    Caption = 'Data file:'
  end
  object Label2: TLabel
    Left = 20
    Top = 80
    Width = 75
    Height = 17
    Caption = 'Data file key:'
  end
  object lbl_InfoOpen: TLabel
    Left = 24
    Top = 144
    Width = 258
    Height = 17
    Caption = 'Leave empty if the data file is not protected!'
    Visible = False
  end
  object lbl_InfoNew: TLabel
    Left = 24
    Top = 144
    Width = 322
    Height = 17
    Caption = 'Leave empty if you do not want to protect the data file!'
    Visible = False
  end
  object edit_filename: TEdit
    Left = 20
    Top = 31
    Width = 560
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 0
  end
  object edit_Password: TEdit
    Left = 20
    Top = 103
    Width = 560
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 1
  end
  object Button1: TButton
    Left = 456
    Top = 180
    Width = 124
    Height = 40
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
