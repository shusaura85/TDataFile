object frmRename: TfrmRename
  Left = 0
  Top = 0
  ActiveControl = edit_NewName
  Caption = 'Rename key'
  ClientHeight = 249
  ClientWidth = 705
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  Font.Quality = fqClearTypeNatural
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  DesignSize = (
    705
    249)
  PixelsPerInch = 96
  TextHeight = 17
  object Label6: TLabel
    Left = 15
    Top = 26
    Width = 99
    Height = 17
    Alignment = taRightJustify
    Caption = 'Current key path:'
  end
  object Label7: TLabel
    Left = 8
    Top = 57
    Width = 105
    Height = 17
    Alignment = taRightJustify
    Caption = 'Current key name:'
  end
  object Shape1: TShape
    Left = 8
    Top = 96
    Width = 689
    Height = 5
    Brush.Style = bsClear
  end
  object Label1: TLabel
    Left = 32
    Top = 130
    Width = 82
    Height = 17
    Alignment = taRightJustify
    Caption = 'New key path:'
  end
  object Label2: TLabel
    Left = 25
    Top = 161
    Width = 88
    Height = 17
    Alignment = taRightJustify
    Caption = 'New key name:'
  end
  object edit_OldPath: TEdit
    Left = 120
    Top = 23
    Width = 561
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    ReadOnly = True
    TabOrder = 0
  end
  object edit_OldName: TEdit
    Left = 119
    Top = 54
    Width = 561
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    ReadOnly = True
    TabOrder = 1
  end
  object edit_NewPath: TEdit
    Left = 120
    Top = 127
    Width = 561
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object edit_NewName: TEdit
    Left = 120
    Top = 158
    Width = 561
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 0
    Top = 208
    Width = 705
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    ExplicitLeft = -66
    ExplicitTop = 258
    ExplicitWidth = 771
    DesignSize = (
      705
      41)
    object btn_Save: TButton
      Left = 621
      Top = 3
      Width = 75
      Height = 30
      Anchors = [akTop, akRight]
      Caption = 'Save'
      Default = True
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 687
    end
    object Button2: TButton
      Left = 5
      Top = 3
      Width = 75
      Height = 30
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
