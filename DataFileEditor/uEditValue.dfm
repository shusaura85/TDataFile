object frmEdit: TfrmEdit
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Edit value'
  ClientHeight = 413
  ClientWidth = 771
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  Font.Quality = fqClearTypeNatural
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 17
  object pages: TPageControl
    AlignWithMargins = True
    Left = 5
    Top = 70
    Width = 761
    Height = 297
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = page_Int8
    Align = alClient
    TabOrder = 0
    object page_String: TTabSheet
      Caption = 'Edit string value'
      object edit_String: TMemo
        AlignWithMargins = True
        Left = 20
        Top = 20
        Width = 713
        Height = 225
        Margins.Left = 20
        Margins.Top = 20
        Margins.Right = 20
        Margins.Bottom = 20
        Align = alClient
        Lines.Strings = (
          'edit_String')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object page_Int8: TTabSheet
      Caption = 'Edit 8-bit Integer'
      ImageIndex = 6
      DesignSize = (
        753
        265)
      object Label11: TLabel
        Left = 24
        Top = 144
        Width = 74
        Height = 17
        Caption = 'Hexadecimal'
      end
      object lbl_Int8_outside_range: TLabel
        Left = 24
        Top = 119
        Width = 545
        Height = 17
        AutoSize = False
        Caption = '%error_outside_range%'
      end
      object Label14: TLabel
        Left = 24
        Top = 64
        Width = 46
        Height = 17
        Caption = 'Decimal'
      end
      object Label16: TLabel
        Left = 24
        Top = 10
        Width = 73
        Height = 17
        Caption = 'Integer type:'
      end
      object edit_Int8: TEdit
        Left = 24
        Top = 88
        Width = 701
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnKeyUp = edit_Int8KeyUp
      end
      object edit_Int8_Hex: TEdit
        Left = 24
        Top = 168
        Width = 701
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        CharCase = ecUpperCase
        MaxLength = 2
        TabOrder = 1
        OnKeyUp = edit_Int8_HexKeyUp
      end
      object edit_int8_type: TEdit
        Left = 24
        Top = 33
        Width = 300
        Height = 25
        ReadOnly = True
        TabOrder = 2
        Text = 'Int8 | 8-bit Signed Integer [-128 .. 127]'
      end
    end
    object page_Int16: TTabSheet
      Caption = 'Edit 16-bit Integer'
      ImageIndex = 7
      DesignSize = (
        753
        265)
      object Label12: TLabel
        Left = 24
        Top = 10
        Width = 73
        Height = 17
        Caption = 'Integer type:'
      end
      object Label17: TLabel
        Left = 24
        Top = 64
        Width = 46
        Height = 17
        Caption = 'Decimal'
      end
      object lbl_Int16_outside_range: TLabel
        Left = 24
        Top = 119
        Width = 545
        Height = 17
        AutoSize = False
        Caption = '%error_outside_range%'
      end
      object Label19: TLabel
        Left = 24
        Top = 144
        Width = 74
        Height = 17
        Caption = 'Hexadecimal'
      end
      object edit_int16_type: TEdit
        Left = 24
        Top = 33
        Width = 300
        Height = 25
        ReadOnly = True
        TabOrder = 0
        Text = 'Int16 | 16-bit Signed Integer [-32768 .. 32767]'
      end
      object edit_Int16: TEdit
        Left = 24
        Top = 88
        Width = 701
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnKeyUp = edit_Int16KeyUp
      end
      object edit_Int16_Hex: TEdit
        Left = 24
        Top = 168
        Width = 701
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        CharCase = ecUpperCase
        MaxLength = 4
        TabOrder = 2
        OnKeyUp = edit_Int16_HexKeyUp
      end
    end
    object page_Integer: TTabSheet
      Caption = 'Edit 32-bit Integer'
      ImageIndex = 1
      DesignSize = (
        753
        265)
      object Label1: TLabel
        Left = 24
        Top = 64
        Width = 46
        Height = 17
        Caption = 'Decimal'
      end
      object Label2: TLabel
        Left = 24
        Top = 144
        Width = 74
        Height = 17
        Caption = 'Hexadecimal'
      end
      object lbl_Integer_outside_range: TLabel
        Left = 24
        Top = 119
        Width = 545
        Height = 17
        AutoSize = False
        Caption = '%error_outside_range%'
      end
      object Label8: TLabel
        Left = 24
        Top = 10
        Width = 73
        Height = 17
        Caption = 'Integer type:'
      end
      object edit_Integer: TEdit
        Left = 24
        Top = 88
        Width = 701
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnKeyUp = edit_IntegerKeyUp
      end
      object edit_Integer_Hex: TEdit
        Left = 24
        Top = 168
        Width = 701
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 1
        OnKeyUp = edit_Integer_HexKeyUp
      end
      object edit_int32_type: TEdit
        Left = 24
        Top = 33
        Width = 300
        Height = 25
        ReadOnly = True
        TabOrder = 2
        Text = 'Int32 | 32-bit Signed Integer'
      end
    end
    object page_Int64: TTabSheet
      Caption = 'Edit 64-bit Integer'
      ImageIndex = 3
      DesignSize = (
        753
        265)
      object Label3: TLabel
        Left = 24
        Top = 64
        Width = 46
        Height = 17
        Caption = 'Decimal'
      end
      object lbl_Int64_outside_range: TLabel
        Left = 24
        Top = 119
        Width = 545
        Height = 17
        AutoSize = False
        Caption = '%error_outside_range%'
      end
      object Label5: TLabel
        Left = 24
        Top = 144
        Width = 74
        Height = 17
        Caption = 'Hexadecimal'
      end
      object Label9: TLabel
        Left = 24
        Top = 10
        Width = 73
        Height = 17
        Caption = 'Integer type:'
      end
      object edit_Int64: TEdit
        Left = 24
        Top = 88
        Width = 701
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnKeyUp = edit_Int64KeyUp
      end
      object edit_Int64_Hex: TEdit
        Left = 24
        Top = 168
        Width = 701
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        CharCase = ecUpperCase
        MaxLength = 16
        TabOrder = 1
        OnKeyUp = edit_Int64_HexKeyUp
      end
      object edit_int64_type: TEdit
        Left = 24
        Top = 33
        Width = 300
        Height = 25
        ReadOnly = True
        TabOrder = 2
        Text = 'Int64 | 64-bit Signed Integer'
      end
    end
    object page_Float: TTabSheet
      Caption = 'Edit float'
      ImageIndex = 4
      DesignSize = (
        753
        265)
      object Label4: TLabel
        Left = 24
        Top = 10
        Width = 60
        Height = 17
        Caption = 'Float type:'
      end
      object Label10: TLabel
        Left = 24
        Top = 64
        Width = 92
        Height = 17
        Caption = 'Value (Decimal):'
      end
      object edit_float_type: TEdit
        Left = 24
        Top = 33
        Width = 300
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 0
        Text = 'Double | 64-bit Float'
      end
      object edit_float: TEdit
        Left = 24
        Top = 88
        Width = 701
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
    end
    object page_Calendar: TTabSheet
      Caption = 'Edit date time'
      ImageIndex = 5
      object Label13: TLabel
        Left = 24
        Top = 9
        Width = 30
        Height = 17
        Caption = 'Date:'
      end
      object Label15: TLabel
        Left = 24
        Top = 105
        Width = 31
        Height = 17
        Caption = 'Time:'
      end
      object calendar_time: TDateTimePicker
        Left = 24
        Top = 128
        Width = 201
        Height = 45
        Date = 45040.000000000000000000
        Time = 0.684921574073087000
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -27
        Font.Name = 'Segoe UI'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        Kind = dtkTime
        ParentFont = False
        TabOrder = 0
      end
      object calendar_date: TDateTimePicker
        Left = 24
        Top = 32
        Width = 201
        Height = 45
        Date = 45040.000000000000000000
        Time = 0.684921574073087000
        Checked = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -27
        Font.Name = 'Segoe UI'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        ParentFont = False
        TabOrder = 1
        OnChange = calendar_dateChange
      end
    end
    object page_Stream: TTabSheet
      Caption = 'Edit binary data'
      ImageIndex = 2
      object Panel3: TPanel
        Left = 0
        Top = 232
        Width = 753
        Height = 33
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object chk_hex_AllowInsert: TCheckBox
          Left = 8
          Top = 8
          Width = 233
          Height = 17
          Caption = 'Allow insert and delete'
          TabOrder = 0
          OnClick = chk_hex_AllowInsertClick
        end
        object Button1: TButton
          Left = 639
          Top = 4
          Width = 110
          Height = 25
          Caption = 'Export to file'
          TabOrder = 1
          OnClick = Button1Click
        end
        object Button3: TButton
          Left = 520
          Top = 4
          Width = 110
          Height = 25
          Caption = 'Import from file'
          TabOrder = 2
          OnClick = Button3Click
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 372
    Width = 771
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      771
      41)
    object btn_Save: TButton
      Left = 687
      Top = 3
      Width = 75
      Height = 30
      Anchors = [akTop, akRight]
      Caption = 'Save'
      Default = True
      ModalResult = 1
      TabOrder = 0
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
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 771
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      771
      65)
    object Label6: TLabel
      Left = 44
      Top = 10
      Width = 54
      Height = 17
      Alignment = taRightJustify
      Caption = 'Key path:'
    end
    object Label7: TLabel
      Left = 40
      Top = 41
      Width = 58
      Height = 17
      Alignment = taRightJustify
      Caption = 'Key value:'
    end
    object edit_ValuePath: TEdit
      Left = 104
      Top = 7
      Width = 658
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      ReadOnly = True
      TabOrder = 0
    end
    object edit_ValueName: TEdit
      Left = 104
      Top = 38
      Width = 658
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      ReadOnly = True
      TabOrder = 1
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 721
    Top = 114
  end
  object SaveDialog1: TSaveDialog
    Left = 721
    Top = 162
  end
end
