object FormAppConfig: TFormAppConfig
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 114
  ClientWidth = 335
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 21
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 325
    Height = 104
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Caption = #1057#1054#1052' '#1087#1086#1088#1090#1099
    TabOrder = 0
    ExplicitWidth = 319
    ExplicitHeight = 108
    object Panel1: TPanel
      Left = 2
      Top = 23
      Width = 321
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 0
      ExplicitWidth = 315
      object Shape1: TShape
        Left = 1
        Top = 36
        Width = 319
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Panel2: TPanel
        Left = 1
        Top = 1
        Width = 200
        Height = 35
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = #1057#1090#1077#1085#1076
        TabOrder = 1
      end
      object ComboBoxComport: TComboBox
        Left = 210
        Top = 4
        Width = 98
        Height = 26
        Style = csOwnerDrawFixed
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 20
        ItemIndex = 0
        ParentFont = False
        TabOrder = 0
        Text = 'COM1'
        OnChange = ComboBoxComportChange
        OnDropDown = ComboBoxComportDropDown
        Items.Strings = (
          'COM1')
      end
    end
    object Panel17: TPanel
      Left = 2
      Top = 61
      Width = 321
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 1
      ExplicitWidth = 315
      object Panel18: TPanel
        Left = 1
        Top = 1
        Width = 200
        Height = 36
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = #1044#1072#1090#1095#1080#1082' '#1074#1083#1072#1078#1085#1086#1089#1090#1080
        TabOrder = 0
      end
      object ComboBoxComportHumidity: TComboBox
        Left = 210
        Top = 4
        Width = 98
        Height = 26
        Style = csOwnerDrawFixed
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 20
        ItemIndex = 0
        ParentFont = False
        TabOrder = 1
        Text = 'COM1'
        OnChange = ComboBoxComportChange
        OnDropDown = ComboBoxComportDropDown
        Items.Strings = (
          'COM1')
      end
    end
  end
end
