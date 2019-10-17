object FormProducts: TFormProducts
  Left = 0
  Top = 0
  Caption = 'FormProducts'
  ClientHeight = 253
  ClientWidth = 1112
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StringGrid1: TStringGrid
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 1106
    Height = 247
    Align = alClient
    BorderStyle = bsNone
    DefaultRowHeight = 22
    DefaultDrawing = False
    FixedColor = clBackground
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    GradientEndColor = clBlack
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnDrawCell = StringGrid1DrawCell
    OnMouseDown = StringGrid1MouseDown
    OnSelectCell = StringGrid1SelectCell
    ColWidths = (
      64
      64
      64
      64
      64)
    RowHeights = (
      22)
  end
  object PopupMenu1: TPopupMenu
    Left = 552
    Top = 128
    object N1: TMenuItem
      Tag = 1
      Caption = #1055#1086#1082#1072#1079#1072#1090#1100' '#1074#1099#1073#1088#1072#1085#1085#1099#1077' '#1075#1088#1072#1092#1080#1082#1080
      OnClick = N2Click
    end
    object N2: TMenuItem
      Caption = #1057#1082#1088#1099#1090#1100' '#1074#1099#1073#1088#1072#1085#1085#1099#1077' '#1075#1088#1072#1092#1080#1082#1080
      OnClick = N2Click
    end
  end
end
