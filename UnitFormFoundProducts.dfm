object FormFoundProducts: TFormFoundProducts
  Left = 0
  Top = 0
  Caption = 'FormFoundProducts'
  ClientHeight = 365
  ClientWidth = 734
  Color = clHighlightText
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 18
  object Splitter1: TSplitter
    Left = 412
    Top = 0
    Width = 5
    Height = 365
    Color = clGradientInactiveCaption
    ParentColor = False
    ExplicitLeft = 244
  end
  object StringGrid1: TStringGrid
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 406
    Height = 359
    Align = alLeft
    BorderStyle = bsNone
    ColCount = 2
    DefaultRowHeight = 22
    DefaultDrawing = False
    FixedColor = clBackground
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    GradientEndColor = clBlack
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    TabOrder = 0
    OnDrawCell = StringGrid1DrawCell
    OnSelectCell = StringGrid1SelectCell
    ColWidths = (
      64
      64)
    RowHeights = (
      22)
  end
  object Panel2: TPanel
    Left = 417
    Top = 0
    Width = 317
    Height = 365
    Align = alClient
    BevelOuter = bvNone
    Caption = #1047#1072#1075#1088#1091#1078#1072#1102#1090#1089#1103' '#1076#1072#1085#1085#1099#1077' '#1075#1088#1072#1092#1080#1082#1072
    TabOrder = 1
    ExplicitLeft = 0
    ExplicitHeight = 41
  end
end
