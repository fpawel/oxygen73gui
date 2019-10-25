object FormEditSerialsDialog: TFormEditSerialsDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1042#1074#1086#1076' '#1089#1077#1088#1080#1081#1085#1099#1093' '#1085#1086#1084#1077#1088#1086#1074' '#1069#1061#1071
  ClientHeight = 331
  ClientWidth = 556
  Color = clBtnHighlight
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object StringGrid1: TStringGrid
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 550
    Height = 325
    Align = alClient
    BiDiMode = bdLeftToRight
    BorderStyle = bsNone
    ColCount = 3
    DefaultColWidth = 100
    DefaultRowHeight = 22
    DefaultDrawing = False
    FixedColor = clBackground
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    GradientEndColor = clBlack
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    ParentBiDiMode = False
    ParentFont = False
    TabOrder = 0
    OnDrawCell = StringGrid1DrawCell
    OnSetEditText = StringGrid1SetEditText
    RowHeights = (
      22)
  end
end
