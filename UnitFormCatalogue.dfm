object FormCatalogue: TFormCatalogue
  Left = 0
  Top = 0
  Caption = 'FormCatalogue'
  ClientHeight = 299
  ClientWidth = 357
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
    Top = 41
    Width = 351
    Height = 255
    Align = alClient
    BorderStyle = bsNone
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
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
    ParentFont = False
    TabOrder = 0
    OnDrawCell = StringGrid1DrawCell
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 357
    Height = 38
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    object ComboBox1: TComboBox
      AlignWithMargins = True
      Left = 11
      Top = 7
      Width = 126
      Height = 26
      Margins.Right = 35
      Style = csOwnerDrawFixed
      Color = clHighlightText
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 20
      ItemIndex = 0
      ParentFont = False
      TabOrder = 0
      Text = '11.11.2018'
      OnChange = ComboBox1Change
      Items.Strings = (
        '11.11.2018')
    end
  end
end
