object FormOxygen73: TFormOxygen73
  Left = 0
  Top = 0
  Caption = #1069#1061#1071' O2'
  ClientHeight = 587
  ClientWidth = 1088
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 18
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 1088
    Height = 587
    Align = alClient
    BevelOuter = bvNone
    Caption = 'PanelMain'
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 907
    object Splitter1: TSplitter
      Left = 393
      Top = 30
      Width = 5
      Height = 557
      Color = clGradientInactiveCaption
      ParentColor = False
      OnMoved = Splitter1Moved
      ExplicitLeft = 184
      ExplicitTop = 31
      ExplicitHeight = 556
    end
    object Panel2: TPanel
      Left = 398
      Top = 30
      Width = 690
      Height = 557
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Panel1'
      Constraints.MinHeight = 100
      ShowCaption = False
      TabOrder = 0
      ExplicitTop = 31
      ExplicitWidth = 509
      ExplicitHeight = 556
      object Splitter2: TSplitter
        Left = 0
        Top = 250
        Width = 690
        Height = 5
        Cursor = crVSplit
        Align = alTop
        Color = clGradientInactiveCaption
        ParentColor = False
        OnMoved = Splitter2Moved
        ExplicitLeft = 6
        ExplicitTop = 248
        ExplicitWidth = 717
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 690
        Height = 250
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Panel1'
        ShowCaption = False
        TabOrder = 0
        ExplicitWidth = 509
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 30
      Width = 393
      Height = 557
      Align = alLeft
      BevelOuter = bvNone
      Caption = 'Panel4'
      ShowCaption = False
      TabOrder = 1
      ExplicitTop = 31
      ExplicitHeight = 556
    end
    object PanelTop: TPanel
      Left = 0
      Top = 0
      Width = 1088
      Height = 30
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Color = clGradientInactiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 2
      object Panel5: TPanel
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 68
        Height = 20
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alLeft
        BevelOuter = bvNone
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        ExplicitHeight = 21
      end
      object Panel6: TPanel
        AlignWithMargins = True
        Left = 83
        Top = 5
        Width = 462
        Height = 20
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alLeft
        Alignment = taLeftJustify
        BevelOuter = bvNone
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        ExplicitHeight = 15
      end
      object Panel7: TPanel
        AlignWithMargins = True
        Left = 555
        Top = 5
        Width = 68
        Height = 20
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alLeft
        BevelOuter = bvNone
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        ExplicitLeft = 661
        ExplicitHeight = 15
      end
      object Panel8: TPanel
        AlignWithMargins = True
        Left = 633
        Top = 5
        Width = 450
        Height = 20
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        Alignment = taLeftJustify
        BevelOuter = bvNone
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        ExplicitLeft = 5
        ExplicitWidth = 572
        ExplicitHeight = 15
      end
    end
  end
  object ImageList4: TImageList
    ColorDepth = cd32Bit
    BlendColor = clWindow
    BkColor = clWhite
    DrawingStyle = dsTransparent
    Height = 20
    Width = 20
    Left = 535
    Top = 122
    Bitmap = {
      494C010106002403040014001400FFFFFF002110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000500000002800000001002000000000000032
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000947331E958D6840CDA67B
      4DDD8D6840CD46321D9400000009000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000040D0F52000000100000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A1784DE0A58C72DBA58C72DBA58C72DBA58C
      72DBA58C72DBA58C72DBCDAE8FF3FDFBF9FFDEAF7EFFF5C28CFFFDCC98FFFDCC
      98FFFDCC98FFF5C28DFE7E5A35C5000000090000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001B4D59C80CABE0F70138
      55940000000D0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000AA8E72DFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFE5C29EFFF7C490FFFDCC98FFFDCC98FFFFFF
      FFFFFDCC98FFFDCC98FFF5C28DFE46321D940000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000002010118110F0D44110F
      0D44110F0D44110F0D44110F0D44110F0D44110F0D44183942890F78A9D20098
      FFFF0D6DADD41313124E110F0D440C0A08380000000000000000000000000000
      0000000000000000000000000000AA8E72DFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFE2B382FFFDCC98FFFDCC98FFFDCC98FFFFFF
      FFFFFDCC98FFFDCC98FFFDCC98FF8D6840CD0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001F1B175AFBDEBBFFFBDE
      BBFFFBDEBBFFFBDEBBFFFBDEBBFFFBDEBBFFFBDEBBFFEFDBBEFF39A8EFFF0098
      FFFF0098FFFF39A8EFFFEDDABFFFAA967ED20000000000000000000000000000
      0000000000000000000000000000AA8E72DFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFE2B17CFFFDCC98FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFDCC98FFA57A4DDC0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001F1B175AFBDEBBFFFBDE
      BBFFFBDEBBFFFBDEBBFFFBDEBBFFFBDEBBFFFBDEBBFFFBDEBBFFEEDBBEFF39A8
      EFFF0098FFFF0098FFFF39A8EFFFA29585D40000000000000000000000000000
      0000000000000000000000000000AA8E72DFFFFFFFFFFDFAF8FFEDD9C4FFF4E8
      DBFFE1C09DFFE1C09DFFEBD4BCFFDFAD78FFFDCC98FFFDCC98FFFDCC98FFFFFF
      FFFFFDCC98FFFDCC98FFFDCC98FF8D6840CD0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001F1B175AFBDEBBFFFBDE
      BBFFEEBF8BFFEEBF8BFFDA8A37FFD57D23FFD57D23FFDB8E3DFFDE9447FFE9CE
      AAFF39A8EFFF0098FFFF0098FFFF2C99DEF50000000E00000000000000000000
      0000000000000000000000000000AA8E72DFFFFFFFFFFFFFFFFFFBF6F2FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFE5C19BFFF5C28CFFFDCC98FFFDCC98FFFFFF
      FFFFFDCC98FFFDCC98FFF3C08AFE47331D950000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001F1B175AFBDEBBFFFBDE
      BBFFF9D8B3FFF9D8B3FFF5CFA2FFF3CB9EFFF3CB9EFFF4D0A4FFF5D1A7FFF6D3
      ABFFEDD7BAFF39A8EFFF0098FFFF0098FFFF244256A00000000E000000000000
      0000000000000000000000000000AA8E72DFFFFFFFFFFDFAF8FFEDD9C4FFF5EA
      DEFFE5C7A9FFE5C7A9FFE5C8AAFFF1E0D0FFDEAE7DFFF5C28CFFFDCC98FFFDCC
      98FFFDCC98FFF5C38EFE815C37C7000000090000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001F1B175AFBDEBBFFFBDE
      BBFFEEBF8BFFEEBF8BFFE4A45FFFDD9243FFDD9243FFDD9243FFDD9243FFDD92
      43FFE2A15AFFEDDABFFF39A8EFFF73A4C2F4C0BAACFC3F3D4C9F0000000D0000
      0000000000000000000000000000AA8E72DFFFFFFFFFFDFAF8FFEDD9C4FFF4E8
      DBFFE1C09DFFE1C09DFFE1C09DFFE1C09DFFE9D0B7FFE0B68BFFE2B383FFE2B0
      7BFFE2B383FF47331E9500000009000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001F1B175AFBDEBBFFFBDE
      BBFFF5CCA0FFF5CCA0FFEEBE8AFFEBB479FFEBB479FFEBB479FFEBB479FFEBB4
      79FFECB67DFFFBDEBBFFEFDABEFFBDB4A2F45C5A6EBF6969D2F5020205290000
      0000000000000000000000000000AA8E72DFFFFFFFFFFFFFFFFFFBF6F2FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001F1B175AFBDEBBFFFBDE
      BBFFF5CCA0FFF5CCA0FFEEBE8AFFEBB479FFEBB479FFEBB479FFEBB479FFEBB4
      79FFECB67DFFFBDEBBFFFBDEBBFFA99581D4343468AC07070F43000000000000
      0000000000000000000000000000AA8E72DFFFFFFFFFFDFAF8FFEDD9C4FFF5EA
      DEFFE5C7A9FFE5C7A9FFE5C7A9FFE8CEB3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFCEAE8DF50000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001F1B175AFBDEBBFFFBDE
      BBFFEEBF8BFFEEBF8BFFE4A45FFFDD9243FFDD9243FFDD9243FFDD9243FFDD92
      43FFDF964AFFFBDEBBFFFBDEBBFFAA967ED20000000000000000000000000000
      0000000000000000000000000000AA8E72DFFFFFFFFFFDFAF8FFEDD9C4FFF4E8
      DBFFE1C09DFFE1C09DFFE1C09DFFE1C09DFFE1C09DFFE5C8AAFFFFFFFFFFFFFF
      FFFFAA8E72DF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001F1B175AFBDEBBFFFBDE
      BBFFF9D8B3FFF9D8B3FFF7D4AAFFF5D1A7FFF5D1A7FFF5D1A7FFF5D1A7FFF5D1
      A7FFF5D1A8FFFBDEBBFFFBDEBBFFAA967ED20000000000000000000000000000
      0000000000000000000000000000AA8E72DFFFFFFFFFFFFFFFFFFDFAF8FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFAA8E72DF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001F1B175AFBDEBBFFFBDE
      BBFFEEBF8BFFEEBF8BFFE4A45FFFDD9243FFDD9243FFDD9243FFDD9243FFDD92
      43FFDF964AFFFBDEBBFFFBDEBBFFAA967ED20000000000000000000000000000
      0000000000000000000000000000AA8E72DFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFAA8E72DF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003A2D2681FBDEBBFFFBDE
      BBFFFBDEBBFFFBDEBBFFFBDEBBFFFBDEBBFFFBDEBBFFFBDEBBFFFBDEBBFFFBDE
      BBFFFBDEBBFFFBDEBBFFFBDEBBFFD3B296EE0000000000000000000000000000
      0000000000000000000000000000AA8E72DFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD6A672FFE4C19DFFE4C19DFFE3BF
      9AFFC48C52FA0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003E2C2489EDC2A3FFEDC2
      A3FFEDC2A3FFEDC2A3FFEDC2A3FFEDC2A3FFEDC2A3FFEDC2A3FFEDC2A3FFEDC2
      A3FFEDC2A3FFEDC2A3FFEDC2A3FFCB9E84F00000000000000000000000000000
      0000000000000000000000000000AA8E72DFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE2BE97FFFEF0DFFFFDEEDCFFCFA4
      75F90F0A05450000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000160A075AB5503EFFB550
      3EFFB5503EFFB5503EFFB5503EFFB5503EFFB5503EFFB5503EFFB5503EFFB550
      3EFFB5503EFFB5503EFFB5503EFF7A362AD20000000000000000000000000000
      0000000000000000000000000000AA8E72DFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE2BE97FFFDEEDCFFCFA475F90F0A
      0545000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000C0504449B4436ED9E46
      36EE9E4636EE9E4636EE9E4636EE9E4636EE9E4636EE9E4636EE9E4636EE9E46
      36EE9E4636EE9E4636EE9E4636EE5D281FB70000000000000000000000000000
      0000000000000000000000000000AA8E72DFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE2BB93FFCFA475F90F0A05450000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A57A4DE4A88C6CDFA88C6CDFA88C6CDFA88C
      6CDFA88C6CDFA88C6CDFA88C6CDFA88C6CDFC58D51FB0D090541000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000522190F693B2C
      1C88050302280000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001A130A5C3D2B198B030201210000
      0000000000000000000000000000000000000000000000000000000000000000
      00000E0A06435B4228A85B4228A80E0A06430000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000067F5F41C5EFD8BFFDFAEA
      D5FFB69877E4251B126C00000000000000000000000000000000000000000000
      0000000000000000000000000003614528AEE0AE7AF7F9C892FFA87D4DDF0302
      0121000000000000000000000000000000000000000000000000000000000000
      000063482DAEFEDCB6FFFEDCB6FF63482DAE0000000000000000000000000000
      0000000000000000000000000000000000000000000049362398533E2AA0533E
      2AA0533E2AA0533E2AA0533E2AA0533E2AA0533E2AA0533E2AA0533E2AA0533E
      2AA0533E2AA0533E2AA0533E2AA0533E2AA0533E2AA0533E2AA0493623980000
      0000000000000000000000000000000000010D09054300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000D090543000000010000000000000000241A106BF0D9C0FD9B7B58D77053
      35BBDFC8B1F5C4A98AEA271D126F000000000000000000000000000000000000
      00000000000000000003624629AEE9B984FAFAC893FFF2C088FFF9C793FF3D2B
      198A000000000000000000000000000000000000000000000000000000000000
      0009906D47CFFEDCB6FFFEDCB6FF906D47CF0000000900000000000000000000
      00000000000000000000000000000000000000000000AC9074E0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAC9074E00000
      0000000000000000000000000001543D28A1C6A380EE36281982000000000000
      0000000000000000000000000000000000000000000000000000000000003628
      1982C6A380EE543D28A100000001000000003D2D1D8BFBEAD6FF6F5235BA0705
      0232BC9F7EE8FEF0DFFFC4A98AEA281D13700000000000000000000000000000
      000000000003604428ADE9B884FAFAC893FFDDA467FFFAC893FFE0AE7CF71A13
      0A5C00000000000000000000000000000000000000000000000A513A249E9E7B
      52D8F0CBA2FCFEDCB6FFFEDCB6FFF0CBA2FC9E7B52D8503A239D0000000A0000
      00000000000000000000000000000000000000000000AC9074E0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAC9074E00000
      00000000000000000000543D28A1E2CCB2F7FEF0DFFFD0B79AF0362819820000
      000000000000000000000000000000000000000000000000000036281982D0B7
      9AF0FEF0DFFFE2CCB2F7543D28A1000000000503022AB69877E4DEC8AEF5BB9D
      7EE7FCEDDBFFFEF0DFFFFEF0DFFFC6A98DEB291F137200000000000000000000
      00025F4327ACE9B883FAFAC893FFDDA467FFFAC893FFECBC87FB624729AF0000
      00000000000000000001705232BB654A2EB0281D11718D6A43CFF9D5ACFFFDDB
      B5FFEFC59AFFE5B989FFE5B989FFEFC69BFFFDDBB5FFF6D1A9FE8D6A43CF281D
      1171654A2EB0705232BB000000010000000000000000AC9074E0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAC9074E00000
      0000000000000000000036281982D0B79AF0FEF0DFFFFEF0DFFFD0B79AF03628
      1982000000000000000000000000000000000000000036281982D0B79AF0FEF0
      DFFFFEF0DFFFD0B79AF0362819820000000000000000241A106AC2A688EAFEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFC6AB8FEB2A1F1473000000015D43
      26ABE9B784FAFAC893FFDDA467FFFAC893FFEFBD88FC65472AB1000000030000
      000000000000281D1171E1BD94F7FEDCB6FFF5CEA3FFFDDBB5FFF1C99DFFE0B2
      83FFF0D6B7FFFBEBD8FFFBEBD8FFEFD5B6FFE0B283FFF1CA9DFFFDDBB5FFF5CE
      A3FFFEDCB6FFE1BD94F7281D11710000000000000000AC9074E0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAC9074E00000
      000000000000000000000000000036281982D0B79AF0FEF0DFFFFEF0DFFFCEB5
      98EF3628198200000000000000000000000036281982CEB598EFFEF0DFFFFEF0
      DFFFD0B79AF03628198200000000000000000000000000000000241A106AC0A5
      84E9FEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFC8AD8FEC8B663ED1E9B7
      83FAFDCC98FFDFA66AFFFAC893FFEFBE88FC674A2BB300000003000000000000
      000000000000705130BBF9D5ADFFFEDCB6FFFEDCB6FFF1C99EFFE4BE96FFFEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFE4BD94FFF1CA9FFFFEDC
      B6FFFEDCB6FFF9D5ADFF705130BB0000000000000000AC9074E0FFFFFFFFFFFF
      FFFFFFFFFFFFEFDCC9FFFFFFFFFFFFFFFFFFFFFFFFFFF4E7D9FFE5C7A9FFE5C7
      A9FFE5C7A9FFE5C7A9FFE7CBAFFFFFFFFFFFFFFFFFFFFFFFFFFFAC9074E00000
      00000000000000000000000000000000000036281982CEB598EFFEF0DFFFFEF0
      DFFFCEB598EF362819820000000036281982CEB598EFFEF0DFFFFEF0DFFFCEB5
      98EF362819820000000000000000000000000000000000000000000000002319
      1069BEA284E8FEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFE9C9A5FEE9B5
      80FFFDCC98FFFDCC98FFF0BE88FC694A2AB40000000300000000000000000000
      0000000000000000000C7B5A37C3F6D1A9FEFDDBB4FFE0B383FFFEF0DFFFFEF0
      DFFFFBEAD6FFC0A283E9C0A283E9FBEAD6FFFEF0DFFFFEF0DFFFE0B283FFFDDB
      B4FFF6D1A9FE7B5A37C30000000C0000000000000000AC9074E0FFFFFFFFFFFF
      FFFFFFFFFFFFF1E1D1FFE1C09DFFFDFAF7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAC9074E00000
      0000000000000000000000000000000000000000000036281982CEB598EFFEF0
      DFFFFEF0DFFFCEB498EF755536C0CEB498EFFEF0DFFFFEF0DFFFCEB598EF3628
      1982000000000000000000000000000000000000000000000000000000000000
      000021180F66BEA284E8FEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFECCC
      A8FFE9B67EFFF0BF89FC6A4A2BB5000000040000000000000000000000000000
      00000000000000000000100A0648EBC298FDEFC699FFF0D6B7FFFEF0DFFFFAEA
      D5FF785A3CC00201001C0201001D7A5C3DC2FBEAD6FFFEF0DFFFF0D6B7FFEFC6
      9BFFEBC298FD100A0648000000000000000000000000AC9074E0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFAF4EEFFDFBB96FFF6EBE1FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAC9074E00000
      000000000000000000000000000000000000000000000000000036281982CEB5
      98EFFEF0DFFFFEF0DFFFFAE8D4FFFEF0DFFFFEF0DFFFCEB598EF362819820000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000020180F65BDA183E7FEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFECCCA8FF9F7245DF3A2B1B8746342294150F085200000004000000000000
      000000000000000000003123157CF8D3ABFFE5B989FFFCECD8FFFEF0DFFFBFA2
      83E90201001C00000000000000000201001DBFA283E9FEF0DFFFFBEBD8FFE5B9
      8AFFF8D3ABFF3123157C000000000000000000000000AC9074E0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFAF4EEFFDFBB96FFF6EBE1FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAC9074E00000
      0000000000000000000000000000000000000000000000000000000000003628
      1982CEB498EFFEF0DFFFFEF0DFFFFEF0DFFFCEB498EF36281982000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000020170F64C7A888EEFEF0DFFFFEF0DFFFFEF0DFFFFAE9
      D4FFDDB183FFDBBA96F9F9E7D3FFFCECD9FFE3CCB0F9836241C9000000120000
      000000000000000000003123157CF8D3ABFFE5B989FFFCECD8FFFEF0DFFFBEA1
      83E90201001C00000000000000000201001DBFA283E9FEF0DFFFFBEBD8FFE5B9
      8AFFF8D3ABFF3123157C000000000000000000000000AC9074E0FFFFFFFFFFFF
      FFFFFFFFFFFFF1E1D1FFE1C09DFFFDFAF7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAC9074E00000
      0000000000000000000000000000000000000000000000000000000000003628
      1982CEB498EFFEF0DFFFFEF0DFFFFEF0DFFFCEB498EF36281982000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000100C0749BB9060EDE8C5A0FFFEF0DFFFF9E7D3FFDAAD
      80FEFBEBD8FFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFBEAD7FF6A5036B40000
      00000000000000000000100A0648EBC298FDEFC598FFF0D6B8FFFEF0DFFFFAE9
      D5FF775A3CBF0201001B0201001B785A3CC0FBEAD6FFFEF0DFFFF0D6B7FFEFC5
      9AFFEBC298FD100A0648000000000000000000000000AC9074E0FFFFFFFFFFFF
      FFFFFFFFFFFFEFDCC9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAC9074E00000
      000000000000000000000000000000000000000000000000000036281982CEB5
      98EFFEF0DFFFFEF0DFFFFAE8D4FFFEF0DFFFFEF0DFFFCEB598EF362819820000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000100C0749B08A61E2FEDCB6FFBC9062EDC7A685EEDAAE80FEFBEA
      D6FFFEF0DFFFFEF0DFFFF7E4CDFFB89774E6F1DCC6FCFEF0DFFFBCA080E70201
      011E000000000000000C7B5A37C3F6D1A9FEFDDBB4FFE0B384FFFEF0DFFFFEF0
      DFFFFBEAD6FFBEA183E9BFA183E9FBEAD6FFFEF0DFFFFEF0DFFFE0B383FFFDDB
      B4FFF6D1A9FE7B5A37C30000000C0000000000000000AC9074E0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAC9074E00000
      0000000000000000000000000000000000000000000036281982CEB598EFFEF0
      DFFFFEF0DFFFCEB498EF755536C0CEB498EFFEF0DFFFFEF0DFFFCEB598EF3628
      1982000000000000000000000000000000000000000000000000000000000000
      0000100C0749B0885FE2FEDCB6FFB08A61E2100C07492C211377DBB994F9FEF0
      DFFFFEF0DFFF9B7C5AD73325187F010100196E5236B8F1DCC6FCF7E3CBFF2A1F
      137300000000705130BBF9D5ADFFFEDCB6FFFEDCB6FFF1C99DFFE4BE96FFFEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFE4BE95FFF1C99DFFFEDC
      B6FFFEDCB6FFF9D5ADFF705130BB0000000000000000AC9074E0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAC9074E00000
      00000000000000000000000000000000000036281982CEB598EFFEF0DFFFFEF0
      DFFFCEB598EF362819820000000036281982CEB598EFFEF0DFFFFEF0DFFFCEB5
      98EF362819820000000000000000000000000000000000000000030201224230
      1D90B08A61E2FEDCB6FFB0885FE2100C07490000000037281A83F9E7D2FFFEF0
      DFFFF7E4CDFF3326177F0000000000000000000000066C5035B6EFD8BFFD4332
      209100000000281D1171E1BD94F7FEDCB6FFF5CEA3FFFDDBB5FFF1C99EFFE0B4
      84FFF0D5B8FFFCECD8FFFCECD8FFF0D6B7FFE0B383FFF1C99EFFFDDBB5FFF5CE
      A3FFFEDCB6FFE1BD94F7281D11710000000000000000AC9074E0FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAC9074E00000
      000000000000000000000000000036281982D0B79AF0FEF0DFFFFEF0DFFFCEB5
      98EF3628198200000000000000000000000036281982CEB598EFFEF0DFFFFEF0
      DFFFD0B79AF03628198200000000000000000000000005030128B1895CE4FAD6
      ADFFFEDCB6FFB08A61E2100C0749000000000000000047352295FDEEDDFFFEF0
      DFFFB99978E60201001C00000000000000000000000000000005735435BD1F16
      0D650000000000000001705232BB654A2EB0281D11718D6943CFF9D5ADFFFDDB
      B3FFEFC699FFE5B989FFE5B989FFEFC59AFFFDDBB4FFF6D2A9FE8D6A43CF281D
      1171654A2EB0705132BA000000010000000000000000BF9E7DE8FEE5CBFFFEE5
      CBFFFEE5CBFFFEE5CBFFFEE5CBFFFEE5CBFFFEE5CBFFFEE5CBFFFEE5CBFFFEE5
      CBFFFEE5CBFFFEE5CBFFFEE5CBFFFEE5CBFFFEE5CBFFFEE5CBFFBF9E7DE80000
      0000000000000000000036281982D0B79AF0FEF0DFFFFEF0DFFFD0B79AF03628
      1982000000000000000000000000000000000000000036281982D0B79AF0FEF0
      DFFFFEF0DFFFD0B79AF0362819820000000000000000785737C0FDDBB3FFFEDC
      B6FFFAD6AEFF42301D9000000000000000000000000017110A57E2CBB1F8FEF0
      DFFFF0DDC7FC705336B900000006000000000000000000000000000000040000
      000900000000000000000000000000000000000000000000000C513B249E9F7B
      53D9F0CBA2FCFEDCB6FFFEDCB6FFF0CBA2FC9F7B53D9513A249E0000000B0000
      00000000000000000000000000000000000000000000D3AD86F0FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFD3AD86F00000
      00000000000000000000543D28A1E2CCB2F7FEF0DFFFD0B79AF0362819820000
      000000000000000000000000000000000000000000000000000036281982D0B7
      9AF0FEF0DFFFE2CCB2F7543D28A100000000150F0852D7B086F4FEDCB6FFFDDA
      B4FFB1895CE40302012200000000000000000000000000000004866545CAFBEB
      D8FFFEF0DFFFF0DDC7FC715437BB000000060000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0009906D47CFFEDCB6FFFEDCB6FF906D47CF0000000900000000000000000000
      00000000000000000000000000000000000000000000D3AD86F0FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFD3AD86F00000
      0000000000000000000000000001543D28A1C6A380EE36281982000000000000
      0000000000000000000000000000000000000000000000000000000000003628
      1982C6A380EE543D28A100000001000000000000000B7D5C39C4D8AF84F47657
      37BE040301270000000000000000000000000000000000000000010000156E54
      38B8C5A787EBF8E5CFFFF1DDC6FD7B5A39C30000000700000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000063482DAEFEDCB6FFFEDCB6FF63482DAE0000000000000000000000000000
      0000000000000000000000000000000000000000000049362398533E2AA0533E
      2AA0533E2AA0533E2AA0533E2AA0533E2AA0533E2AA0533E2AA0533E2AA0533E
      2AA0533E2AA0533E2AA0533E2AA0533E2AA0533E2AA0533E2AA0493623980000
      0000000000000000000000000000000000010D09054300000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000D090543000000010000000000000000000000000000000B150E08530000
      0000000000000000000000000000000000000000000000000000000000000000
      0000040201263225187E463420941F160D640000000A00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000B08053D4E38239B4E38239B0B08053D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000050000000280000000100010000000000E00100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object MainMenu1: TMainMenu
    Images = ImageList4
    Left = 334
    Top = 127
    object N7: TMenuItem
      Caption = #1055#1072#1088#1090#1080#1103' '#1069#1061#1071
      object N6: TMenuItem
        Caption = #1042#1074#1086#1076' '#1089#1077#1088#1080#1081#1085#1099#1093' '#1085#1086#1084#1077#1088#1086#1074
        ImageIndex = 4
        OnClick = N6Click
      end
      object N8: TMenuItem
        Caption = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102
        ImageIndex = 5
        OnClick = N8Click
      end
    end
    object N1: TMenuItem
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      object N4: TMenuItem
        Caption = #1054#1089#1085#1086#1074#1085#1099#1077
        ImageIndex = 1
        OnClick = N4Click
      end
      object N5: TMenuItem
        Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077
        ImageIndex = 0
        OnClick = N5Click
      end
    end
    object N2: TMenuItem
      Caption = #1054#1082#1085#1086
      object N3: TMenuItem
        Caption = #1050#1086#1085#1089#1086#1083#1100
        ImageIndex = 2
      end
    end
  end
end
