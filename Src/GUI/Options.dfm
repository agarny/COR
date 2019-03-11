object OptionsForm: TOptionsForm
  Left = 195
  Top = 203
  ActiveControl = PageCtrl
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 350
  ClientWidth = 365
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageCtrl: TPageControl
    Left = 4
    Top = 4
    Width = 357
    Height = 313
    ActivePage = PageCtrlGen
    TabOrder = 0
    object PageCtrlGen: TTabSheet
      Caption = 'General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PageCtrlGenSep: TBevel
        Left = 4
        Top = 25
        Width = 341
        Height = 2
        Shape = bsTopLine
      end
      object PageCtrlGenProxyFrame: TBevel
        Left = 4
        Top = 52
        Width = 341
        Height = 113
        Shape = bsFrame
      end
      object PageCtrlGenProxyServerLab: TLabel
        Left = 8
        Top = 59
        Width = 36
        Height = 13
        Caption = '&Server:'
        FocusControl = PageCtrlGenProxyServerVal
        Transparent = True
      end
      object PageCtrlGenProxyPortLab: TLabel
        Left = 236
        Top = 59
        Width = 24
        Height = 13
        Caption = 'P&ort:'
        FocusControl = PageCtrlGenProxyPortVal
        Transparent = True
      end
      object PageCtrlGenProxyAuthenticationMethodLab: TLabel
        Left = 8
        Top = 85
        Width = 113
        Height = 13
        Caption = 'Authentication &method:'
        FocusControl = PageCtrlGenProxyAuthenticationMethodVal
        Transparent = True
      end
      object PageCtrlGenProxyAuthFrame: TBevel
        Left = 8
        Top = 107
        Width = 333
        Height = 54
        Shape = bsFrame
      end
      object PageCtrlGenProxyAuthPasswordLab: TLabel
        Left = 12
        Top = 140
        Width = 50
        Height = 13
        Caption = 'P&assword:'
        FocusControl = PageCtrlGenProxyAuthPasswordVal
        Transparent = True
      end
      object PageCtrlGenProxyAuthUsernameLab: TLabel
        Left = 12
        Top = 114
        Width = 52
        Height = 13
        Caption = '&Username:'
        FocusControl = PageCtrlGenProxyAuthUsernameVal
        Transparent = True
      end
      object PageCtrlGenCheckForNewVersionAtStartupCB: TCheckBox
        Left = 4
        Top = 4
        Width = 181
        Height = 17
        Caption = 'Check for &new version at startup'
        TabOrder = 0
      end
      object PageCtrlGenConnectThroughProxyServerCB: TCheckBox
        Left = 4
        Top = 31
        Width = 164
        Height = 17
        Caption = 'Connect through &proxy server'
        TabOrder = 1
        OnClick = PageCtrlGenProxySettingsClick
      end
      object PageCtrlGenProxyAuthAskForPasswordCB: TCheckBox
        Left = 236
        Top = 138
        Width = 101
        Height = 17
        Caption = 'Ask &for password'
        TabOrder = 7
        OnClick = PageCtrlGenProxySettingsClick
      end
      object PageCtrlGenProxyAuthenticationMethodVal: TComboBox
        Left = 124
        Top = 81
        Width = 106
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        OnChange = PageCtrlGenProxySettingsClick
        Items.Strings = (
          'Basic'
          'NTLM')
      end
      object PageCtrlGenProxyServerVal: TJvValidateEdit
        Left = 48
        Top = 56
        Width = 182
        Height = 21
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        EditText = '0'
        TabOrder = 2
      end
      object PageCtrlGenProxyPortVal: TJvValidateEdit
        Left = 264
        Top = 56
        Width = 73
        Height = 21
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        EditText = '0'
        TabOrder = 3
      end
      object PageCtrlGenProxyAuthUsernameVal: TJvValidateEdit
        Left = 68
        Top = 111
        Width = 162
        Height = 21
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        EditText = '0'
        TabOrder = 5
      end
      object PageCtrlGenProxyAuthPasswordVal: TJvValidateEdit
        Left = 68
        Top = 136
        Width = 162
        Height = 21
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        EditText = '0'
        TabOrder = 6
      end
    end
    object PageCtrlEditor: TTabSheet
      Caption = 'Editor'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PageCtrlEditorPageCtrl: TPageControl
        Left = 4
        Top = 4
        Width = 341
        Height = 277
        ActivePage = PageCtrlEditorPageCtrlGen
        TabOrder = 0
        object PageCtrlEditorPageCtrlGen: TTabSheet
          Caption = 'General'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object PageCtrlEditorPageCtrlGenViewLab: TLabel
            Left = 148
            Top = 7
            Width = 26
            Height = 13
            Caption = '&View:'
            FocusControl = PageCtrlEditorPageCtrlGenViewVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlGenInsertCaretLab: TLabel
            Left = 148
            Top = 31
            Width = 61
            Height = 13
            Caption = 'Insert &caret:'
            FocusControl = PageCtrlEditorPageCtrlGenInsertCaretVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlGenOverwriteCaretLab: TLabel
            Left = 148
            Top = 55
            Width = 80
            Height = 13
            Caption = '&Overwrite caret:'
            FocusControl = PageCtrlEditorPageCtrlGenOverwriteCaretVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlGenTabIndentLab: TLabel
            Left = 148
            Top = 79
            Width = 55
            Height = 13
            Caption = '&Tab indent:'
            FocusControl = PageCtrlEditorPageCtrlGenTabIndentVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlGenInsertModeCB: TCheckBox
            Left = 4
            Top = 6
            Width = 77
            Height = 17
            Caption = '&Insert mode'
            TabOrder = 0
          end
          object PageCtrlEditorPageCtrlGenGrouUndoCB: TCheckBox
            Left = 4
            Top = 30
            Width = 77
            Height = 17
            Caption = '&Group undo'
            TabOrder = 1
          end
          object PageCtrlEditorPageCtrlGenScrollPastEOFCB: TCheckBox
            Left = 4
            Top = 54
            Width = 93
            Height = 17
            Caption = 'Scroll past &EOF'
            TabOrder = 2
          end
          object PageCtrlEditorPageCtrlGenAutoIndentCB: TCheckBox
            Left = 4
            Top = 78
            Width = 101
            Height = 17
            Caption = '&Auto indent mode'
            TabOrder = 3
          end
          object PageCtrlEditorPageCtrlGenShowScrollHintCB: TCheckBox
            Left = 4
            Top = 150
            Width = 93
            Height = 17
            Caption = 'Show scroll &hint'
            TabOrder = 6
          end
          object PageCtrlEditorPageCtrlGenSmartTabsCB: TCheckBox
            Left = 4
            Top = 102
            Width = 69
            Height = 17
            Caption = 'S&mart tabs'
            TabOrder = 4
          end
          object PageCtrlEditorPageCtrlGenBackspaceUnindentsCB: TCheckBox
            Left = 4
            Top = 126
            Width = 125
            Height = 17
            Caption = 'Backspace &unindents'
            TabOrder = 5
          end
          object PageCtrlEditorPageCtrlGenViewVal: TComboBox
            Left = 232
            Top = 4
            Width = 97
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 8
            OnChange = UpdateFontSample
          end
          object PageCtrlEditorPageCtrlGenInsertCaretVal: TComboBox
            Left = 232
            Top = 28
            Width = 97
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 9
            OnChange = UpdateFontSample
            Items.Strings = (
              'Vertical line'
              'Horizontal line'
              'Half block'
              'Full block')
          end
          object PageCtrlEditorPageCtrlGenOverwriteCaretVal: TComboBox
            Left = 232
            Top = 52
            Width = 97
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 10
            OnChange = UpdateFontSample
            Items.Strings = (
              'Vertical line'
              'Horizontal line'
              'Half block'
              'Full block')
          end
          object PageCtrlEditorPageCtrlGenTabIndentVal: TJvValidateEdit
            Left = 232
            Top = 76
            Width = 97
            Height = 21
            CriticalPoints.MaxValueIncluded = False
            CriticalPoints.MinValueIncluded = False
            EditText = '0'
            TabOrder = 11
          end
          object PageCtrlEditorPageCtrlGenWordWrapCB: TCheckBox
            Left = 4
            Top = 174
            Width = 73
            Height = 17
            Caption = '&Word wrap'
            TabOrder = 7
          end
        end
        object PageCtrlEditorPageCtrlDisp: TTabSheet
          Caption = 'Display'
          ImageIndex = 2
          object PageCtrlEditorPageCtrlDispSampleLab: TLabel
            Left = 4
            Top = 94
            Width = 38
            Height = 13
            Caption = 'Sam&ple:'
            FocusControl = PageCtrlEditorPageCtrlDispSampleVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlDispFontLab: TLabel
            Left = 4
            Top = 64
            Width = 26
            Height = 13
            Caption = '&Font:'
            FocusControl = PageCtrlEditorPageCtrlDispFontVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlDispSizeLab: TLabel
            Left = 227
            Top = 64
            Width = 23
            Height = 13
            Caption = '&Size:'
            FocusControl = PageCtrlEditorPageCtrlDispSizeVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlDispSep: TBevel
            Left = 4
            Top = 54
            Width = 325
            Height = 2
            Shape = bsTopLine
          end
          object PageCtrlEditorPageCtrlDispRightMarginLab: TLabel
            Left = 188
            Top = 7
            Width = 64
            Height = 13
            Caption = 'Right &margin:'
            FocusControl = PageCtrlEditorPageCtrlDispRightMarginVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlDispGutterWidthLab: TLabel
            Left = 188
            Top = 32
            Width = 64
            Height = 13
            Caption = 'Gutter &width:'
            FocusControl = PageCtrlEditorPageCtrlDispGutterWidthVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlDispFontVal: TComboBox
            Left = 40
            Top = 60
            Width = 161
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 4
            OnChange = UpdateFontSample
          end
          object PageCtrlEditorPageCtrlDispSizeVal: TComboBox
            Left = 256
            Top = 60
            Width = 73
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 5
            OnChange = UpdateFontSample
          end
          object PageCtrlEditorPageCtrlDispShowRightMarginCB: TCheckBox
            Left = 4
            Top = 5
            Width = 105
            Height = 17
            Caption = 'Show &right margin'
            TabOrder = 0
            OnClick = PageCtrlEditorPageCtrlDispShowRightMarginCBClick
          end
          object PageCtrlEditorPageCtrlDispShowGutterCB: TCheckBox
            Left = 4
            Top = 30
            Width = 77
            Height = 17
            Caption = 'Show &gutter'
            TabOrder = 2
            OnClick = PageCtrlEditorPageCtrlDispShowGutterCBClick
          end
          object PageCtrlEditorPageCtrlDispSampleVal: TSyntaxEdit
            Left = 4
            Top = 111
            Width = 325
            Height = 134
            Cursor = crArrow
            Align = alCustom
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 6
            TabStop = False
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Terminal'
            Gutter.Font.Style = []
            Highlighter = SynCellMLSyn
            Lines.Strings = (
              'def model hodgkin_huxley_squid_axon_1952 as'
              '   def unit millisecond from'
              '      unit second {pref: milli};'
              '   enddef;'
              ''
              '   def comp sodium_channel as'
              '      var i_Na: microA_per_cm2 {pub: out};'
              '      var g_Na: milliS_per_cm2 {init: 120.0};'
              '      var E_Na: millivolt;'
              '      var time: millisecond {pub: in, priv: out};'
              '      var V: millivolt {pub: in, priv: out};'
              '      var E_R: millivolt {pub: in};'
              '      var m: dimensionless {priv: in};'
              '      var h: dimensionless {priv: in};'
              '      var x: dimensionless {priv: in};'
              ''
              '      E_Na = E_R+115.0{millivolt};'
              '      i_Na = g_Na*pow(m, 3.0{dimensionless})*h*(V-E_Na);'
              '   enddef;'
              'enddef;')
            Options = [eoAutoIndent, eoDragDropEditing, eoGroupUndo, eoNoCaret, eoNoSelection, eoRightMouseMovesCursor, eoScrollHintFollows, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces]
            ReadOnly = True
            NewFile = False
            RemovedKeystrokes = <
              item
                Command = ecContextHelp
                ShortCut = 112
              end>
            AddedKeystrokes = <
              item
                Command = ecContextHelp
                ShortCut = 16496
              end>
          end
          object PageCtrlEditorPageCtrlDispRightMarginVal: TJvValidateEdit
            Left = 256
            Top = 4
            Width = 73
            Height = 21
            CriticalPoints.MaxValueIncluded = False
            CriticalPoints.MinValueIncluded = False
            EditText = '0'
            TabOrder = 1
            OnChange = PageCtrlEditorPageCtrlDispRightMarginValChange
          end
          object PageCtrlEditorPageCtrlDispGutterWidthVal: TJvValidateEdit
            Left = 256
            Top = 29
            Width = 73
            Height = 21
            CriticalPoints.MaxValueIncluded = False
            CriticalPoints.MinValueIncluded = False
            EditText = '0'
            TabOrder = 3
            OnChange = PageCtrlEditorPageCtrlDispGutterWidthValChange
          end
        end
        object PageCtrlEditorPageCtrlCol: TTabSheet
          Caption = 'Colour'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object PageCtrlEditorPageCtrlColElementLab: TLabel
            Left = 4
            Top = 8
            Width = 42
            Height = 13
            Caption = '&Element:'
            FocusControl = PageCtrlEditorPageCtrlColElementVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlColBackgroundLab: TLabel
            Left = 132
            Top = 8
            Width = 60
            Height = 13
            Caption = '&Background:'
            FocusControl = PageCtrlEditorPageCtrlColBackgroundVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlColForegroundLab: TLabel
            Left = 132
            Top = 34
            Width = 60
            Height = 13
            Caption = '&Foreground:'
            FocusControl = PageCtrlEditorPageCtrlColForegroundVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlColAlphaLab: TLabel
            Left = 200
            Top = 58
            Width = 31
            Height = 13
            Caption = '&Alpha:'
            FocusControl = PageCtrlEditorPageCtrlColAlphaSB
            Transparent = True
          end
          object PageCtrlEditorPageCtrlColAlphaVal: TLabel
            Left = 237
            Top = 58
            Width = 3
            Height = 13
            FocusControl = PageCtrlEditorPageCtrlColBackgroundVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlColAlphaS: TShape
            Left = 272
            Top = 58
            Width = 57
            Height = 13
            Brush.Color = clWindow
          end
          object PageCtrlEditorPageCtrlColBackgroundVal: TColorBox
            Left = 200
            Top = 4
            Width = 129
            Height = 22
            Selected = clWindow
            ItemHeight = 16
            TabOrder = 1
            OnChange = UpdateIntElementAndEditorSample
          end
          object PageCtrlEditorPageCtrlColForegroundVal: TColorBox
            Left = 200
            Top = 30
            Width = 129
            Height = 22
            Selected = clWindowText
            ItemHeight = 16
            TabOrder = 2
            OnChange = UpdateIntElementAndEditorSample
          end
          object PageCtrlEditorPageCtrlColBoldCB: TCheckBox
            Left = 132
            Top = 56
            Width = 41
            Height = 17
            Caption = 'B&old'
            TabOrder = 3
            OnClick = UpdateIntElementAndEditorSample
          end
          object PageCtrlEditorPageCtrlColItalicCB: TCheckBox
            Left = 132
            Top = 74
            Width = 41
            Height = 17
            Caption = '&Italic'
            TabOrder = 4
            OnClick = UpdateIntElementAndEditorSample
          end
          object PageCtrlEditorPageCtrlColUnderlineCB: TCheckBox
            Left = 132
            Top = 92
            Width = 65
            Height = 17
            Caption = '&Underline'
            TabOrder = 5
            OnClick = UpdateIntElementAndEditorSample
          end
          object PageCtrlEditorPageCtrlColEditorSample: TSyntaxEdit
            Left = 4
            Top = 111
            Width = 325
            Height = 134
            Cursor = crArrow
            Align = alCustom
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 6
            TabStop = False
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Terminal'
            Gutter.Font.Style = []
            Highlighter = SynCellMLSyn
            Lines.Strings = (
              'def model hodgkin_huxley_squid_axon_1952 as'
              '   def unit millisecond from'
              '      unit second {pref: milli};'
              '   enddef;'
              ''
              '   def comp sodium_channel as'
              '      var i_Na: microA_per_cm2 {pub: out};'
              '      var g_Na: milliS_per_cm2 {init: 120.0};'
              '      var E_Na: millivolt;'
              '      var time: millisecond {pub: in, priv: out};'
              '      var V: millivolt {pub: in, priv: out};'
              '      var E_R: millivolt {pub: in};'
              '      var m: dimensionless {priv: in};'
              '      var h: dimensionless {priv: in};'
              '      var x: dimensionless {priv: in};'
              ''
              '      E_Na = E_R+115.0{millivolt};'
              '      i_Na = g_Na*pow(m, 3.0{dimensionless})*h*(V-E_Na);'
              '   enddef;'
              'enddef;')
            Options = [eoAutoIndent, eoDragDropEditing, eoGroupUndo, eoNoCaret, eoNoSelection, eoRightMouseMovesCursor, eoScrollHintFollows, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces]
            ReadOnly = True
            NewFile = False
            RemovedKeystrokes = <
              item
                Command = ecContextHelp
                ShortCut = 112
              end>
            AddedKeystrokes = <
              item
                Command = ecContextHelp
                ShortCut = 16496
              end>
          end
          object PageCtrlEditorPageCtrlColAlphaSB: TScrollBar
            Left = 200
            Top = 75
            Width = 129
            Height = 17
            LargeChange = 10
            PageSize = 0
            TabOrder = 7
            TabStop = False
            OnChange = PageCtrlEditorPageCtrlColAlphaSBChange
          end
          object PageCtrlEditorPageCtrlColElementVal: TVSTListBox
            Left = 4
            Top = 25
            Width = 117
            Height = 82
            DefaultNodeHeight = 13
            Header.AutoSizeIndex = 0
            Header.DefaultHeight = 17
            Header.Font.Charset = DEFAULT_CHARSET
            Header.Font.Color = clWindowText
            Header.Font.Height = -11
            Header.Font.Name = 'Tahoma'
            Header.Font.Style = []
            Header.MainColumn = -1
            Header.Options = [hoColumnResize, hoDrag]
            Margin = 0
            ScrollBarOptions.HorizontalIncrement = 13
            ScrollBarOptions.VerticalIncrement = 13
            TabOrder = 0
            TextMargin = 2
            TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
            TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
            OnChange = PageCtrlEditorPageCtrlColElementValChange
            NodeIndex = -1
            Columns = <>
          end
        end
        object PageCtrlEditorPageCtrlCmdView: TTabSheet
          Caption = 'Command Viewer'
          ImageIndex = 3
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object PageCtrlEditorPageCtrlCmdViewSizeLab: TLabel
            Left = 4
            Top = 59
            Width = 23
            Height = 13
            Caption = '&Size:'
            FocusControl = PageCtrlEditorPageCtrlCmdViewSizeVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlCmdViewSubFontLab: TLabel
            Left = 150
            Top = 59
            Width = 45
            Height = 13
            Caption = 'Sub fo&nt:'
            FocusControl = PageCtrlEditorPageCtrlCmdViewSubFontPB
            Transparent = True
          end
          object PageCtrlEditorPageCtrlCmdViewSubFontVal: TLabel
            Left = 211
            Top = 59
            Width = 3
            Height = 13
            Transparent = True
          end
          object PageCtrlEditorPageCtrlCmdViewCommentLab: TLabel
            Left = 4
            Top = 119
            Width = 27
            Height = 13
            Caption = 'Note:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsUnderline]
            ParentFont = False
            WordWrap = True
          end
          object PageCtrlEditorPageCtrlCmdViewBackgroundLab: TLabel
            Left = 150
            Top = 8
            Width = 60
            Height = 13
            Caption = '&Background:'
            FocusControl = PageCtrlEditorPageCtrlCmdViewBackgroundVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlCmdViewForegroundLab: TLabel
            Left = 150
            Top = 34
            Width = 60
            Height = 13
            Caption = '&Foreground:'
            FocusControl = PageCtrlEditorPageCtrlCmdViewForegroundVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlCmdViewCommentVal: TLabel
            Left = 36
            Top = 119
            Width = 293
            Height = 27
            AutoSize = False
            Caption = 
              'use Alt+D and Alt+Shift+D within the editor to change the way th' +
              'e equation viewer is to format an equation'
            WordWrap = True
          end
          object PageCtrlEditorPageCtrlCmdViewFontLab: TLabel
            Left = 4
            Top = 34
            Width = 26
            Height = 13
            Caption = '&Font:'
            FocusControl = PageCtrlEditorPageCtrlCmdViewFontVal
            Transparent = True
          end
          object PageCtrlEditorPageCtrlCmdViewSubFontPB: TScrollBar
            Left = 246
            Top = 59
            Width = 83
            Height = 13
            LargeChange = 10
            Max = 90
            Min = 10
            PageSize = 1
            Position = 10
            TabOrder = 5
            TabStop = False
            OnChange = UpdateCmdGraphSample
          end
          object PageCtrlEditorPageCtrlCmdViewOptimisedFontSizeCB: TCheckBox
            Left = 4
            Top = 7
            Width = 109
            Height = 17
            Caption = '&Optimised font size'
            TabOrder = 0
            OnClick = UpdateCmdGraphSample
          end
          object PageCtrlEditorPageCtrlCmdViewSizeVal: TComboBox
            Left = 34
            Top = 55
            Width = 73
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 2
            OnChange = UpdateCmdGraphSample
          end
          object PageCtrlEditorPageCtrlCmdViewUnderscoreForSubCB: TCheckBox
            Left = 4
            Top = 80
            Width = 141
            Height = 17
            Caption = '&Underscore for subscripts'
            TabOrder = 6
            OnClick = UpdateCmdGraphSample
          end
          object PageCtrlEditorPageCtrlCmdViewGreekSymbolsCB: TCheckBox
            Left = 176
            Top = 80
            Width = 89
            Height = 17
            Caption = '&Greek symbols'
            TabOrder = 7
            OnClick = UpdateCmdGraphSample
          end
          object PageCtrlEditorPageCtrlCmdViewBackgroundVal: TColorBox
            Left = 216
            Top = 4
            Width = 113
            Height = 22
            Selected = clWindow
            ItemHeight = 16
            TabOrder = 3
            OnChange = UpdateCmdGraphSample
          end
          object PageCtrlEditorPageCtrlCmdViewForegroundVal: TColorBox
            Left = 216
            Top = 30
            Width = 113
            Height = 22
            Selected = clWindowText
            ItemHeight = 16
            TabOrder = 4
            OnChange = UpdateCmdGraphSample
          end
          object PageCtrlEditorPageCtrlCmdViewPanel: TPanel
            Left = 4
            Top = 150
            Width = 325
            Height = 95
            BevelOuter = bvLowered
            TabOrder = 9
            object PageCtrlEditorPageCtrlCmdViewPanelCmdGraph: TCmdGraph
              Left = 1
              Top = 1
              Width = 323
              Height = 93
              Align = alClient
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Times New Roman'
              Font.Style = []
            end
          end
          object PageCtrlEditorPageCtrlCmdViewDigitGroupingCB: TCheckBox
            Left = 4
            Top = 98
            Width = 85
            Height = 17
            Caption = '&Digit grouping'
            TabOrder = 8
            OnClick = UpdateCmdGraphSample
          end
          object PageCtrlEditorPageCtrlCmdViewFontVal: TComboBox
            Left = 34
            Top = 30
            Width = 109
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 1
            OnChange = UpdateCmdGraphSample
          end
        end
      end
    end
    object PageCtrlComp: TTabSheet
      Caption = 'Computation'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PageCtrlCompPageCtrl: TPageControl
        Left = 3
        Top = 5
        Width = 341
        Height = 277
        ActivePage = PageCtrlCompPageCtrlGen
        TabOrder = 0
        object PageCtrlCompPageCtrlGen: TTabSheet
          Caption = 'General'
          ImageIndex = 2
          object PageCtrlCompPageCtrlGenDurationLab: TLabel
            Left = 4
            Top = 7
            Width = 45
            Height = 13
            Caption = '&Duration:'
            FocusControl = PageCtrlCompPageCtrlGenDurationVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlGenDurationUnitLab: TLabel
            Left = 107
            Top = 7
            Width = 13
            Height = 13
            Caption = 'ms'
            Transparent = True
          end
          object PageCtrlCompPageCtrlGenOutputLab: TLabel
            Left = 4
            Top = 32
            Width = 38
            Height = 13
            Caption = '&Output:'
            FocusControl = PageCtrlCompPageCtrlGenOutputVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlGenOutputUnitLab: TLabel
            Left = 107
            Top = 32
            Width = 13
            Height = 13
            Caption = 'ms'
            Transparent = True
          end
          object PageCtrlCompPageCtrlGenSep: TBevel
            Left = 4
            Top = 54
            Width = 325
            Height = 2
            Shape = bsTopLine
          end
          object PageCtrlCompPageCtrlGenCompilerLab: TLabel
            Left = 4
            Top = 64
            Width = 45
            Height = 13
            Caption = 'Compiler:'
            FocusControl = PageCtrlCompPageCtrlGenCompilerVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlGenCompilerFrame: TBevel
            Left = 4
            Top = 85
            Width = 325
            Height = 60
            Shape = bsFrame
          end
          object PageCtrlCompPageCtrlGenCompilerLocationLab: TLabel
            Left = 8
            Top = 92
            Width = 44
            Height = 13
            Caption = '&Location:'
            FocusControl = PageCtrlCompPageCtrlGenCompilerLocationVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlGenCompilerNoteLab: TLabel
            Left = 8
            Top = 114
            Width = 27
            Height = 13
            Caption = 'Note:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsUnderline]
            ParentFont = False
            WordWrap = True
          end
          object PageCtrlCompPageCtrlGenCompilerNoteVal: TLabel
            Left = 41
            Top = 114
            Width = 284
            Height = 27
            AutoSize = False
            Caption = 
              'the internal compiler will automatically be used, should a probl' +
              'em occur with this compiler'
            WordWrap = True
          end
          object PageCtrlCompPageCtrlGenDurationVal: TJvValidateEdit
            Left = 59
            Top = 4
            Width = 45
            Height = 21
            CriticalPoints.MaxValueIncluded = False
            CriticalPoints.MinValueIncluded = False
            EditText = '0'
            TabOrder = 0
          end
          object PageCtrlCompPageCtrlGenOutputVal: TJvValidateEdit
            Left = 59
            Top = 29
            Width = 45
            Height = 21
            CriticalPoints.MaxValueIncluded = False
            CriticalPoints.MinValueIncluded = False
            EditText = '0'
            TabOrder = 1
          end
          object PageCtrlCompPageCtrlGenCompilerVal: TComboBox
            Left = 59
            Top = 60
            Width = 126
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 3
            OnChange = PageCtrlCompPageCtrlGenCompilerValChange
          end
          object PageCtrlCompPageCtrlGenCompilerLocationVal: TJvValidateEdit
            Left = 59
            Top = 89
            Width = 237
            Height = 21
            CriticalPoints.MaxValueIncluded = False
            CriticalPoints.MinValueIncluded = False
            EditText = '0'
            TabOrder = 4
            OnExit = PageCtrlCompPageCtrlGenCompilerLocationValExit
          end
          object PageCtrlCompPageCtrlGenCompilerLocationBtn: TButton
            Left = 300
            Top = 89
            Width = 25
            Height = 21
            Caption = '...'
            TabOrder = 5
            OnClick = PageCtrlCompPageCtrlGenCompilerLocationBtnClick
          end
          object PageCtrlCompPageCtrlGenDebugModeCB: TCheckBox
            Left = 173
            Top = 6
            Width = 80
            Height = 17
            Caption = 'De&bug mode'
            TabOrder = 2
            OnClick = UpdateIntElementAndEditorSample
          end
        end
        object PageCtrlCompPageCtrlInt: TTabSheet
          Caption = 'Integration'
          object PageCtrlCompPageCtrlIntIntegratorLab: TLabel
            Left = 4
            Top = 8
            Width = 54
            Height = 13
            Caption = '&Integrator:'
            FocusControl = PageCtrlCompPageCtrlIntIntegratorVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlIntFixedMaxTimeStepLab: TLabel
            Left = 4
            Top = 32
            Width = 131
            Height = 13
            Caption = 'Fixed / maximum &time step:'
            FocusControl = PageCtrlCompPageCtrlIntFixedTimeStepVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlIntFixedTimeStepUnitAndSepLab: TLabel
            Left = 199
            Top = 32
            Width = 23
            Height = 13
            Caption = 'ms  /'
            Transparent = True
          end
          object PageCtrlCompPageCtrlIntMaxTimeStepUnitLab: TLabel
            Left = 277
            Top = 32
            Width = 13
            Height = 13
            Caption = 'ms'
            Transparent = True
          end
          object PageCtrlCompPageCtrlIntMaxNbOfStepsLab: TLabel
            Left = 4
            Top = 57
            Width = 129
            Height = 13
            Caption = 'Maximum number of &steps:'
            FocusControl = PageCtrlCompPageCtrlIntMaxNbOfStepsVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlIntMethodLab: TLabel
            Left = 4
            Top = 83
            Width = 36
            Height = 13
            Caption = '&Mehod:'
            FocusControl = PageCtrlCompPageCtrlIntMethodVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlIntIteratorLab: TLabel
            Left = 4
            Top = 108
            Width = 42
            Height = 13
            Caption = 'It&erator:'
            FocusControl = PageCtrlCompPageCtrlIntIteratorVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlIntRelAbsTolLab: TLabel
            Left = 4
            Top = 207
            Width = 142
            Height = 13
            Caption = 'Relative / absolute t&olerance:'
            FocusControl = PageCtrlCompPageCtrlIntRelTolVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlIntRelAbsTolSepLab: TLabel
            Left = 199
            Top = 207
            Width = 7
            Height = 13
            Caption = ' /'
            Transparent = True
          end
          object PageCtrlCompPageCtrlIntLinearSolverLab: TLabel
            Left = 4
            Top = 133
            Width = 65
            Height = 13
            Caption = '&Linear solver:'
            FocusControl = PageCtrlCompPageCtrlIntLinearSolverVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlIntUpperLowerHalfBandwidthLab: TLabel
            Left = 4
            Top = 182
            Width = 144
            Height = 13
            Caption = 'Upper / lower &half-bandwidth:'
            FocusControl = PageCtrlCompPageCtrlIntUpperHalfBandwidthVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlIntUpperLowerHalfBandwidthSepLab: TLabel
            Left = 199
            Top = 182
            Width = 7
            Height = 13
            Caption = ' /'
            Transparent = True
          end
          object PageCtrlCompPageCtrlIntPreconditionerLab: TLabel
            Left = 4
            Top = 158
            Width = 73
            Height = 13
            Caption = '&Preconditioner:'
            FocusControl = PageCtrlCompPageCtrlIntPreconditionerVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlIntIntegratorVal: TComboBox
            Left = 151
            Top = 4
            Width = 141
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 0
            OnChange = UpdateCmdGraphSample
          end
          object PageCtrlCompPageCtrlIntFixedTimeStepVal: TJvValidateEdit
            Left = 151
            Top = 29
            Width = 45
            Height = 21
            CriticalPoints.MaxValueIncluded = False
            CriticalPoints.MinValueIncluded = False
            EditText = '0'
            TabOrder = 1
          end
          object PageCtrlCompPageCtrlIntMaxTimeStepVal: TJvValidateEdit
            Left = 229
            Top = 29
            Width = 45
            Height = 21
            CriticalPoints.MaxValueIncluded = False
            CriticalPoints.MinValueIncluded = False
            EditText = '0'
            TabOrder = 2
          end
          object PageCtrlCompPageCtrlIntMaxNbOfStepsVal: TJvValidateEdit
            Left = 151
            Top = 54
            Width = 45
            Height = 21
            CriticalPoints.MaxValueIncluded = False
            CriticalPoints.MinValueIncluded = False
            EditText = '0'
            TabOrder = 3
          end
          object PageCtrlCompPageCtrlIntMethodVal: TComboBox
            Left = 151
            Top = 79
            Width = 105
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 4
            OnChange = UpdateCmdGraphSample
          end
          object PageCtrlCompPageCtrlIntIteratorVal: TComboBox
            Left = 151
            Top = 104
            Width = 105
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 5
            OnChange = UpdateCmdGraphSample
          end
          object PageCtrlCompPageCtrlIntRelTolVal: TJvValidateEdit
            Left = 151
            Top = 204
            Width = 45
            Height = 21
            CriticalPoints.MaxValueIncluded = False
            CriticalPoints.MinValueIncluded = False
            EditText = '0'
            TabOrder = 10
          end
          object PageCtrlCompPageCtrlIntAbsTolVal: TJvValidateEdit
            Left = 213
            Top = 204
            Width = 45
            Height = 21
            CriticalPoints.MaxValueIncluded = False
            CriticalPoints.MinValueIncluded = False
            EditText = '0'
            TabOrder = 11
          end
          object PageCtrlCompPageCtrlIntLinearSolverVal: TComboBox
            Left = 151
            Top = 129
            Width = 105
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 6
            OnChange = UpdateCmdGraphSample
          end
          object PageCtrlCompPageCtrlIntUpperHalfBandwidthVal: TJvValidateEdit
            Left = 151
            Top = 179
            Width = 45
            Height = 21
            CriticalPoints.MaxValueIncluded = False
            CriticalPoints.MinValueIncluded = False
            EditText = '0'
            TabOrder = 8
          end
          object PageCtrlCompPageCtrlIntLowerHalfBandwidthVal: TJvValidateEdit
            Left = 213
            Top = 179
            Width = 45
            Height = 21
            CriticalPoints.MaxValueIncluded = False
            CriticalPoints.MinValueIncluded = False
            EditText = '0'
            TabOrder = 9
          end
          object PageCtrlCompPageCtrlIntPreconditionerVal: TComboBox
            Left = 151
            Top = 154
            Width = 105
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 7
            OnChange = UpdateCmdGraphSample
          end
        end
        object PageCtrlCompPageCtrlGraphPanel: TTabSheet
          Caption = 'Graphical Panel'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object PageCtrlCompPageCtrlGraphPanelBackgroundLab: TLabel
            Left = 150
            Top = 7
            Width = 60
            Height = 13
            Caption = '&Background:'
            FocusControl = PageCtrlCompPageCtrlGraphPanelBackgroundVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlGraphPanelAxesLab: TLabel
            Left = 182
            Top = 33
            Width = 28
            Height = 13
            Caption = '&Axes:'
            FocusControl = PageCtrlCompPageCtrlGraphPanelAxesVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlGraphPanelGridlinesLab: TLabel
            Left = 167
            Top = 59
            Width = 44
            Height = 13
            Caption = '&Gridlines:'
            FocusControl = PageCtrlCompPageCtrlGraphPanelGridlinesVal
            Transparent = True
          end
          object PageCtrlCompPageCtrlGraphPanelPanel: TPanel
            Left = 4
            Top = 84
            Width = 324
            Height = 162
            BevelKind = bkSoft
            BevelOuter = bvNone
            Caption = 'PageCtrlCompPageCtrlGraphPanelPanel'
            TabOrder = 6
            object PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel: TOGLGraphPanel
              Left = 0
              Top = 0
              Width = 320
              Height = 158
              WinAPI = True
              Align = alClient
              Color = clWhite
              ParentBackground = False
              TabOrder = 0
              XAxis.LineSpecs.Color = clWindowFrame
              XAxis.LineSpecs.Alpha = 1.000000000000000000
              XAxis.LineSpecs.Size = 2
              XAxis.LineSpecs.Style = psSolid
              XAxis.LineSpecs.Color = -16777210
              XAxis.Max = 10.000000000000000000
              XAxis.Length = 10.000000000000000000
              XAxis.Grid.LineSpecs.Color = cl3DDkShadow
              XAxis.Grid.LineSpecs.Alpha = 1.000000000000000000
              XAxis.Grid.LineSpecs.Style = psDot
              XAxis.Grid.LineSpecs.Color = -16777195
              XAxis.Grid.Interval = 1.000000000000000000
              XAxis.Grid.IntervalType = itAutomatic
              XAxis.Min = 0.000000000000000000
              XAxis.Max = 10.000000000000000000
              XAxis.Start = 0.000000000000000000
              XAxis.Length = 10.000000000000000000
              YAxis.LineSpecs.Color = clWindowFrame
              YAxis.LineSpecs.Alpha = 1.000000000000000000
              YAxis.LineSpecs.Size = 2
              YAxis.LineSpecs.Style = psSolid
              YAxis.LineSpecs.Color = -16777210
              YAxis.Min = -1.000000000000000000
              YAxis.Max = 1.000000000000000000
              YAxis.Start = -1.000000000000000000
              YAxis.Length = 2.000000000000000000
              YAxis.Grid.LineSpecs.Color = cl3DDkShadow
              YAxis.Grid.LineSpecs.Alpha = 1.000000000000000000
              YAxis.Grid.LineSpecs.Style = psDot
              YAxis.Grid.LineSpecs.Color = -16777195
              YAxis.Grid.Interval = 1.000000000000000000
              YAxis.Grid.IntervalType = itAutomatic
              YAxis.Min = -1.000000000000000000
              YAxis.Max = 1.000000000000000000
              YAxis.Start = -1.000000000000000000
              YAxis.Length = 2.000000000000000000
              Coords.LineSpecs.Color = clTeal
              Coords.LineSpecs.Alpha = 1.000000000000000000
              Coords.LineSpecs.Style = psDash
              Coords.LineSpecs.Color = 8421376
              Coords.BothAxes = False
              Dims.LineSpecs.Color = clPurple
              Dims.LineSpecs.Alpha = 1.000000000000000000
              Dims.LineSpecs.Style = psSolid
              Dims.LineSpecs.Color = 8388736
              Region.LineSpecs.Color = clMaroon
              Region.LineSpecs.Alpha = 1.000000000000000000
              Region.LineSpecs.Style = psSolid
              Region.LineSpecs.Color = 128
              Region.FillingSpecs.Color = clYellow
              Region.FillingSpecs.Alpha = 0.150000005960464500
              Region.FillingSpecs.Color = 65535
              AutoYRange = True
            end
          end
          object PageCtrlCompPageCtrlGraphPanelUseGradientForTracesCB: TCheckBox
            Left = 4
            Top = 6
            Width = 127
            Height = 17
            Caption = 'Use gradient for &traces'
            TabOrder = 0
          end
          object PageCtrlCompPageCtrlGraphPanelShowAxesCB: TCheckBox
            Left = 4
            Top = 24
            Width = 69
            Height = 17
            Caption = 'Show a&xes'
            TabOrder = 1
            OnClick = PageCtrlCompPageCtrlGraphPanelShowAxesCBClick
          end
          object PageCtrlCompPageCtrlGraphPanelBackgroundVal: TColorBox
            Left = 216
            Top = 4
            Width = 113
            Height = 22
            Selected = clWhite
            ItemHeight = 16
            TabOrder = 3
            OnChange = PageCtrlCompPageCtrlGraphPanelBackgroundValChange
          end
          object PageCtrlCompPageCtrlGraphPanelAxesVal: TColorBox
            Left = 216
            Top = 30
            Width = 113
            Height = 22
            ItemHeight = 16
            TabOrder = 4
            OnChange = PageCtrlCompPageCtrlGraphPanelAxesValChange
          end
          object PageCtrlCompPageCtrlGraphPanelGridlinesVal: TColorBox
            Left = 217
            Top = 56
            Width = 113
            Height = 22
            Selected = clGray
            ItemHeight = 16
            TabOrder = 5
            OnChange = PageCtrlCompPageCtrlGraphPanelGridlinesValChange
          end
          object PageCtrlCompPageCtrlGraphPanelShowGridlinesCB: TCheckBox
            Left = 3
            Top = 42
            Width = 85
            Height = 17
            Caption = 'Show g&ridlines'
            TabOrder = 2
            OnClick = PageCtrlCompPageCtrlGraphPanelShowGridlinesCBClick
          end
          object PageCtrlCompPageCtrlGraphPanelShowLabelsCB: TCheckBox
            Left = 3
            Top = 60
            Width = 72
            Height = 17
            Caption = 'Show &labels'
            TabOrder = 7
            OnClick = PageCtrlCompPageCtrlGraphPanelShowLabelsCBClick
          end
        end
      end
    end
  end
  object OKBtn: TButton
    Left = 101
    Top = 321
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 189
    Top = 321
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object SynCellMLSyn: TSynCellMLSyn
    Left = 328
    Top = 4
  end
end
